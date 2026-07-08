package mcc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import mcc.common._
import mcc.common.Constants._
import freechips.rocketchip.rocket.CoreInterrupts

object MccHexReader {
  def apply(path: String): Map[Int, BigInt] = {
    val src = scala.io.Source.fromFile(path)
    val result = scala.collection.mutable.Map[Int, BigInt]()
    var addr = 0
    for (line <- src.getLines()) {
      val trimmed = line.trim
      if (trimmed.startsWith("@")) {
        addr = Integer.parseInt(trimmed.drop(1), 16)
      } else if (trimmed.nonEmpty && !trimmed.startsWith("//")) {
        result(addr) = BigInt(trimmed, 16)
        addr += 1
      }
    }
    src.close()
    result.toMap
  }
}

class MccTestHarness(
  memBytes:   Int              = 16384,
  baseAddr:   Long             = 0x80000000L,
  tohostAddr: Long             = 0x80001000L,
  initData:   Map[Int, BigInt] = Map.empty
)(implicit p: Parameters) extends Module {

  implicit val conf: MccCoreParams = MccCoreParams(xprlen = 32)

  // ATAN MemTier config: 4-byte bus, 1 tier with atomic support on hostIn.
  // nBanks=2 required by MemTierScratchpad (log2Ceil needs at least 1 bit).
  val memWords = memBytes / 4
  implicit val mc: ATA8.MemSystemConfig = ATA8.MemSystemConfig(
    tiers         = Seq(ATA8.TierConfig(
                      nWritePorts = 0,
                      nReadPorts  = 0,
                      nBanks      = 2,
                      bankDepth   = memWords / 2,
                      atomic      = true)),
    dataBusSize   = 4,
    arithDataWidth = 8,
    addrWidth     = 16,
    sourceWidth   = 4,   // TLXbar allocates 10 IDs/master → rounded to 16 → needs log2Ceil(16)=4 bits
  )

  val io = IO(new Bundle {
    val success = Output(Bool())
    val tohost  = Output(UInt(32.W))
    val cycles  = Output(UInt(64.W))
  })

  val core = Module(new mcc.stage5.Core())

  val cycleCount = RegInit(0.U(64.W))
  cycleCount := cycleCount + 1.U
  io.cycles := cycleCount

  core.io.ddpath    := DontCare
  core.io.dcpath    := DontCare
  core.io.interrupt := 0.U.asTypeOf(new CoreInterrupts(false))
  core.io.hartid    := 0.U
  core.io.reset_vector := baseAddr.U

  // ── Instruction memory: async Mem + ROM init ──────────────────────────────
  // MemTier uses SyncReadMem so can't serve same-cycle instruction fetches;
  // keep a separate async Mem for imem.
  val addrBits = log2Ceil(memBytes)
  def wordIdx(addr: UInt): UInt = addr(addrBits - 1, 2)

  val imem = Mem(memWords, Vec(4, UInt(8.W)))

  val initDone: Bool = if (initData.nonEmpty) {
    val entries  = initData.toSeq.sortBy(_._1)
    val romAddrs = VecInit(entries.map { case (a, _) => a.U(log2Ceil(memWords).W) })
    val romData  = VecInit(entries.map { case (_, d) =>
      VecInit(Seq(
        ((d >>  0) & 0xFF).U(8.W),
        ((d >>  8) & 0xFF).U(8.W),
        ((d >> 16) & 0xFF).U(8.W),
        ((d >> 24) & 0xFF).U(8.W),
      ))
    })
    val idx  = RegInit(0.U(log2Ceil(entries.size + 1).W))
    val done = idx === entries.size.U
    when(!reset.asBool && !done) {
      imem.write(romAddrs(idx), romData(idx))
      idx := idx + 1.U
    }
    done
  } else true.B

  core.io.imem.req.ready := true.B
  val imem_rdata = imem.read(wordIdx(core.io.imem.req.bits.addr))
  core.io.imem.resp.valid     := initDone && core.io.imem.req.valid
  core.io.imem.resp.bits.data := Cat(imem_rdata(3), imem_rdata(2), imem_rdata(1), imem_rdata(0))

  // ── Data memory + Semaphore System ───────────────────────────────────────
  val memTier = Module(new ATA8.MemTier(mc.tiers.head, nRwPorts = 0))

  // ATA8.Configuration for SemSystem — same bus geometry as mc.
  // Plain val (not implicit) to avoid shadowing mc as the MemBusConfig implicit.
  val semConf = ATA8.Configuration(
    bus       = ATA8.BusParams(dataBusSize = 4, addrWidth = 16, sourceWidth = 4),
    semaphore = ATA8.SemaphoreParams(nSemaphores = 1, queueSize = 2, generationWidth = 0),
  )

  // Semaphore 0 mapped at harness offset 0x4000 (mcc address = baseAddr + 0x4000).
  // MemTier occupies offsets 0x0000 – 0x3FFF; SemSystem starts at 0x4000.
  //   word 0 (byte offset 0x4000): full register
  //   word 1 (byte offset 0x4004): empty register
  val SemSys = Module(new ATA8.SemSystem(1)(semConf))

  // TLXbar routes mcc dmem to MemTier (slave 0) or SemSystem (slave 1) by address.
  //   slave 0: (base=0,      mask=0x3FFF) → offsets 0x0000 – 0x3FFF
  //   slave 1: (base=0x4000, mask=0x00FF) → offsets 0x4000 – 0x40FF
  val hostDemux = Module(new ATA8.TLXbar(ATA8.TLXbarConfig(
    nMasters = 1,
    slaves   = Seq(
      ATA8.TLSlaveConfig(addressSet = Seq((mc.tierBases(0), mc.tierSizes(0) - 1))),
      ATA8.TLSlaveConfig(addressSet = Seq((BigInt(0x4000),  BigInt(0x00FF)))),
    )
  )))

  // ── A channel: mcc → TLXbar ──────────────────────────────────────────────
  val mccA      = core.io.dmem.a
  val mccOffset = (mccA.bits.address - baseAddr.U)(15, 0)

  hostDemux.io.in(0).a.valid        := mccA.valid && initDone
  hostDemux.io.in(0).a.bits.opcode  := mccA.bits.opcode
  hostDemux.io.in(0).a.bits.param   := mccA.bits.param
  hostDemux.io.in(0).a.bits.size    := mc.dataBusSize.U
  hostDemux.io.in(0).a.bits.source  := mccA.bits.source
  hostDemux.io.in(0).a.bits.address := mccOffset
  hostDemux.io.in(0).a.bits.mask    := mccA.bits.mask
  hostDemux.io.in(0).a.bits.data    := mccA.bits.data
  hostDemux.io.in(0).a.bits.corrupt := 0.U
  mccA.ready := hostDemux.io.in(0).a.ready && initDone

  // ── D channel: TLXbar → mcc ──────────────────────────────────────────────
  core.io.dmem.d.valid        := hostDemux.io.in(0).d.valid
  core.io.dmem.d.bits.opcode  := hostDemux.io.in(0).d.bits.opcode
  core.io.dmem.d.bits.param   := hostDemux.io.in(0).d.bits.param
  core.io.dmem.d.bits.size    := hostDemux.io.in(0).d.bits.size
  core.io.dmem.d.bits.source  := hostDemux.io.in(0).d.bits.source
  core.io.dmem.d.bits.sink    := 0.U
  core.io.dmem.d.bits.denied  := 0.U
  core.io.dmem.d.bits.data    := hostDemux.io.in(0).d.bits.data
  core.io.dmem.d.bits.corrupt := 0.U
  hostDemux.io.in(0).d.ready  := core.io.dmem.d.ready

  // ── TLXbar out(0) → MemTier ──────────────────────────────────────────────
  hostDemux.io.out(0) <> memTier.io.hostIn

  // ── TLXbar out(1) → SemSystem ────────────────────────────────────────────
  // Address transform: strip semBase then convert byte → word index.
  // addr 0x4000 → bit[7:2] = 0 → full reg; addr 0x4004 → bit[7:2] = 1 → empty reg.
  SemSys.io.inPorts(0).a.valid        := hostDemux.io.out(1).a.valid
  SemSys.io.inPorts(0).a.bits.opcode  := hostDemux.io.out(1).a.bits.opcode
  SemSys.io.inPorts(0).a.bits.param   := hostDemux.io.out(1).a.bits.param
  SemSys.io.inPorts(0).a.bits.size    := hostDemux.io.out(1).a.bits.size
  SemSys.io.inPorts(0).a.bits.source  := hostDemux.io.out(1).a.bits.source
  SemSys.io.inPorts(0).a.bits.address := hostDemux.io.out(1).a.bits.address(7, 2)
  SemSys.io.inPorts(0).a.bits.mask    := hostDemux.io.out(1).a.bits.mask
  SemSys.io.inPorts(0).a.bits.data    := hostDemux.io.out(1).a.bits.data
  SemSys.io.inPorts(0).a.bits.corrupt := 0.U
  hostDemux.io.out(1).a.ready         := SemSys.io.inPorts(0).a.ready

  hostDemux.io.out(1).d.valid         := SemSys.io.inPorts(0).d.valid
  hostDemux.io.out(1).d.bits.opcode   := SemSys.io.inPorts(0).d.bits.opcode
  hostDemux.io.out(1).d.bits.param    := SemSys.io.inPorts(0).d.bits.param
  hostDemux.io.out(1).d.bits.size     := SemSys.io.inPorts(0).d.bits.size
  hostDemux.io.out(1).d.bits.source   := SemSys.io.inPorts(0).d.bits.source
  hostDemux.io.out(1).d.bits.sink     := 0.U
  hostDemux.io.out(1).d.bits.denied   := 0.U
  hostDemux.io.out(1).d.bits.data     := SemSys.io.inPorts(0).d.bits.data
  hostDemux.io.out(1).d.bits.corrupt  := 0.U
  SemSys.io.inPorts(0).d.ready        := hostDemux.io.out(1).d.ready

  // ── Semaphore initialisation ──────────────────────────────────────────────
  // Push one SemProgInst on the first cycle after reset: programs semaphore 0
  // with full=64, empty=0. The TriggerSystem fires it in ~4 cycles, well before
  // mcc starts executing (initDone is asserted after ~1026 ROM-load cycles).
  val semInitDone = RegInit(false.B)
  SemSys.io.instructionStream.valid                   := !semInitDone
  SemSys.io.instructionStream.bits                    := DontCare
  SemSys.io.instructionStream.bits.payload.semAddr    := 0.U
  SemSys.io.instructionStream.bits.payload.initFull   := 64.U
  SemSys.io.instructionStream.bits.payload.initEmpty  := 0.U
  SemSys.io.instructionStream.bits.payload.generation := 0.U
  SemSys.io.instructionStream.bits.payload.eventMode  := ATA8.SemEventModes.RW
  SemSys.io.instructionStream.bits.row.depCount       := 0.U
  when(SemSys.io.instructionStream.fire) { semInitDone := true.B }

  // ── tohost detection ──────────────────────────────────────────────────────
  val tohostOff = (tohostAddr - baseAddr).toInt
  val isTohostWrite = mccA.fire &&
    (mccA.bits.opcode === mcc.common.TilelinkOpcodes.PutFullData ||
     mccA.bits.opcode === mcc.common.TilelinkOpcodes.PutPartialData) &&
    (mccA.bits.address - baseAddr.U) === tohostOff.U

  val tohostReg = RegInit(0.U(32.W))
  when(isTohostWrite) { tohostReg := mccA.bits.data }

  io.tohost  := tohostReg
  io.success := tohostReg === 1.U
}
