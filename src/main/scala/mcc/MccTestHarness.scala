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

  val memWords = memBytes / 4

  // Bus geometry for mcc's own dmem port: 32-bit CPU address space, 4-byte
  // (one-word) beats, single in-flight transaction. Plain val (not implicit)
  // to avoid ambiguity with atanBusConf below as the ATA8.MemBusConfig implicit.
  val coreBusConf: ATA8.MemBusConfig =
    ATA8.Configuration(bus = ATA8.BusParams(dataBusSize = 4, addrWidth = 32, sourceWidth = 1))

  val io = IO(new Bundle {
    val success    = Output(Bool())
    val tohost     = Output(UInt(32.W))
    val cycles     = Output(UInt(64.W))
    // Test-only mirror of the core's dmem TileLink bus: chisel3.simulator's
    // peek/poke only sees the top-level DUT's own IO, so internal submodule
    // signals like core.io.dmem aren't peekable directly from a test.
    val dmemMirror = Output(new ATA8.TilelinkPort(coreBusConf.tlBus))
  })

  val core = Module(new mcc.stage5.Core()(p, conf, coreBusConf))

  // Field-by-field (not a bundle-level :=): io.dmemMirror is Output-wrapped,
  // so its `a`/`d` sub-bundles lose the Decoupled/Flipped-ness that
  // core.io.dmem.a/d have, and a bulk connect between differently-flipped
  // bundle types is rejected by firtool.
  io.dmemMirror.a.valid := core.io.dmem.a.valid
  io.dmemMirror.a.ready := core.io.dmem.a.ready
  io.dmemMirror.a.bits  := core.io.dmem.a.bits
  io.dmemMirror.d.valid := core.io.dmem.d.valid
  io.dmemMirror.d.ready := core.io.dmem.d.ready
  io.dmemMirror.d.bits  := core.io.dmem.d.bits

  val cycleCount = RegInit(0.U(64.W))
  cycleCount := cycleCount + 1.U
  io.cycles := cycleCount

  core.io.ddpath    := DontCare
  core.io.dcpath    := DontCare
  core.io.interrupt := 0.U.asTypeOf(new CoreInterrupts(false))
  core.io.hartid    := 0.U
  core.io.reset_vector := baseAddr.U

  // ── Instruction memory: async Mem + ROM init ──────────────────────────────
  // The data-memory path below uses SyncReadMem, which can't serve
  // same-cycle instruction fetches; keep a separate async Mem for imem.
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

  // ── Data memory ───────────────────────────────────────────────────────────
  implicit val _coreBusConf: ATA8.MemBusConfig = coreBusConf

  val dataHandler = Module(new ATA8.TLScratchpadHandler(
    ATA8.TLScratchConfig(read = true, write = true, atomic = true, tlConfig = coreBusConf.tlBus)
  ))

  val spmConfig = ATA8.SPMConfig(bankDepth = memBytes / coreBusConf.dataBusSize,
                            writeports = 1,
                            readports = 1)

  val dataMem = Module(new ATA8.MemTierScratchpad(spmConfig)(coreBusConf))

  dataHandler.io.wMem.get <> dataMem.io.Writeport(0)
  dataHandler.io.rMem.get <> dataMem.io.Readport(0)

  // Single dedicated client: no shared-reservation system needed.
  dataHandler.io.amoReserve.get.ready   := true.B
  dataHandler.io.reserveIn.get(0).valid := false.B
  dataHandler.io.reserveIn.get(0).bits  := DontCare

  // ── Semaphore System ──────────────────────────────────────────────────────
  // ATAN-side (16-bit local address) bus geometry shared by TLXbar and
  // SemSystem. Plain val (not implicit) to avoid shadowing coreBusConf as
  // the ambient ATA8.MemBusConfig implicit.
  val atanBusConf = ATA8.Configuration(
    bus       = ATA8.BusParams(dataBusSize = 4, addrWidth = 16, sourceWidth = 4),
    semaphore = ATA8.SemaphoreParams(nSemaphores = 1, queueSize = 2, generationWidth = 0),
  )

  // Semaphore 0 mapped at harness offset 0x4000 (mcc address = baseAddr + 0x4000).
  // Data memory occupies offsets 0x0000 – (memBytes-1); SemSystem starts at 0x4000.
  //   word 0 (byte offset 0x4000): full register
  //   word 1 (byte offset 0x4004): empty register
  val SemSys = Module(new ATA8.SemSystem(1)(atanBusConf))

  // TLXbar routes mcc dmem to the data memory (slave 0) or SemSystem (slave 1) by address.
  //   slave 0: (base=0,      mask=memBytes-1) → offsets 0x0000 – (memBytes-1)
  //   slave 1: (base=0x4000, mask=0x00FF)     → offsets 0x4000 – 0x40FF
  val hostDemux = Module(new ATA8.TLXbar(ATA8.TLXbarConfig(
    nMasters = 1,
    slaves   = Seq(
      ATA8.TLSlaveConfig(addressSet = Seq((BigInt(0), BigInt(memBytes - 1)))),
      ATA8.TLSlaveConfig(addressSet = Seq((BigInt(0x4000),  BigInt(0x00FF)))),
    ),
    tl       = atanBusConf.tlBus,
  )))

  // ── mcc dmem ↔ TLXbar ────────────────────────────────────────────────────
  // core.io.dmem and hostDemux.io.in(0) are both ATA8.TilelinkPort now, so a
  // plain <> handles every field except the ones that genuinely differ:
  // address (mcc's 32-bit CPU address needs rebasing into ATAN's 16-bit local
  // window) and gating the A channel on ROM-load completion.
  val mccA = core.io.dmem.a

  core.io.dmem <> hostDemux.io.in(0)

  hostDemux.io.in(0).a.bits.address := (mccA.bits.address - baseAddr.U)(15, 0)
  hostDemux.io.in(0).a.valid        := mccA.valid && initDone
  mccA.ready                        := hostDemux.io.in(0).a.ready && initDone

  // ── TLXbar out(0) → data memory ───────────────────────────────────────────
  hostDemux.io.out(0) <> dataHandler.io.tl

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
    (mccA.bits.opcode === ATA8.TilelinkOpcodes.PutFullData ||
     mccA.bits.opcode === ATA8.TilelinkOpcodes.PutPartialData) &&
    (mccA.bits.address - baseAddr.U) === tohostOff.U

  val tohostReg = RegInit(0.U(32.W))
  when(isTohostWrite) { tohostReg := mccA.bits.data }

  io.tohost  := tohostReg
  io.success := tohostReg === 1.U
}
