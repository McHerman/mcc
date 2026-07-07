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
  memBytes:   Int              = 65536,
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
    sourceWidth   = 1,
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

  // ── Data memory: ATAN MemTier (atomic=true) ───────────────────────────────
  val memTier = Module(new ATA8.MemTier(mc.tiers.head, nRwPorts = 0))

  // A channel: mcc TilelinkPort → ATAN TilelinkPort
  // - address: strip baseAddr to get in-scratchpad byte offset
  // - size: always send dataBusSize (full word); sub-word extraction done below
  val mccA = core.io.dmem.a
  memTier.io.hostIn.a.valid        := mccA.valid && initDone
  memTier.io.hostIn.a.bits.opcode  := mccA.bits.opcode
  memTier.io.hostIn.a.bits.param   := mccA.bits.param
  memTier.io.hostIn.a.bits.size    := mc.dataBusSize.U
  memTier.io.hostIn.a.bits.source  := mccA.bits.source
  memTier.io.hostIn.a.bits.address := (mccA.bits.address - baseAddr.U)(15, 0)
  memTier.io.hostIn.a.bits.mask    := mccA.bits.mask
  memTier.io.hostIn.a.bits.data    := mccA.bits.data
  memTier.io.hostIn.a.bits.corrupt := 0.U
  mccA.ready := memTier.io.hostIn.a.ready && initDone

  // D channel: pass raw 32-bit word from MemTier straight to core.
  // Sub-word shift/sign-extension is handled in mcc's memory stage using ctrl_mem_typ.
  core.io.dmem.d.valid        := memTier.io.hostIn.d.valid
  core.io.dmem.d.bits.opcode  := memTier.io.hostIn.d.bits.opcode
  core.io.dmem.d.bits.param   := memTier.io.hostIn.d.bits.param
  core.io.dmem.d.bits.size    := memTier.io.hostIn.d.bits.size
  core.io.dmem.d.bits.source  := memTier.io.hostIn.d.bits.source
  core.io.dmem.d.bits.sink    := 0.U
  core.io.dmem.d.bits.denied  := 0.U
  core.io.dmem.d.bits.data    := memTier.io.hostIn.d.bits.data
  core.io.dmem.d.bits.corrupt := 0.U
  memTier.io.hostIn.d.ready   := core.io.dmem.d.ready

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
