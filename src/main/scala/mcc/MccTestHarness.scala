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

  // ATAN-side (16-bit local address) bus geometry shared by TLXbar and
  // SemSystem. Plain val (not implicit) to avoid shadowing coreBusConf as
  // the ambient ATA8.MemBusConfig implicit; passed explicitly wherever a
  // full ATA8.Configuration (not just MemBusConfig) is needed, e.g.
  // SemaphoreProgPort below.
  val atanBusConf = ATA8.Configuration(
    bus       = ATA8.BusParams(dataBusSize = 4, addrWidth = 16, sourceWidth = 4),
    semaphore = ATA8.SemaphoreParams(nSemaphores = 16, queueSize = 2, generationWidth = 2),
  )

  val io = IO(new Bundle {
    val success    = Output(Bool())
    val tohost     = Output(UInt(32.W))
    val cycles     = Output(UInt(64.W))
    val semProgPort = Flipped(Decoupled(new ATA8.SemaphoreProgPort()(atanBusConf)))
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

  // ── Instruction memory: synchronous, TileLink-backed, non-blocking fetch ──
  // See mcc.ImemTL: a private MemTierScratchpad bank, a normal
  // TLScratchpadHandler for the boot-time ROM-preload path, and a purpose-
  // built pipelined Get-only responder (2 outstanding requests, alternating
  // source id 0/1) for the fetch-critical read path, so Fetch can issue
  // back-to-back requests instead of stalling on every fetch's latency.
  val imemMod = Module(new ImemTL(memWords, initData)(coreBusConf))

  val mccImemA = core.io.imem.a

  imemMod.io.tl.a.valid        := mccImemA.valid
  imemMod.io.tl.a.bits.opcode  := mccImemA.bits.opcode
  imemMod.io.tl.a.bits.param   := mccImemA.bits.param
  imemMod.io.tl.a.bits.size    := mccImemA.bits.size
  imemMod.io.tl.a.bits.source  := mccImemA.bits.source
  imemMod.io.tl.a.bits.address := (mccImemA.bits.address - baseAddr.U)(15, 0)
  imemMod.io.tl.a.bits.mask    := mccImemA.bits.mask
  imemMod.io.tl.a.bits.data    := mccImemA.bits.data
  imemMod.io.tl.a.bits.corrupt := mccImemA.bits.corrupt
  mccImemA.ready               := imemMod.io.tl.a.ready

  core.io.imem.d.valid        := imemMod.io.tl.d.valid
  core.io.imem.d.bits.opcode  := imemMod.io.tl.d.bits.opcode
  core.io.imem.d.bits.param   := imemMod.io.tl.d.bits.param
  core.io.imem.d.bits.size    := imemMod.io.tl.d.bits.size
  core.io.imem.d.bits.source  := imemMod.io.tl.d.bits.source
  core.io.imem.d.bits.sink    := imemMod.io.tl.d.bits.sink
  core.io.imem.d.bits.denied  := imemMod.io.tl.d.bits.denied
  core.io.imem.d.bits.data    := imemMod.io.tl.d.bits.data
  core.io.imem.d.bits.corrupt := imemMod.io.tl.d.bits.corrupt
  imemMod.io.tl.d.ready       := core.io.imem.d.ready

  val initDone: Bool = imemMod.io.initDone

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
  // Semaphores mapped starting at harness offset 0x4000 (mcc address =
  // baseAddr + 0x4000). Data memory occupies offsets 0x0000 – (memBytes-1);
  // SemSystem starts at 0x4000. Addressing is byte-indexed (matching the
  // literal addresses mcc issues): each semaphore-port occupies
  // `8 << genWidth` bytes (full at +0, empty at +4 for genWidth=0, matching
  // what the C test programs hardcode), so 16 semaphores × 2 ports × 8
  // bytes/port = 256 bytes total.
  val SemSys = Module(new ATA8.SemaphoreBank(1)(atanBusConf))

  SemSys.io.eventPort.ready := true.B

  // TLXbar routes mcc dmem to the data memory (slave 0) or SemSystem (slave 1) by address.
  //   slave 0: (base=0,      mask=memBytes-1) → offsets 0x0000 – (memBytes-1)
  //   slave 1: (base=0x4000, mask=semRegionMask) → offsets 0x4000 – 0x40FF
  val semRegionMask = BigInt(0x00FF)
  val hostDemux = Module(new ATA8.TLXbar(ATA8.TLXbarConfig(
    nMasters = 1,
    slaves   = Seq(
      ATA8.TLSlaveConfig(addressSet = Seq((BigInt(0), BigInt(memBytes - 1)))),
      ATA8.TLSlaveConfig(addressSet = Seq((BigInt(0x4000),  semRegionMask))),
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
  // Mask off the semaphore-region base the same way MemSystem.scala's own
  // hostDemux does for its tiers ("mask the output address so each tier
  // sees local (zero-based) addresses") - TLXbar itself doesn't rebase, so
  // callers do it themselves. No shift: SemaphoreBank is byte-indexed, so
  // the literal address mcc issues (rebased to zero) goes through as-is.
  SemSys.io.inPorts(0).a.valid        := hostDemux.io.out(1).a.valid
  SemSys.io.inPorts(0).a.bits.opcode  := hostDemux.io.out(1).a.bits.opcode
  SemSys.io.inPorts(0).a.bits.param   := hostDemux.io.out(1).a.bits.param
  SemSys.io.inPorts(0).a.bits.size    := hostDemux.io.out(1).a.bits.size
  SemSys.io.inPorts(0).a.bits.source  := hostDemux.io.out(1).a.bits.source
  //SemSys.io.inPorts(0).a.bits.address := hostDemux.io.out(1).a.bits.address - "h4000".U //Remove region offset  
  SemSys.io.inPorts(0).a.bits.address := hostDemux.io.out(1).a.bits.address ^ "h4000".U //Remove region offset  
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
  // Program semaphore 0 with full=64, empty=0 as soon as the prog port can
  // accept it, before handing the port over to io.semProgPort (used by tests
  // that (re)program semaphores themselves, e.g. MccLLVMTest). Restores the
  // boot-time auto-init the C test programs document/expect
  // ("Harness initialises full=64, empty=0 before mcc starts executing"),
  // now driven through SemaphoreBank's progPort instead of the old
  // (SemSystem-only) instructionStream.
  val semInitDone = RegInit(false.B)

  val semInitProg = Wire(new ATA8.SemaphoreProgPort()(atanBusConf))
  semInitProg.addr       := 0.U
  semInitProg.initFull   := 64.U
  semInitProg.initEmpty  := 0.U
  semInitProg.generation := 0.U
  semInitProg.eventMode  := ATA8.SemEventModes.RW

  SemSys.io.progPort.valid := Mux(!semInitDone, true.B, io.semProgPort.valid)
  SemSys.io.progPort.bits  := Mux(!semInitDone, semInitProg, io.semProgPort.bits)
  io.semProgPort.ready     := Mux(!semInitDone, false.B, SemSys.io.progPort.ready)

  when(!semInitDone && SemSys.io.progPort.fire) { semInitDone := true.B }

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
