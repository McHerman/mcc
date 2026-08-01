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
  memBytes:     Int              = 16384,
  baseAddr:     Long             = 0x80000000L,
  tohostAddr:   Long             = 0x80001000L,
  initData:     Map[Int, BigInt] = Map.empty,
  // dataMem (below) is a private scratchpad, NOT shared with imem's memory
  // image -- a C program's `static const` globals live in .rodata, which
  // the linker places in the same unified 0x80000000-based image as .text,
  // but only imem ever gets loaded with that image. Any load/store the CPU
  // issues for such data goes to dataMem instead, which is otherwise never
  // initialised (reads back as garbage/zero). Pass the same word-indexed
  // map (from MccHexReader) here too so those constants are visible on the
  // dmem side at the same addresses.
  dataInitData: Map[Int, BigInt] = Map.empty,
)(implicit p: Parameters) extends Module {

  implicit val conf: MccCoreParams = MccCoreParams(xprlen = 32)

  val memWords = memBytes / 4

  private case class SimpleMemBusConfig(
    dataBusSize: Int, arithDataWidth: Int, addrWidth: Int, sourceWidth: Int
  ) extends ATA8.MemBusConfig
  val coreBusConf: ATA8.MemBusConfig =
    SimpleMemBusConfig(dataBusSize = 4, arithDataWidth = 8, addrWidth = 32, sourceWidth = 1)

  val atanBusConf = ATA8.Configuration(
    bus       = ATA8.BusParams(dataBusSize = 8, addrWidth = 16, sourceWidth = 4),
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
    val hostIn  = Flipped(new ATA8.TilelinkPort(coreBusConf.tlBus))
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

  imemMod.io.host <> io.hostIn


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

  val dataEntries = dataInitData.toSeq.sortBy(_._1)

  val dataPreloadValid  = Wire(Bool())
  val dataPreloadBits   = Wire(chiselTypeOf(dataHandler.io.tl.a.bits))
  val dataPreloadDReady = Wire(Bool())

  val dataInitDone: Bool = if (dataEntries.nonEmpty) {
    val romAddrs = VecInit(dataEntries.map { case (a, _) => (a * 4).U(coreBusConf.addrWidth.W) })
    val romData  = VecInit(dataEntries.map { case (_, d) => d.U(32.W) })
    val loadIdx  = RegInit(0.U(log2Ceil(dataEntries.size + 1).W))
    val done     = loadIdx === dataEntries.size.U

    dataPreloadValid          := !reset.asBool && !done
    dataPreloadBits.opcode    := ATA8.TilelinkOpcodes.PutFullData
    dataPreloadBits.param     := 0.U
    dataPreloadBits.size      := coreBusConf.dataBusSize.U
    dataPreloadBits.source    := 0.U
    dataPreloadBits.address   := romAddrs(loadIdx)
    dataPreloadBits.mask      := "b1111".U
    dataPreloadBits.data      := romData(loadIdx)
    dataPreloadBits.corrupt   := 0.U
    dataPreloadDReady         := true.B

    when(!done && dataHandler.io.tl.d.valid && dataHandler.io.tl.d.ready) { loadIdx := loadIdx + 1.U }

    done
  } else {
    dataPreloadValid := false.B
    dataPreloadBits  := DontCare
    dataPreloadDReady  := true.B
    true.B
  }

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

  val mccA = core.io.dmem.a

  core.io.dmem <> hostDemux.io.in(0)

  hostDemux.io.in(0).a.bits.address := (mccA.bits.address - baseAddr.U)(15, 0)
  hostDemux.io.in(0).a.valid        := mccA.valid && initDone && dataInitDone
  mccA.ready                        := hostDemux.io.in(0).a.ready && initDone && dataInitDone

  when(!dataInitDone) {
    dataHandler.io.tl.a.valid   := dataPreloadValid
    dataHandler.io.tl.a.bits    := dataPreloadBits
    dataHandler.io.tl.d.ready   := dataPreloadDReady
    hostDemux.io.out(0).a.ready := false.B
    hostDemux.io.out(0).d.valid := false.B
    hostDemux.io.out(0).d.bits  := DontCare
  }.otherwise {
    dataHandler.io.tl.a.valid   := hostDemux.io.out(0).a.valid
    dataHandler.io.tl.a.bits    := hostDemux.io.out(0).a.bits
    dataHandler.io.tl.d.ready   := hostDemux.io.out(0).d.ready
    hostDemux.io.out(0).a.ready := dataHandler.io.tl.a.ready
    hostDemux.io.out(0).d.valid := dataHandler.io.tl.d.valid
    hostDemux.io.out(0).d.bits  := dataHandler.io.tl.d.bits
  }

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
