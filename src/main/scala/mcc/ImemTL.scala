//**************************************************************************
// Synchronous, TileLink-backed instruction memory
//--------------------------------------------------------------------------
//
// Backed by its own private ATA8.MemTierScratchpad (a SyncReadMem-based
// bank, not shared with dmem in any way). The boot-time ROM-preload path
// reuses the stock, single-outstanding ATA8.TLScratchpadHandler (write-only)
// since it only runs once at elaboration/reset and its per-transaction cost
// is irrelevant. The fetch-critical read path is served by
// TLPipelinedReadHandler, a purpose-built Get-only responder that supports
// 2 outstanding requests (tagged by alternating TileLink source id 0/1) so
// Fetch can issue back-to-back Gets without waiting for each response.

package mcc

import chisel3._
import chisel3.util._

// Get-only TileLink slave with 2 outstanding requests, tagged by alternating
// source id 0/1. Forwards each accepted Get straight to a MemTierScratchpad
// Readport the same cycle it fires (no extra registering delay beyond the
// scratchpad's own fixed 1-cycle latency), so a.ready never has to wait on
// an in-flight response the way ATA8.TLScratchpadHandler's FSM does.
class TLPipelinedReadHandler(tlConfig: ATA8.TLBusConfig)(implicit c: ATA8.MemBusConfig) extends Module {
  val io = IO(new Bundle {
    val tl       = Flipped(new ATA8.TilelinkPort(tlConfig))
    val rMem     = new ATA8.Readport(Vec(c.dataBusSize, UInt(c.arithDataWidth.W)), Some(16))
    val initDone = Input(Bool())
  })

  val busy = RegInit(VecInit(Seq.fill(2)(false.B)))
  val reqId = io.tl.a.bits.source

  // Tags whichever single request is currently working its way through the
  // scratchpad's fixed 1-cycle read latency. A plain register (not a queue)
  // is provably sufficient: response.valid is exactly RegNext(a.fire), so
  // at most one request's tag is ever awaiting translation at a time -
  // whatever id fired last cycle is exactly the id whose response is
  // landing this cycle.
  val pendingId = Reg(UInt(tlConfig.sourceWidth.W))

  io.tl.a.ready := io.initDone && !busy(reqId)

  io.rMem.request.valid         := io.tl.a.fire
  io.rMem.request.bits.addr.get := io.tl.a.bits.address

  when(io.tl.a.fire) {
    busy(reqId) := true.B
    pendingId   := reqId
  }

  io.tl.d.valid        := io.rMem.response.valid
  io.tl.d.bits.opcode  := ATA8.TilelinkOpcodes.AccessAckData
  io.tl.d.bits.param   := 0.U
  io.tl.d.bits.size    := tlConfig.dataBusSize.U
  io.tl.d.bits.source  := pendingId
  io.tl.d.bits.sink    := 0.U
  io.tl.d.bits.denied  := 0.U
  io.tl.d.bits.data    := io.rMem.response.bits.readData.asUInt
  io.tl.d.bits.corrupt := 0.U

  when(io.tl.d.fire) { busy(pendingId) := false.B }
}

// Top-level imem module: local (already-rebased) 16-bit byte address space,
// private scratchpad bank, stock write handler for ROM preload + pipelined
// read handler for fetch. `initData` is word-indexed from the start of the
// loaded image (as produced by MccHexReader), matching the scratchpad's own
// word-addressed layout directly - no baseAddr rebasing needed here since
// that only matters for the CPU-facing absolute address, handled by the
// caller (mirroring how dmem's harness wiring rebases before its own TLXbar).
class ImemTL(memWords: Int, initData: Map[Int, BigInt])(implicit c: ATA8.MemBusConfig) extends Module {
  val imemTlBus: ATA8.TLBusConfig =
    ATA8.TLBusConfig(dataBusSize = c.dataBusSize, addrWidth = 16, sourceWidth = 1)

  val io = IO(new Bundle {
    val tl       = Flipped(new ATA8.TilelinkPort(imemTlBus))
    val host     = Flipped(new ATA8.TilelinkPort(imemTlBus))
    // Exposed so the harness can gate other things (e.g. dmem access) on
    // the ROM load having finished, same as before.
    val initDone = Output(Bool())
  })

  val scratch = Module(new ATA8.MemTierScratchpad(
    ATA8.SPMConfig(bankDepth = memWords, writeports = 1, readports = 1)
  ))

  val hostWrite = Module(new ATA8.TLScratchpadHandler(
    ATA8.TLScratchConfig(read = false, write = true, atomic = false, tlConfig = imemTlBus)
  ))

  hostWrite.io.wMem.get <> scratch.io.Writeport(0)
  
  // Routes host writes to either Imem or initDone register replace with proper debug interface eventually 
  val hostDemux = Module(new ATA8.TLXbar(ATA8.TLXbarConfig(
    nMasters = 1,
    slaves   = Seq(
      ATA8.TLSlaveConfig(addressSet = Seq((BigInt(0), BigInt(memWords * c.dataBusSize - 1)))),
      ATA8.TLSlaveConfig(addressSet = Seq((BigInt(0xF000),  BigInt(0x01)))),
    ),
    tl       = imemTlBus,
  )))


  //io.host <> scratch.io.Writeport(0)

  io.host <> hostDemux.io.in(0)
  hostDemux.io.out(0) <> hostWrite.io.tl

  val initDone = RegInit(false.B)
  val delay = RegInit(false.B)

  val pendingId = Reg(UInt(imemTlBus.sourceWidth.W))

  hostDemux.io.out(1).a.ready := true.B

  hostDemux.io.out(1).d.valid := false.B
  hostDemux.io.out(1).d.bits := DontCare 


  when(hostDemux.io.out(1).a.valid) {
    initDone := hostDemux.io.out(1).a.bits.data(0)
    pendingId := hostDemux.io.out(1).a.bits.source
    delay := true.B
  }

  when(delay) {
    hostDemux.io.out(1).d.valid := true.B
    hostDemux.io.out(1).d.valid := true.B

    hostDemux.io.out(1).d.valid        := true.B
    hostDemux.io.out(1).d.bits.opcode  := ATA8.TilelinkOpcodes.AccessAckData
    hostDemux.io.out(1).d.bits.param   := 0.U
    hostDemux.io.out(1).d.bits.size    := 4.U
    hostDemux.io.out(1).d.bits.source  := pendingId
    hostDemux.io.out(1).d.bits.sink    := 0.U
    hostDemux.io.out(1).d.bits.denied  := 0.U
    hostDemux.io.out(1).d.bits.data    := 0.U 
    hostDemux.io.out(1).d.bits.corrupt := 0.U
    
  }



  // ---- Fetch-critical read path: pipelined, 2 outstanding requests ----
  val readHandler = Module(new TLPipelinedReadHandler(imemTlBus))
  readHandler.io.rMem     <> scratch.io.Readport(0)
  readHandler.io.initDone := initDone

  io.tl       <> readHandler.io.tl
  io.initDone := initDone
}
