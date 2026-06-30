package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import mcc.common.MccCoreParams

// CSR command encodings used by mcc's control path
object CSR {
  val SZ = 3
  val X = 0.U(SZ.W)
  val N = 0.U(SZ.W) // no-op
  val W = 1.U(SZ.W) // CSRRW / CSRRWI
  val S = 2.U(SZ.W) // CSRRS / CSRRSI
  val C = 3.U(SZ.W) // CSRRC / CSRRCI
  val R = 4.U(SZ.W) // read-only (CSRRS/CSRRC with rs1=0)
  val I = 5.U(SZ.W) // system (ECALL / EBREAK / MRET / DRET)
}

// RISC-V exception cause codes
object Causes {
  val misaligned_fetch    = 0L
  val illegal_instruction = 2L
  val misaligned_load     = 4L
  val misaligned_store    = 6L
  val machine_ecall       = 11L
}

// External interrupt lines driven into the core
class CoreInterrupts(hasBeu: Boolean) extends Bundle {
  val msip = Bool() // machine software interrupt
  val mtip = Bool() // machine timer interrupt
  val meip = Bool() // machine external interrupt
}

// Performance counter slot (inc driven to false.B in mcc dpath)
class PerfCnt extends Bundle {
  val inc = Input(Bool())
}

// Decode IO slot: mcc drives inst in and reads nothing back in standalone use
class CSRDecodeIO(implicit conf: MccCoreParams) extends Bundle {
  val inst             = Input(UInt(conf.xprlen.W))
  val interrupt        = Output(Bool())
  val cause            = Output(UInt(conf.xprlen.W))
  val illegal          = Output(Bool())
  val system_illegal   = Output(Bool())
}

// Minimal CSR IO matching what mcc's dpath accesses
class CSRFileIO(implicit conf: MccCoreParams) extends Bundle {
  val decode   = Vec(1, new CSRDecodeIO)
  val rw = new Bundle {
    val addr  = Input(UInt(12.W))
    val wdata = Input(UInt(conf.xprlen.W))
    val cmd   = Input(UInt(CSR.SZ.W))
    val rdata = Output(UInt(conf.xprlen.W))
  }
  val retire         = Input(Bool())
  val exception      = Input(Bool())
  val pc             = Input(UInt(conf.xprlen.W))
  val evec           = Output(UInt(conf.xprlen.W))
  val tval           = Input(UInt(conf.xprlen.W))
  val interrupt      = Output(Bool())
  val interrupt_cause = Output(UInt(conf.xprlen.W))
  val interrupts     = Input(new CoreInterrupts(false))
  val hartid         = Input(UInt())
  val cause          = Input(UInt(conf.xprlen.W))
  val ungated_clock  = Input(Clock())
  val eret           = Output(Bool())
  val counters       = Vec(0, new PerfCnt)
  val time           = Output(UInt(64.W))
}

// Perf event set wrappers (just type stubs so CSRFile constructor compiles)
class EventSet(pipelinedHPM: (UInt, UInt) => Bool, hpmEventSets: Seq[(String, () => Bool)])
class EventSets(val eventSets: Seq[EventSet])

// Minimal M-mode CSR file.
// Implements mtvec, mepc, mcause, mtval, mstatus, rdcycle/rdtime.
// MRET is detected by instruction word and redirects PC to mepc.
// All other system instructions (ECALL, EBREAK) trap to mtvec.
class CSRFile(perfEventSets: EventSets = new EventSets(Seq()))(implicit p: Parameters, conf: MccCoreParams)
    extends Module {

  val io = IO(new CSRFileIO)
  io := DontCare

  // Cycle / time counter
  val cycle = RegInit(0.U(64.W))
  cycle := cycle + 1.U
  io.time := cycle

  // Machine CSRs
  val mtvec  = RegInit(0.U(conf.xprlen.W))
  val mepc   = RegInit(0.U(conf.xprlen.W))
  val mcause = RegInit(0.U(conf.xprlen.W))
  val mtval  = RegInit(0.U(conf.xprlen.W))
  val mstatus = RegInit(0x1800.U(conf.xprlen.W)) // MPP=3 (M-mode)
  val mie    = RegInit(0.U(conf.xprlen.W))

  // CSR addresses (RISC-V spec)
  val MSTATUS = 0x300.U(12.W)
  val MIE     = 0x304.U(12.W)
  val MTVEC   = 0x305.U(12.W)
  val MEPC    = 0x341.U(12.W)
  val MCAUSE  = 0x342.U(12.W)
  val MTVAL   = 0x343.U(12.W)
  val MCYCLE  = 0xB00.U(12.W)
  val MCYCLEH = 0xB80.U(12.W)
  val CYCLE   = 0xC00.U(12.W)
  val TIME    = 0xC01.U(12.W)
  val CYCLEH  = 0xC80.U(12.W)

  val rdata_mux = MuxLookup(io.rw.addr, 0.U)(Seq(
    MSTATUS -> mstatus,
    MIE     -> mie,
    MTVEC   -> mtvec,
    MEPC    -> mepc,
    MCAUSE  -> mcause,
    MTVAL   -> mtval,
    MCYCLE  -> cycle(31, 0),
    MCYCLEH -> cycle(63, 32),
    CYCLE   -> cycle(31, 0),
    CYCLEH  -> cycle(63, 32),
    TIME    -> cycle(31, 0),
  ))
  io.rw.rdata := rdata_mux

  // CSR write (W = replace, S = set bits, C = clear bits)
  val wen = io.rw.cmd === CSR.W || io.rw.cmd === CSR.S || io.rw.cmd === CSR.C
  val wdata = MuxLookup(io.rw.cmd, io.rw.wdata)(Seq(
    CSR.W -> io.rw.wdata,
    CSR.S -> (rdata_mux | io.rw.wdata),
    CSR.C -> (rdata_mux & ~io.rw.wdata),
  ))
  when(wen) {
    switch(io.rw.addr) {
      is(MSTATUS) { mstatus := wdata }
      is(MIE)     { mie     := wdata }
      is(MTVEC)   { mtvec   := Cat(wdata(conf.xprlen - 1, 2), 0.U(2.W)) }
      is(MEPC)    { mepc    := Cat(wdata(conf.xprlen - 1, 1), 0.U(1.W)) }
      is(MCAUSE)  { mcause  := wdata }
      is(MTVAL)   { mtval   := wdata }
    }
  }

  // Trap into M-mode: save mepc, mcause, mtval
  when(io.exception) {
    mepc   := Cat(io.pc(conf.xprlen - 1, 1), 0.U(1.W))
    mcause := io.cause
    mtval  := io.tval
  }

  // MRET detection: instruction = 0x30200073
  val is_mret = io.decode(0).inst === "h30200073".U && io.rw.cmd === CSR.I

  io.eret := is_mret
  io.evec := Mux(is_mret, mepc, mtvec)

  // No interrupts in this stub
  io.interrupt       := false.B
  io.interrupt_cause := 0.U

  // Decode slot outputs
  io.decode(0).interrupt      := false.B
  io.decode(0).cause          := 0.U
  io.decode(0).illegal        := false.B
  io.decode(0).system_illegal := false.B
}
