//**************************************************************************
// RISCV Processor 5-Stage Memory + Writeback Stage
//--------------------------------------------------------------------------

package mcc.stage5

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket.{CSR, CSRFile, Causes}
import freechips.rocketchip.rocket.CoreInterrupts

import mcc.stage5.Constants._
import mcc.common._

// MEM → WB pipeline register
class MemToWb(implicit val conf: MccCoreParams) extends Bundle {
  val valid  = Bool()
  val wbaddr = UInt(5.W)
  val wbdata = UInt(conf.xprlen.W)
  val rf_wen = Bool()
}

// Control signals from CtlPath to Memory stage
class MemCtlIn extends Bundle {
  val full_stall           = Bool()
  val pipeline_kill        = Bool()
  val mem_exception        = Bool()
  val mem_exception_cause  = UInt(32.W)
}

class Memory(implicit val p: Parameters, val conf: MccCoreParams) extends Module {
  val io = IO(new Bundle {
    val fromExe          = Flipped(new ExToMem)
    val dmem             = new MemPortIo(conf.xprlen)
    val interrupt        = Input(new CoreInterrupts(false))
    val hartid           = Input(UInt())
    val ctl              = Input(new MemCtlIn)
    val bypass_mem       = Output(new BypassPort)
    val wb_out           = Output(new WbPort)
    val exception_target = Output(UInt(32.W))
    // dat to control path
    val mem_ctrl_dmem_val   = Output(Bool())
    val mem_data_misaligned = Output(Bool())
    val mem_store           = Output(Bool())
    val csr_eret            = Output(Bool())
    val csr_interrupt       = Output(Bool())
  })
  io := DontCare

  val wbReg = RegInit(0.U.asTypeOf(new MemToWb))
  io.wb_out.wbaddr := wbReg.wbaddr
  io.wb_out.wbdata := wbReg.wbdata
  io.wb_out.wen    := wbReg.rf_wen

  // Exception tval wires
  val mem_tval_data_ma = Wire(UInt(conf.xprlen.W))
  val mem_tval_inst_ma = io.fromExe.tval_inst_ma

  // CSR
  val csr = Module(new CSRFile(perfEventSets=CSREvents.events))
  csr.io := DontCare
  csr.io.decode(0).inst := io.fromExe.inst
  csr.io.rw.addr        := io.fromExe.inst(CSR_ADDR_MSB, CSR_ADDR_LSB)
  csr.io.rw.wdata       := io.fromExe.alu_out
  csr.io.rw.cmd         := io.fromExe.ctrl_csr_cmd
  csr.io.retire         := wbReg.valid
  csr.io.exception      := io.ctl.mem_exception
  csr.io.pc             := io.fromExe.pc
  io.exception_target   := csr.io.evec

  csr.io.tval := MuxCase(0.U, Array(
    (io.ctl.mem_exception_cause === Causes.illegal_instruction.U) -> io.fromExe.inst,
    (io.ctl.mem_exception_cause === Causes.misaligned_fetch.U)    -> mem_tval_inst_ma,
    (io.ctl.mem_exception_cause === Causes.misaligned_store.U)    -> mem_tval_data_ma,
    (io.ctl.mem_exception_cause === Causes.misaligned_load.U)     -> mem_tval_data_ma
  ))

  val reg_interrupt_handled = RegNext(csr.io.interrupt, false.B)
  val interrupt_edge = csr.io.interrupt && !reg_interrupt_handled

  csr.io.interrupts    := io.interrupt
  csr.io.hartid        := io.hartid
  csr.io.cause         := Mux(io.ctl.mem_exception, io.ctl.mem_exception_cause, csr.io.interrupt_cause)
  csr.io.ungated_clock := clock

  io.csr_interrupt := interrupt_edge
  io.csr_eret      := csr.io.eret
  csr.io.counters.foreach(_.inc := false.B)

  // Data misalignment detection
  val misaligned_mask = Wire(UInt(3.W))
  misaligned_mask := ~(7.U(3.W) << (io.fromExe.ctrl_mem_typ - 1.U)(1, 0))
  io.mem_data_misaligned := (misaligned_mask & io.fromExe.alu_out.apply(2, 0)).orR && io.fromExe.ctrl_mem_val
  io.mem_store           := io.fromExe.ctrl_mem_fcn === M_XWR
  mem_tval_data_ma       := io.fromExe.alu_out

  io.mem_ctrl_dmem_val := io.fromExe.ctrl_mem_val

  // WB mux
  val mem_wbdata = MuxCase(io.fromExe.alu_out, Array(
    (io.fromExe.ctrl_wb_sel === WB_ALU) -> io.fromExe.alu_out,
    (io.fromExe.ctrl_wb_sel === WB_PC4) -> io.fromExe.alu_out,
    (io.fromExe.ctrl_wb_sel === WB_MEM) -> io.dmem.resp.bits.data,
    (io.fromExe.ctrl_wb_sel === WB_CSR) -> csr.io.rw.rdata
  ))

  // MEM bypass (combinational, before WB register)
  io.bypass_mem.rf_wen := io.fromExe.ctrl_rf_wen
  io.bypass_mem.wbaddr := io.fromExe.wbaddr
  io.bypass_mem.data   := mem_wbdata

  // Data memory request
  io.dmem.req.valid     := io.fromExe.ctrl_mem_val && !io.mem_data_misaligned
  io.dmem.req.bits.addr := io.fromExe.alu_out
  io.dmem.req.bits.fcn  := io.fromExe.ctrl_mem_fcn
  io.dmem.req.bits.typ  := io.fromExe.ctrl_mem_typ
  io.dmem.req.bits.data := io.fromExe.rs2_data

  // MEM → WB pipeline register write
  when (!io.ctl.full_stall) {
    wbReg.valid  := io.fromExe.valid && !io.ctl.mem_exception && !interrupt_edge
    wbReg.wbaddr := io.fromExe.wbaddr
    wbReg.wbdata := mem_wbdata
    wbReg.rf_wen := Mux(io.ctl.mem_exception || interrupt_edge, false.B, io.fromExe.ctrl_rf_wen)
  } .otherwise {
    wbReg.valid  := false.B
    wbReg.rf_wen := false.B
  }

  val wb_inst = RegNext(io.fromExe.inst)

  if (conf.trace) {
    printf("Cyc= %d [%d] pc=[%x] W[r%d=%x][%d] Op1=[r%d][%x] Op2=[r%d][%x] inst=[%x] %c%c%c DASM(%x)\n",
      csr.io.time(31,0),
      csr.io.retire,
      RegNext(io.fromExe.pc),
      wbReg.wbaddr,
      wbReg.wbdata,
      wbReg.rf_wen,
      RegNext(io.fromExe.rs1_addr),
      RegNext(io.fromExe.op1_data),
      RegNext(io.fromExe.rs2_addr),
      RegNext(io.fromExe.op2_data),
      wb_inst,
      MuxCase(Str(" "), Seq(
        io.ctl.pipeline_kill -> Str("K"),
        io.ctl.full_stall    -> Str("F"))),
      Str(" "),
      Mux(csr.io.exception, Str("X"), Str(" ")),
      wb_inst)
  }
}
