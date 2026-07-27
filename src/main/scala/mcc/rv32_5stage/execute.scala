//**************************************************************************
// RISCV Processor 5-Stage Execute Stage
//--------------------------------------------------------------------------

package mcc.stage5

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket.CSR

import mcc.stage5.Constants._
import mcc.common._

// EXE → MEM pipeline register
class ExToMem(implicit val conf: MccCoreParams) extends Bundle {
  val valid        = Bool()
  val pc           = UInt(conf.xprlen.W)
  val inst         = UInt(conf.xprlen.W)
  val alu_out      = UInt(conf.xprlen.W)  // WB_PC4 already muxed in
  val wbaddr       = UInt(5.W)
  val rs1_addr     = UInt(5.W)
  val rs2_addr     = UInt(5.W)
  val op1_data     = UInt(conf.xprlen.W)
  val op2_data     = UInt(conf.xprlen.W)
  val rs2_data     = UInt(conf.xprlen.W)
  val ctrl_rf_wen  = Bool()
  val ctrl_mem_val = Bool()
  val ctrl_mem_fcn = UInt(2.W)
  val ctrl_mem_typ = UInt(3.W)
  val ctrl_wb_sel  = UInt(2.W)
  val ctrl_csr_cmd = UInt(CSR.SZ.W)
  val tval_inst_ma = UInt(conf.xprlen.W)  // misaligned fetch tval for CSR
}

// Control signals from CtlPath to Execute stage
class ExeCtlIn extends Bundle {
  val full_stall    = Bool()
  val pipeline_kill = Bool()
  val exe_pc_sel    = UInt(2.W)
}

class Execute(implicit val p: Parameters, val conf: MccCoreParams) extends Module {
  val io = IO(new Bundle {
    val fromDec         = Flipped(new DecToEx)
    val toMem           = new ExToMem
    val ctl             = Input(new ExeCtlIn)
    val bypass          = Output(new BypassPort)
    // branch/jump targets (combinational, feed back to Fetch)
    val brjmp_target    = Output(UInt(32.W))
    val jump_reg_target = Output(UInt(32.W))
    // dat to control path
    val exe_br_eq            = Output(Bool())
    val exe_br_lt            = Output(Bool())
    val exe_br_ltu           = Output(Bool())
    val exe_br_type          = Output(UInt(4.W))
    val exe_inst_misaligned  = Output(Bool())
  })
  io := DontCare

  val memReg = RegInit(0.U.asTypeOf(new ExToMem))
  io.toMem := memReg

  // ALU
  val exe_alu_op1  = io.fromDec.op1_data.asUInt
  val exe_alu_op2  = io.fromDec.op2_data.asUInt
  val alu_shamt    = exe_alu_op2(4,0).asUInt
  val exe_adder_out = (exe_alu_op1 + exe_alu_op2)(conf.xprlen-1, 0)

  // RV32M multiply: sign-extend operands to 33b based on the op, then a single
  // signed multiply covers MUL/MULH/MULHSU/MULHU (lhs is signed unless MULHU,
  // rhs is signed only for MULH).
  val alu_fun    = io.fromDec.ctrl_alu_fun
  val mul_lhs    = Cat((alu_fun === ALU_MULH || alu_fun === ALU_MULHSU) && exe_alu_op1(31), exe_alu_op1).asSInt
  val mul_rhs    = Cat(alu_fun === ALU_MULH && exe_alu_op2(31), exe_alu_op2).asSInt
  val mul_result = (mul_lhs * mul_rhs).asUInt

  val exe_alu_out = MuxCase(io.fromDec.inst.asUInt, Array(
    (io.fromDec.ctrl_alu_fun === ALU_ADD)    -> exe_adder_out,
    (io.fromDec.ctrl_alu_fun === ALU_SUB)    -> (exe_alu_op1 - exe_alu_op2).asUInt,
    (io.fromDec.ctrl_alu_fun === ALU_AND)    -> (exe_alu_op1 & exe_alu_op2).asUInt,
    (io.fromDec.ctrl_alu_fun === ALU_OR)     -> (exe_alu_op1 | exe_alu_op2).asUInt,
    (io.fromDec.ctrl_alu_fun === ALU_XOR)    -> (exe_alu_op1 ^ exe_alu_op2).asUInt,
    (io.fromDec.ctrl_alu_fun === ALU_SLT)    -> (exe_alu_op1.asSInt < exe_alu_op2.asSInt).asUInt,
    (io.fromDec.ctrl_alu_fun === ALU_SLTU)   -> (exe_alu_op1 < exe_alu_op2).asUInt,
    (io.fromDec.ctrl_alu_fun === ALU_SLL)    -> ((exe_alu_op1 << alu_shamt)(conf.xprlen-1, 0)).asUInt,
    (io.fromDec.ctrl_alu_fun === ALU_SRA)    -> (exe_alu_op1.asSInt >> alu_shamt).asUInt,
    (io.fromDec.ctrl_alu_fun === ALU_SRL)    -> (exe_alu_op1 >> alu_shamt).asUInt,
    (io.fromDec.ctrl_alu_fun === ALU_COPY_1) -> exe_alu_op1,
    (io.fromDec.ctrl_alu_fun === ALU_COPY_2) -> exe_alu_op2,
    (io.fromDec.ctrl_alu_fun === ALU_MUL)    -> mul_result(31, 0),
    (io.fromDec.ctrl_alu_fun === ALU_MULH)   -> mul_result(63, 32),
    (io.fromDec.ctrl_alu_fun === ALU_MULHSU) -> mul_result(63, 32),
    (io.fromDec.ctrl_alu_fun === ALU_MULHU)  -> mul_result(63, 32)
  ))

  val brjmp_offset     = io.fromDec.op2_data
  val exe_brjmp_target = io.fromDec.pc + brjmp_offset
  val exe_jump_reg_target = exe_adder_out & ~1.U(conf.xprlen.W)
  val exe_pc_plus4     = (io.fromDec.pc + 4.U)(conf.xprlen-1, 0)

  io.brjmp_target    := exe_brjmp_target
  io.jump_reg_target := exe_jump_reg_target

  io.exe_inst_misaligned :=
    (exe_brjmp_target(1,0).orR    && io.ctl.exe_pc_sel === PC_BRJMP) ||
    (exe_jump_reg_target(1,0).orR && io.ctl.exe_pc_sel === PC_JALR)

  io.exe_br_eq   := io.fromDec.op1_data === io.fromDec.rs2_data
  io.exe_br_lt   := io.fromDec.op1_data.asSInt < io.fromDec.rs2_data.asSInt
  io.exe_br_ltu  := io.fromDec.op1_data.asUInt < io.fromDec.rs2_data.asUInt
  io.exe_br_type := io.fromDec.ctrl_br_type

  io.bypass.rf_wen := io.fromDec.ctrl_rf_wen
  io.bypass.wbaddr := io.fromDec.wbaddr
  io.bypass.data   := exe_alu_out

  // EXE → MEM pipeline register write
  when (io.ctl.pipeline_kill) {
    memReg.valid        := false.B
    memReg.inst         := BUBBLE
    memReg.ctrl_rf_wen  := false.B
    memReg.ctrl_mem_val := false.B
    memReg.ctrl_csr_cmd := false.B
  } .elsewhen (!io.ctl.full_stall) {
    memReg.valid        := io.fromDec.valid
    memReg.pc           := io.fromDec.pc
    memReg.inst         := io.fromDec.inst
    memReg.alu_out      := Mux(io.fromDec.ctrl_wb_sel === WB_PC4, exe_pc_plus4, exe_alu_out)
    memReg.wbaddr       := io.fromDec.wbaddr
    memReg.rs1_addr     := io.fromDec.rs1_addr
    memReg.rs2_addr     := io.fromDec.rs2_addr
    memReg.op1_data     := io.fromDec.op1_data
    memReg.op2_data     := io.fromDec.op2_data
    memReg.rs2_data     := io.fromDec.rs2_data
    memReg.ctrl_rf_wen  := io.fromDec.ctrl_rf_wen
    memReg.ctrl_mem_val := io.fromDec.ctrl_mem_val
    memReg.ctrl_mem_fcn := io.fromDec.ctrl_mem_fcn
    memReg.ctrl_mem_typ := io.fromDec.ctrl_mem_typ
    memReg.ctrl_wb_sel  := io.fromDec.ctrl_wb_sel
    memReg.ctrl_csr_cmd := io.fromDec.ctrl_csr_cmd
    memReg.tval_inst_ma := Mux(io.ctl.exe_pc_sel === PC_BRJMP, exe_brjmp_target, exe_jump_reg_target)
  }
}
