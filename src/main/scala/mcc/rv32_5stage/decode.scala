//**************************************************************************
// RISCV Processor 5-Stage Decode Stage
//--------------------------------------------------------------------------

package mcc.stage5

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket.CSR

import mcc.stage5.Constants._
import mcc.common._

// DEC → EXE pipeline register
class DecToEx(implicit val conf: MccCoreParams) extends Bundle {
  val valid        = Bool()
  val inst         = UInt(32.W)
  val pc           = UInt(conf.xprlen.W)
  val wbaddr       = UInt(5.W)
  val rs1_addr     = UInt(5.W)
  val rs2_addr     = UInt(5.W)
  val op1_data     = UInt(conf.xprlen.W)
  val op2_data     = UInt(conf.xprlen.W)
  val rs2_data     = UInt(conf.xprlen.W)
  val ctrl_br_type = UInt(4.W)
  val ctrl_op2_sel = UInt(3.W)
  val ctrl_alu_fun = UInt(4.W)
  val ctrl_wb_sel  = UInt(2.W)
  val ctrl_rf_wen  = Bool()
  val ctrl_mem_val = Bool()
  val ctrl_mem_fcn = UInt(2.W)
  val ctrl_mem_typ = UInt(3.W)
  val ctrl_csr_cmd = UInt(CSR.SZ.W)
}

// Bypass forwarding port (EXE or MEM stage → DEC stage)
class BypassPort(implicit val conf: MccCoreParams) extends Bundle {
  val rf_wen = Bool()
  val wbaddr = UInt(5.W)
  val data   = UInt(conf.xprlen.W)
}

// Writeback port (WB stage → DEC stage regfile write + WB bypass)
class WbPort(implicit val conf: MccCoreParams) extends Bundle {
  val wen    = Bool()
  val wbaddr = UInt(5.W)
  val wbdata = UInt(conf.xprlen.W)
}

// Control signals from CtlPath to Decode stage
class DecCtlIn extends Bundle {
  val dec_stall     = Bool()
  val full_stall    = Bool()
  val dec_kill      = Bool()
  val pipeline_kill = Bool()
  val op1_sel       = UInt(2.W)
  val op2_sel       = UInt(3.W)
  val alu_fun       = UInt(4.W)
  val wb_sel        = UInt(2.W)
  val rf_wen        = Bool()
  val mem_val       = Bool()
  val mem_fcn       = UInt(2.W)
  val mem_typ       = UInt(3.W)
  val csr_cmd       = UInt(CSR.SZ.W)
  val br_type       = UInt(4.W)
}

class Decode(implicit val p: Parameters, val conf: MccCoreParams) extends Module {
  val io = IO(new Bundle {
    val fromFetch  = Flipped(new FetchToDec)
    val toExe      = new DecToEx
    val ctl        = Input(new DecCtlIn)
    val ddpath     = Flipped(new DebugDPath())
    val bypass_exe = Input(new BypassPort)
    val bypass_mem = Input(new BypassPort)
    val wb_in      = Input(new WbPort)
    // dat to control path
    val dec_valid  = Output(Bool())
    val dec_inst   = Output(UInt(conf.xprlen.W))
  })
  io := DontCare

  val exeReg = RegInit(0.U.asTypeOf(new DecToEx))
  io.toExe := exeReg

  val dec_rs1_addr = io.fromFetch.inst(19, 15)
  val dec_rs2_addr = io.fromFetch.inst(24, 20)
  val dec_wbaddr   = io.fromFetch.inst(11, 7)

  val regfile = Module(new RegisterFile())
  regfile.io.rs1_addr  := dec_rs1_addr
  regfile.io.rs2_addr  := dec_rs2_addr
  val rf_rs1_data       = regfile.io.rs1_data
  val rf_rs2_data       = regfile.io.rs2_data
  regfile.io.waddr     := io.wb_in.wbaddr
  regfile.io.wdata     := io.wb_in.wbdata
  regfile.io.wen       := io.wb_in.wen
  regfile.io.dm_addr   := io.ddpath.addr
  io.ddpath.rdata      := regfile.io.dm_rdata
  regfile.io.dm_en     := io.ddpath.validreq
  regfile.io.dm_wdata  := io.ddpath.wdata

  val imm_itype  = io.fromFetch.inst(31,20)
  val imm_stype  = Cat(io.fromFetch.inst(31,25), io.fromFetch.inst(11,7))
  val imm_sbtype = Cat(io.fromFetch.inst(31), io.fromFetch.inst(7),
                       io.fromFetch.inst(30,25), io.fromFetch.inst(11,8))
  val imm_utype  = io.fromFetch.inst(31, 12)
  val imm_ujtype = Cat(io.fromFetch.inst(31), io.fromFetch.inst(19,12),
                       io.fromFetch.inst(20), io.fromFetch.inst(30,21))
  val imm_z      = Cat(Fill(27,0.U), io.fromFetch.inst(19,15))

  val imm_itype_sext  = Cat(Fill(20,imm_itype(11)),  imm_itype)
  val imm_stype_sext  = Cat(Fill(20,imm_stype(11)),  imm_stype)
  val imm_sbtype_sext = Cat(Fill(19,imm_sbtype(11)), imm_sbtype, 0.U)
  val imm_utype_sext  = Cat(imm_utype, Fill(12,0.U))
  val imm_ujtype_sext = Cat(Fill(11,imm_ujtype(19)), imm_ujtype, 0.U)

  val dec_alu_op2 = MuxCase(0.U, Array(
    (io.ctl.op2_sel === OP2_RS2)    -> rf_rs2_data,
    (io.ctl.op2_sel === OP2_ITYPE)  -> imm_itype_sext,
    (io.ctl.op2_sel === OP2_STYPE)  -> imm_stype_sext,
    (io.ctl.op2_sel === OP2_SBTYPE) -> imm_sbtype_sext,
    (io.ctl.op2_sel === OP2_UTYPE)  -> imm_utype_sext,
    (io.ctl.op2_sel === OP2_UJTYPE) -> imm_ujtype_sext
  )).asUInt

  val dec_op1_data = Wire(UInt(conf.xprlen.W))
  val dec_op2_data = Wire(UInt(conf.xprlen.W))
  val dec_rs2_data = Wire(UInt(conf.xprlen.W))

  if (USE_FULL_BYPASSING) {
    dec_op1_data := MuxCase(rf_rs1_data, Array(
      (io.ctl.op1_sel === OP1_IMZ) -> imm_z,
      (io.ctl.op1_sel === OP1_PC)  -> io.fromFetch.pc,
      ((io.bypass_exe.wbaddr === dec_rs1_addr) && (dec_rs1_addr =/= 0.U) && io.bypass_exe.rf_wen) -> io.bypass_exe.data,
      ((io.bypass_mem.wbaddr === dec_rs1_addr) && (dec_rs1_addr =/= 0.U) && io.bypass_mem.rf_wen) -> io.bypass_mem.data,
      ((io.wb_in.wbaddr      === dec_rs1_addr) && (dec_rs1_addr =/= 0.U) && io.wb_in.wen)         -> io.wb_in.wbdata
    ))
    dec_op2_data := MuxCase(dec_alu_op2, Array(
      ((io.bypass_exe.wbaddr === dec_rs2_addr) && (dec_rs2_addr =/= 0.U) && io.bypass_exe.rf_wen && (io.ctl.op2_sel === OP2_RS2)) -> io.bypass_exe.data,
      ((io.bypass_mem.wbaddr === dec_rs2_addr) && (dec_rs2_addr =/= 0.U) && io.bypass_mem.rf_wen && (io.ctl.op2_sel === OP2_RS2)) -> io.bypass_mem.data,
      ((io.wb_in.wbaddr      === dec_rs2_addr) && (dec_rs2_addr =/= 0.U) && io.wb_in.wen         && (io.ctl.op2_sel === OP2_RS2)) -> io.wb_in.wbdata
    ))
    dec_rs2_data := MuxCase(rf_rs2_data, Array(
      ((io.bypass_exe.wbaddr === dec_rs2_addr) && (dec_rs2_addr =/= 0.U) && io.bypass_exe.rf_wen) -> io.bypass_exe.data,
      ((io.bypass_mem.wbaddr === dec_rs2_addr) && (dec_rs2_addr =/= 0.U) && io.bypass_mem.rf_wen) -> io.bypass_mem.data,
      ((io.wb_in.wbaddr      === dec_rs2_addr) && (dec_rs2_addr =/= 0.U) && io.wb_in.wen)         -> io.wb_in.wbdata
    ))
  } else {
    dec_op1_data := MuxCase(rf_rs1_data, Array(
      (io.ctl.op1_sel === OP1_IMZ) -> imm_z,
      (io.ctl.op1_sel === OP1_PC)  -> io.fromFetch.pc
    ))
    dec_rs2_data := rf_rs2_data
    dec_op2_data := dec_alu_op2
  }

  // DEC → EXE pipeline register write
  when ((io.ctl.dec_stall && !io.ctl.full_stall) || io.ctl.pipeline_kill) {
    exeReg.valid        := false.B
    exeReg.inst         := BUBBLE
    exeReg.wbaddr       := 0.U
    exeReg.ctrl_rf_wen  := false.B
    exeReg.ctrl_mem_val := false.B
    exeReg.ctrl_mem_fcn := M_X
    exeReg.ctrl_csr_cmd := CSR.N
    exeReg.ctrl_br_type := BR_N
  } .elsewhen (!io.ctl.dec_stall && !io.ctl.full_stall) {
    exeReg.pc           := io.fromFetch.pc
    exeReg.rs1_addr     := dec_rs1_addr
    exeReg.rs2_addr     := dec_rs2_addr
    exeReg.op1_data     := dec_op1_data
    exeReg.op2_data     := dec_op2_data
    exeReg.rs2_data     := dec_rs2_data
    exeReg.ctrl_op2_sel := io.ctl.op2_sel
    exeReg.ctrl_alu_fun := io.ctl.alu_fun
    exeReg.ctrl_wb_sel  := io.ctl.wb_sel

    when (io.ctl.dec_kill) {
      exeReg.valid        := false.B
      exeReg.inst         := BUBBLE
      exeReg.wbaddr       := 0.U
      exeReg.ctrl_rf_wen  := false.B
      exeReg.ctrl_mem_val := false.B
      exeReg.ctrl_mem_fcn := M_X
      exeReg.ctrl_csr_cmd := CSR.N
      exeReg.ctrl_br_type := BR_N
    } .otherwise {
      exeReg.valid        := io.fromFetch.valid
      exeReg.inst         := io.fromFetch.inst
      exeReg.wbaddr       := dec_wbaddr
      exeReg.ctrl_rf_wen  := io.ctl.rf_wen
      exeReg.ctrl_mem_val := io.ctl.mem_val
      exeReg.ctrl_mem_fcn := io.ctl.mem_fcn
      exeReg.ctrl_mem_typ := io.ctl.mem_typ
      exeReg.ctrl_csr_cmd := io.ctl.csr_cmd
      exeReg.ctrl_br_type := io.ctl.br_type
    }
  }

  io.dec_valid := io.fromFetch.valid
  io.dec_inst  := io.fromFetch.inst
}
