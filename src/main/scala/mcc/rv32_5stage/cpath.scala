//**************************************************************************
// RISCV Processor 5-Stage Control Path
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2012 Jan 20
//
// Supports both a fully-bypassed datapath (with stalls for load-use), and a
// fully interlocked (no bypass) datapath that stalls for all hazards.

package mcc.stage5

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._

import freechips.rocketchip.rocket.{CSR, Causes}

import mcc.stage5.Constants._
import mcc.common._
import mcc.common.Instructions._

// ---- Decode pattern: one entry per instruction, all signals explicit ------
case class RvPattern(
  encoding: BitPat,
  brType:   UInt,
  op1Sel:   UInt,
  op2Sel:   UInt,
  rs1Oen:   Boolean,
  rs2Oen:   Boolean,
  aluFun:   UInt,
  wbSel:    UInt,
  rfWen:    Boolean,
  memEn:    Boolean,
  memFcn:   UInt,
  memTyp:   UInt,
  csrCmd:   UInt,
  fenceI:   Boolean,
) extends DecodePattern {
  override def bitPat = encoding
}

// ---- Decode fields: one per control signal --------------------------------
object ValidField  extends BoolDecodeField[RvPattern] {
  def name = "valid"
  def genTable(op: RvPattern): BitPat = y
}
object BrTypeField extends DecodeField[RvPattern, UInt] {
  def name = "br_type" ; def chiselType = UInt(4.W)
  def genTable(op: RvPattern): BitPat = BitPat(op.brType)
}
object Op1SelField extends DecodeField[RvPattern, UInt] {
  def name = "op1_sel" ; def chiselType = UInt(2.W)
  def genTable(op: RvPattern): BitPat = BitPat(op.op1Sel)
}
object Op2SelField extends DecodeField[RvPattern, UInt] {
  def name = "op2_sel" ; def chiselType = UInt(3.W)
  def genTable(op: RvPattern): BitPat = BitPat(op.op2Sel)
}
object Rs1OenField extends BoolDecodeField[RvPattern] {
  def name = "rs1_oen"
  def genTable(op: RvPattern): BitPat = if (op.rs1Oen) y else n
}
object Rs2OenField extends BoolDecodeField[RvPattern] {
  def name = "rs2_oen"
  def genTable(op: RvPattern): BitPat = if (op.rs2Oen) y else n
}
object AluFunField extends DecodeField[RvPattern, UInt] {
  def name = "alu_fun" ; def chiselType = UInt(4.W)
  def genTable(op: RvPattern): BitPat = BitPat(op.aluFun)
}
object WbSelField  extends DecodeField[RvPattern, UInt] {
  def name = "wb_sel"  ; def chiselType = UInt(2.W)
  def genTable(op: RvPattern): BitPat = BitPat(op.wbSel)
}
object RfWenField  extends BoolDecodeField[RvPattern] {
  def name = "rf_wen"
  def genTable(op: RvPattern): BitPat = if (op.rfWen) y else n
}
object MemEnField  extends BoolDecodeField[RvPattern] {
  def name = "mem_en"
  def genTable(op: RvPattern): BitPat = if (op.memEn) y else n
}
object MemFcnField extends DecodeField[RvPattern, UInt] {
  def name = "mem_fcn" ; def chiselType = UInt(1.W)
  def genTable(op: RvPattern): BitPat = BitPat(op.memFcn)
}
object MemTypField extends DecodeField[RvPattern, UInt] {
  def name = "mem_typ" ; def chiselType = UInt(3.W)
  def genTable(op: RvPattern): BitPat = BitPat(op.memTyp)
}
object CsrCmdField extends DecodeField[RvPattern, UInt] {
  def name = "csr_cmd" ; def chiselType = UInt(CSR.SZ.W)
  def genTable(op: RvPattern): BitPat = BitPat(op.csrCmd)
}
object FenceIField extends BoolDecodeField[RvPattern] {
  def name = "fencei"
  def genTable(op: RvPattern): BitPat = if (op.fenceI) y else n
}

// ---- IO bundles ----------------------------------------------------------

class CtlToDatIo extends Bundle()
{
  val dec_stall  = Output(Bool())
  val full_stall = Output(Bool())
  val exe_pc_sel = Output(UInt(2.W))
  val br_type    = Output(UInt(4.W))
  val if_kill    = Output(Bool())
  val dec_kill   = Output(Bool())
  val op1_sel    = Output(UInt(2.W))
  val op2_sel    = Output(UInt(3.W))
  val alu_fun    = Output(UInt(4.W))
  val wb_sel     = Output(UInt(2.W))
  val rf_wen     = Output(Bool())
  val mem_val    = Output(Bool())
  val mem_fcn    = Output(UInt(2.W))
  val mem_typ    = Output(UInt(3.W))
  val csr_cmd    = Output(UInt(CSR.SZ.W))
  val fencei     = Output(Bool())

  val pipeline_kill        = Output(Bool())
  val mem_exception        = Output(Bool())
  val mem_exception_cause  = Output(UInt(32.W))
}

class CtlPath(implicit val conf: MccCoreParams) extends Module
{
  val io = IO(new Bundle {
    val dcpath = Flipped(new DebugCPath())
    val imem = new MemPortIo(conf.xprlen)
    val dmem = new TilelinkPort()
    val dat  = Flipped(new DatToCtlIo())
    val ctl  = new CtlToDatIo()
  })
  io := DontCare

  // ---- Instruction decode table ------------------------------------------
  //          enc        BR     op1      op2         r1    r2     ALU         wb      rw     me     mf     mt     csr    fi
  private val instructions: Seq[RvPattern] = Seq(
    RvPattern(LW,        BR_N,  OP1_RS1, OP2_ITYPE,  true, false, ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_W,  CSR.N, false),
    RvPattern(LB,        BR_N,  OP1_RS1, OP2_ITYPE,  true, false, ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_B,  CSR.N, false),
    RvPattern(LBU,       BR_N,  OP1_RS1, OP2_ITYPE,  true, false, ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_BU, CSR.N, false),
    RvPattern(LH,        BR_N,  OP1_RS1, OP2_ITYPE,  true, false, ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_H,  CSR.N, false),
    RvPattern(LHU,       BR_N,  OP1_RS1, OP2_ITYPE,  true, false, ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_HU, CSR.N, false),
    RvPattern(SW,        BR_N,  OP1_RS1, OP2_STYPE,  true, true,  ALU_ADD,    WB_X,   false, true,  M_XWR, MT_W,  CSR.N, false),
    RvPattern(SB,        BR_N,  OP1_RS1, OP2_STYPE,  true, true,  ALU_ADD,    WB_X,   false, true,  M_XWR, MT_B,  CSR.N, false),
    RvPattern(SH,        BR_N,  OP1_RS1, OP2_STYPE,  true, true,  ALU_ADD,    WB_X,   false, true,  M_XWR, MT_H,  CSR.N, false),

    RvPattern(AUIPC,     BR_N,  OP1_PC,  OP2_UTYPE,  false, false, ALU_ADD,   WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(LUI,       BR_N,  OP1_X,   OP2_UTYPE,  false, false, ALU_COPY_2,WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),

    RvPattern(ADDI,      BR_N,  OP1_RS1, OP2_ITYPE,  true, false, ALU_ADD,    WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(ANDI,      BR_N,  OP1_RS1, OP2_ITYPE,  true, false, ALU_AND,    WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(ORI,       BR_N,  OP1_RS1, OP2_ITYPE,  true, false, ALU_OR,     WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(XORI,      BR_N,  OP1_RS1, OP2_ITYPE,  true, false, ALU_XOR,    WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(SLTI,      BR_N,  OP1_RS1, OP2_ITYPE,  true, false, ALU_SLT,    WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(SLTIU,     BR_N,  OP1_RS1, OP2_ITYPE,  true, false, ALU_SLTU,   WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(SLLI_RV32, BR_N,  OP1_RS1, OP2_ITYPE,  true, false, ALU_SLL,    WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(SRAI_RV32, BR_N,  OP1_RS1, OP2_ITYPE,  true, false, ALU_SRA,    WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(SRLI_RV32, BR_N,  OP1_RS1, OP2_ITYPE,  true, false, ALU_SRL,    WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),

    RvPattern(SLL,       BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_SLL,    WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(ADD,       BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(SUB,       BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_SUB,    WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(SLT,       BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_SLT,    WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(SLTU,      BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_SLTU,   WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(AND,       BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_AND,    WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(OR,        BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_OR,     WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(XOR,       BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_XOR,    WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(SRA,       BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_SRA,    WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(SRL,       BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_SRL,    WB_ALU, true,  false, M_X,   MT_X,  CSR.N, false),

    RvPattern(JAL,       BR_J,  OP1_RS1, OP2_UJTYPE, false, false, ALU_X,     WB_PC4, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(JALR,      BR_JR, OP1_RS1, OP2_ITYPE,  true, false,  ALU_X,     WB_PC4, true,  false, M_X,   MT_X,  CSR.N, false),
    RvPattern(BEQ,       BR_EQ, OP1_RS1, OP2_SBTYPE, true, true,  ALU_X,      WB_X,   false, false, M_X,   MT_X,  CSR.N, false),
    RvPattern(BNE,       BR_NE, OP1_RS1, OP2_SBTYPE, true, true,  ALU_X,      WB_X,   false, false, M_X,   MT_X,  CSR.N, false),
    RvPattern(BGE,       BR_GE, OP1_RS1, OP2_SBTYPE, true, true,  ALU_X,      WB_X,   false, false, M_X,   MT_X,  CSR.N, false),
    RvPattern(BGEU,      BR_GEU,OP1_RS1, OP2_SBTYPE, true, true,  ALU_X,      WB_X,   false, false, M_X,   MT_X,  CSR.N, false),
    RvPattern(BLT,       BR_LT, OP1_RS1, OP2_SBTYPE, true, true,  ALU_X,      WB_X,   false, false, M_X,   MT_X,  CSR.N, false),
    RvPattern(BLTU,      BR_LTU,OP1_RS1, OP2_SBTYPE, true, true,  ALU_X,      WB_X,   false, false, M_X,   MT_X,  CSR.N, false),

    RvPattern(CSRRWI,    BR_N,  OP1_IMZ, OP2_X,      true, true,  ALU_COPY_1, WB_CSR, true,  false, M_X,   MT_X,  CSR.W, false),
    RvPattern(CSRRSI,    BR_N,  OP1_IMZ, OP2_X,      true, true,  ALU_COPY_1, WB_CSR, true,  false, M_X,   MT_X,  CSR.S, false),
    RvPattern(CSRRW,     BR_N,  OP1_RS1, OP2_X,      true, true,  ALU_COPY_1, WB_CSR, true,  false, M_X,   MT_X,  CSR.W, false),
    RvPattern(CSRRS,     BR_N,  OP1_RS1, OP2_X,      true, true,  ALU_COPY_1, WB_CSR, true,  false, M_X,   MT_X,  CSR.S, false),
    RvPattern(CSRRC,     BR_N,  OP1_RS1, OP2_X,      true, true,  ALU_COPY_1, WB_CSR, true,  false, M_X,   MT_X,  CSR.C, false),
    RvPattern(CSRRCI,    BR_N,  OP1_IMZ, OP2_X,      true, true,  ALU_COPY_1, WB_CSR, true,  false, M_X,   MT_X,  CSR.C, false),

    RvPattern(ECALL,     BR_N,  OP1_X,   OP2_X,      false, false, ALU_X,     WB_X,   false, false, M_X,   MT_X,  CSR.I, false),
    RvPattern(MRET,      BR_N,  OP1_X,   OP2_X,      false, false, ALU_X,     WB_X,   false, false, M_X,   MT_X,  CSR.I, false),
    RvPattern(DRET,      BR_N,  OP1_X,   OP2_X,      false, false, ALU_X,     WB_X,   false, false, M_X,   MT_X,  CSR.I, false),
    RvPattern(EBREAK,    BR_N,  OP1_X,   OP2_X,      false, false, ALU_X,     WB_X,   false, false, M_X,   MT_X,  CSR.I, false),
    RvPattern(WFI,       BR_N,  OP1_X,   OP2_X,      false, false, ALU_X,     WB_X,   false, false, M_X,   MT_X,  CSR.N, false),

    RvPattern(FENCE_I,   BR_N,  OP1_X,   OP2_X,      false, false, ALU_X,     WB_X,   false, false, M_X,   MT_X,  CSR.N, true),
    RvPattern(FENCE,     BR_N,  OP1_X,   OP2_X,      false, false, ALU_X,     WB_X,   false, false, M_X,   MT_X,  CSR.N, false),

    // Zaamo: word AMOs
    RvPattern(AMOADD,    BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_W,  CSR.N, false),
    RvPattern(AMOSWAP,   BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_W,  CSR.N, false),
    RvPattern(AMOXOR,    BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_W,  CSR.N, false),
    RvPattern(AMOOR,     BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_W,  CSR.N, false),
    RvPattern(AMOAND,    BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_W,  CSR.N, false),
    RvPattern(AMOMIN,    BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_W,  CSR.N, false),
    RvPattern(AMOMAX,    BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_W,  CSR.N, false),
    RvPattern(AMOMINU,   BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_W,  CSR.N, false),
    RvPattern(AMOMAXU,   BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_W,  CSR.N, false),
    // Zabha: halfword AMOs
    RvPattern(AMOADD_H,  BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_H,  CSR.N, false),
    RvPattern(AMOSWAP_H, BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_H,  CSR.N, false),
    RvPattern(AMOXOR_H,  BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_H,  CSR.N, false),
    RvPattern(AMOOR_H,   BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_H,  CSR.N, false),
    RvPattern(AMOAND_H,  BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_H,  CSR.N, false),
    RvPattern(AMOMIN_H,  BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_H,  CSR.N, false),
    RvPattern(AMOMAX_H,  BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_H,  CSR.N, false),
    RvPattern(AMOMINU_H, BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_H,  CSR.N, false),
    RvPattern(AMOMAXU_H, BR_N,  OP1_RS1, OP2_RS2,    true, true,  ALU_ADD,    WB_MEM, true,  true,  M_XRD, MT_H,  CSR.N, false),
  )

  private val decodeFields: Seq[DecodeField[RvPattern, _ <: Data]] = Seq(
    ValidField, BrTypeField, Op1SelField, Op2SelField,
    Rs1OenField, Rs2OenField, AluFunField, WbSelField,
    RfWenField, MemEnField, MemFcnField, MemTypField,
    CsrCmdField, FenceIField,
  )

  private val table  = new DecodeTable(instructions, decodeFields)
  private val result = table.decode(io.dat.dec_inst)

  val cs_val_inst: Bool = result(ValidField)
  val cs_br_type        = result(BrTypeField)
  val cs_op1_sel        = result(Op1SelField)
  val cs_op2_sel        = result(Op2SelField)
  val cs_rs1_oen: Bool  = result(Rs1OenField)
  val cs_rs2_oen: Bool  = result(Rs2OenField)
  val cs_alu_fun        = result(AluFunField)
  val cs_wb_sel         = result(WbSelField)
  val cs_rf_wen: Bool   = result(RfWenField)
  val cs_mem_en: Bool   = result(MemEnField)
  val cs_mem_fcn        = result(MemFcnField)
  val cs_msk_sel        = result(MemTypField)
  val cs_csr_cmd        = result(CsrCmdField)
  val cs_fencei: Bool   = result(FenceIField)

  // ---- Branch logic -------------------------------------------------------

  val ctrl_exe_pc_sel = Mux(io.ctl.pipeline_kill         , PC_EXC,
                        Mux(io.dat.exe_br_type === BR_N  , PC_4,
                        Mux(io.dat.exe_br_type === BR_NE , Mux(!io.dat.exe_br_eq,  PC_BRJMP, PC_4),
                        Mux(io.dat.exe_br_type === BR_EQ , Mux( io.dat.exe_br_eq,  PC_BRJMP, PC_4),
                        Mux(io.dat.exe_br_type === BR_GE , Mux(!io.dat.exe_br_lt,  PC_BRJMP, PC_4),
                        Mux(io.dat.exe_br_type === BR_GEU, Mux(!io.dat.exe_br_ltu, PC_BRJMP, PC_4),
                        Mux(io.dat.exe_br_type === BR_LT , Mux( io.dat.exe_br_lt,  PC_BRJMP, PC_4),
                        Mux(io.dat.exe_br_type === BR_LTU, Mux( io.dat.exe_br_ltu, PC_BRJMP, PC_4),
                        Mux(io.dat.exe_br_type === BR_J  , PC_BRJMP,
                        Mux(io.dat.exe_br_type === BR_JR , PC_JALR,
                                                           PC_4
                    ))))))))))

  val ifkill  = (ctrl_exe_pc_sel =/= PC_4) || cs_fencei || RegNext(cs_fencei)
  val deckill = (ctrl_exe_pc_sel =/= PC_4)

  // ---- Exception handling -------------------------------------------------

  io.ctl.pipeline_kill := (io.dat.csr_eret || io.ctl.mem_exception || io.dat.csr_interrupt)

  val dec_illegal = (!cs_val_inst && io.dat.dec_valid)

  // ---- Stall signal logic -------------------------------------------------

  val stall   = Wire(Bool())

  val dec_rs1_addr = io.dat.dec_inst(19, 15)
  val dec_rs2_addr = io.dat.dec_inst(24, 20)
  val dec_wbaddr   = io.dat.dec_inst(11, 7)
  val dec_rs1_oen  = Mux(deckill, false.B, cs_rs1_oen)
  val dec_rs2_oen  = Mux(deckill, false.B, cs_rs2_oen)

  val exe_reg_wbaddr      = Reg(UInt())
  val mem_reg_wbaddr      = Reg(UInt())
  val wb_reg_wbaddr       = Reg(UInt())
  val exe_reg_ctrl_rf_wen = RegInit(false.B)
  val mem_reg_ctrl_rf_wen = RegInit(false.B)
  val wb_reg_ctrl_rf_wen  = RegInit(false.B)
  val exe_reg_illegal     = RegInit(false.B)

  val exe_reg_is_csr = RegInit(false.B)

  // TODO rename stall==hazard_stall full_stall == cmiss_stall
  val full_stall = Wire(Bool())
  when (!stall && !full_stall)
  {
     when (deckill)
     {
        exe_reg_wbaddr      := 0.U
        exe_reg_ctrl_rf_wen := false.B
        exe_reg_is_csr      := false.B
        exe_reg_illegal     := false.B
     }
     .otherwise
     {
        exe_reg_wbaddr      := dec_wbaddr
        exe_reg_ctrl_rf_wen := cs_rf_wen
        exe_reg_is_csr      := cs_csr_cmd =/= CSR.N && cs_csr_cmd =/= CSR.I
        exe_reg_illegal     := dec_illegal
     }
  }
  .elsewhen (stall && !full_stall)
  {
     // kill exe stage
     exe_reg_wbaddr      := 0.U
     exe_reg_ctrl_rf_wen := false.B
     exe_reg_is_csr      := false.B
     exe_reg_illegal     := false.B
  }
  when (!full_stall) {
    mem_reg_wbaddr      := exe_reg_wbaddr
    wb_reg_wbaddr       := mem_reg_wbaddr
    mem_reg_ctrl_rf_wen := exe_reg_ctrl_rf_wen
    wb_reg_ctrl_rf_wen  := mem_reg_ctrl_rf_wen
  }

  val exe_inst_is_load = RegInit(false.B)

  when (!full_stall)
  {
     exe_inst_is_load := cs_mem_en && (cs_mem_fcn === M_XRD)
  }

  // Clear instruction exception when returning from trap
  when (io.dat.csr_eret)
  {
     exe_reg_illegal    := false.B
  }

  // Stall for load-use hazard and CSR
  stall := ((exe_inst_is_load) && (exe_reg_wbaddr === dec_rs1_addr) && (exe_reg_wbaddr =/= 0.U) && dec_rs1_oen) ||
           ((exe_inst_is_load) && (exe_reg_wbaddr === dec_rs2_addr) && (exe_reg_wbaddr =/= 0.U) && dec_rs2_oen) ||
           (exe_reg_is_csr)

  // stall full pipeline on D$ miss
  val dmem_val = io.dat.mem_ctrl_dmem_val
  full_stall := !((dmem_val && io.dmem.d.valid) || !dmem_val)


  io.ctl.dec_stall  := stall
  io.ctl.full_stall := full_stall
  io.ctl.exe_pc_sel := ctrl_exe_pc_sel
  io.ctl.br_type    := cs_br_type
  io.ctl.if_kill    := ifkill
  io.ctl.dec_kill   := deckill
  io.ctl.op1_sel    := cs_op1_sel
  io.ctl.op2_sel    := cs_op2_sel
  io.ctl.alu_fun    := cs_alu_fun
  io.ctl.wb_sel     := cs_wb_sel
  io.ctl.rf_wen     := cs_rf_wen

  // stall IF while fencei drains through EXE
  io.ctl.fencei     := cs_fencei || RegNext(cs_fencei)

  // Exception priority matters!
  io.ctl.mem_exception := RegNext((exe_reg_illegal || io.dat.exe_inst_misaligned) && !io.dat.csr_eret) || io.dat.mem_data_misaligned
  io.ctl.mem_exception_cause := Mux(RegNext(exe_reg_illegal),            Causes.illegal_instruction.U,
                                Mux(RegNext(io.dat.exe_inst_misaligned), Causes.misaligned_fetch.U,
                                Mux(io.dat.mem_store,                    Causes.misaligned_store.U,
                                                                         Causes.misaligned_load.U
                                )))

  // convert CSR instructions with rs1 == 0 to read-only CSR commands
  val rs1_addr = io.dat.dec_inst(RS1_MSB, RS1_LSB)
  val csr_ren = (cs_csr_cmd === CSR.S || cs_csr_cmd === CSR.C) && rs1_addr === 0.U
  io.ctl.csr_cmd := Mux(csr_ren, CSR.R, cs_csr_cmd)

  io.ctl.mem_val    := cs_mem_en
  io.ctl.mem_fcn    := cs_mem_fcn
  io.ctl.mem_typ    := cs_msk_sel

}
