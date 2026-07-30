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

class Memory(implicit val p: Parameters, val conf: MccCoreParams, val bus: ATA8.MemBusConfig) extends Module {
  val io = IO(new Bundle {
    val fromExe          = Flipped(new ExToMem)
    //val dmem             = new MemPortIo(conf.xprlen)
    val dmem             = new ATA8.TilelinkPort(bus.tlBus)
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

  // AMO uses rs1 directly as address; regular loads/stores use ALU output (rs1 + imm)
  val is_amo   = io.fromExe.inst(6, 0) === "b0101111".U
  val mem_addr = Mux(is_amo, io.fromExe.op1_data, io.fromExe.alu_out)

  // Data misalignment detection
  val misaligned_mask = Wire(UInt(3.W))
  misaligned_mask := ~(7.U(3.W) << (io.fromExe.ctrl_mem_typ - 1.U)(1, 0))

  // Handle address range excempt from checking
  val mem_addr_exempt = conf.mmioNoAlignCheckBase.map { base =>
    mem_addr >= base.U && mem_addr < (base + conf.mmioNoAlignCheckSize).U
  }.getOrElse(false.B)
  io.mem_data_misaligned := (misaligned_mask & mem_addr(2, 0)).orR && io.fromExe.ctrl_mem_val && !mem_addr_exempt
  io.mem_store           := io.fromExe.ctrl_mem_fcn === M_XWR
  mem_tval_data_ma       := mem_addr

  io.mem_ctrl_dmem_val := io.fromExe.ctrl_mem_val

  // Sub-word extraction for loads: MemTier returns a full 32-bit word, so shift
  // and sign/zero-extend here based on ctrl_mem_typ and the byte offset within
  // the word.  mem_addr is already available from line 95.
  // Some addresses are excempt 
  val load_d       = io.dmem.d.bits.data
  val load_shifted = Mux(mem_addr_exempt, load_d, (load_d >> Cat(mem_addr(1, 0), 0.U(3.W)))(31, 0))
  val load_data = MuxLookup(io.fromExe.ctrl_mem_typ, load_shifted)(Seq(
    MT_B  -> Cat(Fill(24, load_shifted(7)),  load_shifted(7,  0)),
    MT_BU -> Cat(0.U(24.W),                  load_shifted(7,  0)),
    MT_H  -> Cat(Fill(16, load_shifted(15)), load_shifted(15, 0)),
    MT_HU -> Cat(0.U(16.W),                  load_shifted(15, 0)),
  ))

  // WB mux
  val mem_wbdata = MuxCase(io.fromExe.alu_out, Array(
    (io.fromExe.ctrl_wb_sel === WB_ALU) -> io.fromExe.alu_out,
    (io.fromExe.ctrl_wb_sel === WB_PC4) -> io.fromExe.alu_out,
    (io.fromExe.ctrl_wb_sel === WB_MEM) -> load_data,
    (io.fromExe.ctrl_wb_sel === WB_CSR) -> csr.io.rw.rdata
  ))

  // MEM bypass (combinational, before WB register)
  io.bypass_mem.rf_wen := io.fromExe.ctrl_rf_wen
  io.bypass_mem.wbaddr := io.fromExe.wbaddr
  io.bypass_mem.data   := mem_wbdata

  // Data memory request (TileLink A channel)
  val amo_funct5     = io.fromExe.inst(31, 27)
  val mem_byte_off   = mem_addr(1, 0)
  val mem_byte_shift = Cat(mem_byte_off, 0.U(3.W))  // byte_off * 8

  // funct5 → TileLink A opcode
  val amo_opcode = MuxLookup(amo_funct5, ATA8.TilelinkOpcodes.ArithmeticData)(Seq(
    1.U  -> ATA8.TilelinkOpcodes.LogicalData,  // AMOSWAP
    4.U  -> ATA8.TilelinkOpcodes.LogicalData,  // AMOXOR
    8.U  -> ATA8.TilelinkOpcodes.LogicalData,  // AMOOR
    12.U -> ATA8.TilelinkOpcodes.LogicalData,  // AMOAND
  ))

  // funct5 → TileLink A param
  val amo_param = MuxLookup(amo_funct5, ATA8.ArithmeticDataParam.ADD)(Seq(
    0.U  -> ATA8.ArithmeticDataParam.ADD,
    1.U  -> ATA8.LogicalDataParam.SWAP,
    4.U  -> ATA8.LogicalDataParam.XOR,
    8.U  -> ATA8.LogicalDataParam.OR,
    12.U -> ATA8.LogicalDataParam.AND,
    16.U -> ATA8.ArithmeticDataParam.MIN,
    20.U -> ATA8.ArithmeticDataParam.MAX,
    24.U -> ATA8.ArithmeticDataParam.MINU,
    28.U -> ATA8.ArithmeticDataParam.MAXU,
  ))

  io.dmem.a.valid := io.fromExe.ctrl_mem_val && !io.mem_data_misaligned

  io.dmem.a.bits.opcode := MuxCase(ATA8.TilelinkOpcodes.Get, Seq(
    (io.fromExe.ctrl_mem_fcn === M_XWR) -> ATA8.TilelinkOpcodes.PutFullData,
    is_amo                               -> amo_opcode,
  ))

  io.dmem.a.bits.param := Mux(is_amo, amo_param, 0.U)

  // mcc never bursts: every request is exactly one bus beat. Byte/half/word
  // width and load sign-extension are carried entirely by `mask`, below.
  io.dmem.a.bits.size := bus.dataBusSize.U

  io.dmem.a.bits.source  := 0.U
  io.dmem.a.bits.address := mem_addr

  // mask: byte enables shifted to the correct lanes
  io.dmem.a.bits.mask := MuxLookup(io.fromExe.ctrl_mem_typ, "b1111".U(4.W))(Seq(
    MT_B  -> ("b0001".U(4.W) << mem_byte_off)(3, 0),
    MT_BU -> ("b0001".U(4.W) << mem_byte_off)(3, 0),
    MT_H  -> ("b0011".U(4.W) << mem_byte_off)(3, 0),
    MT_HU -> ("b0011".U(4.W) << mem_byte_off)(3, 0),
    MT_W  -> "b1111".U(4.W),
    MT_WU -> "b1111".U(4.W),
  ))

  // data: pre-shifted to the correct byte lanes. In the alignment-exempt
  // MMIO window (see mem_addr_exempt above) each address is its own
  // independent register rather than a lane within a shared word, so the
  // operand (e.g. an AMO's ADD delta) must go out unshifted.
  io.dmem.a.bits.data    := Mux(mem_addr_exempt, io.fromExe.rs2_data,
                               (io.fromExe.rs2_data << mem_byte_shift)(31, 0))
  io.dmem.a.bits.corrupt := 0.U

  // D channel: core always accepts responses immediately
  io.dmem.d.ready := true.B



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
