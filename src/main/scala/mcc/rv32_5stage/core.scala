//**************************************************************************
// RISCV Processor
//--------------------------------------------------------------------------

package mcc.stage5

import chisel3._
import mcc.common._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket.CoreInterrupts

class Core()(implicit val p: Parameters, val conf: MccCoreParams, val bus: ATA8.MemBusConfig) extends Module {
  val io = IO(new Bundle {
    val ddpath       = Flipped(new DebugDPath())
    val dcpath       = Flipped(new DebugCPath())
    val imem         = new MemPortIo(conf.xprlen)
    val dmem         = new ATA8.TilelinkPort()
    val interrupt    = Input(new CoreInterrupts(false))
    val hartid       = Input(UInt())
    val reset_vector = Input(UInt())
  })

  val c   = Module(new CtlPath())
  val fet = Module(new Fetch())
  val dec = Module(new Decode())
  val exe = Module(new Execute())
  val mem = Module(new Memory())
  val wb  = Module(new Writeback())

  // Instruction memory: CtlPath first (DontCare req), Fetch last (wins)
  c.io.imem    <> io.imem
  io.imem      <> fet.io.imem

  // Data memory: CtlPath first (DontCare req), Memory last (wins)
  c.io.dmem    <> io.dmem
  io.dmem      <> mem.io.dmem

  // Debug paths
  wb.io.ddpath  <> io.ddpath
  c.io.dcpath   <> io.dcpath

  // Reset / hart
  fet.io.reset_vector  := io.reset_vector
  mem.io.interrupt     := io.interrupt
  mem.io.hartid        := io.hartid

  // Pipeline: Fetch → Decode → Execute → Memory
  dec.io.fromFetch <> fet.io.toDec
  exe.io.fromDec   <> dec.io.toExe
  mem.io.fromExe   <> exe.io.toMem

  // Fetch needs targets from Execute and Memory
  fet.io.exe_brjmp_target    := exe.io.brjmp_target
  fet.io.exe_jump_reg_target := exe.io.jump_reg_target
  fet.io.exception_target    := mem.io.exception_target

  // Bypass: Execute and Memory forward to Decode
  dec.io.bypass_exe <> exe.io.bypass
  dec.io.bypass_mem <> mem.io.bypass_mem

  // Writeback: Memory WB register → Writeback regfile write + Decode WB bypass
  wb.io.wb_in  <> mem.io.wb_out
  dec.io.wb_in <> mem.io.wb_out

  // Regfile read: Decode sends addresses, Writeback returns data
  dec.io.rfRead <> wb.io.rfRead

  // Control path: Fetch uses the full CtlToDatIo bundle
  fet.io.ctl <> c.io.ctl

  // Control to Decode
  dec.io.ctl.dec_stall     := c.io.ctl.dec_stall
  dec.io.ctl.full_stall    := c.io.ctl.full_stall
  dec.io.ctl.dec_kill      := c.io.ctl.dec_kill
  dec.io.ctl.pipeline_kill := c.io.ctl.pipeline_kill
  dec.io.ctl.op1_sel       := c.io.ctl.op1_sel
  dec.io.ctl.op2_sel       := c.io.ctl.op2_sel
  dec.io.ctl.alu_fun       := c.io.ctl.alu_fun
  dec.io.ctl.wb_sel        := c.io.ctl.wb_sel
  dec.io.ctl.rf_wen        := c.io.ctl.rf_wen
  dec.io.ctl.mem_val       := c.io.ctl.mem_val
  dec.io.ctl.mem_fcn       := c.io.ctl.mem_fcn
  dec.io.ctl.mem_typ       := c.io.ctl.mem_typ
  dec.io.ctl.csr_cmd       := c.io.ctl.csr_cmd
  dec.io.ctl.br_type       := c.io.ctl.br_type

  // Control to Execute
  exe.io.ctl.full_stall    := c.io.ctl.full_stall
  exe.io.ctl.pipeline_kill := c.io.ctl.pipeline_kill
  exe.io.ctl.exe_pc_sel    := c.io.ctl.exe_pc_sel

  // Control to Memory
  mem.io.ctl.full_stall           := c.io.ctl.full_stall
  mem.io.ctl.pipeline_kill        := c.io.ctl.pipeline_kill
  mem.io.ctl.mem_exception        := c.io.ctl.mem_exception
  mem.io.ctl.mem_exception_cause  := c.io.ctl.mem_exception_cause

  // Dat: stage outputs → CtlPath
  c.io.dat.dec_valid           := dec.io.dec_valid
  c.io.dat.dec_inst            := dec.io.dec_inst
  c.io.dat.exe_br_eq           := exe.io.exe_br_eq
  c.io.dat.exe_br_lt           := exe.io.exe_br_lt
  c.io.dat.exe_br_ltu          := exe.io.exe_br_ltu
  c.io.dat.exe_br_type         := exe.io.exe_br_type
  c.io.dat.exe_inst_misaligned := exe.io.exe_inst_misaligned
  c.io.dat.mem_ctrl_dmem_val   := mem.io.mem_ctrl_dmem_val
  c.io.dat.mem_data_misaligned := mem.io.mem_data_misaligned
  c.io.dat.mem_store           := mem.io.mem_store
  c.io.dat.csr_eret            := mem.io.csr_eret
  c.io.dat.csr_interrupt       := mem.io.csr_interrupt
}
