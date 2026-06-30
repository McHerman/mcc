//**************************************************************************
// RISCV Processor 5-Stage Datapath
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2012 Jan 13
//
// TODO refactor stall, kill, fencei, flush signals. They're more confusing than they need to be.

package mcc.stage5

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket.{CSR, CSRFile, Causes}
import freechips.rocketchip.rocket.CoreInterrupts

import mcc.stage5.Constants._
import mcc.common._


class FetchToDec(implicit val conf: MccCoreParams) extends Bundle()
{
  //val valid         = Bool(false.B)
  val valid         = Bool()
  //val inst          = RegInit(BUBBLE)
  val inst          = UInt(32.W) 
  val pc            = UInt(conf.xprlen.W)
}

class Fetch(implicit val p: Parameters, val conf: MccCoreParams) extends Module {
  val io = IO(new Bundle {
    val imem                = new MemPortIo(conf.xprlen)
    val ctl                 = Flipped(new CtlToDatIo())
    val reset_vector        = Input(UInt())
    val toDec               = new FetchToDec
    val exe_brjmp_target    = Input(UInt(32.W))
    val exe_jump_reg_target = Input(UInt(32.W))
    val exception_target    = Input(UInt(32.W))
  })
  io := DontCare

  //**********************************
  // Pipeline State Registers

  val if_reg_pc = RegInit(io.reset_vector)
  val decReg    = Reg(new FetchToDec)

  io.toDec := decReg

  //**********************************
  // Instruction Fetch Stage
  val if_pc_next = Wire(UInt(32.W))
  
  // Instruction fetch buffer
  val if_buffer_in = Wire(new DecoupledIO(new MemResp(conf.xprlen)))
  if_buffer_in.bits := io.imem.resp.bits
  if_buffer_in.valid := io.imem.resp.valid
  assert(!(if_buffer_in.valid && !if_buffer_in.ready), "Instruction backlog")
  
  val if_buffer_out = Queue(if_buffer_in, entries = 1, pipe = false, flow = true)
  if_buffer_out.ready := !io.ctl.dec_stall && !io.ctl.full_stall
  
  // Instruction PC buffer
  val if_pc_buffer_in = Wire(new DecoupledIO(UInt(conf.xprlen.W)))
  if_pc_buffer_in.bits := if_reg_pc
  if_pc_buffer_in.valid := if_buffer_in.valid
  
  val if_pc_buffer_out = Queue(if_pc_buffer_in, entries = 1, pipe = false, flow = true)
  if_pc_buffer_out.ready := if_buffer_out.ready
  
  // Instruction fetch kill flag buffer
  val if_reg_killed = RegInit(false.B)
  when ((io.ctl.pipeline_kill || io.ctl.if_kill) && !if_buffer_out.fire)
  {
     if_reg_killed := true.B
  }
  when (if_reg_killed && if_buffer_out.fire)
  {
     if_reg_killed := false.B
  }
  
  // Do not change the PC again if the instruction is killed in previous cycles (when the PC has changed)
  when ((if_buffer_in.fire && !if_reg_killed) || io.ctl.if_kill || io.ctl.pipeline_kill)
  {
     if_reg_pc := if_pc_next
  }
  
  val if_pc_plus4 = (if_reg_pc + 4.asUInt(conf.xprlen.W))
  
  if_pc_next := Mux(io.ctl.exe_pc_sel === PC_4,      if_pc_plus4,
                Mux(io.ctl.exe_pc_sel === PC_BRJMP,  io.exe_brjmp_target,
                Mux(io.ctl.exe_pc_sel === PC_JALR,   io.exe_jump_reg_target,
                /*Mux(io.ctl.exe_pc_sel === PC_EXC*/ io.exception_target)))
  
  // for a fencei, refetch the if_pc (assuming no stall, no branch, and no exception)
  when (io.ctl.fencei && io.ctl.exe_pc_sel === PC_4 &&
        !io.ctl.dec_stall && !io.ctl.full_stall && !io.ctl.pipeline_kill)
  {
     if_pc_next := if_reg_pc
  }
  
  // Instruction Memory
  io.imem.req.valid := if_buffer_in.ready
  io.imem.req.bits.fcn := M_XRD
  io.imem.req.bits.typ := MT_WU
  io.imem.req.bits.addr := if_reg_pc
  
  when (io.ctl.pipeline_kill)
  {
     decReg.valid := false.B
     decReg.inst := BUBBLE
  }
  .elsewhen (!io.ctl.dec_stall && !io.ctl.full_stall)
  {
     when (io.ctl.if_kill || if_reg_killed)
     {
        decReg.valid := false.B
        decReg.inst := BUBBLE
     }
     .elsewhen (if_buffer_out.valid)
     {
        decReg.valid := true.B
        decReg.inst := if_buffer_out.bits.data
     }
     .otherwise
     {
        decReg.valid := false.B
        decReg.inst := BUBBLE
     }
  
     decReg.pc := if_pc_buffer_out.bits
  }
  
}
