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

// One in-flight fetch, indexed by the TileLink source id (0 or 1) it was
// issued with: occupied from the cycle its Get fires until decode consumes
// it, holding whatever the eventual response resolves to.
class FetchSlot(implicit val conf: MccCoreParams) extends Bundle {
  val valid    = Bool()  // issued, not yet consumed by decode
  val resolved = Bool()  // the D-channel response for this slot has arrived
  val killed   = Bool()  // discard as a bubble once resolved (redirect/fencei happened while in flight)
  val pc       = UInt(conf.xprlen.W)
  val data     = UInt(32.W)
}

class Fetch(implicit val p: Parameters, val conf: MccCoreParams, val bus: ATA8.MemBusConfig) extends Module {
  val io = IO(new Bundle {
    val imem                = new ATA8.TilelinkPort(bus.tlBus)
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
  
  // slots store information about issued instruction fetches. Altenates ID's
  val slots   = RegInit(VecInit(Seq.fill(2)(0.U.asTypeOf(new FetchSlot))))
  val issueId = RegInit(0.U(1.W))
  val headId  = RegInit(0.U(1.W))

  val if_pc_next = Wire(UInt(32.W))

  if_pc_next := Mux(io.ctl.exe_pc_sel === PC_4,      if_reg_pc + 4.asUInt(conf.xprlen.W),
                Mux(io.ctl.exe_pc_sel === PC_BRJMP,  io.exe_brjmp_target,
                Mux(io.ctl.exe_pc_sel === PC_JALR,   io.exe_jump_reg_target,
                /*Mux(io.ctl.exe_pc_sel === PC_EXC*/ io.exception_target)))

  // for a fencei, refetch the if_pc (assuming no stall, no branch, and no exception)
  when (io.ctl.fencei && io.ctl.exe_pc_sel === PC_4 &&
        !io.ctl.dec_stall && !io.ctl.full_stall && !io.ctl.pipeline_kill)
  {
     if_pc_next := if_reg_pc
  }

  val kill = io.ctl.if_kill || io.ctl.pipeline_kill

  //**********************************
  // Issue side: send the next PC as soon as its slot frees up, without
  // waiting for any earlier in-flight request's response.
  val issueSlotFree = !slots(issueId).valid

  io.imem.a.valid        := issueSlotFree
  io.imem.a.bits.opcode  := ATA8.TilelinkOpcodes.Get
  io.imem.a.bits.param   := 0.U
  io.imem.a.bits.size    := bus.dataBusSize.U
  io.imem.a.bits.source  := issueId
  io.imem.a.bits.address := if_reg_pc
  io.imem.a.bits.mask    := "b1111".U(bus.dataBusSize.W)
  io.imem.a.bits.data    := 0.U
  io.imem.a.bits.corrupt := 0.U

  // Do not change the PC again if the instruction is killed in previous cycles (when the PC has changed)
  when (kill || io.imem.a.fire)
  {
     if_reg_pc := if_pc_next
  }

  when (io.imem.a.fire) {
    slots(issueId).valid    := true.B
    slots(issueId).resolved := false.B
    slots(issueId).killed   := kill
    slots(issueId).pc       := if_reg_pc
    issueId := ~issueId
  }

  // Mark every already-in-flight slot killed so its eventual response is
  // discarded as a bubble instead of reaching decode.
  when (kill) {
    for (i <- 0 until 2) {
      when (slots(i).valid) { slots(i).killed := true.B }
    }
  }

  //**********************************
  // Response side: always accept immediately; file the data into whichever
  // slot the response's source id names.
  io.imem.d.ready := true.B

  when (io.imem.d.fire) {
    slots(io.imem.d.bits.source).data     := io.imem.d.bits.data
    slots(io.imem.d.bits.source).resolved := true.B
  }

  //**********************************
  // Decode-facing consumption: pop the head slot in program order once its
  // response has resolved.
  val head = slots(headId)

  when (io.ctl.pipeline_kill)
  {
     decReg.valid := false.B
     decReg.inst := BUBBLE
  }
  .elsewhen (!io.ctl.dec_stall && !io.ctl.full_stall)
  {
     when (head.valid && head.resolved)
     {
        val squash = head.killed || kill
        decReg.valid := !squash
        decReg.inst  := Mux(squash, BUBBLE, head.data)
        decReg.pc    := head.pc
        head.valid   := false.B
        headId       := ~headId
     }
     .otherwise
     {
        decReg.valid := false.B
        decReg.inst := BUBBLE
     }
  }
}
