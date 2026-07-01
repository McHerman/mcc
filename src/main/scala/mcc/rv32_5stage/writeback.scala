//**************************************************************************
// RISCV Processor 5-Stage Writeback Stage
//--------------------------------------------------------------------------

package mcc.stage5

import chisel3._

import org.chipsalliance.cde.config.Parameters

import mcc.common._

// Register file read port — defined from Decode's perspective (requester).
// Use Flipped(new RegReadPort) in Writeback.
class RegReadPort(implicit val conf: MccCoreParams) extends Bundle {
  val rs1_addr = Output(UInt(5.W))
  val rs2_addr = Output(UInt(5.W))
  val rs1_data = Input(UInt(conf.xprlen.W))
  val rs2_data = Input(UInt(conf.xprlen.W))
}

class Writeback(implicit val p: Parameters, val conf: MccCoreParams) extends Module {
  val io = IO(new Bundle {
    val wb_in  = Input(new WbPort)
    val rfRead = Flipped(new RegReadPort)
    val ddpath = Flipped(new DebugDPath())
  })

  val regfile = Module(new RegisterFile())
  regfile.io.rs1_addr := io.rfRead.rs1_addr
  regfile.io.rs2_addr := io.rfRead.rs2_addr
  io.rfRead.rs1_data  := regfile.io.rs1_data
  io.rfRead.rs2_data  := regfile.io.rs2_data
  regfile.io.waddr    := io.wb_in.wbaddr
  regfile.io.wdata    := io.wb_in.wbdata
  regfile.io.wen      := io.wb_in.wen
  regfile.io.dm_addr  := io.ddpath.addr
  io.ddpath.rdata     := regfile.io.dm_rdata
  regfile.io.dm_en    := io.ddpath.validreq
  regfile.io.dm_wdata := io.ddpath.wdata
}
