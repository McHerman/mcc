package mcc.common

import chisel3._
import chisel3.util._

class DebugDPath(implicit val conf: MccCoreParams) extends Bundle {
  val addr     = Output(UInt(5.W))
  val wdata    = Output(UInt(32.W))
  val validreq = Output(Bool())
  val rdata    = Input(UInt(32.W))
  val resetpc  = Output(Bool())
}

class DebugCPath(implicit val conf: MccCoreParams) extends Bundle {
  val halt = Output(Bool())
}
