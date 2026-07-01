package mcc.common

import chisel3._
import chisel3.util._

case class MemBusConfig(
  dataBusSize:   Int = 4,   // bytes per TileLink beat
  addrWidth:     Int = 32,  // address bits
  sourceWidth:   Int = 1,   // TileLink source-ID width
  semGenWidth:   Int = 0,   // bank-side semaphore generation width
)

object MemBusConfig {
  def default(): MemBusConfig = MemBusConfig()
}


object TilelinkOpcodes {
  // A channel opcodes
  val PutFullData    = 0.U(3.W)
  val PutPartialData = 1.U(3.W)
  val ArithmeticData = 2.U(3.W)
  val LogicalData    = 3.U(3.W)
  val Get            = 4.U(3.W)
  // D channel opcodes
  val AccessAck      = 0.U(3.W)
  val AccessAckData  = 1.U(3.W)
}

object ArithmeticDataParam {
  val MIN     = 0.U(3.W)
  val MAX     = 1.U(3.W)
  val MINU    = 2.U(3.W)
  val MAXU    = 3.U(3.W)
  val ADD     = 4.U(3.W)
  /// Custom extension
  val SUBU    = 5.U(3.W) // Subtract unsigned
  val AQGREQ  = 6.U(3.W) // Acquire when greater or equal to data
  val ADDU    = 7.U(3.W)   // Add unsigned
}

class TilelinkA(implicit c: MemBusConfig = MemBusConfig()) extends Bundle {
  val opcode  = UInt(3.W)
  val param   = UInt(3.W)
  val size    = UInt(24.W)
  val source  = UInt(c.sourceWidth.W)
  val address = UInt(c.addrWidth.W)
  val mask    = UInt(c.dataBusSize.W)
  val data    = UInt((c.dataBusSize * 8).W)
  val corrupt = UInt(1.W)
}

class TilelinkD(implicit c: MemBusConfig = MemBusConfig()) extends Bundle {
  val opcode  = UInt(3.W)
  val param   = UInt(2.W)
  val size    = UInt(24.W)
  val source  = UInt(c.sourceWidth.W)
  val sink    = UInt(c.sourceWidth.W)
  val denied  = UInt(1.W)
  val data    = UInt((c.dataBusSize * 8).W)
  val corrupt = UInt(1.W)
}

class TilelinkPort(implicit c: MemBusConfig = MemBusConfig()) extends Bundle {
  val a = Decoupled(new TilelinkA)
  val d = Flipped(Decoupled(new TilelinkD))
}
