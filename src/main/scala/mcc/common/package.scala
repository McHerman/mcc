package mcc.common

import chisel3._
import mcc.common.MemoryOpConstants

// Minimal core parameters for standalone mcc integration.
// Stripped of all chipyard/diplomacy dependencies.
case class MccCoreParams(
  ports:  Int     = 2,
  xprlen: Int     = 32,
  trace:  Boolean = false,
)

// memory.scala and other common files import `Constants._`
object Constants extends MemoryOpConstants
