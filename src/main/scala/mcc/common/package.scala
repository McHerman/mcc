package mcc.common

import chisel3._
import mcc.common.MemoryOpConstants

// Minimal core parameters for standalone mcc integration.
// Stripped of all chipyard/diplomacy dependencies.
case class MccCoreParams(
  ports:  Int     = 2,
  xprlen: Int     = 32,
  trace:  Boolean = false,

  // ATAN hardware semaphores are not byte addressed, and should therefore be exemempt from the cores allignment checking
  // The following flags are used to specify regions that should not be checked for alligment
  mmioNoAlignCheckBase: Option[Long] = None,
  mmioNoAlignCheckSize: Long         = 0,
)

// memory.scala and other common files import `Constants._`
object Constants extends MemoryOpConstants
