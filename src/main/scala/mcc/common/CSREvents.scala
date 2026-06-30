package mcc.common

import chisel3._
import freechips.rocketchip.rocket.{EventSet, EventSets}

object CSREvents {
  val events = new EventSets(Seq(
    new EventSet((mask, hit) => false.B, Seq(("placeholder", () => false.B)))
  ))
}
