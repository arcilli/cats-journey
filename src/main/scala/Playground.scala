package org.arrnaux

import cats.Eval

object Playground {
  val meaningOfLife = Eval.later {
    println("computing abstractions")
    42
  }

  def main(args: Array[String]): Unit = {
    println(meaningOfLife.value)
  }
}
