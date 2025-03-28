package io.github.armeddon

import MultiArgument._
import MultiArgument.implicits._
import Statement.implicits._
import StandartLibrary.number

import Evaluator.evaluate


object Main {
  def main(args: Array[String]) = {
    println(evaluate(program, bindings))
  }

  lazy val program: MultiArgument =
    Application(Variable("pred"), number(4))

  lazy val bindings: List[Statement.Binding] = {
    import StandartLibrary.bindings._
    List(zero, succ, add, mul, trueValue, falseValue, pred)
  }
}
