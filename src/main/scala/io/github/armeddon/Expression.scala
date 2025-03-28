package io.github.armeddon

sealed trait Expression {
  override def toString = MultiArgument.fromExpression(this).toString
}
object Expression {
  case class Variable(name: String) extends Expression
  case class Definition(variable: String, value: Expression) extends Expression
  case class Application(function: Expression, argument: Expression)
      extends Expression
}
