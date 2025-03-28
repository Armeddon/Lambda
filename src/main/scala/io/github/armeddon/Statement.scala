package io.github.armeddon

sealed trait Statement
object Statement {
  case class Binding(name: String, value: Expression) extends Statement
  case class BindingMulti(name: String, value: MultiArgument) extends Statement
  object implicits {
    implicit def fromMulti(multi: BindingMulti): Binding =
      Binding(multi.name, multi.value.toExpression)
  }
}
