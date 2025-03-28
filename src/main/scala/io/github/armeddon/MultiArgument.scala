package io.github.armeddon

import scala.collection.immutable.Nil

sealed trait MultiArgument {
  override def toString: String
  def toExpression: Expression
}
object MultiArgument {
  case class Variable(name: String) extends MultiArgument {
    override def toString = name
    def toExpression = Expression.Variable(name)
  }
  case class Definition(names: List[String], value: MultiArgument)
      extends MultiArgument {
    override def toString = s"(${names.mkString(" ")} -> ${value.toString})"
    def toExpression = names match {
      case name :: rest =>
        Expression.Definition(name, Definition(rest, value).toExpression)
      case Nil => value.toExpression
    }
  }
  object Definition {
    def apply(name: String, value: MultiArgument): MultiArgument =
      Definition(List(name), value)
  }
  case class Application(
      function: MultiArgument,
      arguments: List[MultiArgument]
  ) extends MultiArgument {
    override def toString =
      s"(${function.toString} ${arguments.map(_.toString).mkString(" ")})"
    def toExpression = arguments match {
      case argument :: Nil =>
        Expression.Application(function.toExpression, argument.toExpression)
      case argument :: rest =>
        Application(Application(function, List(argument)), rest).toExpression
      case _ => throw new Exception("The function called on 0 arguments")
    }
  }
  object Application {
    def apply(function: MultiArgument, argument: MultiArgument): MultiArgument =
      Application(function, List(argument))
  }
  object implicits {
    implicit def multiToExpression(multi: MultiArgument): Expression =
      multi.toExpression
  }
  def fromExpression(expr: Expression): MultiArgument = expr match {
    case Expression.Variable(name) => Variable(name)
    case Expression.Definition(variable, value) =>
      fromExpression(value) match {
        case Definition(names, value) =>
          Definition(variable :: names, value)
        case value => Definition(variable, value)
      }
    case Expression.Application(function, argument) =>
      fromExpression(function) match {
        case Application(function, arguments) =>
          Application(
            function,
            arguments :+ fromExpression(argument)
          )
        case function =>
          Application(function, fromExpression(argument))
      }
  }
}
