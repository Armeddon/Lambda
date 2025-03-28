package io.github.armeddon

import scala.collection.immutable.{Set, Map, Nil}
import io.github.armeddon.Expression.{Variable, Definition, Application}

import scala.util.Random

object Evaluator {
  val randomNameLength = 3
  def evaluate(
      main: Expression,
      bindings: List[Statement.Binding]
  ): Option[Expression] = {
    for {
      bindings <- defineAll(bindings, Map())
      bindingsMap = bindings.map(bd => bd.name -> bd.value).toMap
      newMain <- evaluate(
        main,
        bindingsMap,
        Set()
      )
      result <-
        if (main == newMain) Some(main)
        else evaluate(newMain, bindings)
    } yield result
  }

  private def defineAll(
      bindings: List[Statement.Binding],
      functions: Map[String, Expression]
  ): Option[List[Statement.Binding]] = bindings match {
    case head :: tail =>
      for {
        head <- define(head, functions)
        tail <- defineAll(tail, functions + (head.name -> head.value))
      } yield head :: tail
    case Nil => Some(Nil)
  }

  private def define(
      binding: Statement.Binding,
      functions: Map[String, Expression]
  ): Option[Statement.Binding] =
    binding match {
      case Statement.Binding(name, value) =>
        evaluate(binding.value, functions, Set()).map(
          Statement.Binding(name, _)
        )
    }

  private def evaluate(
      expression: Expression,
      functions: Map[String, Expression],
      variables: Set[String]
  ): Option[Expression] = for {
    result <- expression match {
      case Variable(name) =>
        if (variables(name)) Some(Variable(name))
        else functions.get(name)
      case Definition(variable, value) =>
        for {
          value <- evaluate(value, functions, variables + variable)
        } yield Definition(
          variable,
          value
        )
      case Application(function, argument) =>
        for {
          function <- evaluate(function, functions, variables)
          argument <- evaluate(argument, functions, variables)
          application <- applyExpression(
            function,
            argument,
            functions,
            variables
          )
        } yield application
    }
    fin <-
      if (result == expression) Some(result)
      else evaluate(result, functions, variables)
  } yield fin

  private def applyExpression(
      function: Expression,
      argument: Expression,
      functions: Map[String, Expression],
      variables: Set[String]
  ): Option[Expression] = function match {
    case Variable(name) =>
      if (variables(name)) Some(Application(function, argument))
      else
        functions
          .get(name)
          .flatMap(applyExpression(_, argument, functions, variables))
    case Definition(variable, value) => {
      val oldNames = getVariableNames(argument)
      val usedNames = variables ++ getVariableNames(function)
      val (newNames, _) =
        oldNames.foldLeft((Map.empty[String, String], usedNames)) {
          case ((mapping, usedNames), oldName) =>
            val newName = generateUniqueName(usedNames)
            (mapping + (oldName -> newName), usedNames + newName)
        }
      Some(
        substitute(
          value,
          variable,
          newNames.foldLeft(argument) { case (arg, (oldName, newName)) =>
            rename(arg, oldName, newName)
          }
        )
      )
    }
    case Application(func, arg) =>
      Some(Expression.Application(function, argument))
  }

  private def substitute(
      expr: Expression,
      name: String,
      value: Expression
  ): Expression = expr match {
    case Variable(variable) =>
      if (name == variable) value else Variable(variable)
    case Definition(variable, result) =>
      if (name == variable) Definition(variable, result)
      else Definition(variable, substitute(result, name, value))
    case Application(function, argument) =>
      Application(
        substitute(function, name, value),
        substitute(argument, name, value)
      )
  }

  private def rename(
      expr: Expression,
      name: String,
      newName: String
  ): Expression = expr match {
    case Variable(variable) =>
      if (name == variable) Variable(newName) else Variable(variable)
    case Definition(variable, value) =>
      if (name == variable) Definition(newName, rename(value, name, newName))
      else Definition(variable, rename(value, name, newName))
    case Application(function, argument) =>
      Application(
        rename(function, name, newName),
        rename(argument, name, newName)
      )
  }

  private def getVariableNames(
      expr: Expression
  ): Set[String] = expr match {
    case Variable(name)              => Set()
    case Definition(variable, value) => getVariableNames(value) + variable
    case Application(function, argument) =>
      getVariableNames(function) ++ getVariableNames(argument)
  }

  private def generateUniqueName(
      names: Set[String]
  ): String = {
    val chars = ('a' to 'z')
    def randomString: String = (1 to randomNameLength)
      .map(_ => chars(Random.nextInt(chars.length)))
      .mkString

    Iterator.continually(randomString).find(!names.contains(_)).get
  }
}
