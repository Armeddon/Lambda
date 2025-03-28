package io.github.armeddon

object StandartLibrary {
  import MultiArgument._
  object bindings {
    val zero =
      Statement.BindingMulti(
        "zero",
        Definition(List("f", "x"), Variable("x"))
      )
    val succ = Statement.BindingMulti(
      "succ",
      Definition(
        List("n", "f", "x"),
        Application(
          Variable("f"),
          Application(
            Variable("n"),
            List(
              Variable("f"),
              Variable("x")
            )
          )
        )
      )
    )
    val add = Statement.BindingMulti(
      "add",
      Definition(
        List("n", "m"),
        Application(
          Variable("n"),
          List(Variable("succ"), Variable("m"))
        )
      )
    )
    val mul = Statement.BindingMulti(
      "mul",
      Definition(
        List("n", "m"),
        Application(
          Variable("n"),
          List(
            Application(Variable("m"), Variable("succ")),
            Variable("zero")
          )
        )
      )
    )
    val trueValue = Statement.BindingMulti(
      "true",
      Definition(List("a", "b"), Variable("a"))
    )
    val falseValue = Statement.BindingMulti(
      "false",
      Definition(List("a", "b"), Variable("b"))
    )
    val pred = Statement.BindingMulti(
      "pred",
      Definition(
        List("n", "f", "x"),
        Application(
          Variable("n"),
          List(
            Definition(
              List("g", "h"),
              Application(
                Variable("h"),
                Application(Variable("g"), Variable("f"))
              )
            ),
            Definition("u", Variable("x")),
            Definition("u", Variable("u"))
          )
        )
      )
    )
  }
  def number(num: Int): MultiArgument =
    if (num <= 0) bindings.zero.value
    else Application(Variable("succ"), number(num - 1))

}
