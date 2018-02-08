package scheme.Definication

import scheme.{Environment, Expression}

class Definication(variable: String, value: Expression) extends Expression {
  override def eval(): Expression = {
    val definication: Map[String, Expression] = Map(variable -> value)
    this
  }

}
