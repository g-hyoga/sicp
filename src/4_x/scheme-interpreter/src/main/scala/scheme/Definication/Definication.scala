package scheme.Definication

import scheme.{Environment, Expression}

class Definication(variable: String, value: Expression) extends Expression with Environment {
  override val variables: Map[String, Expression]

  override def eval(): Unit = {
    val definication: Map[String, Expression] = Map(variable -> value)
    variables ++ definication
  }

}
