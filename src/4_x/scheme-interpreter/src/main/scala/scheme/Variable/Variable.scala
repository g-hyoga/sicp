package scheme.Variable

import scheme.{Environment, Expression}

class Variable(val s: String, val vs: Map[String, Expression]) extends Expression with Environment {
  override val variables: Map[String, Expression] = vs

  def lookup(s: String): Expression = {
    variables.getOrElse(s, new Expression {
      override def eval(): String =  ""
    })
  }

  override def eval(): Expression = lookup(s)

}
