package scheme.Variable

import scheme.{Environment, Expression}

class Variable(val s: String, env: Environment) extends Expression {
  def lookup(s: String): Option[Expression] = {
    env.lookupVariableValue(s)
  }

  override def eval(): Expression = lookup(s).get

}
