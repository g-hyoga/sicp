package scheme

class Environment(variables: Map[String, Expression]) {
  def lookupVariableValue(k: String): Option[Expression] = {
    return variables.get(k)
  }

  def setVariable(key: String, expr: Expression): Environment = {
    return new Environment(variables ++ Map(key -> expr))
  }


}
