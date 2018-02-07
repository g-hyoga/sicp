package scheme

import scheme.Definication.Definication
import scheme.SelfEvaluating.Num.Num
import scheme.SelfEvaluating.Str.Str
import scheme.parser.Node

case class ExprNode(expr: Expression, exprNodes: Seq[ExprNode])

class Generator {
  private def generate(str: String): Expression = {
    if (str.matches("""\d+""")) {
      new Num(str.toDouble)
    } else if (str.matches("""\"*\"""")) {
      new Str(str)
    } else if (str.matches("""define""")) {
      new Definication(str)
    } else {
      throw new Error("generator error")
    }
  }

  def generate(node: Node): ExprNode = {
    new ExprNode(generate(node.value), node.nodes.map(generate(_)))
  }
}
