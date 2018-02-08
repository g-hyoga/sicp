package scheme

import scheme.Definication.Definication
import scheme.SelfEvaluating.Num.Num
import scheme.SelfEvaluating.Str.Str
import scheme.parser.Node

case class ExprNode(expr: Expression, exprNodes: Seq[ExprNode])

class Generator {
  private def go(node: Node): Expression = {
    if (node.value.matches("""\d+""")) { // number
      new Num(node.value.toDouble)
    } else if (node.value.matches("""\"*\"""")) { // string
      new Str(node.value)
    } else if (node.value.matches("""define""")) {
      new Definication()
    }
  }

  def generate(node: Node): ExprNode = {
    new ExprNode(go(node), node.nodes.map(generate(_)))
  }
}
