package scheme

import scheme.SelfEvaluating.Num.Num
import scheme.parser.Node

class Evaluator {
  def eval(node: Node, env: Environment): Expression = {
    if (node.value.matches("""\d+""")) {
      new Num(node.value.toDouble)
    } else {
      apply(node, env)
    }
  }

  def apply(node: Node, env: Environment): Num = {
    val procedure = (a: Num, b: Num) => a + b
    val args: Seq[Num] = node.nodes.tail.map(eval(_, env))

    args.map(_.eval()).fold(0)((a, b) => a + b)
  }
}
