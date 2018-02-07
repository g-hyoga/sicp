package scheme

import scheme.SelfEvaluating.Num.Num
import scheme.SelfEvaluating.Str.Str
import scheme.parser.Node

class Generator {
  def generate(node: Node): Expression = {
    if (node.value.matches("""\d+""")) {
      new Num(node.value.toDouble)
    } else if (node.value.matches("""\"*\"""")) {
      new Str(node.value)
    } else {
      new Str(node.value)
    }
  }
}
