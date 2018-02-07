package scheme.parser

class Parser(input: String) {
  case class Node(value: String, var nodes: Seq[Node]) {
    def setNode(ns: Seq[Node]) = {
      nodes = ns
    }
  }

  def tokenize: Seq[String] = {
    input.replaceAll("\n", "")
      .replaceAll("\t", "")
      .replaceAll(" {2}", " ")
      .replaceAll("\\(", " ( ")
      .replaceAll("\\)", " ) ")
      .split(" ")
      .filter(_ != "")
  }

  def parse: Seq[Node] = {
    var seq: Seq[String] = tokenize.toList

    def go(node: Node): Seq[Node] = seq match {
      case head :: Nil => node.nodes
      case head :: tail if (head == ")") => node.nodes
      case head :: tail if (head == "(") => {
        seq.drop(1)
        node.nodes :+ new Node("", go(node))
      }
      case head :: tail => {
        node.setNode(node.nodes :+ new Node(head, Seq()))
        seq.drop(1)
        go(node)
      }
    }

    go(new Node("", Seq()))
  }

}
