package scheme.parser

case class Node(value: String, var nodes: Seq[Node]) {
  def setNode(ns: Seq[Node]) = {
    nodes = ns
  }
}

class Parser(input: String) {

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

    def go(node: Node): Node = seq match {
      case head :: Nil => {
        node.setNode(node.nodes :+ new Node(head, Seq()))
        node
      }
      case head :: tail if (head == ")") => {
        seq = seq.drop(1)
        node
      }
      case head :: tail if (head == "(") => {
        seq = seq.drop(1)
        val newNode =  go(new Node("", Seq()))
        node.setNode(node.nodes :+ newNode)
        go(node)
      }
      case head :: tail => {
        node.setNode(node.nodes :+ new Node(head, Seq()))
        seq = seq.drop(1)
        go(node)
      }
    }

    go(new Node("", Seq())).nodes
  }

}
