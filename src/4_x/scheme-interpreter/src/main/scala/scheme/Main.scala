package scheme

import scheme.parser.{Node, Parser}

object Main extends App {
  val code =
    """
      (+ 1 2)
    """.stripMargin

  val parser = new Parser(code)
  val node: Node = parser.parse(0)
  val generator = new Generator
  println(generator.generate(node))
}



