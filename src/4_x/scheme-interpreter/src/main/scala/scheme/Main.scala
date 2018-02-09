package scheme

import scheme.parser.{Node, Parser}

object Main extends App {
  val code =
    """
       (+ 1 2)
       (define plus1 (lambda (x) (+ x 1)))
    """.stripMargin

  val parser = new Parser(code)
  val node: Node = parser.parse(0)

  println("hoge")

  val env: Environment = new Environment(Map())
  val evaluator = new Evaluator
  println(evaluator.eval(node, env))
}



