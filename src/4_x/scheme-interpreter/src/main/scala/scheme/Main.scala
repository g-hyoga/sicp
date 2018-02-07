package scheme

import scheme.parser.Parser

object Main extends App {
  val code = "(define hoge (lambda (x) (+ 1 2 3)))"

  val parser = new Parser(code)
  parser.parse.foreach(println)
}



