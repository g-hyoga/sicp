package scheme

import scheme.SelfEvaluating.Num.Num
import scheme.SelfEvaluating.Str.Str

object Main extends App {
  val num: Num = new Num(3)
  val str: Str = new Str("hoge")

  println(num.eval())
  println(str.eval())
}



