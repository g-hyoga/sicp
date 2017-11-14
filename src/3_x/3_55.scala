object Main {
  def main(args: Array[String]): Unit = {

    def nature(): Stream[Int] = 1 #:: nature.map(_+1)

    def particalSums(s: Stream[Int]): Stream[Int] = s.head #:: (particalSums(s) zip s.tail map{ case (r: Int, t: Int) => r + t })

    println(particalSums(nature).take(10).toList)
  }
}
