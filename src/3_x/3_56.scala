object Main {
  def main(args: Array[String]): Unit = {

    def nature(): Stream[Int] = 1 #:: nature.map(_+1)
    def multipleOf(i: Int): Stream[Int] = i #:: multipleOf(i).map(_*i)

    def merge(s1: Stream[Int], s2: Stream[Int]): Stream[Int] = {
      if (s1 == Stream.empty) s2
      else if (s2 == Stream.empty) s1
      else {
        if (s1.head < s2.head) s1.head #:: merge(s1.tail, s2)
        else if (s1.head > s2.head) s2.head #:: merge(s2.tail, s1)
        else s1.head #:: merge(s1.tail, s2.tail)
      }
    }

    def scaleStream(s: Stream[Int], n: Int): Stream[Int] = s.map( _ * n )

    def s(): Stream[Int] = 1 #:: merge(scaleStream(s, 2), merge(scaleStream(s, 3), scaleStream(s, 5)))

    println(s.take(15).toList)
  }
}
