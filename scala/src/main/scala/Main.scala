object Main {

  def main(args: Array[String]): Unit = {
    val c1 = new Rectangular(1, 2)
    val c2 = new Polar(1, 45)

    val r1 = new Real(1.2)
    val r2 = new Real(2.3)

    val q1 = new Rational(1, 2)
    val q2 = new Rational(2, 1)

    val z1 = new Integer(1)
    val z2 = new Integer(2)

    println(c1.add(q1, q1.contents))
    println(r1.add(q1, q1.numer, q1.denom))
  }
}
