class Rational(n: Double, d: Double) {
  require(d != 0)
  private val g = gcd(n.abs, d.abs)

  def numer: Double = n / g

  def denom: Double = d / g

  def contents(r: Rational): Double = r.numer / r.denom

  def this(n: Double) = this(n, 1)

  def add(that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def sub(that: Rational): Rational =
    new Rational(
      numer * that.denom - that.numer * denom,
      denom * that.denom
    )

  def mul(that: Rational): Rational =
    new Rational(numer * that.numer, denom * that.denom)

  def div(that: Rational): Rational =
    new Rational(numer * that.denom, denom * that.numer)

  def equ(that: Rational): Boolean = {
    (that.numer == numer) && (that.denom == denom)
  }

  def isZero(that: Rational): Boolean = {
    that.numer == 0
  }

  def raise(): Real = {
    new Real(numer / denom)
  }

  private def gcd(a: Double, b: Double): Double =
    if (b == 0) a else gcd(b, a % b)

  override def toString = numer + "/" + denom
}



