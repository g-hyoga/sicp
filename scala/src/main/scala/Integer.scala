class Integer(a: Int) {
  val num: Int = a
  val tag = "integer"
  val contents = Array(num)

  def add(b: Integer): Integer = {
    new Integer(num + b.num)
  }

  def sub(b: Integer): Integer = {
    new Integer(num - b.num)
  }

  def mul(b: Integer): Integer = {
    new Integer(num * b.num)
  }

  def div(b: Integer): Integer = {
    new Integer(num / b.num)
  }

  def equ(b: Integer): Boolean = {
    num == b.num
  }

  def raise(): Rational = {
    new Rational(num, 1)
  }

  def equZero(): Boolean = {
    num == 0
  }

  override def toString() = { num.toString }
}


