class Real(a: Double) {
  val num = a

  def add[B](b: B, contents: (B => Double)*): Real = {
    if (contents.length == 1) {
      new Real(num + contents(0)(b))
    } else if (contents.length == 2) {
      new Real(num + contents(0)(b) / contents(1)(b))
    }
    error("引数多い")
  }

  def sub(b: Real): Real = {
    new Real(num - b.num)
  }

  def mul(b: Real): Real = {
    new Real(num * b.num)
  }

  def div(b: Real): Real = {
    new Real(num / b.num)
  }

  def equ(b: Real): Boolean = {
    num == b.num
  }

  def equZero(): Boolean = {
    num == 0.0
  }

  def raise(): Complex = {
    new Rectangular(num, 0.0)
  }

  override def toString() = { num.toString }
}
