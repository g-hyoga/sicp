abstract class Complex {
  val realPart: Double
  val imagPart: Double
  val magnitude: Double
  val angle: Double

  def add[A](a: A, contents: (A => Double)*): Rectangular = {
    if (contents.length == 1) {
      new Rectangular(realPart + contents(0)(a), imagPart)
    }else if (contents.length == 2) {
      new Rectangular(realPart + contents(0)(a), imagPart + contents(1)(a))
    }
    error("引数多い")
  }

  def sub(c: Complex): Rectangular = {
    new Rectangular(realPart - c.realPart, imagPart - c.imagPart)
  }

  def mul(c: Complex): Polar = {
    new Polar(magnitude * c.magnitude, angle + c.angle)
  }

  def div(c: Complex): Polar = {
    new Polar(magnitude / c.magnitude, angle - c.angle)
  }

}

class Rectangular(r: Double, i: Double) extends Complex {
  val realPart = r
  val imagPart = i
  val magnitude: Double = magnitude(this)
  val angle: Double = angle(this)

  private def magnitude(r: Rectangular): Double = {
    Math.sqrt(Math.pow(realPart, 2) + Math.pow(imagPart, 2))
  }

  private def angle(r: Rectangular): Double = {
    Math.atan2(imagPart, realPart)
  }

  def toPolar(r: Rectangular): Polar = {
    new Polar(magnitude, angle)
  }

  def isZero(c: Complex): Boolean = {
    (c.realPart == 0) && (c.imagPart == 0)
  }

  override def toString() = {
    "realPart:" + realPart + " imagPart:" + imagPart
  }

}

class Polar(m: Double, a: Double) extends Complex {
  val magnitude = m
  val angle = a
  val realPart: Double = realPart(this)
  val imagPart: Double = imagPart(this)

  private def realPart(p: Polar): Double = {
    magnitude * Math.cos(Math.PI * angle / 180.0)
  }

  private def imagPart(p: Polar): Double = {
    magnitude * Math.sin(Math.PI * angle / 180.0)
  }

  def toRectangular(p: Polar): Rectangular = {
    new Rectangular(realPart, imagPart)
  }

  def isZero(c: Complex): Boolean = {
    c.magnitude == 0
  }

  override def toString() = {
    "magnitude:" + magnitude + " angle:" + angle
  }
}
