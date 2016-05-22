package jml.example.scalajsrxdom

class Rational(n: Int, d: Int) {
  private val pgcd = Rational.pgcd(scala.math.max(n.abs,d.abs), scala.math.min(n.abs,d.abs))
  val numer: Int = n.abs / pgcd * scala.math.signum(n) * scala.math.signum(d)
  val denum: Int = d.abs / pgcd
  override def toString():String = if( denum==1 ) numer.toString else numer.toString + "/" + denum.toString
}

object Rational {
  def pgcd(a:Int, b:Int):Int = if(b==0) a else pgcd(b, a%b) 
  def apply(n:Int) = new Rational(n, 1)
  def apply(n:Int, d:Int) = new Rational(n, d)
 
  implicit object Fraction extends scala.math.Fractional[Rational] {
    def compare(x: Rational, y: Rational): Int = x.numer*y.denum - y.numer*x.denum
    def plus(x: Rational, y: Rational): Rational = Rational(x.numer*y.denum + y.numer*x.denum, x.denum * y.denum)
    def minus(x: Rational, y: Rational): Rational = Rational(x.numer*y.denum - y.numer*x.denum, x.denum * y.denum)
    def times(x: Rational, y: Rational): Rational = Rational(x.numer*y.numer, x.denum * y.denum)
    def div(x: Rational, y: Rational): Rational = Rational(x.numer*y.denum, x.denum * y.numer)
    def negate(x: Rational): Rational = Rational(-x.numer, -x.denum)
    def fromInt(x: Int): Rational = Rational(x)
    def toInt(x: Rational): Int = x.numer.toInt / x.denum.toInt
    def toLong(x: Rational): Long = x.numer.toLong / x.denum.toLong
    def toFloat(x: Rational): Float = x.numer.toFloat / x.denum.toFloat
    def toDouble(x: Rational): Double = x.numer.toDouble / x.denum.toDouble
  }

  private [Rational] val Parser = "([0-9]+)\\(/([0-9]+)\\)?".r
  def unapply(s: String): Option[Rational] = s match {
    case Parser(numer, denum) => Some(Rational(numer.toInt, denum.toInt))
    case _ => None
  }
}