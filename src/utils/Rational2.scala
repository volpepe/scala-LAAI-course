package utils

/* The class can be improved as follows using private methods: */
class Rational2 (x: Int, y: Int) {
  private def gcd(a: Int, b : Int): Int = // private method
    if (b == 0) a
    else gcd(b, a % b)
  val numer = x / gcd(x,y)
  val denom = y / gcd(x,y)
  def add(r: Rational2) = new Rational2(
    numer * r.denom + r.numer * denom,
    denom * r.denom)
  def neg = new Rational2(-numer, denom)
  def sub(r: Rational2) = add(r.neg)
  def less(r: Rational2) =
    numer * r.denom < r.numer * denom
  def max(r: Rational2) =
    if (this.less(r)) r
    else this
}

