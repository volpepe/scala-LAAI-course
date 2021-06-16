package utils

/* ...Or even like this, adding operators override */
class Rational3(x: Int, y: Int) {
  private def gcd(a: Int, b : Int): Int = // private method
    if (b == 0) a
    else gcd(b, a % b)
  val numer = x / gcd(x,y)
  val denom = y / gcd(x,y)
  def +(r: Rational3) = new Rational3(
    numer * r.denom + r.numer * denom,
    denom * r.denom)
  def neg = new Rational3(-numer, denom)
  def -(r: Rational3) = this + (r.neg)
  def <(r: Rational3) =
    numer * r.denom < r.numer * denom
  def max(r: Rational3) =
    if (this < r) r
    else this
}
/* This works because of the infix notation. Basically, +,-,*... are all unary functions.
 * This means that they take a single parameter. Also, they are called on another member of the same class.
 * For example, a sum is: 4.add(5) (or 4.+(5)).
 * Infix notation can be used in these cases, writing
 * 4 add 5 or 4 + 5.
 */