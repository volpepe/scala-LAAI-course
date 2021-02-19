package utils

/*
Complex data structures can be defined with classes.

For example, we can create Rational numbers this way:
- a rational number is a pair of integers representing the numerator and the denominator
 */

class Rational(val x: Int, val y: Int) {
  def add(r:Rational) = new Rational(
    x*r.y + r.x*y, y * r.y
  )
  def neg = new Rational(-x, y)
  def sub(r: Rational): Rational = add(r.neg)
}