
/////// TRAITS


trait Common {
  def philosophize()
}

trait Philosophical extends Common {
  override def philosophize() =
    println("I am "+toString+", therefore I am!")
  override def toString = "green"
}

trait Colored extends Common {
  override def philosophize() =
    println("It ain't easy being "+toString+"!")
}

class Animal {
}

class Frog extends Animal with Philosophical
  with Colored{
  override def toString = "green"
}

class Dog extends Animal with Colored
  with Philosophical {
  override def toString = "brown"
}

val frog = new Frog
val dog = new Dog

frog.philosophize()
dog.philosophize()





// TRAITS TO WRITE CODE TO BE APPLIED TO DIFFERENT CLASSES

class Rational(x: Int, y: Int) {
  val numer = x; val denom = y
  override def toString = numer + "/" + denom
}

class Segment(x: Int, y: Int) {
  val x_axis = x; val y_axis = y
  override def toString = "<" + x_axis + "," + y_axis + ">"
}

// how to define comparison operators >, <, >=, <=
// for both classes?

// TRAIT  TotOrder

trait TotOrder[T] {
  def compare(r:T): Double
  // positive if this is greater
  def > (r:T) = (this compare r) > 0
  def < (r:T) = (this compare r) < 0
  def >= (r:T) = !(this < r)
  def <= (r:T) = !(this > r)
}

trait Multip[T] {
  def sum(r1:T, r2:T): T
  def zero: T
  def * (x: Int) = multip(x,zero)
  private def multip(x:Int, acc: T): T =
    if (x==0) acc else multip(x-1, sum(this.asInstanceOf[T],acc))
}

class OrdRat(x: Int, y: Int) extends Rational(x, y)
  with TotOrder[OrdRat] with Multip[OrdRat] {
  def compare (r: OrdRat) =
    (numer * r.denom - r.numer * denom)
  def zero = new OrdRat(0,1)
  def sum(r1:OrdRat, r2: OrdRat) =
    new OrdRat(r1.numer * r2.denom + r2.numer * r1.denom,
      r1.denom * r2.denom)
}

class OrdSeg(x: Int, y: Int) extends Segment(x, y)
  with TotOrder[OrdSeg] with Multip[OrdSeg]{
  def compare (r: OrdSeg) = (
    Math.sqrt((x_axis * x_axis) + (y_axis * y_axis)) -
      Math.sqrt((r.x_axis * r.x_axis) + (r.y_axis * r.y_axis))
    )
  def zero = new OrdSeg(0,0)
  def sum(r1:OrdSeg, r2: OrdSeg) =
    new OrdSeg(r1.x_axis+r2.x_axis, r1.y_axis+r2.y_axis)
}

new OrdRat(5,3) > new OrdRat(9,5)
new OrdSeg(5,3) > new OrdSeg(4,4)
new OrdRat(5,3) * 2
new OrdSeg(5,3) * 2



