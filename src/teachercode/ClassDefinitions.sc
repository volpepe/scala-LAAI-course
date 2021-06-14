////// CLASSES

// CLASSES

abstract class Greeter {
  val message:String
//  = "Hello World!"
  def SayHi() = println(message)
}

//val greeter = new Greeter()
//greeter.SayHi()

class SpanishGreeter extends Greeter {
  val message="Hola Mundo!" //explicit override
}

class ItalianGreeter extends Greeter {
  override val message="Ciao Mondo!" //explicit override
}

val greeter1: Greeter = new ItalianGreeter()
val greeter2: Greeter = new SpanishGreeter

greeter1.SayHi
greeter2.SayHi()

class GenericGreeter(override val message: String) extends Greeter {
//  override val message = msg
}

val greeter3: Greeter = new GenericGreeter("Bounjour Monde!")
greeter3.SayHi()

class GenericWithDefaultGreeter(override val message: String)
  extends Greeter {
  def this() = this("Hallo Welt!") //additional constructor
}

val greeter4 = new GenericWithDefaultGreeter("Hej Verden!")
greeter4.SayHi()
val greeter5 = new GenericWithDefaultGreeter
greeter5.SayHi()



// MODELING DATA

class Rational (x: Int, y: Int) {
  private def gcd(a:Int, b:Int): Int =
    if (b==0) a else gcd(b, a%b)
  val numer = x / gcd(x,y)
  val denom = y / gcd(x,y)
  def add(r: Rational) = new Rational(
    numer * r.denom + r.numer * denom, denom * r.denom)
  def neg = new Rational(-numer, denom)
  def sub(r: Rational) = this.add(r.neg)
  def < (r: Rational) =
    numer * r.denom < r.numer * denom
  //def max (r:Rational) =   ..
  override def toString = numer+"/"+denom
}

val r1=new Rational(10,6)

val r2=new Rational(8,5)

r1 < r2


// DATA STRUCTURES

abstract class IntSet {
  def isEmpty: Boolean
  def add(x: Int): IntSet
  def contains(x: Int): Boolean
  //def union(s: IntSet): IntSet
}

object Empty extends IntSet {
  def isEmpty = true
  def contains(x: Int): Boolean = false
  def add(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet)
  extends IntSet {
  def isEmpty = false
  def contains(x: Int): Boolean =
    if  (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  def add(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left add x, right)
    else if (x > elem) new NonEmpty(elem, left, right add x)
    else this
  override def toString = "{"+left+elem+right+"}"
}

val set1:IntSet = (Empty) add 3 add 2 add 5
val set2:IntSet = (Empty) add 1 add 4 add 5

set1 contains 2
set2 contains 2
//set1 union set2


