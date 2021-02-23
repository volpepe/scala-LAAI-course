/*********************** LESSON 4 ****************************/
/**
 * Traits are semi-implemented interfaces.
 * They can contain an implementation for some of the methods
 * but cannot be instantiated until the rest of the methods are implemented.
 */
trait Common {
    // basically an interface
    def philosophize()
}

/**
 * Traits can be extended easily.
 */
trait Philosophical extends Common {
    def philosophize() = println("I am "+toString+", therefore I am!")
}

trait Colored extends Common {
    def philosophize() = println("It ain't easy being "+toString+"!")
}

/**
 * In Java, a class can implement an arbitrary number of interfaces, but only extend one class.
 * This is why traits can be important.
 */

class Animal{}

/**
 * We can basically import some code from traits that can be reused
 * without explicitly being part of a specific class/abstract class
 */
class Frog extends Animal with Philosophical {
    // special with keyword for traits
    override def toString: String = "green"
}

class Dog extends Animal with Colored {
    override def toString: String = "brown"
}

val frog = new Frog
val dog = new Dog

frog.philosophize()
dog.philosophize()

/**
 * Note: for a class we can inherit from multiple traits, BUT
 * if we have two different implementations for the same method
 * the compiler will give an error.
 * We can actually allow the coexistence of two implementations for
 * the same methods using the override keyword specifying an importing order
 */

trait Colored extends Common {
    // override, so we are saying to consider this one over Philosophical
    override def philosophize(): Unit = println("It ain't easy being "+toString+"!")
}

// only works if Colored is after Philosophical
class Cat extends Animal with Philosophical
                         with Colored {
    override def toString: String = "orange"
}

val cat = new Cat
cat.philosophize()

/**
 * Many functionalities in the standard library are implemented as traits
 */


/**
 * Another less trivial example: we want to use traits as additional
 * properties for some classes that we defined previously
 */
class Rational(x:Int, y:Int){
    val numer = x
    val denom = y
    override def toString: String = numer + "/" + denom
}

class Segment(x:Int, y:Int) {
    val x_axis = x
    val y_axis = y
    override def toString: String = "<"+x_axis+","+y_axis+">"
}

trait TotOrder[T] { // unknown type
    // the compare mechanism has to be implemented externally by
    // whoever extends the trait
    def compare(r:T): Double
    // return a positive number if the value of the same type
    // is bigger, a negative one if the value is smaller...
    def >(r:T) = (this compare r) > 0
    def <(r:T) = (this compare r) < 0
    def >=(r:T) = !(this < r)
    def <=(r:T) = !(this > r)
}

// This other trait defines multiplication for any class
// where a sum and a zero element is defined
trait Multip[T]{
    def sum(r1:T, r2:T):T
    def zero: T
    def *(x:Int) = multip(x,zero)
    private def multip(x:Int, acc:T):T =
        if (x==0) acc else multip(x-1, sum(this.asInstanceOf[T],acc))
}

// We can use these two traits in a class that defines a total
// ordered Rational
class OrdRat(x:Int, y:Int) extends Rational(x,y)
    with TotOrder[OrdRat] with Multip[OrdRat] {
    // compare is implemented as the difference between rationals
    def compare(r:OrdRat) = (numer*r.denom-r.numer*denom)
    // zero element for the multiplication or sum
    def zero = new OrdRat(0,1)
    // implementation of sum
    def sum(r1:OrdRat,r2:OrdRat) =
        new OrdRat( r1.numer*r2.denom+r2.numer*r1.denom, //new num
            r1.denom*r2.denom) // new denom
}

// Also, we can simply add these functionalities to the Segment class
class OrdSeg(x:Int, y:Int) extends Segment(x,y)
    with TotOrder[OrdSeg] with Multip[OrdSeg] {
    def compare(r:OrdSeg) = (
        // norm 2
        Math.sqrt((x_axis*x_axis) + (y_axis*y_axis)) -
        Math.sqrt((r.x_axis*r.x_axis) + (r.y_axis*r.y_axis))
    )
    def zero = new OrdSeg(0,0)
    def sum(r1:OrdSeg, r2:OrdSeg) =
        new OrdSeg(r1.x_axis+r2.x_axis, r1.y_axis+r2.y_axis)
}

// From these examples we can see that traits are used to enrich user-defined
// data types by adding some properties to the class.

val x1 = new OrdRat(10,3)
val x2 = new OrdRat(20,17)
x1<x2
x1>x2
x1*5

/**
 * It is also easy to extend the language. Let's try and redefine the boolean class
 */
trait Bool {
    // if ifThenElse is true, we return the first parameter,
    // otherwise the second
    def ifThenElse[T](t: => T, e: => T): T //call by name
    def &&(x:Bool) = ifThenElse(x,ff) //if true return x else ff
    def ||(x:Bool) = ifThenElse(tt,x) //if true, return true else return x
    def not        = ifThenElse(ff, tt) //if true, return false else true
}

//If we are not extending any class but just using the trait,
//we use the extend keyword
object tt extends Bool{
    override def ifThenElse[T](t: => T, e: => T): T = t
}
object ff extends Bool{
    override def ifThenElse[T](t: => T, e: => T): T = e
}