import scala.annotation.tailrec
//--------------------------------- TRAITS ----------------------------------------//

/*
 * Traits are semi-implemented interfaces.
 * They can contain an implementation for some of the methods
 * but cannot be instantiated until the rest of the methods are implemented.
 */
trait Common {
    // basically an interface
    def philosophize()
}

/*
 * Traits can be extended easily.
 */
trait Philosophical extends Common {
    def philosophize() = println("I am "+toString+", therefore I am!")
}

trait Colored extends Common {
    def philosophize() = println("It ain't easy being "+toString+"!")
}

/*
 * In Java, a class can implement an arbitrary number of interfaces, but only extend one class.
 * This is why traits can be important.
 */

class Animal{}

/*
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

/*
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

/*
 * Many functionalities in the standard library are implemented as traits
 */


/*
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
    // The compare mechanism has to be implemented externally by
    // whoever extends the trait
    def compare(r:T): Double
    // Return a positive number if the value of the same type
    // is bigger, a negative one if the value is smaller...
    def >(r:T) = (this compare r) > 0
    def <(r:T) = (this compare r) < 0
    def >=(r:T) = !(this < r)
    def <=(r:T) = !(this > r)
}

// This other trait defines multiplication for any class
// that defines how a summing is done between two of its elements
// and has a zero element
trait Multip[T]{
    def sum(r1:T, r2:T):T // How do we sum two values of type T?
    def zero: T           // What is the 0 element for type T
    def *(x:Int) = multip(x,zero)
    @tailrec
    private def multip(x:Int, acc:T):T =
        if (x==0) acc else multip(x-1, sum(this.asInstanceOf[T],acc))
}                                       //note: we know that "this" is of type T, but the
                                        //compiler doesn't and when type-checking is done
                                        //we need to explicitly define it.

// We can use these two traits in a class that defines a total
// ordered Rational
class OrdRat(x:Int, y:Int) extends Rational(x,y)
    with TotOrder[OrdRat] with Multip[OrdRat] {
    // Compare is implemented as the difference between rationals
    // because it's a method that must evaluate to either a negative
    // or a positive number.
    def compare(r:OrdRat) = numer*r.denom - r.numer*denom
    // Zero element for the multiplication or sum
    def zero = new OrdRat(0,1)
    // Implementation of sum
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

/*
 * super in traits is defined dynamically: it does not refer to the trait or to its father,
 * but to the class that makes use of the trait. We say that super is dynamically bound.
 */
trait TestSuper[T]{
    def print() = println("super.toString inside trait: " + super.toString)
}

class TraitSuperTest1 extends Object with TestSuper[TraitSuperTest1] { }
class TraitSuperTest2 extends Object with TestSuper[TraitSuperTest2] { }

new TraitSuperTest1().print()
new TraitSuperTest2().print()


/*
 * For the next example, see the implementation of the Bool
 * trait in utils.Bool
 */

import utils.{Bool, ff, tt}

val boolVar:Bool = tt
val otherBoolVar:Bool = tt.not // ff
val testOr = boolVar || otherBoolVar // tt
val testAnd = boolVar && otherBoolVar // ff
val testIf1 = boolVar.ifThenElse(tt,ff) // tt, because boolVar is tt
val testIf2 = otherBoolVar.ifThenElse(tt,ff) // ff, because boolVar is ff

/*
 * We can say that Scala is scalable also in the way anyone can
 * define new classes/functionalities which resemble native ones.
 */

println("End.")