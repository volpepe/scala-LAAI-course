/*************** CLASSES *****************/

/**
Classes allow the programmer to instantiate variables and objects that
mix properties and methods. This allow for a higher order modelling.
 */
class Greeter {
    val message = "Hello World!"
    def SayHi() = println(message)
}

val greeter = new Greeter()
greeter.SayHi()

/** We can extend classes too, so that all properties
 * and methods are copied from the parent class.
 * We can of course override something of the superclass in order
 * to personalize a superclass.
 * Methods can also be overridden. */
class SpanishGreeter extends Greeter {
    override val message: String = "Hola Mundo!"
}

class ItalianGreeter extends Greeter {
    override val message: String = "Ciao Mondo!"
}

val greeter1: Greeter = new ItalianGreeter()
// object construction with no parameters can be called without parentheses
val greeter2 = new SpanishGreeter

greeter1.SayHi()
greeter2.SayHi() // method calls with no parameters is also allowed but deprecated.

/**
In Scala we can consider the whole body of the class declaration as
the default constructor for the class. We can of course pass some parameters
to the constructor.
 */
class GenericGreeter(msg:String) extends Greeter {
    override val message: String = msg
}

val greeter3 = new GenericGreeter("Bonjour Monde!")
greeter3.SayHi()

/** We can also have a default constructor and eventually other
constructors that account for optionality of parameters.
We can override the constructor for the message val right inside of the parenthesis.
 */
class GenericWithDefaultGreeter(override val message: String) extends Greeter {
    // New constructor with no parameters called to execute the chain
    def this() = this("Hallo Welt!") // Additional constructor
}
/**
All constructors should call the constructor of the parent in order
to execute the entire chain of constructor and generate a new object.
 */
val greeter5 = new GenericWithDefaultGreeter() // We call the default constructor
greeter5.SayHi()

// Difference of types:
// - Static type: what the compiler expects at compile time
// - Dynamic type: depends on what happens at runtime and how a variable is "filled" during the execution


/*
Abstract Class: a class which is not entirely defined
 */
abstract class Greeter {
    val message:String // missing
    def SayHi() = println(message)
}

/* We can ignore the override keyword when instantiating abstract classes */
class SpanishGreeter {
    val message = "Hola Mundo!" // There is nothing to override, we are just instantiating what's missing.
}


/* Modeling data
- see utils.Rational
 */
import utils.Rational3
val x = new Rational3(3, 4)
val y = new Rational3(5,10)
x + y
x - y

/* Data Structures */

/** This abstract class only contains method definitions.
This is what is known as an INTERFACE. It exposes the public methods that this class has.
 */
abstract class IntSet {
    def isEmpty: Boolean //check if the set is empty
    def add(x: Int): IntSet //generates a new set
    def contains(x:Int): Boolean // checks if an element is present in the set
    //def union(x:IntSet): IntSet // unifies this set with another set
}

/* Since we have defined it as an abstract class, we need to extend it rather than implementing*/


/**
 * We represent a NonEmpty set as a binary tree
 * @param elem root of the tree
 * @param left left branch of the tree (set)
 * @param right right branch of the tree (set)
 */
class NonEmpty(elem:Int, left:IntSet, right: IntSet) extends IntSet {
    def isEmpty: Boolean = false
    def contains(x: Int): Boolean = {
        if (x < elem) left contains x // we use infix notation
        else if (x > elem) right contains x
        else true
    }
    def add(x:Int): IntSet = {
        if (x < elem) new NonEmpty(elem, left add x, right) //infix notation is super powerful i love it
        else if (x > elem) new NonEmpty(elem, left, right add x)
        else this
    }
    //def union(x: IntSet): IntSet = {

    //}
    override def toString: String = "{"+left+elem+right+"}"
}

/* Note: using object there is an implicit new. This creates a singleton object in memory which
  we can refer to without specifying a name. */
/**
 * This is an empty set. We separate empty sets from non empty sets.
 */
object Empty extends IntSet {
    def isEmpty: Boolean = true
    def contains(x:Int) = false
    def add(x: Int): IntSet = new NonEmpty(x,Empty,Empty)
    //def union(x:IntSet) = x
    override def toString: String = "."
}

var emptySet = Empty
var set = emptySet add 3 // we create a new NonEmpty IntSet with elem as root
println(set)
set = set add 5 // we create a new NonEmpty IntSet that is shared between data structures
// It is in fact a tree by itself, but is also shared to be part of a subtree of the root tree.
// This phenomena is called SHARING.
// The garbage collector keeps this in mind and never removes shared objects.
// The NonEmpty set replaces the Empty IntSet on the right of the root.
println(set)

//We can also do this
val set1 = Empty add 2 add 5 add 7
set1 contains 3
set1 contains 2

// Try to define a union function between sets for next time

/* Lists can either be arrays or items + a pointer to the rest of the list. */