//--------------------------------- Classes ----------------------------------------//

/*
 * Classes allow the programmer to instantiate variables and objects that
 * mix properties and methods. This allow for a higher order modelling.
 */
class Greeter {
    val message = "Hello World!"
    def SayHi() = println(message)
}

val greeter = new Greeter()
greeter.SayHi()

/*
 * We can extend classes too, so that all properties
 * and methods are copied from the parent class.
 * We can of course override something of the superclass in order
 * to personalize a superclass.
 * Methods can also be overridden.
 */
class SpanishGreeter extends Greeter {
    // Notice the override keyword for overriding methods
    override val message: String = "Hola Mundo!"
}

class ItalianGreeter extends Greeter {
    override val message: String = "Ciao Mondo!"
}

val greeter1: Greeter = new ItalianGreeter()
// Object construction with no parameters can be called without parentheses
val greeter2 = new SpanishGreeter

greeter1.SayHi()
greeter2.SayHi() // Method calls with no parenthesis is also allowed but deprecated.

/*
 * In Scala we can consider the whole body of the class declaration as
 * the default constructor for the class. We can of course pass some parameters
 * to the constructor.
 */
class GenericGreeter(msg:String) extends Greeter {
    override val message: String = msg
}

val greeter3 = new GenericGreeter("Bonjour Monde!")
greeter3.SayHi()

/* We can also have a default constructor and eventually other
 * constructors that account for optionality of parameters.
 * We can override the constructor for the message val right inside of the parenthesis.
 */
class GenericWithDefaultGreeter(override val message: String) extends Greeter {
    // New constructor with no parameters called to execute the chain
    def this() = this("Hallo Welt!") // Additional constructor: if we call GenericWithDefaultGreeter without
                                     // specifying a message, the default "Hallo Welt!" is used as an override
                                     // of the empty message val.
}
/*
 * All constructors should call the constructor of the parent in order
 * to execute the entire chain of constructor and generate a new object.
 */
val greeter5 = new GenericWithDefaultGreeter() // We call the default constructor
greeter5.SayHi()

// Difference of types:
// - Static type: what the compiler expects at compile time
// - Dynamic type: depends on what happens at runtime and how a variable is "filled" during the execution


/*
 * Scala supports abstract classes, classes which are not entirely defined
 */
abstract class Greeter {
    val message:String // missing
    def SayHi() = println(message)
}

/* We can ignore the override keyword when instantiating abstract classes */
class AnotherSpanishGreeter extends Greeter {
    val message = "Hola Mundo!" // There is nothing to override, we are just instantiating what's missing.
}

val spanishGreeter = new AnotherSpanishGreeter
spanishGreeter.SayHi()


/* Modeling data
- see utils.Rational
 */
import utils.Rational3
val x = new Rational3(3, 4)
val y = new Rational3(5,10)
println(x + y) // infix notation
println(x - y)
println(x < y)

//------------------------------- Data Structures ---------------------------------------//

/*
 * This abstract class only contains empty method definitions.
 * This is what is known as an INTERFACE. It exposes the public methods that this class has.
 */
abstract class IntSet {
    def isEmpty: Boolean // Checks if the set is empty
    def add(x: Int): IntSet // Generates a new set by adding an element to the elements of this one
    def contains(x:Int): Boolean // Checks if an element is present in the set
    def union(that: IntSet): IntSet // Generates a new IntSet by unifying the elements of two sets
}

/* Since we have defined it as an abstract class, we need to extend it rather than use implement like in Java */

/**
 * We represent a NonEmpty set as a binary tree
 * @param elem root of the tree
 * @param left left branch of the tree (IntSet)
 * @param right right branch of the tree (IntSet)
 */
class NonEmpty(val elem:Int, val left:IntSet, val right: IntSet) extends IntSet {
    def isEmpty: Boolean = false
    def contains(x: Int): Boolean = {
        if (x < elem) left contains x // we use infix notation
        else if (x > elem) right contains x
        else true // if not > nor < it is equal.
    }
    def add(x:Int): IntSet = {
        /*
         * Regarding adding element to a set: as it can be seen, instead of directly modifying an object,
         * a new object is always returned. This might seem like a bad idea, a slower, more garbage-prone
         * implementation. But! Scala relies heavily on memory SHARING, which is a mechanism that allows
         * data structures created from slightly modifying other data structures to NOT be copy-pasted
         * a tremendously high amount of time in memory, but instead recycles the part that is in common
         * between the two structures and only add what is needed to separate one structure from the other.
         *
         * In the case below, adding an element to this NonEmpty set means adding an element in one of the
         * subtrees. In both cases either the left or right sets remain equal: in memory they are shared
         * between the newly evaluated NonEmpty set and the old set.
         * The garbage collector always keeps track of the data that shared between variables and never
         * deletes them unless the variables that share that memory are all unused.
         */
        if (x < elem) new NonEmpty(elem, left add x, right) // Infix notation is super powerful I love it
        else if (x > elem) new NonEmpty(elem, left, right add x)
        else this
    }
    def union(that: IntSet): IntSet = {
        if (that.isEmpty) this
        // Add the root to the other set and then make the union between this left set and the new
        // set and then with this right set and the new set.
        else {
            // We need a strong casting. It's safe because if this else is executed it means
            // that the set is NonEmpty, so it's just a cast to itself.
            // But! It's needed because otherwise we wouldn't be able to use the left and right
            // sets (as well as the root element) of this data structure, since Empty is also an
            // IntSet but it doesn't have neither of those.
            val cast = that.asInstanceOf[NonEmpty]
            this add cast.elem union cast.left union cast.right
        }
    }
    // We can override what the string looks like when we print this element by overriding the definition
    // of the function toString
    override def toString: String = "{"+left+elem+right+"}"
}

/* Note: using object there is an implicit new. This creates a singleton object in memory which
  we can refer to without specifying a name. */
/*
 * This is an empty set. We separate empty sets from non empty sets.
 * All empty sets are the same set, so we implement it as an object and not a class
 * which is instead useful when we want to spawn many similar objects
 */
object Empty extends IntSet {
    def isEmpty: Boolean = true
    // Any element cannot be inside this set, so contains is always false
    def contains(x:Int) = false
    // Adding an element returns a new NonEmpty set.
    // We can see the effect of sharing also here: since Empty is an object and not a class,
    // we don't need to instantiate a new object of class Empty all the time we create a new
    // NonEmpty set. Instead, both references to Empty point to the same location in memory
    // thus we are saving space with this singleton.
    def add(x: Int): IntSet = new NonEmpty(x,Empty,Empty)
    // When we unify an IntSet with an Empty set, it evaluates to the IntSet.
    def union(that: IntSet) = that

    override def toString: String = "."
}

var emptySet = Empty // We create an Empty set
var set = emptySet add 3 // We then add element 3 to the Empty set, thus obtaining a NonEmpty set with element 3.
println(set)
set = set add 5
println(set)

//We can also do this
val set1 = Empty add 2 add 5 add 7 // Because each operation returns a new Set which is immediately passed to the
                                   // next operation.
set1 contains 3
set1 contains 2

// set1 is 2,5,7
println(set1 union emptySet)
println(emptySet union set1)
println(set1 union (Empty add 1 add 8 add 9 add 10))