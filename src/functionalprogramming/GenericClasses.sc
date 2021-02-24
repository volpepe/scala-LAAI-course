/************ INTRODUCTION (LESSON 4) ******************/

/**
 * Collections are provided to us as a Generic class, in the sense
 * that the type that the collection will hold is decided at run-time,
 * so at compile time we need to specify an "unknown type", a sort of
 * type variable that is used when building the class.
 */

/**
 * Here's an example of where these Generic Classes could be useful
 */

/**
 * We define an IntList that can only hold integers
 */
trait IntList {
    def isEmpty: Boolean
    def head: Int
    def tail: IntList
}

/**
 * We then implement two different classes: one object for the empty list, one for the non-empty
 */
class Cons(val head:Int, val tail: IntList) extends IntList{
    override def isEmpty: Boolean = false
}

object Nil extends IntList {
    override def isEmpty: Boolean = true
    override def head: Int = throw new NoSuchElementException("Nil.head")
    override def tail: IntList = throw  new NoSuchElementException("Nil.tail")
}

/**
 * Of course, if we want to construct a list of Double values, we would have to instantiate another
 * class and rewrite all elements, right?
 *
 * Not with generics. We treat the type of the list as a variable.
 */
trait List[T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
}
class Cons[T](val head:T, val tail: List[T]) extends List[T]{
    override def isEmpty: Boolean = false
}
// Note that we cannot create a generic object, so we use class. This is because
// objects naturally have a single type. How could we instantiate an object whose type is
// unknown?
class Nil[T] extends List[T] {
    override def isEmpty: Boolean = true
    override def head: T = throw new NoSuchElementException("Nil.head")
    override def tail: List[T] = throw  new NoSuchElementException("Nil.tail")
}

// To create a single Nil object, we use the singleton pattern.
def singleton[T](elem:T) = new Cons[T](elem, new Nil[T])    //Nil[T] is only called inside here

// As an exercise: write a function nth that given a list l and an integer n
// returns the n-th element of the list or throws an IndexOutOfBoundsException if the length
// of the list is smaller than n
def nth[T](l:List[T], n:Int): T = {
    // note that the exception is of type Nothing and is therefore a compatible return value
    if (l.isEmpty) throw new IndexOutOfBoundsException(n+" is not a valid index for the list.")
    if (n==0) l.head
    else nth(l.tail, n-1)
}

/**
 * One key concept of Object Oriented programming is polymorphism,
 * that is using taking advantage of the subclassing and generics tools to construct
 * portions of code that deal with objects of different types.
 * - In particular, with subtyping we can use subclasses where objects of superclasses are expected
 * - With generics, we can express types as a parameter.
 */

/**
 * Type bounds: we can impose constraints on the types that the function can be parametrized as.
 *
 * Here's a toy example: we want to implement a function id that given an IntList returns the
 * same list
 */
def id(l: IntList):IntList = l
/**
 * If l is Nil, we want the function to return a Nil object,
 * while if l is non-empty (Cons), then we want a Cons object as response.
 * The problem is that we get an object of type IntList as a response, so we get
 * a superclass. If there are methods that are exclusive to Cons, we wouldn't be able
 * to call them (or at least, the type-checker wouldn't be able to recognize the new list
 * to be a Cons).
 *
 * We need to be precise with the signature of this function.
 */
def id[T <: IntList](l: T): T = l
/**
 * With this syntax, we say that T can be any type that is a subtype of IntList.
 * (even Null or Nothing...)
 * The type becomes parametrized and is chosen at run-time.
 *
 * We impose an upper bound to the parametrized type T. We can also express
 * lower bounds, using >.
 */
