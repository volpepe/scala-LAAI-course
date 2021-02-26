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

/****************** LESSON 5 *****************/

/**
 * Remember: for each couple of types there is always a common supertype
 * and a common subtype.
 */

/**
 * Covariance: imagine a parameter type like C[T]. We also have two
 * concrete types A and B that are candidates for the parameter T.
 * If we instantiate C[T] with A, we get C[A] and if we instantiate C[T]
 * with B, we get C[B].
 * Now, let's assume that A < B, that is A is a subtype of B.
 * If A < B implies C[A] < C[B], the parametric type is called covariant.
 *
 * Covariance is a property for parametric types.
 * A parametric type is covariant if the subtyping relation is maintained
 * within the instantiation of the parameter.
 *
 * Covariance is useful, but it can also be quite dangerous when applied on
 * mutable data structures.
 * - In Java, Arrays are immutable and the code in Covariance.java is compiled correctly,
 *      but its execution throws an Exception.
 * - In Scala, arrays are mutable and therefore not covariant (invariant).
 * - A covariant and immutable data structure in the Scala standard library is Vector.
 * - While defining a parametrized class, using a + in front of the parameters
 *      will define a parametric covariant type. Only immutable data structures can be covariant.
 */

class Base {
    def f() = println("Base type")
}
class Derived extends Base {
    override def f(): Unit = print("Derived type")
    def g() = println("Only in derived class")
}
//watch this again
//val arr1: Array[Derived] = Array(new Derived)
//val arr2: Array[Base] = Array(new Base)

/**
 * Covariance is good because we can extract subtype objects from
 * a supertype data structure.
 * Note that we need to instruct the compiler to consider List as a
 * covariant class.
 */
trait List[+T] {    //covariance is applied with this +
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
}
class ConsList[T](val head:T, val tail: List[T]) extends List[T]{
    override def isEmpty: Boolean = false
}
class NilList[T] extends List[T] {
    override def isEmpty: Boolean = true
    override def head: T = throw new NoSuchElementException("Nil.head")
    override def tail: List[T] = throw  new NoSuchElementException("Nil.tail")
}
def newl: List[Base] =
    new ConsList[Derived](new Derived, new NilList[Derived])

// Now, we can extract Derived types from newl which is a List of Base.
newl.head.f() // --> will print "Derived Type" even though of static type Base.
// newl.head.g // this method does not exist on the head element, because it is seen as a Base type object.

// Now, we try to implement the append function
trait List[+T]{
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
    //def append(e:T):List[T] // There is an error here! Why?
    /**
     * In scala we can declare variant classes like so:
     * - class C[+A] --> C is covariant
     * - class C[A] --> C is invariant (nonvariant)
     * - class C[-A] --> C is CONTRAVARIANT
     *
     * What does contravariant mean?
     * Contravariant is the opposite of covariant. It means
     * that if A is a subtype of B, C[A] is a SUPERTYPE of C[B].
     * It means that the relationship between the two types
     * changes direction.
     *
     * Let's take as example Function1 from the standard library.
     * This is the trait for functions with 1 parameter.
     * trait Function1[-T1, +R]
     * Function1 is the type of all functions with 1 parameter. When we
     * invoke a function of 1 parameter on an object, an object og type Function1
     * is instantiated and the method apply, which contains the body of the function,
     * is called. apply is supposed to take as parameter an element of type T1
     * and return an element of type R.
     * Function1 is covariant with respect to R (the return type)
     * and contravariant with respect to T1 (the parameter).
     * Why is this? See concepts.FunctionConstraints
     *
     * "covariant type T occurs in contravariant position in type T."
     * It's only fine to use a covariant type as return type. This is the case
     * in all the other functions in the class, but in the function append
     * we are using a parameter of type T inside the function. Therefore, we should
     * rewrite the declaration.
     */
    // def append[U](e:U):List[T] // but U is not in relation with T
    // an alternative is to define the trait as trait List[-U, +T], but this way we lose the relation between the two parameters
    def append[U >: T](e:U): List[U] // U is a supertype of T and append has contravariance on the parameter
}
class ConsList[T](val head:T, val tail: List[T]) extends List[T]{
    override def isEmpty: Boolean = false
    override def append[U >: T](e: U): List[U] = new ConsList[U](head, tail append e)
}
// NOW we can use object. Using object will require to use a type that is subtype of all
// other type: Nothing!
// Since this is an object now, we can avoid all "new NilList".
object NilList extends List[Nothing] {
    override def isEmpty: Boolean = true
    override def head= throw new NoSuchElementException("Nil.head")
    override def tail = throw  new NoSuchElementException("Nil.tail")
    // U will now be any type, with no relation to anything whatsoever
    // since "Nothing" is always a subtype for U.
    override def append[U](e:U) = new ConsList(e, NilList)
}
