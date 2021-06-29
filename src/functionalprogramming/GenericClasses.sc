import scala.annotation.tailrec
//-------------------------------- GENERIC CLASSES -----------------------------------//

/*
 * Collections are provided to us as Generic classes, in the sense
 * that the type that the collection will hold is decided at run-time,
 * so at compile time we need to specify an "unknown type", a sort of
 * type variable that is used when building the class.
 */

/*
 * Here's an example of where these Generic Classes could be useful
 */

/*
 * We define an IntList that can only hold integers.
 * We follow the traditional Lisp-like ConsList implementation,
 * so we have a head element and a tail which is a pointer to the
 * rest of the list (or better, another IntList)
 */
trait IntList {
    def isEmpty: Boolean
    def head: Int
    def tail: IntList
}

/*
 * Similarly to what we have done with the Empty-NonEmpty set implementation,
 * we create one object for the empty list and one class for the non-empty
 */
class Cons(val head:Int, val tail: IntList) extends IntList{
    override def isEmpty: Boolean = false
    // head and tail are already taken care of by the val keyword, which
    // automatically creates the correspondent fields in the class.
}

object Nil extends IntList {
    override def isEmpty: Boolean = true
    override def head: Int = throw new NoSuchElementException("Nil.head")
    override def tail: IntList = throw new NoSuchElementException("Nil.tail")
}

/*
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
// Note that we CANNOT CREATE A GENERIC OBJECTS, so we use class. This is because
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
@tailrec
def nth[T](l:List[T], n:Int): T = {
    // Note that the exception is of type Nothing and is therefore a compatible return value,
    // because Nothing is a subtype of any possible type
    if (l.isEmpty) throw new IndexOutOfBoundsException()
    if (n==0) l.head
    else nth(l.tail, n-1)
}

val listTest = new Cons[Double](5.3, new Cons[Double](4.6, new Nil[Double]))
nth(listTest, 0)
nth(listTest, 1)
// nth(listTest, 2) <-- will cause an exception

/*
 * One key concept of Object Oriented programming is polymorphism,
 * that is taking advantage of the subclassing and generics tools to construct
 * portions of code that deal with objects of different types.
 * - In particular, with subtyping we can use subclasses where objects of superclasses are expected
 * - With generics, we can express types as a parameter.
 */

/*
 * Type bounds: we can impose constraints on the types that the function can be parametrized as.
 *
 * Here's a toy example: we want to implement a function id that given an IntList returns the
 * same list
 */
def id(l: IntList):IntList = l
/*
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
/*
 * With this syntax, we say that T can be any type that is a subtype of IntList.
 * (even Null or Nothing...)
 * The type becomes parametrized and is chosen at run-time.
 *
 * We impose an upper bound to the parametrized type T. We can also express
 * lower bounds, using >, which means that we expect type T being a superclass
 * of whatever is expressed after the >:
 */

/****************** LESSON 5 *****************/

/*
 * Remember: for each couple of types there is always a common supertype
 * and a common subtype.
 */

/*
 * Covariance: imagine a parameter type like C[T]. We also have two
 * concrete types A and B that are candidates for the parameter T.
 * If we instantiate C[T] with A, we get C[A] and if we instantiate C[T]
 * with B, we get C[B].
 * Now, let's assume that A <: B, that is A is a subtype of B.
 * If A <: B implies C[A] <: C[B], the parametric type is called covariant.
 *
 * Covariance is a property for parametric types.
 * A parametric type is covariant if the subtyping relation is maintained
 * within the instantiation of the parameter.
 *
 * In other words, covariance is a property that says something about the
 * subtyping of complex types given the subtyping of its components.
 * It's important because we have seen that subtyping implies substitution.
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

// See javaexamples.Covariance.java for an example on why covariance could be
// problematic. Scala solves that problem by making Arrays mutable and non-covariant.
// If we need Covariance, Scala provides an immutable covariant data structure
// that is the Vector.

/*
 * Let's create two classes: a base one and a derived one which is a subclass of the base
 */
class Base {
    def f() = println("Base type")
}
class Derived extends Base {
    override def f(): Unit = print("Derived type")
    def g() = println("Only in derived class")
}
/*
 * Why do we need covariance exactly? Covariance is good because we can
 * use data structures made of subtypes whenever a data structure containing
 * the supertype is needed. ... or vice versa, if we talk about contravariance.
 * Note that we need to instruct the compiler to consider List as a
 * covariant class.
 */
trait List[+T] {    // Covariance is applied with this +.
                    // This list of type T will be covariant, so the relation that
                    // type T has with respect to other types will be maintained.
                    // When we use List[T] and a function expects a List[Person], we can
                    // only pass a List[Person], while with List[+T] we can pass a List[Man]
                    // (where Man is a subtype of Person) because the two lists are in a
                    // subtyping relationship.
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
}
/*
 * Notice that we extend List[T], but List[T] is covariant, so the T is not
 * a single type: instead it indicates T and any of its subtypes. We can instantiate
 * a ConsList of type Base and fill it with Derived types, because since covariance is
 * respected, we know that we can extract an element of this list and use it as the
 * element of the subtype they belong to. Basically, we use covariance so that we don't
 * need to "squash" list elements to a specific supertype.
 */
class ConsList[T](val head:T, val tail: List[T]) extends List[T]{
    override def isEmpty: Boolean = false
}
class NilList[T] extends List[T] {
    override def isEmpty: Boolean = true
    override def head: T = throw new NoSuchElementException("Nil.head")
    override def tail: List[T] = throw  new NoSuchElementException("Nil.tail")
}
// Here we exemplify the thing we have just said: a List of type Base that is actually
// a ConsList of type Derived (a subtype)
def newl: List[Base] =  // Base is covariant, so a List[Base] can be a ConsList[Derived]
    new ConsList[Derived](new Derived, new NilList[Derived])

// Now, we can extract Derived types from newl which is a List of Base.
newl.head.f() // --> Will print "Derived Type" even though of static type Base.
//newl.head.g() // This method does not exist on the head element,
                // because it is seen as a Base type object by the compiler
                // (which only looks at static types)
// Casting the head of the list to a Derived is perfectly fine because that list is
// indeed of Derived type.
newl.head.asInstanceOf[Derived].g()

/*
 * In Scala we can declare variant classes like so:
 * - class C[+A] --> C is covariant
 * - class C[A] --> C is invariant (non-variant)
 * - class C[-A] --> C is CONTRAVARIANT
 *
 * What does contravariant mean?
 * Contravariant is the opposite of covariant. It means
 * that if A is a subtype of B, C[A] is a SUPERTYPE of C[B].
 * It means that the relationship between the two types
 * changes direction.
 *
 * For example, if we declared the List[T] from before contravariant
 * (List[-T]), we would have that whenever a function wants a
 * List[Man] or a List[Woman] we can pass a List[Person] using correctly
 * Liskov's substitution principle, because it turns out that List[Person]
 * would be a subtype of List[Man].
 *
 * Let's take as example Function1 from the standard library.
 * In Scala functions are actually objects that implement this trait
 * Function1. Like any other object, it can be put in a subtyping
 * relationship with other functions, so that we can substitute a function
 * with another using Liskov's Principle.
 *
 * This is the trait that all functions with 1 parameter implement.
 * trait Function1[-T1, +R]
 *
 * Function1 is the type of all functions with 1 parameter. When we
 * invoke a function of 1 parameter on an object, an object of type Function1
 * is instantiated and the method "apply", which contains the body of the function,
 * is called. "apply" is supposed to take as parameter an element of type T1
 * and return an element of type R.
 *
 * How can we put in relationship a function f: A=>B with a function g: C=>D?
 * Trait Function1's signature tells us that Function1 is covariant with respect to
 * R (the return type) and contravariant with respect to T1 (the parameter).
 *
 * See concepts.FunctionConstraints for an example.
 */

def useFunction(f: Int=>Int): Unit = {
    val x:Int = f(1)
    x
}

// To create a function that is in subtyping relationship with an Int,Int function,
// we need that the parameter is a supertype of Int, while the return value should
// be a subtype of Int (in this case, Int, because the only subtype of Int is Nothing
// and we cannot return Nothing).
val oneFunction = new Function1[AnyVal, Int] {
    def apply(x: AnyVal): Int = x.asInstanceOf[Int]+1
}

/*
 * As you can see the compiler is perfectly OK with dealing with this function
 * even though useFunction should accept only Int-to-Int functions.
 * But! Our function is indeed a subtype of an Int-Int function.
 */
val a = useFunction(oneFunction)

// Now, we try to implement the append function
trait List[+T]{
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
    //def append(e:T):List[T] // There is an error here! Why?
    /*
     * "covariant type T occurs in contravariant position in type T."
     * Basically what this error is telling us is that we are using a covariant
     * type as an argument of a function, while arguments of parametrized functions
     * always need to be contravariant, to preserve subtyping and allow substitution,
     * for the reason we have seen before.
     *
     * Covariant types should only be the evaluation type. This is the case
     * in all the other functions in the class, because they all have no parameters,
     * but in the function append there is a parameter that we tell the compiler
     * being covariant.
     *
     * The important take out message here is that:
     * - Contravariant types cannot appear as a return type, nor as a lower bound on the type
     * - Covariant types cannot appear as a parameter type, nor as a higher bound on the type
     * And also, remember: covariance only works well with immutable collections.
     *
     * Therefore, we should rewrite the declaration.
     */

    // An alternative is to define the trait as trait List[-U, +T],
    // but in this way we lose the relation between the two types.
    //def append(e:U): List[U, T]

    // Another alternative is to write the function like this:
    // def append[U](e:U):List[T]
    // But U is not in relation with T. We want the contravariant relationship
    // to be as explicit as possible.

    /* What we really want is a parametrized type that is a supertype of U in order
     * to have a substitutable function.
     * The best way to do this is:
     */
    def append[U >: T](e:U): List[U] // U is a supertype of T and append has contravariance on the parameter
                                     // Notice that we need to return a List[U] because the list now contains
                                     // an element which is not T and we cannot call T's methods on it.
                                     // Of course this doesn't change the List's elements, which will be of static
                                     // type U, but can be easily cast to their original expected type T.
}
class ConsList[T](val head:T, val tail: List[T]) extends List[T]{
    override def isEmpty: Boolean = false
    override def append[U >: T](e: U): List[U] = new ConsList[U](head, tail append e)
}
// Finally, with this concept we can use "object" for defining a NilList.
// Using object was not allowed because we cannot instantiate an Object of a generic class.
// Now, since List is covariant we can create a List[A] where A is a subtype of T and
// have a NilList that is subtype of a List[A] which becomes a subtype of a List[T].
// Of course, the limitation of not being possible of instantiating an object with
// a parametrized type remains, so we need to use a type which is by definition the
// subtype of all other types (so that the subtyping relationship remains
// for any possible type T. By definition, the type we are looking for is: Nothing!
object NilList extends List[Nothing] {
    override def isEmpty: Boolean = true
    override def head= throw new NoSuchElementException("Nil.head")
    override def tail = throw  new NoSuchElementException("Nil.tail")
    // U will now be any type, with no relation to anything whatsoever
    // since "Nothing" is always a subtype for U, so it doesn't make sense
    // to write U >: Nothing
    override def append[U](e:U) = new ConsList(e, NilList)
}
// Since this is an object now, we can avoid all "new NilList".

/*
 * This way, whenever we expect a List[T] we can pass a NilList because it's
 * basically a List[Nothing] so it can be passed whenever we expect any
 * List of any parameter type.
 */