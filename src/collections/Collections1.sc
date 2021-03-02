/** Lesson 6: Collections */

/**
 Collections are a standard library of data structures.

 All collections are immutable data structures and most of them are covariant.

 There are mainly 3 groups of collections:
 - Sets
 - Maps
 - Sequences

 - Sets are unordered groups of data of a certain type.
 - Sequences on the other hand are ordered groups of data
 - Maps, finally, are associations key-value. They can be seen as partial functions,
    because all keys can be considered the domain of the function, while
    all values can be considered the codomain.

 - Note that in the standard library, Set, Iterable, Map, Seq and LinearSeq and IndexedSet are
    defined as traits. Classes that complete these traits are also provided of course.
    For example, List is an abstract class.
 - On the other hand, we can use a notation like this:
 */
Set(1,3,3)

/**
 * This is because we have "standard implementations" for some types.
 * Another example: we can instantiate an Iterable, but, following the tree,
 * we end up with a List.
 */
Iterable(1,2,3,4,5)

/**
 - Sequences are subdivided in Indexed Sequences and Linear Sequences.
    - In Indexed Sequences we can access a value by providing an index
    - Linear Sequences on the other hand can only be accessed linearly by selecting the first element
        or accessing the tail of the sequence to move further.
    Lists, for example, are Linear Sequences.
 */

/**
 * LIST:
 * We have already seen and re-implemented Lists.
 * They are immutable, homogeneous and recursive (head, tail).
 * They can be generated with:
 */
List("one", "two", "three")
// An empty List is the object Nil.
List() == Nil
// Another construction operator:
val nums = 1 :: 2 :: 3 :: 4 :: Nil
// We need to have Nil at the end since this will be the tail of the last element
// Lists have many operations
nums.length

/**
 * In PatternMatching the isort algorithm is implemented also using patterns.
 */

/**
 * We can use higher order function to perform elaborations on the data in the collections.
 *
 * 1) Suppose we have a list of Doubles and we want to obtain a
 * List where all elements are scaled by a number n.
 */
def scaleList(xs: List[Double], factor:Double) : List[Double] =
    xs match {
        case Nil => xs
        case y::ys => y * factor :: scaleList(ys, factor)
    }
/**
 * But! In the standard library we have the map function.
 */
def scaleList(xs: List[Double], factor: Double) =
    xs map (x => x*factor) // we need to pass as argument a function that describes what will be applied to every element.

/**
 Another pattern is the selection of elements in a list that satisfy a given condition.
 */
def posElems(xs: List[Int]): List[Int] =
    xs match {
        case Nil => xs
            // Filter positive elements in the list.
        case y :: ys => if (y > 0) y :: posElems(ys) else posElems(ys)
    }
// ...or we can use filter
def posElem(xs:List[Int]):List[Int] =
    xs filter (x => x > 0)

/** Here's some examples: */
val stringList = List("Bologna", "Modena", "Reggio Emilia", "Piacenza")
stringList map (_.length) // List[Int] that contains the length of each string
stringList filter (_.length < 8) //List[String] that contains only names shorter than 8 chars

/**
 Some variations of filter:
 */
stringList filterNot (_.length < 8)
// partition separes the two lists in two parts according to what satosfies the function and what doesn't
stringList partition (_.length < 8)
// takeWhile returns a list that contains all elements of the list in order until they satisfy the function.
stringList takeWhile (_.length < 8)
// dropWhile returns the remainder
stringList dropWhile (_.length < 8)
// span separates the list in two using takeWhile and dropWhile.
stringList span (_.length < 8)

// from a list of a type return a List of pairs where the first element is an element
// and the second element is how many times it is repeated in the sequence until it's changed.
def pack[T](xs: List[T]): List[List[T]] =
    // break the list in Lists that contain the same value
    xs match {
        case Nil => Nil
            // if there is an element, separate the lists using span.
            // the first list will be composed of all elements equal to the head element
        case _ => xs span (x => (x == xs.head)) match {
            // take these two lists and create a single one by appending the list obtained
            // by the same process on the second list r.
            case (l,r) => l :: pack(r)
        }
    }

val listString = List("a", "a", "a", "b", "b", "c", "c", "a")
pack(listString)
// finally
def lisLen[T](xs:List[T]): List[(T,Int)] = {
    pack(xs) map (x => (x.head, x.length))
}
lisLen(listString)
lisLen(List(8,8,8,5,5,4,2,2,3,8))

/**
 Other patterns: reductions (combination of elements of the list)
 - sum and product
 */
def sum(xs:List[Int]):Int = xs match {
    case Nil => 0
    case x :: xs => x + sum(xs)
}
def product(xs:List[Int]):Int = xs match {
    case Nil => 1
    case x :: xs => x * sum(xs)
}

//..or use reduceLeft
def sum(xs: List[Int]) = {
    // 0 is added in front of xs, then we apply reduceLeft to this new list.
    // We add 0 because we expect the list to be non-empty, and 0 is the neutral element
    // for the sum operation.
    (0::xs) reduceLeft ((x,y) => x+y)
}
def product(xs: List[Int]) =
    (1::xs) reduceLeft ((x,y) => x*y)
// also: foldLeft
// foldLeft is expected to receive two parameters. An initial value (return in case the list is empty)
// and a transformation function.
// the first parameter is also called "accumulator"
def sum(xs:List[Int]) = (xs foldLeft 0)(_+_) //note: these two parameters are passed separately
def product(xs:List[Int]) = (xs foldLeft 1)(_*_) //also, only some parameters can be expressed
// this is a notation used in functional programming:
// f(a,b) = f(a)(b) = f' = f(a) ..do stuff.. f'(b) --> only at this point f'(b) will be executed

// another example:
// find longest string. returns a string.
// note: reduceLeft requires that x is a supertype of y, because they will be compared
stringList reduceLeft((x:String, y:String) => if(x.length > y.length) x else y)

// the initial value can be of a different type
// there can be no relation between the types of x and y. x will be just the first value of the accumulator.
(stringList foldLeft 0)((x: Int,y: String) =>
    if(x > y.length) x else y.length)

// we also have reduceRight and foldRight (we start aggregating from the right instead of left)
// Note: in foldRight, the accumulator is the second parameter.

//problem 1: concatenate two lists:
def concat[T](xs: List[T], ys:List[T]):List[T] =
    //xs = x1::x2::...::xn::Nil
    //ys = y1::y2::...::yn::Nil
    // = x1::x2::...::xn::ys
    // we start from the right, using ys as accumulator. the result of the
    // first concatenation xn::ys will be the accumulator for the following process.
    (xs foldRight ys)(_ :: _) //start from ys as accumulator, the accumulator operator will be the appending function

def myL = List(27,31,45,18)
def myL2 = List(17,29,30,1)
concat(myL,myL2)

//problem 2: reverse a list
def reverse[T](xs:List[T]):List[T] = {
    //start the accumulator with an empty list, append the rightmost element in front of the accumulator and so on..
    //note: we don't want to use Nil instead of an empty list of [T] because the type checker
    //expects the accumulator not to change type
    (xs foldLeft List[T]())((l:List[T], x:T) => x::l)
}
reverse(myL)