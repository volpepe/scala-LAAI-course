/** Lesson 6: Collections */

/*
 * Collections are a standard library of data structures.
 *
 * All collections are immutable data structures and most of them are covariant.
 *
 * There are mainly 3 groups of collections:
 * - Sets
 * - Maps
 * - Sequences
 *
 * - Sets are unordered groups of data of a certain type.
 * - Sequences on the other hand are ordered groups of data
 * - Maps, finally, are associations key-value. They can be seen as partial functions,
 *   because all keys can be considered the domain of the function, while
 *   all values can be considered the codomain.
 *
 * - Note that in the standard library, Set, Iterable, Map, Seq and LinearSeq and IndexedSet are
 *   defined as traits. Classes that complete these traits are also provided of course.
 *   For example, List is an abstract class.
 * - On the other hand, we can use a notation like this:
 */
Set(1,3,3)

/*
 * This is because we have "standard implementations" for some types.
 * Another example: we can instantiate an Iterable, but, following the tree,
 * we end up with a List.
 */
Iterable(1,2,3,4,5)

/*
 * - Sequences are subdivided in Indexed Sequences and Linear Sequences.
 * - In Indexed Sequences we can access a value by providing an index
 * - Linear Sequences on the other hand can only be accessed linearly by selecting the first element
 *   or accessing the tail of the sequence to move further.
 *  Lists, for example, are Linear Sequences.
 */

/*
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

/*
 * In PatternMatching the isort algorithm is implemented also using patterns.
 */

/*
 * We can use already implemented higher order function to perform
 * elaborations on the data in the collections.
 */

/*
 * 1) Suppose we have a list of Doubles and we want to obtain a
 * List where all elements are scaled by a number n.
 */
def scaleList(xs: List[Double], factor:Double) : List[Double] =
    xs match {
        case Nil => xs
        case y::ys => y * factor :: scaleList(ys, factor)
    }
/*
 * But! In the standard library we have the map function.
 */
def scaleList(xs: List[Double], factor: Double) = {
    // map is a higher order function that requires we as argument a function
    // that explains the kind of transformation we want to do on every element of the list.
    xs map (x => x*factor)
}

/*
  * 2) Another computational pattern is the selection of elements in a
  * list that satisfy a given condition.
 */
// Example: filter positive elements in the list.
def posElems(xs: List[Int]): List[Int] =
    xs match {
        case Nil => xs
            // y > 0 is a boolean condition that can effectively be passed
            // as a high-order argument.
        case y :: ys => if (y > 0) y :: posElems(ys) else posElems(ys)
    }
// ...or we can use the higher order function filter:
def posElem(xs: List[Int]): List[Int] =
    xs filter (x => x > 0)

/* Here are some examples of map and filter: */
val stringList = List("Bologna", "Modena", "Reggio Emilia", "Piacenza")
stringList map (_.length) // List[Int] that contains the length of each string
stringList filter (_.length < 8) //List[String] that contains only names shorter than 8 chars


/*
 * 3) Some variations of filter:
 */
// filterNot maintains all elements that DON'T respect the following condition
stringList filterNot (x => x.length < 8)
// partition separates the two lists in two parts according to what satisfies the function and what doesn't.
// It's some kind of filter + filterNot combination
stringList partition (_.length < 8)
// takeWhile returns a list that contains all elements of the list in order until they satisfy the function.
stringList takeWhile (_.length < 8)
// dropWhile returns the remainder
stringList dropWhile (_.length < 8)
// span separates the list in two using takeWhile and dropWhile, similarly to what partition does.
stringList span (_.length < 8)

// From a list of a type return a List of pairs where the first element is an element
// and the second element is how many times it is repeated in the sequence until it's changed.
def pack[T](xs: List[T]): List[List[T]] =
    // Break the list in Lists that contain the same value
    xs match {
        // If the list is empty, just evaluates to an empty list
        case Nil => Nil
        // If there is at least an element, separate the lists using span.
        // Span evaluates to a tuple containing two lists. We can apply additional
        // pattern matching to this tuple in order to define how to treat the two lists.
        case _ => xs span (x => (x == xs.head)) match {
            // Take these two lists and create a single one by appending the list obtained
            // by the same process on the second list r.
            case (l,r) => l :: pack(r)
        }
    }

val listString = List("a", "a", "a", "b", "b", "c", "c", "a")
pack(listString)

/*
 * Another possible exercise can be mapping each element of the list to a pair
 * containing elements and the number of consecutive repetitions
 */
def lisLen[T](xs:List[T]): List[(T,Int)] = {
    // Again, we make use of matching multiple elements with a single pattern
    pack(xs) map (x => (x.head, x.length))
}
lisLen(listString)
lisLen(List(8,8,8,5,5,4,2,2,3,8))

/*
 * 3) Other patterns: reductions (combination of elements of the list)
 * - For example, summing all elements of the list or making a product, etc...
 */
def sum(xs:List[Int]):Int = xs match {
    case Nil => 0
    case x :: xs => x + sum(xs)
}
def product(xs:List[Int]):Int = xs match {
    case Nil => 1
    case x :: xs => x * sum(xs)
}

/*
 * There are two directions in which the reduction can happen:
 * - We talk about right-associativity when the operations are grouped from right to left
 *      - For example, x1+(x2+....+(xn+0))))): groups are on the right, so the computation proceeds
 *        from right to left
 * - We talk about left-associativity in the opposite scenario, when operations are grouped
 *   from left to right
 *      - For example, (((((0+x1)+....+)+xn: groups are formed on the left, so the computation proceeds
 *        from left to right.
 */

// There are higher order functions that reduce elements from the left or from the right.
// One of these is reduceLeft.
def sum(xs: List[Int]) = {
    // 0 is added in front of xs, then we apply reduceLeft to this new list.
    // We add 0 because we expect the list to be non-empty, and 0 is the neutral element
    // for the sum operation.
    // We do: ((((0 op x1) op x2....) op xn
    // reduceLeft basically tells us how to combine two elements of the list
    // and it proceeds left to right.
    (0::xs) reduceLeft ((x,y) => x+y)
}
def product(xs: List[Int]) = {
    // For the product, we need to apply the neutral element of the product instead (1)
    (1::xs) reduceLeft ((x,y) => x*y)
}

// foldLeft is another operation that does the same thing without the initial appending:
// foldLeft is expected to receive two parameters. An initial value (return in case the list is empty)
// and a transformation function.
// The first parameter is also called "accumulator"
def sum(xs:List[Int]) = (xs foldLeft 0)(_+_) // Note: the way this works is that we pass the sum function
                                             // to the foldLeft function. This can be more easily seen if we
                                             // write:
def sum(xs:List[Int]) = xs.foldLeft(0)(_+_)
// For product:
def product(xs:List[Int]) = (xs foldLeft 1)((x,y) => x*y)
// This a common notation used in functional programming:
// f(a,b) = f(a)(b) -> f' = f(a). f(a) is executed and then we call f'(b) --> only at this point f'(b) will be executed

/*
 * This might be clearer if we look at the implementation.
 *
 * def foldLeft[U](z: U)(op: (U, T) => U): U = this match {
        case Nil => z
        case x :: xs => (xs foldLeft op(z, x))(op)
    }
 *
 * foldLeft receives a parameter z and a function op. When dealing with an empty
 * list, it evaluates to the accumulator z, while if the list contains something,
 * it first calls the function op on the accumulator and the head element, then
 * recursively calls itself on the tail. It makes sense because it first evaluates the sum
 * between the accumulator and the head. This becomes the new accumulator for the rest of the
 * folding operation.
 *
 * def reduceLeft[U >: T](op: (U, T) => U): U = this match {
 *      case Nil => throw new Error("Nil.reduceLeft")
 *      case x :: xs => (xs foldLeft x)(op)
 * }
 *
 * reduceLeft is in fact so similar to foldLeft that is actually calls it using the first element
 * of the list (the neutral value) as an accumulator.
 *
 * Notice that reduceLeft requires that the function takes into account two types: T and U.
 * U needs to be a supertype of T because this way we can downcast T to U and return a U.
 * On the other hand, foldLeft can have an initial value of a different type, but that type has
 * no bounds with respect to T.
 */

// Here's another example:
// Find longest string in a List[String]. It returns the longest string.
// We can use reduceLeft to have a function that compares the length of two elements at a time,
// starting from the left, and evaluates to the longest one.
stringList reduceLeft((x:String, y:String) => if(x.length > y.length) x else y)
// The same function can be rewritten using foldLeft, which starts accumulating with the first element
// of the list, but replaces it as it goes on. But in general, foldLeft has problems when it can
// deal with empty lists, because it evaluates to the accumulator if the list is empty, but the accumulator
// may also not exist if it refers to the list (like list.head()). This means that when we use foldLeft
// it is preferable to have a Number accumulator, so that we can return 0, 1 or whatever without problems.
//
// We can say that the accumulator is the length of the longest string up to that point, so
// we use foldLeft starting from 0 and substituting the new length to the accumulator whenever it's
// larger. This way we return the length, not the String itself.
(stringList foldLeft 0)((x: Int,y: String) =>
    if(x > y.length) x else y.length)
// reduceLeft wouldn't be able to deal with this because we need to compare Strings, not an Int and a String
// (supertype constraint)

// We also have reduceRight and foldRight (we start aggregating from the right instead of left)
/*
 * The right versions basically compute:
 * - x1 op (x2 op ... (xn-1 op xn))))...) for reduceRight
 * - x1 op (......(xn op z)))))))...) for foldRight
 *
 * The main difference is that the accumulator for foldRight is at the end and the order of
 * evaluations changes, because we start computing an answer from the end of the list, coming
 * left.

 * def reduceRight[U >: T](op: (T, U) => U): U = this match {
 *      case Nil => throw new Error("Nil.reduceRight")
 *      case x :: Nil => x
 *      case x :: xs => op(x, xs.reduceRight(op))
  }
   def foldRight[U](z: U)(op: (T, U) => U): U = this match {
 *      case Nil => z
 *      case x :: xs => op(x, (xs foldRight z)(op))
  }
 */

// We can see an example for the problem of concatenating two lists:
def concat[T](xs: List[T], ys:List[T]):List[T] =
    // xs = x1::x2::...::xn::Nil
    // ys = y1::y2::...::yn::Nil
    // concat(xs,ys) = x1::x2::...::xn::ys
    // Basically, we can use foldRight, using ys as accumulator.
    // Why? Because foldRight considers each element and the accumulator and decides how to change
    // the accumulator. The accumulator can also change by making it longer as in appending elements.
    // The result of the first concatenation xn::ys will be the accumulator for the following processes.
    (xs foldRight ys)(_ :: _) //start from ys as accumulator, the accumulator operator will be the appending function

def myL = List(27,31,45,18)
def myL2 = List(17,29,30,1)
concat(myL,myL2)

// Problem 2: reverse a list
def reverse[T](xs:List[T]):List[T] = {
    // Not foldRight because we would give the last element of the original list the last position in the new
    // list. We must instead start considering elements from the first and leftmost, so that new elements coming
    // in will "push it back".
    //(xs foldRight List[T]())(_::_)
    // We need to start the accumulator with an empty list, append the leftmost element
    // in front of the accumulator and so on...
    // Note: we don't want to use Nil instead of an empty list of [T] because the type checker
    // expects the accumulator not to change type. (Nil is not List[T])
    (xs foldLeft List[T]())((l:List[T], x:T) => x::l)
}
reverse(myL)

/*
 * So:
 * - Use foldLeft when there is an accumulator that gradually collects partial results from single
 *   elements of the list, starting from the left and going to the right
 * - Use reduceLeft when we need to consider two consecutive elements of the list and the result of
 *   the transformation has the same type of the list. Be sure to not pass an empty list to any reduce*
 *   method, because they would deal with empty lists by throwing errors, since they need to be able to
 *   have at least 2 elements (the head and the tail) for the reduction
 * - Use foldRight and reduceRight with the same criteria when we need to consider element from the end
 *   of the list to the first elements.
 * - Remember that order is respected. If we consider first the first element and push it in another list,
 *   this will be pushed deeper and deeper in the list while we consider new elements. For example, the
 *   following function evaluates to the same input list:
 */
def id[T](xs: List[T]): List[T] = {
    (xs foldRight List[T]())((x: T, acc: List[T]) => x :: acc)
}
println(myL)
id(myL)

// Other examples for foldLeft/foldRight:
def mapFun[T,U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())((el: T, acc: List[U]) => f(el) :: acc)

def lenFun[T](xs: List[T]): Int =
    (xs foldLeft 0)((acc: Int, el: T) => acc+1)

println(mapFun(myL, (x:Int) => x+2))
println(lenFun(myL))