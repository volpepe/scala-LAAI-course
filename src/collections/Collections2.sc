/************ LESSON 6 ****************/
/*
 * Vectors are a type of collections really similar to standard Arrays.
 * Still, since they are collections, they are immutable. If an element in a
 * Vector is modified, a new Vector is actually created. This is not a simple copy-paste
 * in memory: there is an optimized mechanism called append-only that simply wraps the
 * data of the old Vector and explains in which part the new Vector is different.

 * Vectors are linear structures structured as trees. Each node in the tree contains
 * 32 pointers to 32 subtrees. In leaves, the 32 pointers point to 32 elements.
 * Since Vectors are immutable structures, the depth of these trees is fixed.

 * With respect to Arrays, they are faster in access, since the time complexity
 * is just O(depth) rather than O(n) if we want to reach the last element.

 * Also, updates to Vectors create new Vectors, but keeping the same structure of the tree,
 * simply adding/modifying elements or nodes. The previous vector of course remains the same.
 * As said before, this update is not a simple copy-paste in memory.
 * - We just do the minimal copy-pasting. Suppose we only change an element.
 *   We need to copy-paste the leaf where that element lies and only change the pointer
 *   to the new element.
 * - Then, we need to update the father of that node so to point to the new leaf as "next leaf".
 *   Therefore, the father is copy-pasted, only changing that pointer.
 * - This goes on up to the root, which is copy-pasted and updated with the pointer to the new
 *   subtree.
 * - The other subtrees are shared with the previous Vector.
 */

// Vectors are created similarly to Lists:
val nums = Vector(1,2,3,-88)

// They support the same operations as lists, except :: which is replaced by :+ or +:
// which creates a new Vector. The : are always on the side of the sequence.
// x +: xs (x is appended at the beginning of the old vector xs)
// xs :+ x (x is appended at the end of the old vector xs)

// A common operation is:
nums.updated(2, 5)
// Which creates a vector where element at index 2 is replaced by a 5.

/*
 * Ranges are another simple kind of sequence. There are three operators to create
 * ranges:
 */
val r : Range = 1 until 5 // 1,2,3,4
val r : Range = 1 to 5 // 1,2,3,4,5
val r = 1 to 10 by 3 // modify step (1,4,7,10)
val r = 6 to 1 by -2 // 6,4,2
/*
 * Note: ranges do not store the whole range in memory. Rather, the information
 * provided are used to generate the following element.

 * Both Vectors and Ranges implement the Seq trait. This contains many common operations:
 */
r exists (x => x > 0)
r forall (x => x > 0)
r zip r //a sequence of pair form corresponding elements of sequence 1 and seq 2.
//seq.unzip splits a sequence of pairs into two sequences.
r.max
r.min
r.sum
r.product

// Problem: implement cartesian product (produces a sequence of (x,y) where
// x goes from 1 to M and y from 1 to N
def cartProduct(M: Int, N:Int): Seq[(Int, Int)] = {
  // note: in the map implementation we map elements from a collection to elements
  // in another. this is a biijective function.
  // using flatMap, we map elements in a collection to mini-collections in another collection.
  // this is also achievable by map, but what flatMap does is merge together all elements
  // of these mini-collections.
  (1 to N) flatMap (x => (1 to M) map (y => (x,y)))
  // here we map the range 1-N (each element called "x") to a range 1-M.
  // Elements in this range are called "y" and we map each y to a pair (y,x).
}
cartProduct(5,6)

// scalar product
def scalarProduct(xs:Vector[Double], ys:Vector[Double]):Double = {
  // we create a sequence of pairs of elements from xs and ys.
  // we map this sequence to a sequence of values multiplying the first
  // element with the second
  // finally we sum over the whole sequence.
  (xs zip ys).map(xy => xy._1 * xy._2).sum
}

// check if a number is prime
def isPrime(n:Int):Boolean = {
  // a number is prime only if dividable by 1 and itself.
  // 2 until n are potential divisors
  // check for each divisor that the rest is non-zero.
  (2 until n) forall (d => (n%d != 0))
}

/*
 In imperative programming, we usually use loops to traverse sequences until
 interesting values are found. For example: compute the set of pairs
 of integers between 1 and N having sum which is prime, with no repetitions
 If we do this with classical imperative programming we would write:
 */
def genSumPrime(N:Int) = {
  // extract values from a sequence, map them to pairs
  // check pairs
  (1 to N) flatMap
    (i => (1 to i) map (j => (i, j))) filter
    (pair => isPrime(pair._1 + pair._2))
}

/*
 Scala has a ad-hoc syntax for this typical programming pattern:
 */
def genSumPrime(N:Int) =
  for {
    i <- 1 to N // generators: take a value from a sequence
    j <- 1 to i
    if isPrime(i+j) // this is called filter
  } yield (i,j) // generate a Sequence by getting elements from this generator

// we can use it for the scalar product too!
def scalarProduct(xs: Vector[Double],
                  ys: Vector[Double]) : Double = {
  // the generator can contain an entire pattern, not only a single value.
  // so, we only use one generator extracting values from a collection of pairs
  // getting pairs and yielding a combination of the elements of the pair.
  (for ((x,y) <- xs zip ys) yield x * y).sum
}

// for expressions can also be used as queries on data.
// case classes are particular classes such that on their objects we can apply
// pattern matching.
case class Book(title: String, authors: List[String])

val books: List[Book] = List (
  Book(
    title = "Structure and Interpretation of Computer Programs",
    authors = List("Abelson, Harald", "Sussman, Gerald J.")),
  Book(
    title = "Introduction to Functional Programming",
    authors = List("Bird, Richard", "Wadler, Phil")),
  Book(
    title = "Effective Java",
    authors = List("Bloch, Joshua")),
  Book(
    title = "Java Puzzlers",
    authors = List("Bloch, Joshua", "Gafter, Neal")),
  Book(title = "Programming in Scala",
    authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill"))
)

for {
  b <- books      // iterate on books
  a <- b.authors  // iterate on authors of each book
  if a startsWith "Bird"    // filter
} yield b.title

// or:
for {
  b <- books
  if (b.title indexOf "Program") >= 0 // is "Program" in the book's title?
} yield b.title

// with one generator we can have a one-liner approach
for (b <- books if (b.title indexOf "Program") >= 0) yield b.title

/*
 Interestingly enough, the for construct works on user-defined data structures as well
 given that some methods are implemented.
 - map
 - flatMap
 - withFilter

 If we imagine a collection C producing a collection R value by value (e),
 we are basically doing a map operation.

 for {
  x <- C
 ] yield e

 is translated to

 C map (x => e)

 When we have two collections C1 and C2 and two generators
 that extract values x1 and x2, yielding e that considers both of them
 to produce a collection, we write:

 for {
  x1 <- C1
  x2 <- C2
 } yield e

 is translated to

 C1 flatMap(x1 => C2 map (x2 => e))

 Therefore, map and flatMap are needed to use the for construct on our data structure.
 */
val n1 = 1 to 10

for {
  a <- n1
  if a>0
  b <- n1
  c = a+b
} yield c
// equals to
n1.withFilter(a => a > 0).flatMap(a => n1.map(b => a+b))

/*
 Sets
 - Sets are covariant
 - Sets are unordered
 - Sets do not have duplicates
 */
val fruit = Set("apple", "banana", "pear")
fruit filter (_.startsWith("a"))
fruit.nonEmpty
fruit contains "apple"
fruit + "strawberry" // add one element
fruit ++ Set("strawberry", "kiwi") // add another set
// of course, a set is immutable.

/*
 N-queens problem with sets (using backtracking)
 Compute all possible solutions.
 */
def queens(n:Int) = {
  def isSafe(col:Int, queens: Vector[Int]): Boolean = {
    val row = queens.length
    val queensWithRows = (0 until row) zip queens //pair row-queen position
    queensWithRows forall (p =>
      (col != p._2) && (math.abs(col - p._2) != row - p._1))
  }

  def placeQueens(k:Int): Set[Vector[Int]] = {
    if (k == 0) Set(Vector())
    else
      for {
        // proceed recursively.
        // first place k-1 queens, then compute all additional placements
        // for the kth queen on the kth row
        queens <- placeQueens(k-1)
        // each queen is represented by the column it is placed in.
        // note that we are reducing the chessboard on the rows, not on the columns
        // therefore we need to generate positions until n.
        col <- 0 until n
        if isSafe(col, queens) //check if this placement is safe wrt all previous queens
      } yield queens :+ col   //add the safe placement we have found.
  }
  placeQueens(5)
}

queens(5)

/*
 Maps.
 Maps are an association Key -> Value. Note that K -> V is equal to (K,V).
 */
val romanNumerals = Map("I"->1, "V"->5, "X"->10)
/*
 Maps implement iterable, so we can go over its keys.
 Also, it implements the traits Function1, because a Map is kind of like a function.
 */
romanNumerals("I")
/*
 We can say that a Map is a partial function, because it maps inputs to outputs.
 We can associate a default value to a Map in order to complete its domain (so that
 we can use it with values that are not part of the domain)
 */
val romanNumeralsTotal = romanNumerals withDefaultValue "unknown"
romanNumeralsTotal("C")

// some more examples on slides.

// groupBy: a method to partition a collection depending on some function
val donuts: Seq[(String, Double)] = Seq(
  ("Plain Donut", 2.5), //name-price
  ("Strawberry Donut", 4.2),
  ("Glazed Donut", 3.3),
  ("Plain Donut", 2.8), // repeated information, multiple prices
  ("Glazed Donut", 3.1)
)
donuts groupBy (_._1) //put together elements with the same first element of the pair
// the return value is a Map String->List[(String, Double)], so key->List(elements)