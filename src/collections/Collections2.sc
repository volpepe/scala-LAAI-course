/** ********** LESSON 6 *************** */
/*
 * Vectors are a type of collections really similar to standard Arrays.
 * Still, since they are collections, they are immutable. If an element in a
 * Vector is modified, a new Vector is actually created. This is not a simple copy-paste
 * in memory: there is an optimized mechanism called append-only that simply wraps the
 * data of the old Vector and explains in which part the new Vector is different.

 * Vectors are linear structures structured as trees. Each node in the tree contains
 * 32 pointers to 32 subtrees. In leaves, the 32 pointers point to 32 elements.
 * Since Vectors are immutable structures, the depth of these trees is fixed for a given Vector.

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
 *
 * Note that we only copy-paste pointers, not actual classes/values.
 */

// Vectors are created similarly to Lists:
val nums = Vector(1, 2, 3, -88)

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
val r: Range = 1 until 5 // 1,2,3,4
val r: Range = 1 to 5 // 1,2,3,4,5
val r = 1 to 10 by 3 // modify step (1,4,7,10)
val r = 6 to 1 by -2 // 6,4,2
/*
 * Note: ranges do not store the whole range in memory. Rather, the information
 * provided are used to generate the following element.

 * Both Vectors and Ranges implement the Seq trait. This contains many common operations:
 */
r exists (x => x > 0) // True if there is an element such that the function passed as argument holds
r forall (x => x > 0) // True if for all elements in the Sequence the function holds.
r zip r // Evaluates to a sequence of pairs from corresponding elements of sequence 1 and sequence 2.
// seq.unzip splits a sequence of pairs into two sequences with the corresponding elements.
// Here are some already implemented reductions:
r.max
r.min
r.sum
r.product
// Finally, we have flatMap. Classic map applies a function to all elements of a sequence and if we have
// for example a sequence within the sequence (like a List[String]) we apply the function to the whole string.
// flatMap instead applies the function like map but then unrolls each sequence within the sequence so that
// (for example) a String is transformed into character elements.
val listTest = List("Hello", "my", "Name", "is", "FEDERICO")
listTest flatMap ((x: String) => x.toLowerCase)

// Problem: implement a function that generates all pairs in the cartesian product of 1..N and 1..M
// (produce a sequence of (x,y) where x goes from 1 to M and y from 1 to N
def cartProduct(N: Int, M: Int): Seq[(Int, Int)] = {
    // Note: in the map implementation we map elements from a collection to elements
    // in another. This is a bijective function.
    // Using flatMap, we map elements in a collection to sub-collections in another collection.
    // In other words, we create a List[List[T]].
    // This is also achievable by map, but what flatMap does is merge together all elements
    // of these sub-collections. Classic map also requires a flatten operation right after.
    (1 to N) flatMap (x => (1 to M) map (y => (x, y)))
    // Here, we map the range 1-N (each element called "x") to a range 1-M.
    // Elements in this range are called "y" and we map each y to a pair (y,x).
}
cartProduct(5, 6)

// Using only map, we get a vector of vectors for each element x
// Conceptually, all elements are computed, but we need to flatten the result
def mapProduct(N: Int, M: Int) = {
    (1 to N) map (x => (1 to M) map (y => (x, y)))
}
mapProduct(5,6).flatten

// Why do we need flatMap map? Because map always means bijection.
// Here we need to link each number in the first range to all tuples containing it,
// which is not a biijection. It's a case of one-to-many.

// Problem 2: scalar product of two vectors (sum of pointwise products)
def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = {
    // We create a sequence of pairs of elements from xs and ys.
    // We map this sequence to a sequence of values multiplying the first
    // element with the second
    // Finally we sum over the whole sequence.
    ((xs zip ys) map (xy => xy._1 * xy._2)).sum
    // We can access elements of a pair with ._1 and ._2.
}

// Problem 3: check if a number is prime
def isPrime(n: Int): Boolean = {
    // A number is prime only if dividable by 1 and itself.
    // The range of numbers between 2 and n are potential divisors.
    // Check for each divisor that the rest is non-zero.
    (2 until n) forall (d => (n % d != 0))
}

/*
 * In imperative programming, we usually use loops to traverse sequences until
 * interesting values are found. For example: compute the set of pairs
 * of integers between 1 and N having sum which is prime, with no repetitions
 * (for example, if we have (2,5) and (5,2), we must consider only one of them.
 * Classical imperative programming would implement this task using two nested for
 * cycles that generate all possible pairs and then a check on these pairs.
 * In Scala, we have two options. We can use high order functions like before and write:
 */
def genSumPrime(N:Int): Seq[(Int,Int)] = {
    // First of all, notice that to fight the creation of duplicated pairs
    // we can simply generate the second element from 1 to x,
    // the first number of the pair.
    (1 to N) flatMap (x => (1 to x) map (y => (x,y))) filter
    // After the generation of all pairs, we can filter on the basis of the
    // isPrime function.
        (xy => isPrime(xy._1 + xy._2))
}

/*
 * Scala has a ad-hoc syntax for imperative-style programming patterns like the
 * double loop when generating elements from (1 to N) and mapping them to other
 * elements:
 */
def genSumPrime2(N: Int) =
    for {
        i <- 1 to N // Generators: let variable i take a value from a sequence, in order.
        j <- 1 to i
        if isPrime(i + j) // This is a filter for the sequence that discards certain values
                          // of the generator.
    } yield (i, j) // Generate a Sequence by getting elements from this generator
// yield generates elements for a sequence, so the return type is Seq[type generated by the generator]

// Most of the functions we have seen before can be rewritten using the for construct!
def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double = {
    // The generator can contain an entire pattern, not only a single value.
    // So, we only use one generator extracting values from a collection of pairs
    // getting pairs and yielding a combination of the elements of the pair.
    (for {
        (x, y) <- xs zip ys // The pair (x,y) will be extracted by the sequence created from
                            // the zipping of the sequences xs and ys.
    } yield x * y           // We combine the elements together however we want
    ).sum                   // And then we can apply a reduction on them.
}

def cartProduct2(N:Int, M:Int): Seq[(Int,Int)] = {
    for {
        i <- 1 to N
        j <- 1 to M
    } yield (i,j)
}

println(cartProduct(10,8) == cartProduct2(10,8))

/*
 * For expressions are similar to pattern matching in many ways and can in fact be used
 * to make complex queries on structured collections.
 */
case class Book(title: String, authors: List[String])

/*
 * This list looks like a list we can obtain from a JSON or by scraping a database
 * on the web. It explains why Scala can be used for data processing in a very
 * user-friendly way.
 */
val books: List[Book] = List(
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
    b <- books // Iterate on books
    a <- b.authors // Iterate on authors of each book
    if a.toLowerCase startsWith "bird" // Filter authors whose name starts with "bird"
} yield b.title // Return the title of the book

// We can rewrite this query in a more complex way with higher-order functions on Sequences:
books filter (b => b.authors exists (a => a.toLowerCase startsWith "bird")) map (b => b.title)
// But it's probably more expressive with for structures.

// Another query:
for {
    b <- books
    //if (b.title indexOf "Program") >= 0 // is "Program" in the book's title?
    if (b.title contains "Program")
} yield b.title

// With a single-variable generator we can have a one-liner approach
for (b <- books if b.title contains "Program") yield b.title

// Alternative:
books filter (b => b.title contains "Program") map (b => b.title)

/*
 * Interestingly enough, the for construct works on user-defined data structures as well
 * given that some methods are implemented.
 * - map
 * - flatMap
 * - withFilter
 *
 * If we imagine a collection C producing a collection R value by value (e),
 * we are basically doing a map operation.
 *
 * for {
 *    x <- C
 * } yield e
 *
 * is translated to
 *
 * C map (x => e)
 *
 * When we have two collections C1 and C2 and two generators
 * that extract values x1 and x2, yielding e that considers both of them
 * to produce a collection, we write:
 *
 * for {
 *    x1 <- C1
 *    x2 <- C2
 * } yield e
 *
 * is translated to
 *
 * C1 flatMap(x1 => C2 map (x2 => e))
 *
 * Therefore, map and flatMap are needed to use the for construct on our data structure.
 *
 * withFilter is just a "lazy filter" that is computed on the run when considering elements
 * from the range in the generation pipeline.
 */
val n1 = 1 to 10

for {
    a <- n1
    if a > 0
    b <- n1
    c = a + b
} yield c
// Equals to
n1.withFilter(a => a > 0).flatMap(a => n1.map(b => a + b))

/*
 * Another big family of collections that we have are Sets.
 * - Sets are Iterables
 * - Sets are covariant
 * - Sets are unordered
 * - Sets do not have duplicates
 */
val fruit = Set("apple", "banana", "pear")
fruit filter (_.startsWith("a"))
fruit.nonEmpty
fruit contains "apple"
fruit + "strawberry" // Add one element
fruit ++ Set("strawberry", "kiwi") // Add another set
// Of course, a set is immutable, so these instructions create a new Set,
// using sharing where possible.

/*
 * Example: n-queens problem with Sets:
 * Compute all possible solutions recursively and using backtracking.
 *
 * The function receives an integer which is the size of the board.
 * It returns a Set of Vector[Int] (containing the positions of the queens
 * for each row). The Set contains all possible solutions of the problem.
 */
def queens(n: Int): Set[Vector[Int]] = {

    /*
     * This is the function that tries to place queens on the board
     * and returns the correct value of the function.
     */
    def placeQueens(k: Int): Set[Vector[Int]] = {
        /*
         * If the board is of size zero, there is no solution. We return
         * an empty Set (actually, a set with an empty Vector)
         */
        if (k == 0) Set(Vector())
        else
            for {
                // Otherwise, we generate solutions recursively and using a generator.
                // First, place k-1 queens, then compute all additional placements
                // for the kth queen on the kth row
                queens <- placeQueens(k - 1)
                // We then need to generate all possible solutions for the nth queen.
                // Each queen is represented by the column she is placed in, so it seems
                // appropriate to have a variable that ranges on all possible columns for
                // the given row. Now, the number of columns to be considered depends on n
                // which is recursively changed throughout the execution of the function,
                // so we need to iterate variable col from 0 to n (exclusive).
                col <- 0 until n
                // Finally, we yield the computed combination by checking if it's safe with
                // respect to the placement of the previous queens.
                if isSafe(col, queens)
            } yield queens :+ col // Yield a Vector where we append to the previously computed queens
                                  // the safe placement we have found to work for the nth queen.
                                  // Eventually we yield all possible solutions.
    }

    /*
     * This functions checks if a placement (col) is safe with respect to the Vector of queens.
     */
    def isSafe(col: Int, queens: Vector[Int]): Boolean = {
        // How many rows do we have at this point
        val row = queens.length
        // We use the number of rows to iterate from 0 to it and create pairs containing
        // the positions of the queens
        val queensWithRows = (0 until row) zip queens // We create a pair containing
                                                      // row-queen position combinations.
        // All the pairs must respect a series of rules:
        queensWithRows forall (p => {
                (col != p._2) &&        // The nth queen's column must be different
                                        // from all the other positions (no queen on same column)
                (math.abs(col - p._2) != row - p._1)    // The nth queen's columns must not be on
                                                        // the same diagonal as another queen
            // The rows constraint is automatically satisfied because we only have one value
            // per row (the value of the column)
            }
        )
    }

    placeQueens(n)
}

queens(5)

/*
 * The last family is the one of Maps.
 * Maps are an association Key -> Value. Their type is indeed Map[KeyType, ValueType]
 */
val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
/*
 * A Map can also be seen as a Set of Pairs: an alternative representation for the K -> V
 * we have seen before is indeed (K,V)
 *
 * Maps implement iterable, so we can iterate over its elements (treated as pairs)
 *  and also use all high-order methods we have seen up to now like map or flatMap, ...
 *
 * Also, it implements the trait Function1, because a Map is kind of like a function,
 * with a finite domain (the keys) and codomain (the values)
 */
romanNumerals("I") // Returns 1.

/*
 * We can say that a Map is a partial function, because it maps inputs to outputs.
 * We can associate a default value to a Map in order to complete its domain (so that
 * we can use it with values that are not part of the domain)
 */
val romanNumeralsTotal = romanNumerals withDefaultValue "unknown"
romanNumeralsTotal("C") // Will return "unknown"

// With default values, Maps are basically complete functions.

/*
 * Maps also support Optional types. We can use the "get" method to get optional values:
 * - Some("X") tells us that the queried key is in the map and is associated to value X
 * - None tells us that the key is not in the map
 */
val capitalOfCountry = Map("US"->"Washington",
                            "Italy"->"Rome",
                            "France"->"Paris")

capitalOfCountry get "US"
capitalOfCountry get "Venezuela"
capitalOfCountry get "France" match {
    case Some(x) => x
    case None => "unknown"
}

// See concepts.Maps for an additional example.

// groupBy: a method to partition a collection depending on the value returned from a function
// applied to all elements.
val donuts: Seq[(String, Double)] = Seq(
    ("Plain Donut", 2.5), // Name-price
    ("Strawberry Donut", 4.2),
    ("Glazed Donut", 3.3),
    ("Plain Donut", 2.8), // Repeated information: multiple prices
    ("Glazed Donut", 3.1)
)
donuts groupBy (_._1) // Group together elements with the same first element of the pair
// The return value is a Map String->List[(String, Double)], so key->List(elements)
// Each binning value of the function is associated to the elements that fall into it.