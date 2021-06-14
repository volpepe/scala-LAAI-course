// Given K and N generate the pairs (1,K), (2,K), .. , (N,K)

def createPair(V:Int, K:Int): Seq[(Int,Int)] =
  (1 to V) map (x => (x,K))

createPair(3,5)

// Now define the cartesian product
def cartProduct(M:Int, N:Int): Seq[(Int,Int)] =
  (1 to N) flatMap (x => createPair(M,x))

cartProduct(5,4)

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map(xy => xy._1 * xy._2).sum

// Primality check
def isPrime(n: Int): Boolean =
  (2 until n) forall (d => (n%d != 0))

isPrime(35)
isPrime(29)

// All pairs of numbers less than 20 with a prime sum

(1 to 20) flatMap
  (i => (1 to i) map (j => (i, j))) filter
  (pair => isPrime(pair._1 + pair._2))

for {
  i <- 1 to 20
  j <- 1 to i
  if isPrime (i + j)
} yield (i, j)


def scalarProduct2(xs: Vector[Double],
                   ys: Vector[Double]) : Double =
  (for ((x, y) <- xs zip ys) yield x * y).sum


// Exercises with for

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
    authors = List("Bloch, Joshua", "Bird, pinco", "Bird, pallino")),
  Book(
    title = "Java Puzzlers",
    authors = List("Bloch, Joshua", "Gafter, Neal")),
  Book(title = "Programming in Scala",
    authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill"))
)

// useful function

"abracadabra" indexOf "dab"
"abracadabra" indexOf "cab"
"abracadabra" indexOf "abr"


// Books edited by Bird
(for {
  b <- books
  a <- b.authors
  if a startsWith "Bird"
} yield b).toSet


// Books containing in the title the string "Program"

for (
  b <- books
  if (b.title indexOf "Program") >= 0
) yield b



// For comprehension as syntactic sugar

trait Option[+A] {
  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def withFilter(f: A => Boolean): Option[A]
}

case class Some[A](value: A) extends Option[A] {
  def foreach(f: A => Unit): Unit = f(value)
  def map[B](f: A => B): Option[B] = Some(f(value))
  def flatMap[B](f: A => Option[B]): Option[B] = f(value)
  def withFilter(f: A => Boolean): Option[A] = if (f(value)) this else None
}

object None extends Option[Nothing] {
  def foreach(f: Nothing => Unit): Unit = ()
  def map[B](f: Nothing => B): Option[B] = None
  def flatMap[B](f: Nothing => Option[B]): Option[B] = None
  def withFilter(f: Nothing => Boolean): Option[Nothing] = None
  override def toString = "None"
}

def divide(n:Int, m:Int): Option[Int] =
  if (m==0) None else Some(n/m)

val n1: Option[Int] = divide(5,2)
val n2: Option[Int] = divide(5,0)



for {
  a <- n1
} println(a)

//n1.foreach(a => println(a))



for {
  a <- n2
} yield a+1

//n1.map(a => a+1)



for {
  a <- n1
  b <- n2
} yield a+b

//n1.flatMap(a => n2.map(b => a+b))



for {
  a <- n1
  if a>0
  b <- n1
} yield a+b

//n1.withFilter(a => a > 0).
//  flatMap(a => n2.map(b => a + b))



for {
  a <- n1
  if a>0
  b <- n1
  c=a+b
} yield c

//n1.withFilter(a => a > 0).
//  flatMap(a => n2.map(b => (b, a+b)).
//    map({case (b,c) => c}))




// EXERCISE
// Implement the brute force algorithm for the n-queens problem
// A solution is represented as a Vector[Int] with n values:
//   at each position i, the vector contains the column of the
//   queen on line i
// Find all possible solutions (return a Set[List[Int]])

def queens(n: Int) = {
  def isSafe(col: Int, queens: Vector[Int]): Boolean = {
    val row = queens.length
    val queensWithRows = (0 until queens.length) zip queens
    queensWithRows forall (p =>
      (col != p._2) && (math.abs(col - p._2) != row - p._1)
      )
  }
  def placeQueens(k: Int): Set[Vector[Int]] = {
    if (k == 0) Set(Vector())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield queens :+ col
  }
  placeQueens(n)
}

def show(queens: Vector[Int]) = {
  val lines =
    for (col <- queens)
      yield Vector.fill(queens.length)(" *").updated(col," X")

  ("\n" +: (lines flatMap (l => l :+ "\n")))
    .mkString
}

//queens(5)

queens(5) map show

