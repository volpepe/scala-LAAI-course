// A parametric covariant list
/*
trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def append[U >: T](e:U): List[U]
  //def append[U >: T](e:U): List[U]
}

class ConsList[T](val head: T, val tail: List[T])
  extends List[T] {
  def isEmpty = false
  def append[U >: T](e:U) =
    new ConsList[U](head,tail append e)
  //def append[U >: T](e:U) = new ConsList[U](head,tail append e)
}

object NilList extends List[Nothing] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
  def append[U](e:U) =
    new ConsList[U](e, NilList)
  //def append[U >: T](e:U) = new ConsList[U](e,new NilList)
}


class Base {
  def f = println("Base Class")
}

class Derived extends Base {
  override def f = println("Derived Class")
  def g = println("Base Class")
}

def newl: List[Base] =
  new ConsList[Derived](new Derived, NilList)

newl.head.f

//def applyAppend(l: List[Base]) =
//  l append (new Base)

//applyAppend(new ConsList[Base](new Base,new NilList))

//applyAppend(new ConsList[Derived](new Derived,new NilList))



trait genericStruct[T] {
  def f: T => Int
  def apply(elem:T): Int = f(elem)
}
// could genericStruct be covariant?
// assume genericStruct[Derived] subtype of genericStruct[Base]
//gs: genericStruct[Base] = new genericStruct[Derived]
//gs apply (new Base)
//
// Covariant is not admitted in this case; while contravariance is admitted
//gs: genericStruct[Derived] = new genericStruct[Base]
//gs apply (new Derived)


// Non-variant Arrays, Covariant Vectors

val arr1: Array[Derived] = Array(new Derived)
val arr2: Array[Base] = Array(new Base)
  //Array(new Base) //arr1
// the assignment of arr1 to arr2 is not admitted
// because Array is not covariant (because mutable)
arr2(0) = new Base
//arr1(0).g


val vect1: Vector[Derived] = Vector(new Derived)
val vect2: Vector[Base] = vect1
//vect2(0) = new Base
// the modification of the Vector is not admitted
// because it is immutable (hence, considered covariant)
vect1(0).g
*/


// INSERTION SORT

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x < y) x :: xs else y :: insert(x, ys)
}

isort(List(4268,5471,2548,2798,9521,3521,9789,2189))


// QUICKSORT

def msort(xs: List[Int]): List[Int] = {
  val n = xs.length/2
  if (n == 0) xs else
  {
    def merge(xs: List[Int], ys: List[Int]): List[Int] =
/*      xs match {
        case List() => ys
        case x :: xs1 =>
          ys match {
            case List() => xs
            case y :: ys1 =>
              if (x < y) x :: merge(xs1, ys)
              else y :: merge(xs, ys1)
          }
*/
      (xs, ys) match {
        case (List(), ys) => ys
        case (xs, List()) => xs
        case (x::xs1 , y::ys1) =>
                  if (x < y) x :: merge(xs1, ys)
                  else y :: merge(xs, ys1)
      }
    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}


msort(List(4268,5471,2548,2798,9521,3521,9789,2189))


// EVALUATION

val numList = List.fill(1000)(100000).map(scala.util.Random.nextInt)

def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block    // call-by-name
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0)/1000000 + "ms")
  result
}

time(isort(numList))

time(msort(numList))

