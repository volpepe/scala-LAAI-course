/** INTRODUCTION: LESSON 5 **/

// Let's implement two popular sorting algorithms:

// We use the standard library List. A List can be instantiated with:
// List(elem1, elem2, ... elemn)
// List() is an empty list.

// insert returns a List[Int] where x is placed in the correct position.
def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  // we expect x to be the head, xs to be the tail.
  // if the tail is empty, return a List with only that element
  case List() => List(x)
  // otherwise, if xs is another list:
  case y :: ys => if (x < y) x :: xs else y :: insert(x, ys)
}

def isort(xs: List[Int]): List[Int] = xs match {
  // we are saying: a list xs can be matched with two possible patterns:
  // - it is either an empty list (List())
  // - or it has a structure like y :: ys (head - tail)
  case List() => List()
  // y will be our head, ys the tail of the list.
  case y :: ys => insert(y, isort(ys))
  // y :: ys is called "pattern". The language matches the pattern against
  // the value that we are trying to match and if the pattern matches we
  // execute the correspondent body.
}

isort(List(4268,5471,2518,2798,9521,3521,9789,2189))

// Let's now implement MergeSort
def msort(xs: List[Int]):List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs else {
    def merge(xs:List[Int], ys:List[Int]):List[Int] = {
      xs match {
          // empty list: return the other list
        case Nil => ys
          // non-empty list
        case x :: xs1 =>
          // check the other list
          ys match {
              //if ys is empty return the other list
            case Nil => xs
              // if it is non-empty, check the heads and merge the rest
            case y :: ys1 =>
              if(x<y) x :: merge(xs1, ys) //x and then the rest
              else y :: merge(xs, ys1) //y and then the rest
          }
      }
    }
    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

// More compact alternative:
def msort(xs: List[Int]):List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs else {
    def merge(xs:List[Int], ys:List[Int]):List[Int] = {
      // we can even match against a pair of lists.
      (xs, ys) match {
          // if the first list is empty return the second
        case (List(), ys) => ys
          // if the second list is empty return the first
        case (xs, List()) => xs
          // if both are non-empty, merge them
        case (x::xs1, y::ys1) =>
          //both lists are non-empty
          if(x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    }
    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

msort(List(4268,5471,2518,2798,9521,3521,9789,2189))