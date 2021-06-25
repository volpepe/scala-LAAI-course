// EXAM LAAI 2 - 11.06.20

/**
  *   1)
  *   Consider the following declarations:
  *
  *   val S = Set(1,3,4,7)
  *   val S2 = S ++ (S map (_+1))
  *
  *   What is the type and the value of S2?
  *   Justify your answer.
  */

/*
 S2 will be a HashSet[Int] and its value will be (1, 2, 3, 4, 5, 7, 8).
 It will be a HashSet because it's the default implementation of sets.
 The value is due to the fact that ++ is the operator to unify two sets, in this
 case set S seen in the first line and the set created by mapping each element
 of S to itself + 1. Since a set has no duplicate elements, 4 is not repeated.
 */

val S = Set(1,3,4,7)
val S2 = S ++ (S map (_+1))

/**
 * 2)
 * Consider the following expression:

  (List(3,9,12) foldLeft 4)(_ - _)

  Describe its evaluation and the final computed value.
 */

/*
 foldLeft applies the function on the right (in this, case, a subtraction), with operands
 the accumulator (initially 4) and the sequentially all elements of the list, starting from
 the leftmost.
 Thus, the evaluation is ((4-3)-9)-12 and the final value will be -20
 */

(List(3,9,12) foldLeft 4)(_ - _)

/**
 * 3)
 * Consider the following function:

  def mystery(f: Int=>Int, x: Int): Int = f(x+1)

  What is the result of the evaluation of the following expression:

  List(5,1,3) map (x=>x+mystery(_+1,x+1))

  Justify your answer.
 */

/*
 We map each element of the list to itself + a function that receives as parameters a function and
 the element + 1. The argument function adds one to a number. The mystery function passes to this
 function the element + 1. In total, the element from the list is added 3. Then, the map function
 evaluates to List(13,5,9)
 */
def mystery(f: Int=>Int, x: Int): Int = f(x+1)

List(5,1,3) map (x=>x+mystery(_+1,x+1))

/**
 * 4)
 * The following function mystery3 is not tail recursive:

  def max(x1: Int, x2: Int) = if (x1>x2) x1 else x2

  def mystery3(l:List[Int]):Int =
    if (l == Nil) 0
    else max(l.head,mystery3(l.tail))

  Write an equivalent tail recursive function.
 */

def max(x1:Int, x2:Int) = if (x1>x2) x1 else x2

def mystery3(l:List[Int]):Int = {
  def mysteryTailRec(l:List[Int], part:Int):Int = {
    if (l == Nil) part
    else mysteryTailRec(l.tail, max(l.head, part))
  }
  mysteryTailRec(l, 0)
}

/**
 * 5)
 * What does the following function compute?

  def mystery2(l: List[Int]) =
    (0 :: (l filter (_ < 100) map (x=>1))) reduce (_ + _)

  Justify your answer.
 */

/*
 * It computes the number of elements < 100 in the list. It does so by filtering all elements
 * < 100, thus obtaining a smaller List, and transforming each element in the filtered list into a 1.
 * We append a 0 at the beginning in case the List is empty. We then use the reduce function to sum
 * all elements in this List, obtaining exactly the number of elements <100 in the List.
 */