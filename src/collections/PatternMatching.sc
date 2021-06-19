//-------------- PATTERN MATCHING -------------------//

// Let's implement two popular sorting algorithms using pattern matching
// and Lists.

// We use the standard library List. A List can be instantiated with:
// List(elem1, elem2, ... elemn)
// List() is an empty list, which is equal to the object Nil.

// This function returns a List[Int] where x is placed in the appropriate position
// following the order of the List..
def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    // x is the element we want to insert in the list xs.
    // We need to check that the list is not empty first.
    // If the list is empty, return a List with only that element
    case Nil => List(x)
    // Otherwise, if xs is a non empty list, decompose it in head and tail
    // with the pattern notation y :: ys and then try to understand where should
    // element x be placed.
    case y :: ys => if (x < y) x :: xs else y :: insert(x, ys)
}

// This is the function that does the sorting.
def isort(xs: List[Int]): List[Int] = xs match {
    // We are saying: a list xs can be matched with two possible patterns:
    // - it is either an empty list (List() or Nil)
    // - or it has a structure like y :: ys (head + tail)
    case Nil => List()
    // y will be our head, ys the tail of the list.
    // The isort algorithm takes the head of the list and inserts it in the
    // right place. It is a recursive algorithm, so we do this for every element,
    // starting from the last one.
    case y :: ys => insert(y, isort(ys))
    // y :: ys is called "pattern". The language matches the pattern against
    // the value that we are trying to match and if the pattern matches we
    // execute the correspondent body.
}

isort(List(4268, 5471, 2518, 2798, 9521, 3521, 9789, 2189))

// Let's now implement MergeSort
def msort(xs: List[Int]): List[Int] = {
    // Take the middle index
    val n = xs.length / 2
    // If the index is 0, return the head of the list
    if (n == 0) xs else {
        // The merge function takes two lists as arguments and returns
        // an ordered one.
        def merge(xs: List[Int], ys: List[Int]): List[Int] = {
            // We try patterns against the first half of the list.
            xs match {
                // Empty list: return the other list
                case Nil => ys
                // non-empty list: decompose the head from the tail of the first list
                case x :: xs1 =>
                    // Check the other list
                    ys match {
                        //if ys is empty return the other list
                        case Nil => xs
                        // if it is non-empty, put the smallest head first and concatenate merging the rest
                        case y :: ys1 =>
                            if (x < y) x :: merge(xs1, ys) //x and then the rest
                            else y :: merge(xs, ys1) //y and then the rest
                    }
            }
        }
        // Otherwise, split the list in two parts, sort them and merge them.
        val (fst, snd) = xs splitAt n
        merge(msort(fst), msort(snd))
    }
}

/*
 * The merging function is quite complex because we try different types of pattern
 * matching on different lists and the list of checks and patterns becomes excessively long.
 * We can directly use both lists as patterns and check patterns on both of them.
 */

// More compact alternative:
def msort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs else {
        def merge(xs: List[Int], ys: List[Int]): List[Int] = {
            // We can even match against a Pair of lists.
            (xs, ys) match {
                // If the first list is empty return the second
                case (Nil, ys) => ys
                // If the second list is empty return the first
                // Note: alternative way to write (xs, List())
                case (_, Nil) => xs
                // if both are non-empty, merge them
                case (x :: xs1, y :: ys1) =>
                    //both lists are non-empty
                    if (x < y) x :: merge(xs1, ys)
                    else y :: merge(xs, ys1)
            }
        }

        val (fst, snd) = xs splitAt n
        merge(msort(fst), msort(snd))
    }
}

msort(List(4268, 5471, 2518, 2798, 9521, 3521, 9789, 2189))