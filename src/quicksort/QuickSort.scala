package quicksort

import scala.util.Random

object QuickSort {

    //object classes are singleton classes --> we can only generate one object
    //create an array of random 1000 random ints
    val r: Random.type = Random
    //for keyword, generator (i <- 1 to 1000), yield keyword, r.nextInt() generator
    //it means that this random number generation will be executed 1000 time.
    //this will create an array of 1000 random integers.
    val randomArray: Array[Int] = (for (i <- 1 to 1000) yield r.nextInt(100000)).toArray

    def main(args: Array[String]): Unit = {
        //do the sorting
        val sortedArray = quickSort(randomArray)
        //print it with a higher order function
        //in fact: passes println to the function foreach
        sortedArray.foreach(println)
    }

    /* Note that Scala does not need return values
    Also, types are often inferred in functions */

    // quicksort recursive algorithm
    def quickSort(xs: Array[Int]): Array[Int] = {
        if (xs.length <= 1) xs
        else {
            //element of the array that stays in the middle
            val pivot = xs(xs.length / 2)
            //concatenates all elements smaller than the pivot
            //then all elements equals to the pivot
            //finally all elements bigger than the pivot
            Array.concat(
                /* First example of an higher order function
                filter receives a function as input.
                pivot > _ is a function that returns true if
                an element is smaller than pivot, False otherwise*/
                quickSort(xs filter(pivot > _)),
                xs filter (pivot == _),
                //we use underscore to refer to an element of the array
                //another way of writing the same thing is:
                //quickSort(xs filter(x => pivot < x))
                quickSort(xs filter(pivot < _))
            )
        }
    }
}
