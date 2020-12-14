package introchallenge

import scala.io.Source

/*
* This was a challenge that was proposed in a paper
* https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=876288
* that showcased the power of expression of different programming
* languages by solving the same problem with them.
* Important parameters of these experiments were lines of code,
* time for writing, time of execution, simplicity of reading the code,
* etc.
* Scala has a very low amount of lines of code with respect to other
* languages. Also, the amount of time needed to write such a program
* is extremely low. It can take a few minutes rather than hours of work.
*
* Problem: given a sequence of digits (phone digits layout),
* return the corresponding sentences (sequences of corresponding
* words taken from a given dictionary. Recall that:
* 2 = a,b,c
* 3 = d,e,f
* ...
* 9 = w,x,y,z
*
* It's an encoding - decoding program
* The program is correct if it returns all potential decoding of digits
*/
object Challenge {
    // Point to the dictionary on the web
    // This file contains a lot of words line by line.
    val in = Source.fromURL("http://cs.unibo.it/zavattar/words.txt")
    val word = in.getLines().toList filter(
        /* Filters out words that are not entirely made of letters */
        w => w forall(c => c.isLetter)
        )

    val mnem = Map('2'->"ABC", '3'->"DEF", '4'->"GHI", '5'->"JKL",
        '6'->"MNO", '7'->"PQRS", '8'->"TUV", '9'->"WXYZ")

    /* Map from each letter to its corresponding digit */
    val charCode: Map[Char, Char] =
        for {
            // The map mnem populates the (digit, str) pair.
            (digit, str) <- mnem
            // Extract letters from the str part of the pair
            ltr <- str
            // For each letter yield a new element of the map
            // that links that letter to the digit it got associated
            // to in the map.
        } yield  ltr -> digit
    /* The final map therefore links each character to a digit. */

    /* Much of the dirty work is done by the map function.
    * The function returns the code of a letter*/
    def wordCode(word: String): String =
        word.toUpperCase map charCode

    val wordsForNum: Map[String, Seq[String]] =
        /* Associates a sequence of words to a sequence of digits */
        word groupBy wordCode withDefaultValue Seq()

    def encode(number: String): Set[List[String]] = {
        /* This function brute forces by trying to match all possible
        * parts of the digits string with words. */
        if (number.isEmpty) Set(List())
        else (
            for {
                /* This syntax indicates is used for generators.
                * Split will be a generator of numbers that indicate
                * where to split the word */
                split <- 1 to number.length
                /* word will be a generator that iterates on all possible splits*/
                word <- wordsForNum(number take split)
                /* the rest of the word is encoded recursively */
                rest <- encode(number drop split)
            } yield word::rest /* This operator construct a new list from elements */
            /* It's different from concatenation that concatenates two lists.
            * In our case, word is just one element and rest is a list. */
            ).toSet
    }

    def main(args: Array[String]): Unit = {
        print(encode("7225247386"))
    }
}
