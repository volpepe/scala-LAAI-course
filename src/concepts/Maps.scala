package concepts

object Maps {
    /*
     * We can represent polynomials naturally as maps degree -> constant
     * For example: 2x + 5.3x^2 - 4.99x^5 can be represented as:
     * {1 -> 2, 2 -> 5.3, 5 -> 4.99}
     */
    class Polynomial(terms0: Map[Int, Double]) {
        // With this definition we can create polynomials by simply inputting a variable
        // type of degrees and constants
        def this(bindings: (Int, Double)*) = this(bindings.toMap)
        // Any degree that is not present in the map actually gets multiplied by 0.0
        val terms: Map[Int, Double] = terms0 withDefaultValue 0.0
        // This function modifies a Polynomial map by adding a new Pair or modifying an old one
        def adjust(term: (Int, Double)): (Int, Double) = {
            val (k:Int, v:Double) = term
            k -> (v + terms(k))
        }
        // To define an addition between two maps we can write a function using adjust:
    /*  def +(that: Polynomial): Polynomial = {
            // ++ unifies the two maps. In case of duplicated keys,
            // the right hand term is given priority
            new Polynomial(terms ++ (that.terms map adjust))
        }
    */
        def +(that: Polynomial): Polynomial = {
            // An alternative is using foldLeft, which also works on Maps.
            // Of course, the value used in the foldLeft expression is a pair.
            new Polynomial((that.terms foldLeft terms)(addTerm))
        }
        def addTerm(acc: Map[Int,Double], x:(Int,Double)): Map[Int,Double] = {
            val (k:Int, v:Double) = x // We can unpack a Pair like this
            // The new map will be: the old one + the new or updated pair
            // from x.
            acc + (k -> (v + terms(k))) // Alternative representation of Pair.
        }
        override def toString: String = (
            for ((exp, coeff) <- terms.toList.sorted) yield coeff + "x^" + exp) mkString " + "
    }

    def main(args: Array[String]): Unit = {
        val p = new Polynomial(1->3.0, 2->4.5, 5->7.4)
        val s = new Polynomial(1->8.0, 3->9.12, 8->4.7)
        println(p.terms(1))
        println(p+s)
    }
}
