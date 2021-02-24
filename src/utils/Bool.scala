package utils

/**
 * With traits, it is also easy to extend the language.
 * Let's try and redefine the boolean class
 */
trait Bool {
    /**
     * ifThenElse is a special function that behaves differently if called on
     * the true object or on the false object.
     * See the implementation in the two objects below:
     *  - if ifThenElse is invoked on true, we return the first parameter (t),
     *  - if the function is invoked on false, the second (e) is returned
     */
    def ifThenElse[T](t: => T, e: => T): T //call by name
    // All other operators are defined on the basis of the ifThenElse function

    /**
     * and: if && is invoked on true, the other value x is returned,
     * while if invoked on false, it returns the object false.
     */
    def &&(x:Bool) = ifThenElse(x,ff)

    /**
     * or: if || is invoked on true, it returns the object true,
     * while if invoked on false it returns the other value x
     */
    def ||(x:Bool) = ifThenElse(tt,x)

    /**
     * not: if not is invoked on true, it returns false,
     * otherwise it returns true.
     */
    def not        = ifThenElse(ff, tt) //if true, return false else true

    /**
     * equals: if invoked on true, returns the other value x (true if equals,
     * false if not equal), if invoked on false, it returns the opposite og the other value
     * (true if x is false, false if x is true)
     */
    def ==(x:Bool) = ifThenElse(x,x.not)

    /**
     * different: if invoked on true, returns the opposite of x as explained before
     * (x true --> return false), if invoked on false returns x (x true --> return true)
     */
    def !=(x:Bool) = ifThenElse(x.not, x)
}

//If we are not extending any class but just using the trait,
//we use the extend keyword
object tt extends Bool{
    override def ifThenElse[T](t: => T, e: => T): T = t
}
object ff extends Bool{
    override def ifThenElse[T](t: => T, e: => T): T = e
}