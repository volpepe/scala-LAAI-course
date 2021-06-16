package utils

/*
 * With traits, it is also easy to extend the language.
 * Let's try and redefine the boolean class in a trait.
 * We'll also define a True object and a False object
 * which will be our entry point for implementing
 * boolean logic.
 */
trait Bool {
    /*
     * ifThenElse is a special function that behaves differently if called on
     * the true object or on the false object.
     * See the implementation in the two objects below:
     *  - if ifThenElse is invoked on true, we return the first parameter (t),
     *    - because if TRUE then t
     *  - if the function is invoked on false, the second (e) is returned
     *    - because if FALSE then ... else e
     */
    def ifThenElse[T](t: => T, e: => T): T // Call by name so we don't need to evaluate the parameters
                                           // before passing them to the function.
    // All other operators are defined on the basis of the ifThenElse function
    // ifThenElse is basically a mechanism to select the first operator when called on true
    // or the second when called on false

    /*
     * AND operator: if the first object is true, an and simply becomes the evaluation
     * of the second operator, while if the first object is false, and simply return the false
     * object.
     */
    def &&(x:Bool): Bool = ifThenElse(x,ff)

    /*
     * OR operator: if || is invoked on true, it evaluates to true
     * while if it is invoked on the false object, it evaluates to the second
     * object.
     */
    def ||(x:Bool): Bool = ifThenElse(tt,x)

    /*
     * NOT operator: if not is invoked on true, it returns false,
     * otherwise it returns true.
     */
    def not: Bool        = ifThenElse(ff, tt) // if true, evaluates to false,
                                              // if false evaluates to true

    /*
     * BOOLEAN EQUALS operator: if invoked on any value, it evaluates
     * to the second one (true case), or its opposite (false case)
     * In fact: if the "this" value is true, then we evaluate x, because if it is
     * true, then the result is "true". If x is false, the result is false because
     * the two values are not equal.
     * If the "this" value is false, then we evaluate x.not, because if x is true,
     * then the two values are not equals and we need to get a "false", while f x is
     * false, then the two elements are equals, so we need to get a true
     */
    def ==(x:Bool): Bool = ifThenElse(x,x.not)

    /*
     * DIFFERENT operator: has the same logic as before:
     * if invoked on true, returns the opposite of x as explained before (x true --> return false)
     * if invoked on false returns x (x true --> return true)
     */
    def !=(x:Bool): Bool = ifThenElse(x.not, x)
}

// If we are not extending any class but just using the trait,
// we use the extend keyword. An alternative would be
// object tt extends Objects with Bool.
object tt extends Bool{
    override def ifThenElse[T](t: => T, e: => T): T = t // true: ifThenElse evaluates to the THEN branch
}
object ff extends Bool{
    override def ifThenElse[T](t: => T, e: => T): T = e // false: ifThenElse evaluates to the ELSE branch
}