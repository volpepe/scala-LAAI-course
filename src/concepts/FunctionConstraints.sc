/**
 * This function receives a function and executes it, returning its value.
 * The function has a parameter of type A and the result is of type B.
 *
 * What are the characteristics of a function g:C=>D that is passed to this function?
 * - The function g should be a subtype of function f, to be completely compatible with it.
 * - Firstly, the domain C must be a supertype of A. This is because when
 *    useFunction is executed, we call g. g has been built to work on objects of type C.
 *    Therefore, we need C to be compatible with A. This intuition is formalized in the
 *    supertyping relation between C and A, that is: A < C.
 * - Secondly, the codomain D must be a subtype of B. This is because the return type
 *    of the function is cast to B, so D must be compatible with this casting and this is
 *    only achievable if D < B.
 * - Therefore:
 * - C > A: function useFunction should have contravariance on the domain
 * - D < B: function useFunction should have covariance on the codomain
 */
def useFunction[A, B](f: A=>B): Unit = {
  val x:B = f(new A())
  x
}