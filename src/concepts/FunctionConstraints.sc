/*
 * As an example, this function receives a function and executes it,
 * returning its value.
 * The function has a parameter of type A and the result is of type B,
 * while a function g: C=>D that we pass to it can have different parametric
 * types.
 *
 * We know that for Liskov's substitution principle, we can substitute
 * a parameter that expects an object of some type T with an object that
 * is of type A when A >: T.
 * But functions are complex objects: how do we understand if a function is
 * substitutable with another?
 *
 * For g to substitute f, we need g to be completely compatible with function f,
 * in the sense that both the parameter we pass it and the return value
 * (C and D) must be compatible with f's parameters and return values (A and B).
 *
 * - Firstly, the domain (the type of the parameter) C must be a supertype of A.
 *   This is because when useFunction is executed, we call function g.
 *   g has been built to work on objects of type C, but we call it using an object that
 *   has been cast to A within the function. We need C to be compatible with A, so we
 *   basically need that A is a subtype of C, or that C is a supertype of A.
 *   A <: C.
 *   - Basically this can be explained saying that we need to use an object of type A
 *     (the parameter of the caller function) as an object of type C
 *     (the function we are calling), so A needs to be a subtype of C and this can only
 *     mean that C must be a supertype of A.
 *
 * - Secondly, the codomain D must be a subtype of B. This is because the return type
 *   of the function is cast to B, so D must be compatible with this casting and this is
 *   only achievable if D <: B.
 * - Therefore:
 * - g <: f if:
 *    - C >: A: function useFunction should have contravariance on the domain
 *    - D <: B: function useFunction should have covariance on the codomain
 */
def useFunction[A, B](f: A=>B): Unit = {
  val x:B = f(new A())
  x
}