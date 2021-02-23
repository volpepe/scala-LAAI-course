/*********** LESSON 3 ****************/

/**
Fixed point computation.
A fixed point for a function f() is a value x such that f(x) = x.
It's a generalization of the Newton square root method of the previous lesson.
For some class of functions, a fixed point can be computed starting from an initial guess
z and computing f(f(f(...(z)))...)
This can be done passing f as a parameter of a function in Scala
 */

def fixPoint(f: Double => Double) = {
  val tolerance = 0.00001
  def isCloseEnough(x: Double, y:Double) =
  Math.abs((x-y)/x) < tolerance
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(1.0) // Initial guess
}

/**
We have seen that the square root can be seen as a fixed point.
We can use this implementation with the function
sqrt(x) = y --> y*y = x --> y = x/y
 */
def sqrt(x:Double) = fixPoint(y => x/y)

// But calling sqrt(2), the computation diverges dramatically!
/**
This happens because:
- f(1) = 2
- f(2) = 1
- f(1) = 2
...
it goes on forever.

We can instead average successive values.
*/

// This function receives a function and computes a new function which averages
// the two
def average(f:Double=>Double, g:Double=>Double) = {
  (x:Double) => ((f(x)+g(x)) / 2)
}

// Now we can compute the average function between these two
def sqrt(x:Double) = {
  fixPoint(average(y => y, y => x/y))
}

// Therefore computing the square root.
sqrt(2)