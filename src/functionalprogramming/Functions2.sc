import scala.annotation.tailrec

/** ********* LESSON 3 *************** */

/*
 * Fixed point computation.
 * A fixed point for a function f() is a value x such that f(x) = x.
 * It's a generalization of the Newton square root method of the previous lesson.
 * For some class of functions, a fixed point can be computed starting from an initial guess
 * z and computing f(f(f(...(z)))...)
 * This can be done passing f as a parameter of a function in Scala
 */

/*
 * Function fixPoint receives a function f and tries to compute iteratively
 * its fixpoint.
 */
def fixPoint(f: Double => Double) = {
    // Since it is an iterative process, we define a tolerance value:
    // if the relative difference between the previous value and the
    // new one is inferior than the tolerance, we can say we have a good
    // enough approximation
    val tolerance = 0.00001
    def isCloseEnough(x: Double, y: Double) =
        Math.abs((x - y) / x) < tolerance

    @tailrec
    def iterate(guess: Double): Double = {
        // Iterate receives the initial (or previous) value
        // and computes the new one. If it's good enough, it
        // evaluates to that value.

        // Compute the next value by applying the function on the guess.
        // Basically, the previous y is used as an x.
        val next = f(guess)
        // If the approximation is good enough, let the function iterate
        // evaluate to it.
        if (isCloseEnough(guess, next)) next
        // Otherwise, pass the current y as x for the next round
        else iterate(next)
    }

    iterate(1.0) // Initial guess
}

/*
 * We have seen that the square root can be seen as a fixed point.
 * We can use this implementation with the function
 * y = sqrt(x) --> y*y = x --> y = x/y
 *
 * We had seen that starting from the initial guess y, Newton's
 * method computes the fixed point of the hyperbole r/x where r
 * is the number we want to compute the square root of and x is
 * the previous guess.
 */
def sqrt(x: Double) = fixPoint(y => x / y)

// But calling sqrt(2), the computation diverges dramatically!
/*
 * This happens because:
 * - f(1) = 2 (2/1=2)
 * - f(2) = 1 (2/2=1)
 * - f(1) = 2
 * ...
 * it goes on forever.
 *
 * We can instead average successive values.
 */

// This function receives two functions and computes a new function which averages
// the two given x, a value
def average(f: Double => Double, g: Double => Double) = {
    (x: Double) => (f(x) + g(x)) / 2
}

// By construction, y represents the current guess at all iterations
// while x represents the previous guess (or the initial one)

// Now we can compute the average function between these two
def sqrt(x: Double) = {
    /*
     * The function that we want to compute the fixpoint of is not
     * anymore y = x/y, but the function y = (y + x/y)/2, that is the average
     * between the two functions y = y and y = x/y.
     */
    fixPoint(average(y => y, y => x / y))
}

// Therefore computing the square root.
sqrt(2)