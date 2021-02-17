/****************** Lesson 2 **********************/

// Functions
// Block: is a mechanism to combine smaller expressions into a larger one.
// An example is the computation of the square root with Newton's method.

/* We have two functions in a cartesian plane:
* f'(x) = x
* and
* f(x) = r / x
* where r is the value we want to compute the root of.
* The square root of r lays on the x axis where the two functions intersect.
*
* This means that we have to compute f'(x) = f(x) = x = r / x
* So, r = x^2 and x = sqrt(r).
*
* We start with an initial estimate of x (eg. 1) and then improve the found value
* and obtain a converging sequence of numbers that get closer and closer to sqrt(r).
*
* - Setting x = Z1 = 1, we get a vertical line that intersect the two functions
* - We take the average of these two points for Z2 and go on.
* - Given Zi, Zi+1 is [f'(Zi)+f(Zi)] / 2
 */

// A block is a complex expression that is inside curly braces. It can contain
// multiple definitions of other sub-expressions (like other functions, etc.)
// and has a return value that can combine those sub-expressions

def sqrt(z: Double) = {
    /* This is a block. We can compose multiple expressions here */

    /* This function will be in charge of the recursive build of the solution */
    def sqrIter(guess: Double): Double = {
        // Check if the current solution is good enough: if it is just return the current solution
        if (isGoodEnough(guess)) guess
        // Otherwise, do another iteration with an improved guess
        else sqrIter(improve(guess))
    }

    // Improve our guess by taking an average of the intersections
    // Note that z is known because we are inside the sqrt block.
    def improve(guess: Double) = {
        // (f'(guess) + f(guess)) / 2
        ( guess + z / guess ) / 2
    }

    // This functions checks to see if the solution is already good enough
    def isGoodEnough(guess: Double) =
        Math.abs(guess * guess - z) / z < 0.0001

    // Starting point of the block: start with 1 as a guess.
    sqrIter(1.0)

}


sqrt(4)
sqrt(1e-6)
// If numbers are too big and the "good" threshold too low
// the computation can go on indefinitely
sqrt(1e60)

/*
* The block contains local identifiers (like the three function definitions)
* but since it is also the body of a function, local parameters of the
* container functions are also available inside those identifiers.
* */

/* Also, given a block, there is also a block that is on the outside
* of this block. These are called outer blocks. */

/* Less trivial blocks: */
// 1)
def x = 10
def h(y: Int) = x
def result = {
    def x = 20
    // here x will be 20
    // also, when we pass x to the h function, it will be evaluated as 20
    // but since h(y) returns x and x is defined outside of the function,
    // h(x) here will be evaluated as 10.
    x*h(x)
} + x
// here, outside from the block, the value of x is 10
result
// so, the total result is evaluated as 20*10 + 10

/*
* Scala uses a static or lexical scoping:
* If in a block we use a non local identifier,
* it looks in the outer block and then in the outer block
* of the outer block... until it finds a declaration.
*/

/*This can be clearly seen in the above example,
* where the value of x in h(y) is inferred from the
* outer block rather than the definition in the body of the result function.
* In fact, result is not an outer block for h(y)! */

//2)
def f(z: => Int) = {
    def x = 20
    z + 10
}
def g() = {
    def x = 30
    f(x+10)
}
g()

/*
* In this case, we have that f receives the expression x + 10 as an argument but does
* not evaluate it until required from the z + 10 statement.
* When unpacking z, Scala finds out that x has been defined in the same block (def x = 20).
* BUT! x was defined as 30 in the starting block. How does Scala act in this case?
* Scala acts by assigning the value 30 to x, kind of breaking the static scoping concept
* by looking in a completely different block instead of the one it's currently executing
* from.
* This happens because when z is evaluated, the variable x is linked to the definition x = 30.
* z is a package and when we open the package we consider the variables from the block it was
* constructed.
* */

/*
* In order to program Newton's method we have used recursion, which can be less efficient
* than traditional methods because of the overhead of local variables that are instantiated in
* memory at every iteration, filling up the stack way faster.
* In general, any function call has some kind of overhead which leads to a less efficient program
* than a pure algorithmic implementation.
* BUT!
* Scala optimizes the execution in case of TAIL RECURSION, which is a special kind of recursion where
* variables that are returned from a recursive function call are not modified before being passed
* to the following level.
* This allows to avoid the memory overhead, because we can just re-use the same memory location for
* the variable as it won't change.
* */

def f(x: Int): Int = {
    if (x > 5) 10 else f(x-1)
}
// The returned value f(x-1) is never modified, so Scala can optimize this recursive call.
// If we had ... else 1 + f(x-1) this could have not been possible.

/* Another experiment: computation of factorial
* The factorial of 0 is 1, otherwise it is recursively defined as x * fact(x-1) */

def factRic(x: Int): BigInt =
    if (x == 0) 1 else x*factRic(x-1)

// This function is not tail recursive, because the result of the recursive call is modified.
// We can easily tail-recursify this function, though :)
def factTailRec(x: Int) = {
    // Now the recursive call has two parameters. We partially compute the factorial at each step in part.
    // y will be an index that is decreased each time.
    def fact(part: BigInt, y:Int): BigInt = {
        if (y==1) part
        else fact(part*y, y-1)
    }
    fact(1,x) //1 = neutral element of multiplication.
}

factRic(8000)
factTailRec(8000)

// The normal recursion with 9000 fails, while the tail recursive one deals with it just fine.
//factRic(9000)
factTailRec(9000)

/**************************************/

// Higher order functions are functions that can receive another function as parameter.
// We usually manipulate integers, double, objects, ...
// Now we can manipulate functions. We can compose different functions in order to create complex
// expressions.

// An example: we want to apply a function f on the values a to b and sum together the results.
// Three parameters: a, b, and the function f.

// The type A => B indicates the parameter types of the function (input and output).
// Sum is therefore a higher order function as it uses a function as parameter
def sum(f: Int => Int, a: Int, b: Int): Int =
    if (a>b) 0 else f(a) + sum(f, a+1, b)

sum(x => x,1,100)

// We can pass functions through a lambda notation
// x => x*x*x
// These are also called anonymous functions.

// Another important thing is that we can instantiate parameters on multiple levels
// leaving some parameters not instantiated. These will be instantiated in the future.
// This can be easily done by RETURNING A FUNCTION.
// An alternative definition for the above function is:
def sum(f:Int => Int): (Int, Int) => Int = {
    // The above notation means: function sum takes a function f from Int to Int
    // and returns a function that takes two Int parameters and returns an Int.
    def sumF(a: Int, b:Int): Int =
        if (a>b) 0 else f(a) + sumF(a+1,b)

    sumF
}
// The returned type is (Int, Int) => Int
// This is an higher order function that receives a function and returns another one.

// For example:
def sumId = sum(x=>x)
// returns a function whose a and b parameters are to be instantiated
sumId(1,100)
// which we call here

// In python, the partial package provides a similar behaviour.

// The following is an equivalent ad-hoc notation
def sum(f:Int => Int)(a:Int, b:Int): Int =
    if (a>b) 0 else f(a) + sum(f)(a+1,b)

// Note, when this notation is used we need to add an underscore
// or (_,_) in this case to account for unknown parameters
def sumId = sum(x=>x)_
def sumId = sum(x=>x)(_,_)
sumId(1,100)

// Example for a factorial product
def product(f:Int => Double)(a:Int, b:Int): Double =
    if (a>b) 1 else f(a) * product(f)(a+1,b)

// We can instantiate a and b as well in a single call
def fact(n:Int) = product(x=>x)(1,n)
// fact will be a function that uses the product function by instantiating it
// with the same parameter n that is passed to it.
fact(5)

/* We can abstract a general operation for both sum and product.
* We will do so by asking as a parameter the neutral element as well as
* the function to apply on the operands. */
def mapReduce(f: Int => Double,
              combine: (Double, Double) => Double,
              zero: Double)(a:Int, b:Int) : Double =
    if (a>b) zero else
        combine(f(a), mapReduce(f,combine,zero)(a+1, b))

// We can redefine the factorial function as above
def fact2(n:Int) = mapReduce(x=>x, _*_,1)(1,n)
// The _*_ is a special notation that is equivalent to:
// (x,v) => x*y. In this case we leave the variable names undefined.
fact2(5)

def sumId2(n:Int) = mapReduce(x=>x, _+_, 0)(1, n)
sumId2(5)