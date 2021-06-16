import scala.annotation.tailrec

/****************** Lesson 2 **********************/

/* FUNCTIONS
 * This file contains explanations about:
 * - Blocks
 * - Recursion and Tail Recursion
 * - TODO
 */

// We will make an example using the computation of the square root with Newton's method.

/* We have two functions in a cartesian plane:
* f'(x) = x
* and
* f(x) = r / x
* where r is the value we want to compute the root of.
* sqrt(r) lies exactly at the intersection between these two functions.
* The square root of r lays on the x axis where the two functions intersect.
*
* The square root of r is also called fixpoint for the function f(x) = r/x
* In general, a fixpoint of a function f is the point z where f(z) = z.
* In our case, the point where r/x = x.
*
* This means that we have to compute f'(x) = f(x) = x = r / x
* So, r = x^2 and x = sqrt(r).
*
* We start with an initial estimate of x (eg. 1) and then improve the found value
* and obtain a converging sequence of numbers that get closer and closer to sqrt(r).
*
* - Setting x = Z1 = 1, we get a vertical line that intersect the two functions
* - We take the average of these two points on the y axis and consider it as the x coordinate
*   for Z2.
* - We proceed by computing better and better approximations of the square root.
* - Given Zi, Zi+1 is [f'(Zi)+f(Zi)] / 2
*/

// We can use the concept of blocks to make the computation easier.
// Block: is a mechanism to combine smaller expressions into a larger one.
// A block is a complex expression that is inside curly braces. It can contain
// multiple definitions of other sub-expressions (like other functions, etc.).
// The last element of the block must be an expression and will be the value
// of the block. This value may combine the sub-expressions inside the block.
// Blocks have their own environment of variables: whatever is inside the block
// is only seen within it and names that are outside the block can assume a
// different value only valid within the block.
// This is called lexical or static scoping.

def sqrt(z: Double) = {
    /* This is a block. We can compose multiple expressions here */

    /* This function will be in charge of the recursive build of the solution */
    def sqrIter(guess: Double): Double = {
        // Check if the current solution is good enough: if it is just return the current solution
        if (isGoodEnough(guess)) guess
        // Otherwise, do another iteration with an improved guess
        else sqrIter(improve(guess))
    }

    /*
     * Notice that we can call names defined in this outer sub-block in the inner block
     * like the one right above. This makes sense because of course blocks within blocks
     * are still all part of the same outer block. Something we cannot do is access
     * variables internal to the above block in here, like "guess", but if we define a
     * block within the function sqrIter, it can.
     */

    // Improve our guess by taking an average of the intersections
    // Note that z is known because we are inside the sqrt block.
    def improve(guess: Double) = {
        // (f'(guess) + f(guess)) / 2 = (z/guess + guess)/2
        (( z / guess ) + guess ) / 2
    }

    // This function checks to see if the solution is already good enough
    def isGoodEnough(guess: Double) =
        Math.abs(guess * guess - z) / z < 0.0001

    // Starting point of the block: start with 1 as a guess.
    sqrIter(1.0)
    // This is the de facto value of the entire block,
    // because it activates the whole evaluation process.
}

sqrt(4)
sqrt(1e-6)
// If numbers are too big and the "good" threshold too low
// the computation can go on indefinitely
sqrt(1e60)

/*
* The block contains local names (like the three function definitions)
* but since it is also the body of a function, local parameters of the
* container functions are also available inside those blocks.
* */

/* Given a block, there is always a block that is on the outside
 * of this block. These are called outer blocks.
 */

/* Less trivial blocks: */
// 1)
def x = 10
def h(y: Int) = x
// Every function body is a block. x is not a parameter of the function.
// When evaluating x, its substitution is searched starting from the current block
// and looking in every outer block outside it. If there are many definition, the
// one in the first outer block shadows the others. In this case, x is simply
// evaluated as the name x defined in the line above.
def result = {
    def x = 20
    // Here, this definition of the name x will shadow the definition
    // in the outside block and so x in this block will evaluate to 20.
    x*h(x)
    // The function call h(x) is substituted with h(20).
    // h(20) is substituted with the name x, but NOT THE NAME x AS DEFINED
    // IN THIS BLOCK which is equal to 20. The names that we give to variables
    // in Scala or Java or whatever are actually symbols for helping us.
    // In memory, the function h(y: Int) = x is instructed to return a variable
    // that has been defined and linked by the previous definition of x
    // (def x = 10).
    // In other words, since this block is not an outer block for h(y:Int),
    // the value of x is searched outside and found in def x = 10.
    // x*h(x) will be evaluated as 20*10 = 200
} + x
// Here we are in the outer block. We don't see that in the block above the
// name x has been associated with the value 20, so the value of this x is 10.
result
// The total result is evaluated as 20*10 + 10

/*
* Scala uses a static or lexical scoping:
* If in a block we use a non local identifier,
* it looks in the outer block and then in the outer block
* of the outer block... until it finds a declaration.
*/

/*
 * This can be clearly seen in the above example,
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
 * So, in general, using the same name for variables in sibling blocks is not problematic.
 * Some problems may occur when we deal with inner/outer blocks because the definitions of
 * inner blocks actually shadow the outer ones
 */

//----------------------------------------------------------------------------------------------//

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

@tailrec    // The IDE recognizes this is a tail-recursive eligible function.
def f(x: Int): Int = {
    if (x > 5) 10 else f(x-1)
}

/*
 * A recursive function is tail recursive when recursive call is the last thing executed by the function.
 * This means that we must evaluate the result in a forward fashion, not backwards like in traditional
 * recursion. Things like x*f(x-1) require to remember the original caller for applying x and are a
 * siren of alarm for using tail recursion instead.
 *
 * Most traditional recursions can be transformed in tail-recursion optimized functions,
 * usually by means of parameters that contain partial results.
 * Instead of creating calls and proceeding backwards for their evaluation, we directly
 * apply the calls on a shared partial value which at the end will be the result of the expression.
 * We proceed forward.
 */

/* Another experiment: computation of factorial
* The factorial of 0 is 1, otherwise it is recursively defined as x * fact(x-1) */

def factRec(x: Int): BigInt =
    if (x == 0) 1 else x*factRec(x-1)
// This function is not tail recursive, because the last thing done by the function is
// not the recursive call, but the multiplication between the result
// of the evaluation of the previous call and the value of the current call.

// Luckily, we can easily tail-recursify this function :)
def factTailRec(x:Int, partial:Int): BigInt = {
    if (x == 0) partial
    else factTailRec(x-1, partial*x)
}
// The last thing that is done is the recursive call and the computation of the
// new partial result is incorporated in the parameter passing mechanism.

// Blocks allow for an even smoother way for computing the factorial, without the user
// using a parameter for the partial:
def factTailRec(x: Int) = {
    // Now the recursive call has two parameters. We partially compute the factorial at each step in part.
    // y will be an index that is decreased each time. It's different from x because otherwise
    // we would interfere with the x defined in the outer block
    def fact(y:Int, partial: BigInt): BigInt = {
        if (y==1) partial
        else fact(y-1, partial*y)
    }
    // Call the function as the result of the expression
    fact(x, 1) //1 = neutral element of multiplication.
}

factRec(8000)
factTailRec(8000)

// The normal recursion with 9000 fails, while the tail recursive one deals with it just fine,
// because it does not weigh on the stack: it has the same footprint of a loop call.
//factRic(9000)
factTailRec(9000)

//----------------------------------------------------------------------------------------------//

// Higher order functions are functions that can receive another function as parameter.
// We usually manipulate integers, double, objects, ...
// Now we can manipulate functions. We can compose different functions in order to create complex
// expressions.

// An example: we want to apply a function f on the values a to b and sum together the results.
// Three parameters: a, b, and the function f.

// The type A => B indicates the parameter types of the function (input and output).
// Sum is therefore a higher order function as it uses a function as parameter

// This function is the (non tail recursive) implementation for a sum from a to b of
// f(i), however f is defined.
// \sum_{i=a}^b f(i)
def sum(f: Int => Int, a: Int, b: Int): Int =
    if (a>b) 0 else f(a) + sum(f, a+1, b)

// If f simply evaluates to i, we get a sum of numbers from 1 to 100.
sum(x => x, 1, 100)

// We can pass functions through a lambda notation
// x => x*x*x
// These are also called anonymous functions.

// Another important thing is that we can instantiate parameters on multiple levels
// leaving some parameters not instantiated. These will be instantiated in the future.
// This can be easily done by RETURNING A FUNCTION.
// An alternative definition for the above function is:
def sum(f: Int => Int): (Int, Int) => Int = {
    // The above notation means: function sum takes a function f from Int to Int
    // and evaluates to a different function that takes two Int parameters and
    // evaluates to a simple Int.

    // We define the "different (Int, Int) => Int function" in this block
    def sumF(a: Int, b:Int): Int =
        if (a>b) 0 else f(a) + sumF(a+1, b)

    // When we call sum(f), the function evaluates to sumF, so the function sumF
    // gets substituted to the call.
    sumF
}
// The returned type is (Int, Int) => Int
// This is an higher order function that receives a function and returns another one.

// For example:
def sumId = sum(x=>x)
// returns a function whose a and b parameters are to be instantiated
sumId(1,100)
// which we call here, instantiating a and b to 1 and 100.
// This means that we can create "function generators" that we can re-utilize
// when needed. In this case, we just created ourselves a sum(range(x)) function!

// (in python, the partial package provides a similar behaviour)

// The following is an equivalent ad-hoc notation
// Notice the double parameters notation
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
 * the function to apply on the operands.
 */
def mapReduce(f: Int => Double,                      //f is the function to compute the reduction over
              combine: (Double, Double) => Double,   //combine is a function that tells us how to combine subsequent values
              zero: Double)(a:Int, b:Int) : Double = //zero is the neutral element of the combine operation
    if (a>b) zero else
        combine(f(a), mapReduce(f,combine,zero)(a+1, b))

// For example, if combine is the multiplication operator,
// f(a) * mapReduce(f,combine,zero)(a+1,b) is computed

// We can redefine the factorial function as above
def fact2(n:Int) = mapReduce(x=>x, _*_,1)(1,n)
// The _*_ is a special notation that is equivalent to:
// (x,y) => x*y. In this case we leave the variable names undefined.
fact2(5)

def sumId2(n:Int) = mapReduce(x=>x, _+_, 0)(1, n)
sumId2(5)