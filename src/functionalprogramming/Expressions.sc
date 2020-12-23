/******************* Lesson 1 **********************/

/*
* Scala uses a Read-Eval-Print loop like a scripting language,
* but has also a compilation method, Java-like.
*
* Scala is all about evaluating expressions.
* A non-primitive expression is evaluated as follows:
* - take the leftmost operator
* - evaluate its operands
* - apply the operator to the operands
* - names are replaced with their values
* - the evaluation stops when we reach a value
* - type inference is usually deployed
*
* A whole Scala program can be seen as the solving of a complex expression
*/

val radius = 10
val pi = 3.14159

(2*pi)*radius
/*
* Evaluated by replacing names with their right-side assignment
* and then applying left to right operands and operators to the leftmost value
*/

/*
* Difference between def and val:
* - def: the right hand side of the definition is not immediately
*   evaluated.
*   It's actually used to define the body of a function that returns the evaluation
*
* - val: it's a more traditional assignment: the left hand side receives
*   the evaluated left value
*/
//Example
def radius1 = 8+2
val radius2 = 8+2

// So, if we have:
2*pi*radius1
// it will be replaces with 2*pi*(8+2)
2*pi*radius2
// will instead evaluate the expression as 2*pi*10 because the 8+2 has already
// been evaluated.

/*
* Note that with "def", the expression 8+2 is re-evaluated every time we use
* radius1 in our code.
* Instead, we could use "lazy val".
* - the right-hand side is not evaluated at the moment of declaration
* - the first time we need to evaluate the value, the variable is replaced with its
*   value, so that we do not need to re-evaluate it again
* */
lazy val radius3 = 8+2
/*
* We can also use var to have a variable whose value can be changed.
* */
var radius4 = 8+2

2*pi*radius3
2*pi*radius4

/*
* Function arguments are evaluated left to right and the evaluated values
* is replaced in all instances of that variable in the function.
* In scala there is not return: rather we have to think of a function as an evaluation
* of an expression.
* In the arguments of a function there is no type inference, so we need to explicitly
* mention the type of the arguments. Though, there is type inference for the results.
*/
def square(x: Double) = x*x

def sumOfSquares(x:Double, y:Double) =
    square(x)+square(y)

/*
* 2+2 is evaluated as 4, x is evaluated to 3 and y to 4.
* square(x) is evaluated to 9 and after that square(y) is evaluated to
* 16. then the sum is computed and sumOfSquares(3,2+2) is evaluated to 25.
*/
sumOfSquares(3,2+2)

/*
* We can also define recursive functions that never stop the evaluation
* */
def loop: Int = loop + 1

// We get a stack overflow error
// loop

/*
* We can pass arguments to a function in two way:
* Call-by-name vs call-by-value. As long as there are no side effects and
* both evaluation terminate, both reduce to the same final values.
* What changes is that with call-by-value evaluates the value before passing it
* to the function, while call-by-name has a lazy evaluation strategy and the variable
* inside the function is not evaluated until needed
*
* If we call sumoOfSquares(2,2+2) in a call-by-name fashion, y is substituted with
* 2+2, not with 4.
*
* The advantage of CBN is that if a parameter is not needed, it is not evaluated
* The advantage of CBV is that we evaluate every expression only once.
*/

def first(x:Int, y:Int) = x
// The following call will result in a stack overflow when using CBV (because loop will be evaluated
// while it will return just 1 and terminate if we use CBN (because loop is not evaluated and
// will only be evaluated if used.

// first(1,loop)

/*
* In scala, CBV is default. We can use an arrow to indicate CBN.
*/
def first(x:Int, y: => Int) = x
// x will be evaluated as a value, y will be passed as a name
first(1,loop)


/*
* Conditional expressions: similar to Java.
* Remember that there is no such need for a return in Scala.
*/
def and(x:Boolean, y: => Boolean) = {
    // Ignore y if x is false
    if (x) y else false
}

def or(x:Boolean, y: => Boolean) = {
    // Ignore y if x is already true
    if (x) x else y
}

def loopB: Boolean = true && loopB

/*
* The second parameter is passed by name and therefore not evaluated if the first one is false.
*/
and(false, loopB)