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
*   It's actually used to define the body of a function,
*   so you can see it as a name definition that sets up a callback
*   for its evaluation when it will be needed.
*
* - val: it's a more traditional assignment: the left hand side receives
*   the evaluated left value, so the substitution is instantaneous
*/
//Example
def radius1 = 8+2 // radius1 is a name that is evaluated to "8+2"
val radius2 = 8+2      // radius2 is the symbolic name for number 10

// So, if we have:
2*pi*radius1
// it will be replaces with 2*pi*(8+2)
2*pi*radius2
// will instead evaluate the expression as 2*pi*10 because the 8+2 has already
// been evaluated.

/* In general, if we need to use a value multiple times and we don't
* explicitly want it to be evaluated over and over and over again,
* we might feel more comfortable using val.
* With "def", the expression 8+2 is re-evaluated every time we use
* radius1 in our code.
*
* We could also use "lazy val".
* - the right-hand side is not evaluated at the moment of declaration
* - the first time we need to evaluate the value,
*   the variable is replaced with its value, so that we do not
*   need to re-evaluate it again
* */
lazy val radius3 = 8+2
// Next time we will use radius3 it will be substituted (8+2), then evaluated
// (10) and radius3 will keep value 10.
/*
 * Note that neither val, nor lazy val or def are values that can directly be changed.
 * Once a name is defined with these keywords, it remains the same.
 * If we need a variable that can change we need to use the var keyboard.
 */
var radius4 = 8+2

2*pi*radius3
2*pi*radius4

//radius2 = radius2 + 5 <-- IDE recognizes this as an error
radius4 += 2 // <-- this is okay!!

/*
* Function arguments are evaluated left to right and the evaluated values
* is replaced in all instances of that variable in the function.
*
* In Scala there is not a return keyword: we have to think of executing a function
* as evaluating an expression to obtain a value that is used as RHS for a name.
* The fact that functions are expressions like any other RHS is the key point of
* functional programming, because it means that we can effectively pass a function
* as argument to other functions or create a chain of expressions that gets
* computed sequentially.
*
* In the arguments of a function there is no type inference, so we need to explicitly
* mention the type of the arguments. This is done because these arguments have a
* syntactic value and need to be filled with semantics.
* Also, there is type inference for the results, so we need to provide the types that
* are involved in the expression.
*/
def square(x: Double) = x*x

def sumOfSquares(x:Double, y:Double) =
    square(x)+square(y)

/*
 * Order of execution:
 * 2+2 is evaluated as 4, x is evaluated to 3 and y to 4.
 * + is the first operand left-to-right. We need to evaluate square(3) and
 * square(4).
 * square(x) is evaluated to 9 and after that square(y) is evaluated to
 * 16. then the sum is computed and sumOfSquares(3,2+2) is evaluated to 25.
 */
sumOfSquares(3,2+2)

/*
 * We can also define recursive functions that make the evaluation run
 * for an infinite amount of time.
 */
def loop: Int = loop + 1 // <- with def the expression is evaluated
                         // only when called, so we get no errors

// loop <- We get a stack overflow error

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
// first(1,loop) // <-- The following call will result in a stack overflow when
                 // using CBV (because loop will be evaluated while it will
                 // return just 1 and terminate if we use CBN (because loop
                 // is not evaluated and will only be evaluated if used.
/*
 * In scala, CBV is default. We can use an arrow to indicate CBN.
 */
def first(x:Int, y: => Int) = x
// x will be evaluated as a value, y will be passed as a name, so it won't be
// evaluated.
first(1,loop)

/*
 * In a sense, val is similar to CBN because we evaluate the expression on the RHS
 * before "finalizing" the name, while def is similar to CBV because the expression
 * is only evaluated when needed (called)
 */
val valVar: Int = valVar + 1 // Ints default value is 0, so valVar is substituted by 0
                             // and we sum 1 to it, thus obtaining 1.
def defVar: Int = defVar + 1 // Here, there is no default value: defVar IS defVar + 1,
                             // so this will cause am infinite loop of evaluations.
valVar
//defVar <-- This will cause a stack overflow error.

/*
* Conditional expressions: similar to Java.
* Remember that there is no such need for a return in Scala.
*/

/*
 * We use CBN for the second parameter so that we don't evaluate it if it is not needed
 */
def and(x:Boolean, y: => Boolean) = {
    // Ignore y if x is false
    if (x) y else false
}

/*
 * Same for the or definition
 */
def or(x:Boolean, y: => Boolean) = {
    // Ignore y if x is already true
    if (x) x else y
}

/*
 * We define this name as a loop
 */
def loopBool: Boolean = true && loopBool

/*
* The second parameter is passed by name and therefore not evaluated
* if the first one is false.
*/
and(false, loopBool)

/*
 * The same is true for the or if the first parameter is true
 */
or(true, loopBool)
