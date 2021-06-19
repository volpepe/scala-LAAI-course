package concepts
import scala.util.Random

/*
 * Pattern matching is a mechanism to check a value against a pattern. It's a
 * more powerful version of Java's switch statement and can be used in place of
 * if/then/else statements as well.
 */
object PatternMatching {
    /*
     * Pattern matching can also be used to decompose objects. It allows for a more
     * expressive evaluation.
     */
    def main(args: Array[String]): Unit = {
        val x: Int = Random.nextInt(3)
        println(matchTest(x))
        println(Prod(Sum(Number(5), Number(7)), Number(4)).show)
    }
    // Pattern matching is introduced by the match keyword that opens a block
    // The result of the pattern matching is substituted as the RHS of the expression.
    def matchTest(x:Int):String = x match {
        case 0 => "zero"
        case 1 => "one"
        case _ => "other" // case _ is the default case
    }

    /*
     * Case classes are special kinds of classes. They produce immutable objects
     * which are compared by structure, not by reference and require and their parameters
     * are the ways we can identify them. They are particularly useful for pattern matching
     * because we can match them on the type or on their content.
     */
    case class Number(n: Int) extends Expr
    case class Sum(e1:Expr, e2:Expr) extends Expr
    case class Prod(e1:Expr, e2:Expr) extends Expr

    trait Expr {
        def eval: Int = this match {
            case Number(n) => n
            case Sum(e1,e2) => e1.eval+e2.eval  // Recursive evaluation until an Int is evaluated
            case Prod(e1,e2) => e1.eval*e2.eval
        }
        def show: String = this match {
            case Number(n) => n.toString
            case Sum(e1, e2) => e1.show + "+" + e2.show
            case Prod(e1, e2) => e1.parenthesisCheck + "*" + e2.parenthesisCheck
        }
        def parenthesisCheck: String = this match {
            case Sum(_,_) => "(" + show + ")"
            case _ => show
        }
    }
}
