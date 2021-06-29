import scala.annotation.tailrec

// Exam sample
def max(x1: Int, x2: Int) = if (x1>x2) x1 else x2

def mystery3tail(l:List[Int]):Int = {
    @tailrec
    def mystery3rec(l:List[Int], partialMax: Int): Int = {
        if (l == Nil) partialMax
        else mystery3rec(l.tail, max(l.head, partialMax))
    }
    mystery3rec(l, l.head)
}

mystery3tail(List(1,22,4,5,6,7,3,4,6,29,200,43,12,52,35,6,32,2,54,7,68,23,42,16,4,76,4344,645,34))

//----------------------------------------------------//

// Test: left/right associativity on maps

val xmap : Map[Int, Int] = Map(23->10, 24->11, 25->12)
Map(25->13) ++ xmap // no update because ++ gives priority to right)
xmap ++ Map(25->13) // update

// Test: foldLeft/foldRight

(List(3,9,12) foldLeft 4)(_ - _) == ((4-3)-9)-12
(List(3,9,12) foldRight 4)(_ - _) == 3-(9-(12-4))

//----------------------------------------------------//

// You should use notions about variance
class Animal (val size: Int) { }
class Cat(val tail_length: Int) extends Animal(2) { }
class Frog(val tongue_length: Int) extends Animal(1) { }

def frog_compare(f1: Frog, f2: Frog, comparer: (Frog, Frog) => Boolean): Frog = if (comparer(f1, f2)) f1 else f2
def animal_comparer(a1: Animal, a2: Animal): Boolean = a1.size > a2.size

frog_compare(new Frog(1), new Frog(2), animal_comparer)
// It works, because the domain is a supertype of (Frog, Frog), while the codomain is the same.

//-----------------------------------------------------//

var X = 0
var Y = 0

def mystery1(a: Int): Int = {
    X += 1
    if(a == 96) 1
    else mystery1(a+1)
}

def mystery2(a: => Int, b: Int, c: Int): Int = {
    Y += 1
    if (b == 35) c
    else mystery2(a + 1, b - 1, c + 1)
}
mystery2(mystery1(94), 35, 72)
println(X)
// X IS 0 BECAUSE mystery2 HAS A CALL BY NAME PARAMETER!!! SO MYSTERY1 IS NEVER CALLED!!

//---------------------------------------------------//

def mystery(l: List[Int]) =
    (l foldRight List[Int]())((x, y) =>
        if (y.isEmpty || y.head > x) List(x) ++ y
        else y ++ List(x))

mystery(List(5,-2,30,7,-1))

//--------------------------------------------------//

// Since we start with a range, the type of R will be the default IndexedSeq (a Vector).
// If we started with a List we would have gotten a List.

val R = (1 until 7) filter (_ % 1 == 0) map (_ + 4)
val R2 = Set(1,2,3,4,5,6,7) filter (_ % 1 == 0 ) map (_+4)

//-------------------------------------------------//

// How flatMap works:
val x = List("Ciao", "come", "va!!!")
x flatMap (p => p.concat("tao"))

// FIRST it applies the map function, THEN flattens the resulting collection

val x = Set("123", "456", "789")
x flatMap (p => (p.toInt+5).toString)


//--------------------------------------------------//

// Does zip work on collections of different lengths?

val a = List(1,2,3,4,5,6,7,8,9)
val b = List(1,2,4)
a zip b

// Yes, it stops early
//-------------------------------------------------//