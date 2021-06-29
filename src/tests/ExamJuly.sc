/**
 * 1)
 *  Consider the following function:

    def mistery(l:List[Int],f:Int=>Boolean)=
      ((l filter f) foldLeft 0) (_ + _)

    and the following expression:

    mistery(List(5,2,7,9,21,15), ( (x) => (5<x) && (x<16) ))

    What is the result of the evaluation of the above expression?
    Justify your answer.
 */

def mistery(l:List[Int],f:Int=>Boolean)=
    ((l filter f) foldLeft 0) (_ + _)

mistery(List(5,2,7,9,21,15), x => (5<x) && (x<16) )

/**
 * 2)
 * What does the following function compute?

    def mistery2(l:List[Int]) = {
      def f(l:List[Int]):List[Int]=
        if (l.isEmpty) List(0)
        else (l takeWhile (_ == l.head)).length ::
          f(l dropWhile (_ == l.head))

      f(l).max
    }

    Justify your answer.
 */

def mistery2(l:List[Int]) = {
    def f(l:List[Int]):List[Int]=
        if (l.isEmpty) List(0)
        else (l takeWhile (_ == l.head)).length ::
            f(l dropWhile (_ == l.head))

    f(l).max
}

mistery2(List(3,3,3,3,3,5,5,5,5,5,5,5,100,28,234,43,43,43,43,43,43,43,43,7))

/**
 * 3)
 *
 * Are the following declarations correct or not?

    val x1: List[String] = List[Nothing]()
    val x2: Array[String] = Array[Nothing]()

    Justify your answer.
 */

val x1: List[String] = List[Nothing]() // <-- legal
//val x2: Array[String] = Array[Nothing]() // <-- illegal because arrays are
                                           //     mutable, so not covariant


/**
 * 4)
 *
 * Consider the following excerpt of code:

    class A (val x:Int) {
      def get = x+1
    }

    class B (k:Int) extends A(k) {
      override def get = x-1
    }

    val z:A = new B(5)

    println (z.get)

    What is printed? Justify your answer.
 */



class A (val x:Int) {
    def get = x+1
}

class B (k:Int) extends A(k) {
    override def get = x-1
}

val z:A = new B(5)

println(z.get)  //it should print 4 because the object is of static type
                // A but of dynamic type B

/**
 * 5)
 * Describe by words and by means of an example the groupBy higher-order function.
 */
1 to 20 groupBy (_%5)

/*
 * NOTE: GROUPBY ALWAYS RETURNS A MAP OF TYPE [FunctionResult, Vector or List[SeqElement]]
 * because it groups together elements with the function's value as key into Vectors
 */