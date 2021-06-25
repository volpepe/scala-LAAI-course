/**
 * 1)
 */

class A(val x:Int) {
  def m1 = x+1
}

class B(k:Int, val z:Int) extends A(k) {
  override def m1 = z-1
  def m2 = m1
}

val z: A = new B(5,3)
//println(z.m2) <- error

/**
 * 2)
 */
(List(4,5,6) foldRight 0)(_+_)

/**
 * 3)
 */
def mystery2(l:List[String]) =
  (0::(l map (_.length))).reduce(_+_)

def mystery2Test(l:List[String]):Int=
  (l.flatten map (x=>1)).sum

mystery2(List("Hi","my","nama","Jeff")) == mystery2Test(List("Hi","my","nama","Jeff"))


/**
 * 4)
 */
for {
  i <- Set(2,3)
  j <- 1 to i
  k <- i to 2 by -1
} yield (j,k)

for {
  i <- List(2,3)
  j <- 1 to i
  k <- i to 2 by -1
} yield (j,k)

for {
  i <- 2 to 3
  j <- 1 to i
  k <- i to 2 by -1
} yield (j,k)

/**
 * 5)
 */
def mystery(n:Int) =
  List(2,3,5,7) takeWhile (n%_==0)

mystery(42)