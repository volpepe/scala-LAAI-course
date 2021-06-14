// square root with Newton's method
def sqrt(r: Double) = {

  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  def improve(guess: Double) =
    (guess + r / guess) / 2

  def isGoodEnough(guess: Double) =
//    Math.abs(guess * guess - r) < 0.000001
  Math.abs(guess * guess - r)/r  < 0.0001

  sqrtIter(1.0)
}

sqrt(2)

sqrt(1e-6)

sqrt(1e60)

//static/lexical scoping
val x = 10
def h(y: Int) = {x}
def result = {
  val x = 20
  x * h(x)
} + x
result

// static/lexical scoping
def f(z: => Int) = {
  def x=20
  z+10
}
def g() = {
  def x=30
  f(x+10)
}
g()


//recursive implementation of factorial
def factRic(x: Int): BigInt =
  if (x==0) 1 else x*(factRic(x-1))

//tail recursive implementation of factorial
def factTailRec(x: Int) = {
  def fact(part: BigInt, y: Int): BigInt = {
    if (y==1) part
    else fact(part * y, y-1)
  }
  fact(1,x)
}

factTailRec(9000)
//factRic(9000)



def sum(f:Int => Double, a: Int, b:Int): Double =
  if (a>b) 0 else f(a)+sum(f, a+1, b)

sum(x=>x, 1,100)

def sumWithF(f:Int => Double): (Int, Int) => Double = {
  def sumF(a: Int, b: Int): Double =
    if (a>b) 0 else f(a) + sumF(a+1, b)

  sumF
}

def sumId=sumWithF(x=>x)

sumId(1,100)

def sum2(f:Int => Double)(a: Int, b:Int): Double =
  if (a>b) 0 else f(a)+sum2(f)(a+1, b)

def sumId2=sum2(x=>x)_

sumId2(1,100)

//check foldLeft on online documentation

def product(f:Int => Double)(a: Int, b: Int): Double =
  if (a>b) 1 else f(a) * product(f)(a+1, b)

def fact(n: Int) = product(x=>x)(1,n)

fact(5)

def mapReduce(f:Int => Double,
              operator:(Double, Double) => Double,
              zero: Double)
             (a: Int, b: Int): Double =
  if (a>b) zero else
    operator(f(a), mapReduce(f,operator,zero)(a+1, b))

def fact2(n: Int) = mapReduce(x=>x,(x,y)=>x*y,1)(1,n)

//_*_  === (x,y)=>x*y

fact2(5)

def fixPoint(f: Double => Double) = {
  val tolerance = 0.00001
  def isCloseEnough(x: Double, y: Double) =
    Math.abs((x - y) / x) < tolerance
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(1.0)
}

def sqrt2(r: Double) = fixPoint(x => r / x)

//sqrt2(2)

def average(f:Double=>Double, g:Double=>Double) =
  (x:Double) => (f(x)+g(x))/2

def sqrt3(r: Double) = fixPoint(average(x => x, x => r/x))

sqrt3(2)
