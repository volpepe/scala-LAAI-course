val stringList = List("Bologna", "Modena", "Reggio Emilia", "Piacenza")

stringList map (_.length) //List[Int]
stringList filter (_.length < 8) //List[String]

def pack[T](xs: List[T]): List[List[T]] =
  xs match {
    case Nil => Nil
    case _ => (xs span (_ == xs.head)) match {
      case (l, r) => l :: pack(r)
    }
  }

List("a","a","a","b","c","c","a") span (_=="a")

pack(List("a","a","a","b","c","c","a"))

def encode[T] (xs:List[T]) =
  pack(xs) map (l => (l.head, l.length))

encode(List("a","a","a","b","c","c","a"))

encode(List(7,7,7,6,8,8,7))

stringList reduceLeft ((x: String,y: String) =>
  if (x.length > y.length) x else y)

(stringList foldLeft 0) ((x: Int, y: String) =>
  if (x > y.length) x else y.length)

def sum(xs: List[Int]) =
  (0 :: xs) reduceLeft ((x, y) => x + y)

def product(xs: List[Int]) =
  (1 :: xs) reduceLeft ((x, y) => x * y)



/*
sum(myL)

product(myL)

def sumf(xs: List[Int]) = (xs foldRight 0) (_ + _)

def productf(xs: List[Int]) = (xs foldRight 1) (_ * _)

sumf(myL)

productf(myL)
*/

// xs = x1 :: (x2 :: (.....   :: (xn :: Nil)) .. )
// ys = y1 :: (y2 :: (.....   :: (ym :: Nil)) .. )

def myL = List(27,31,45,18)

// concat(xs, ys) = x1 :: (x2 :: (.....  (x{n-1} :: (xn :: ys))) .. )

// (  ((xs ? y1) ? y2) .. ? yn )

def concat[T](xs: List[T], ys: List[T]): List[T] =
//  (xs foldRight ys)(_ :: _)
  (ys foldLeft xs)(_ :+ _)

concat(myL, myL)

// xs = x1 :: (x2 :: (.....   :: (xn :: Nil)) .. )
// reverse(xs) = xn :: (x{n-1} :: (.....  x2 :: (x1 :: Nil)) .. )

def reverse[T](xs: List[T]): List[T] =
  (xs foldLeft  List[T]()) ( (l: List[T], x: T) => x :: l)

reverse(myL)

// xs = x1 :: (x2 :: (.....   :: (xn :: Nil)) .. )
// mapFun(xs,f) = f(x1) :: (f(x2) :: (.....  :: (f(xn) :: Nil)) .. )

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())((x:T, l:List[U]) => f(x) :: l)

mapFun(myL,((x:Int)=>"hello world!"))

