//package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{
  def sum(ints: List[Int]): Int = ints match{
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match{
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
 
  val x = List(1,2,3,4,5) match{
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
   }

  def tail[A](as: List[A]): List[A] = as match{
    case Nil => Nil
    case Cons(a, as) => as
  }

  def setHead[A](l: List[A], a:A): List[A] = l match{
    case Nil => Nil
    case Cons(_, xs) => Cons(a, xs)
  }

  def drop[A](l: List[A], n:Int): List[A] = l match{
    case Nil => Nil
    case Cons(_, as) => 
      if(n==1) as
      else drop(as,n-1)
  }
/*
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match{
      case Nil => Nil
      case Cons(a, as) => 
        if(f(a) == true) as
        else dropWhile(as, f)
  }
  def dropWhileAnswer[A](l: List[A], f: A => Boolean): List[A] = 
*/
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f) 
      case _ => l
   }

  def dropWhileCurry[A](as: List[A])(f: A => Boolean): List[A] = 
    as match {
      case Cons(h,t) if f(h) => dropWhileCurry(t)(f) 
      case _ => as 
   }

   def init[A](l:List[A]): List[A] = l match{
    case Nil => Nil 
    case Cons(x,Nil) => Nil 
    case Cons(x,xs) => Cons(x, init(xs))
   }

   def foldRight[A, B](as: List[A], z:B)(f: (A, B) => B):B = 
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs,z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]):Int =
    foldRight(as, 0)((_,y)  => 1 + y )

  def myfoldLeft[A, B](as: List[A], z:B)(f: (B, A) => B):B =
    as match{
      case Nil => z
      case Cons(x, xs) => f(foldLeft(xs,z)(f), x)
    }

  def foldLeft[A, B](as: List[A], z:B)(f: (B, A) => B):B =
    as match{
      case Nil => z
      case Cons(h, t) => foldLeft(t,f(z,h))(f)
    }

  def sum3(ns: List[Int]) = 
    foldLeft(ns,0)((x,y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length3[A](as: List[A]):Int =
    foldLeft(as, 0)((y,_)  => 1 + y )

  def reverse[A](as: List[A]):List[A] =
    //foldLeft(as, Nil)( (x, xs) => Cons(x, xs) )
    foldLeft(as, List[A]())( (acc, h) => Cons(h, acc) )

/*
  def foldLeft2[A, B](as: List[A], z:B)(f: (B, A) => B):B =
    foldRight(as, z)( )

  def reverse2[A](as: List[A]):List[A] =
    foldLeft2(as, List[A]())( (acc, h) => Cons(h, acc) )
*/

  //def foldRight[A, B](as: List[A], z:B)(f: (A, B) => B):B = 

  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)( (h,acc) => Cons(h,acc) )

/*
  def addOne[A](as:List[Int]):List[Int] = 
    as match{
      case Nil => Nil
      case Cons(x,xs) => Cons(x + 1, addOne(xs))
    }
*/
  def addOne(as:List[Int]):List[Int] = 
    foldRight(as,List[Int]())( (x,xs) => Cons(x + 1,xs))

  def convertString(as:List[Double]):List[String] = 
    foldRight(as, Nil:List[String])( (x,xs) => Cons(x.toString,xs))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)
 
  def map[A,B](as: List[A])(f:A => B): List[B] =
    foldRight(as, Nil:List[B])( (x,xs) => Cons(f(x),xs))

  def myfilter[A](as: List[A])(f:A => Boolean): List[A] =
    foldRight(as, Nil:List[A])( (x,xs) => 
      f(x) match {
        case true => Cons(x,xs)
        case _ => xs
      })

  def filter[A](as: List[A])(f:A => Boolean): List[A] =
    foldRight(as, Nil:List[A])( (x,xs) => if(f(x)) Cons(x,xs) else xs)

  def myflatMap[A,B](as: List[A])(f:A => List[B]):List[B] =
    foldRight(as, Nil:List[B])( (x,xs) => append(f(x),xs) )
  
  def flatMap[A,B](l: List[A])(f:A => List[B]):List[B] =
    concat(map(l)(f))

  def filter2[A](as: List[A])(f:A => Boolean): List[A] =
    flatMap(as)( i => if(f(i)) List(i) else Nil )

  //def addList[A](xs1: List[A])(xs2:List[A]): List[A] =
  //  flatMap(xs2)(j => flatMap(xs1)(i => List(i)) + j)
  def addPairwise(a: List[Int], b:List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }

  def zipWith[A,B,C](a: List[A], b:List[B])(f:(A,B) => C): List[A] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match{
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h,t), Cons(h2,t2)) if h1 == h2 > startsWith(t, t2)
    case _ => false
  }
  def hasSubsequence[A](sup: List[A], sub:List[A]): Boolean = sub match {
    case (xs append ys) startsWith xs
    xs startsWith Nil
    (xs append ys append zs) hasSubsequence ys
    xs hasSubsequence Nil
  }
