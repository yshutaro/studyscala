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
    foldLeft(as, Nil)( (xs, x) => Cons(x,xs))
}
