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

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match{
      case Nil => Nil
      case Cons(a, as) => 
        if(f(a) == true) as
        else dropWhile(as, f)
  }

  def dropWhileAnswer[A](l: List[A], f: A => Boolean): List[A] = 
    l match {
      case Cons(h,t) if !f(h) => dropWhile(t, f) 
      case _ => l
   }

   def init[A](l:List[A]): List[A] = l match{
    case Nil => Nil 
    case Cons(x,Nil) => Nil 
    case Cons(x,xs) => Cons(x, init(xs))
   }
}
