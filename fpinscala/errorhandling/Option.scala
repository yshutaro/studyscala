import scala.{Option => _, Either => _, _}

sealed trait Option[+A]{
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match{
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B):B = this match{
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]):Option[B] = this match{
    case None => ob
    case _ => this
  }
  
  def filter(f: A => Boolean):Option[A] = this match{
    case Some(a) if f(a) => this
    case _ => None
  }

  def mean(xs: Seq[Double]): Option[Double] = 
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }
  
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option{

  def mean(xs: Seq[Double]): Option[Double] = 
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def failinFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!")

    try{
      val x = 42 + 5
      x + y
    }

    catch{case e: Exception => 43}
  }

  def failinFn2(i: Int): Int = {
    try{
      val x = 42 + 5
      x + ((throw new Exception("fail!")) : Int)
    }

    catch{case e: Exception => 43}
  }

  def map2[A,B,C](a:Option[A], b:Option[B])(f:(A, B) => C): Option[C] = {
    //a flatMap (x => (b flatMap (y => Some(f(x,y)))))
    a flatMap (aa => b map (bb => f(aa,bb)))
  }

/*
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match{
    case Nil => None
    case x::xs => x flatMap(xx => xs map (xss => List(xx,sequence(xss))))
  }
*/

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
   a match{
      case Nil => Some(Nil)
      case h :: t => f(h) flatMap (hh => traverse(t)(f) map (hh :: _))
   }
}

object Myapp extends App{
  val x:Seq[Double] = List(1.2,3.2,1.22)
  println(x)
  val y = List()

  val meanFunc = Option.mean _
  val varianceFunc = Option.variance _

  println(meanFunc(x))
  println(varianceFunc(x))
  println(varianceFunc(y))

}
