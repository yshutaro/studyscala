sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE,B]): Either[EE,B] = this match{
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E,B >:A](b: => Either[EE,B]):Either[EE,B] = this match{
  /*
    case Right(a) => b
    case Left(e) => Left(e)
    */
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b:Either[EE,B])(f:(A, B) => C): Either[EE, C] = 
   // a flatMap (aa => b map (bb => f(aa,bb)))
   for { a <- this; b1 <- b } yield f(a,b1) 

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es match {
      case Nil => Right(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
   a match{
      case Nil => Right(Nil)
      case h :: t => f(h) flatMap (hh => traverse(t)(f) map (hh :: _))
   }

   def mkName( name: String): Either[ String, Name] = 
     if (name == "" || name == null) Left(" Name is empty.")
     else Right( new Name(name))

   def mkAge( age: Int): Either[ String, Age] =
    if (age < 0) Left(" Age is out of range.")
    else Right( new Age(age))

   def mkPerson( name: String, age: Int): Either[ String, Person] =
    mkName( name).map2( mkAge( age))( Person(_, _))

}
case class Left[+E](value: E) extends Either[E, Nothing] 
case class Right[+A](value: A) extends Either[Nothing, A]

case class Person( name: Name, age: Age)
sealed class Name( val value: String)
sealed class Age( val value: Int)
/*
object Main{
  def main(args: Array[String]){
    val either = new Either();    
    either.mkPerson("yoshizumi",42)    
  }
}
*/
