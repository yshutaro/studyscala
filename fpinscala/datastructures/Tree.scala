sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{
  def size[A](tree: Tree[A]): Int = tree match{
    case Leaf(value) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def maximum(tree: Tree[Int]): Int = tree match{
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match{
    case Leaf(_) => 0
    //case Branch(left, right) => depth(left) + 1
    case Branch(left, right) => (depth(left) max depth(right) ) + 1
  }

  def map[A,B](t: Tree[A])(f:A => B): Tree[B] = t match{
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f),map(right)(f))
  }

  def fold[A,B](t: Tree[A])(l:A => B)(b: (B,B) => B):B = t match{
    case Leaf(value) => l(value)
    case Branch(left, right) => b(fold(left)(l)(b),fold(right)(l)(b))
  }

  def size2[A](t: Tree[A]):Int = fold(t)(_ => 1)((l,r) => l + r + 1)

  def maximum2(t: Tree[Int]):Int = fold(t)(v => v)((l,r) => l max r)

  def depth2[A](t: Tree[A]):Int = fold(t)(v => 0)((l,r) => (l max r) + 1)

  //def map2[A,B](t: Tree[A])(f:A => B): Tree[B] = fold(t)(v => f(v))((l,r) => Branch(f(l),f(r)))
  def map2[A,B](t: Tree[A])(f:A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}
