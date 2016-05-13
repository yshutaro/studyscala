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
}
