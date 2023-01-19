sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = {
    def go(t: Tree[A], acc: Int): Int = {
      t match {
        case Branch(l, r) => 1 + go(l, acc) + go(r, acc)
        case Leaf(_)      => acc + 1
      }
    }
    go(t, 0)
  }

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Branch(l, r) => maximum(l).max(maximum(r))
      case Leaf(a)      => a
    }
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Branch(l, r) => 1 + depth(l).max(depth(r))
      case Leaf(_)      => 0
    }
  }

  def map[A, B](t: Tree[A])(f: (A => B)): Tree[B] = t match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(a)      => Leaf(f(a))
  }

  def fold[A, B](b: Tree[A], f: A => B, g: (B, B) => B): B =
    b match {
      case Leaf(a)             => f(a)
      case Branch(left, right) => g(fold(left, f, g), fold(right, f, g))
    }

  def main(args: Array[String]): Unit = {
    val leafA = Leaf(1)
    val leafB = Leaf(2)
    val leafC = Leaf(100)
    val leafD = Leaf(4)

    val subBranch1 = Branch(leafA, leafB)
    val subBranch2 = Branch(leafC, leafD)

    val main = Branch(subBranch1, subBranch2)

    val mainSize = size(main)
    println(mainSize)

    val maxi = maximum(main)
    println(maxi)

  }

}
