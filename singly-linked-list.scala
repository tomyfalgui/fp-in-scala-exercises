sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  // exercise 3.2 tail function
  def tail[A](list: List[A]): List[A] = list match {
    case Nil           => sys.error("list is empty")
    case Cons(_, tail) => tail
  }

  // exercise 3.4 drop function
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
      l match {
        case Nil           => sys.error("List is empty")
        case Cons(_, tail) => drop(tail, n - 1)

      }
  }

  // exercise 3.5 dropWhile
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(head, tail) if f(head) => dropWhile(tail)(f)
      case _                           => l
    }

  // exercise 3.6 init
  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(_, Nil) => Nil
      case Cons(h, t)   => Cons(h, init(t))
      case Nil          => sys.error("List is empty")
    }

  // exercise 3.3 setHead function
  def setHead[A](nh: A, list: List[A]): List[A] = list match {
    case Nil           => List()
    case Cons(_, tail) => Cons(nh, tail)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // 3.12
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(as: List[A], z: B): B =
      as match {
        case Nil         => z
        case Cons(x, xs) => go(xs, f(z, x))
      }

    go(as, z)

  }

  // exercise 3.9
  def length[A](as: List[A]) =
    foldRight(as, 0)((_, y) => y + 1)

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  // exercise 3.13
  def length2[A](as: List[A]) =
    foldLeft(as, 0)((x, _) => x + 1)

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  // exercise 3.14
  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x, y) => Cons(x, y))

  // 3.15
  def flatten[A](a1: List[List[A]]): List[A] = {
    foldRight(a1, Nil: List[A])(append)
  }

  // 3.16
  def incrementEach(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((x, y) => Cons(x + 1, y))
  }

  // 3.17
  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def main(args: Array[String]): Unit = {
    val l = List(List(1, 2, 3, 4), List(1, 2, 3, 4))
    val flattened = flatten(l)

    println(flattened)

    val l2 = List(1, 2, 3, 4)
    val l2plus = incrementEach(l2)
    println(l2plus)

    val d = List(1.0, 2.0, 3.0)
    val dstring = doubleToString(d)
    println(dstring)

    val dtimes2 = map(d)(a => a * 2.0)
    println(dtimes2)

  }

}

// exercise 3.1 evaluates to 3
