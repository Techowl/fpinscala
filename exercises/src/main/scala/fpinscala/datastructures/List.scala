package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, t) => Cons(h, t)
    }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h)) dropWhile(t, f)
        else l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, z) => z + 1)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sumLeft(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def productLeft(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def lengthLeft[A](l: List[A]): Int =
    foldLeft(l, 0)((z, _) => z + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((as, a) => Cons(a, as))

  def appendItem[A](l: List[A], a: A): List[A] =
    foldRight(l, Cons(a, Nil))(Cons(_, _))

  def appendList[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)(Cons(_, _))

  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil: List[A])(appendList)

  // Note: having looked at the answer key, my implementations are not quite correct.
  // They do work for division, but I suppose that they might break if I tried to do
  // any kind of short-circuiting -- they would start from the wrong end of the list,
  // and thus short-circuit earlier or later than a TRUE fold left or right would have.
  // Further note, from coming back later: foldRight can't genuinely short-circuit unless
  // we're working off of a lazy list. See chapter 5. We can only short-circuit a traversal
  // if our function is taking an argument "by name" (non-strictly, using the `=>` syntax
  // before the argument's type) AND that function is not forcing the second argument in its
  // body (because, say, it's using a nonstrict operator like `&&` or `||`).
  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B) =
    foldRight(l, z)((a, b) => f(b, a))

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B) =
    foldLeft(l, z)((a, b) => f(b, a))

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def stringify(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  // first I wrote it in terms of foldRight, then without using a fold
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def myOtherMap[A,B](l: List[A])(f: A => B): List[B] =
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h)) Cons(h, filter(t)(f))
        else filter(t)(f)
    }

  def myOtherFilter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((h, z) =>
      if (f(h)) Cons(h, z)
      else z
    )

  def onlyEven(l: List[Int]): List[Int] =
    filter(l)(n => n % 2 == 0)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => append(f(h), flatMap(t)(f))
    }
  }

  def myOtherFlatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    foldRight(l, Nil: List[B])((a, bs) =>
      append(f(a), bs)
    )
  }

  // Am I missing something, or am I right that using flatMap to implement filter is a
  // really weird idea? I'm interpreting this as an intellectual exercise, rather than
  // an actual deep connection between filter and flatMap.
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a =>
      if (f(a)) Cons(a, Nil)
      else Nil
    )

  def addListContents(xs: List[Int], ys: List[Int]): List[Int] =
    (xs, ys) match {
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addListContents(xs, ys))
      case (_, _) => Nil
    }

  def zipWith[X, Y, Z](xs: List[X], ys: List[Y])(f: (X, Y) => Z): List[Z] =
    (xs, ys) match {
      case (Cons(x, xt), Cons(y, yt)) => Cons(f(x, y), zipWith(xt, yt)(f))
      case (_, _) => Nil
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    // iterate through sup; at each step, take sub.length from sup
    // and compare it to sub for equality
    val subLen = length(sub)
    def go[B](as: List[B]): Boolean =
      as match {
        case Nil => false
        case Cons(_, t) =>
          if (take(as, subLen) == sub) true
          else go(t)
      }

    go(sup)
  }

  def take[A](l: List[A], n: Int): List[A] =
    l match {
      case Cons(h, t) if n > 0 => Cons(h, take(t, n - 1))
      case _ => Nil
    }
}
