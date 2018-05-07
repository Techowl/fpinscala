package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(n) => n
      case Branch(l, r) => maximum(l).max(maximum(r))
    }

  def depth[A](t: Tree[A]): Int = {
    // I need two things to be happening: I need to add one to a depth
    // that I'm actively keeping track of with each branching level, AND
    // I need to select `max` values between different branches. I can do
    // this either in two passes (map depth onto a tree, then find its max
    // value) or in one pass. I'm going to attempt one pass here.
    def go[B](t: Tree[B], d: Int): Int =
      t match {
        case Leaf(_) => d
        case Branch(l, r) => go(l, d + 1).max(go(r, d + 1))
      }

    go(t, 0)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  // Note: my folds are like a list's foldRight, in that they pass control back and forth
  // between fold and a function argument (in this case, g). This allows for short-circuiting,
  // which is a strength, but it means that this function is not tail-recursive and therefore
  // is not stack-safe. However, while it's a little hard for me to tell without digging deeper,
  // I think that my implementation might actually be left associative.
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(a) => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  // The great news is that my fold is just like the book's solution. The bad news is that
  // my foldZ was actually unnecessary for the implementation of depthViaFold; instead of
  // using (_ max _) as g, you can use ((b1, b2) => 1 + (b1 max b2)).
  def foldZ[A, B](t: Tree[A], z: B)(f: (A, B) => B)(g: (B, B) => B)(h: B => B): B =
    (t, z) match {
      case (Leaf(a), acc) => f(a, acc)
      case (Branch(l, r), acc) => g(foldZ(l, h(acc))(f)(g)(h), foldZ(r, h(acc))(f)(g)(h))
    }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)((_) => 1)(_ + _ + 1)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(identity)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    foldZ(t, 0)((_, b) => b)(_ max _)(_ + 1)
  //    fold(t)(_ => 0)((b1, b2) => 1 + (b1 max b2)) // the book's superior solution

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])((b1, b2) => Branch(b1, b2))
}