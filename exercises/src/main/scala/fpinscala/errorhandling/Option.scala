package fpinscala.errorhandling


import scala.{Either => _, Option => _, Some => _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case Some(a) => Some(f(a))
      case None => None
    }

  def getOrElse[B>:A](default: => B): B =
    this match {
      case Some(a) => a
      case None => default
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case Some(a) => f(a)
      case None => None
    }
//   map(f) getOrElse None // the book's more elegant solution

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this match {
      case Some(_) => this
      case None => ob
    }
//  this map (Some(_)) getOrElse ob // the book's more elegant solution

  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(a) if f(a) => this
      case _ => None
    }
//    flatMap(a => if (f(a)) Some(a) else None) // the book's more elegant solution
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = {
    val transformed: Option[Seq[Double]] = mean(xs).map((m: Double) => xs.map(x => math.pow(x - m, 2)))
    transformed flatMap mean
    // the book had a SLIGHTLY more terse solution, where they did this on one line:
//    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2)))) // book's solution
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(a0), Some(b0)) => Some(f(a0, b0))
      case _ => None
    }
//  a flatMap (aa => b map (bb => f(aa, bb))) // the book's cleaner solution without pattern matching

  def map3[A,B,C,D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] =
    a flatMap(aa => map2(b, c)(f(aa, _, _)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case None :: as => None
      case Some(aa) :: as => sequence(as) map (aa :: _)
      case Nil => Some(Nil)
    }
  // The book's solution was similar, but combined two of my pattern matches into one.
  // Overall I think the book's solution is better, because it's much less dependent on
  // directly referencing data structures, so would more easily be generalized.
//  a match {
//    case Nil => Some(Nil)
//    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
//  }
  // The book also had a one-liner:
//  a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

  def traverseInefficient[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a map f)

  // This solution of mine is based off the book's solution for sequence using foldRight. I'm
  // afraid that I would not have come up with this by myself, but it makes some sense in retrospect:
  // to build up an Option[List[B]] in one pass, given just a List[A] and an (A => Option[B]), we need
  // to break down the structure of the list and accumulate it into a new Option. Breaking down the
  // structure and accumulating *something* as we go is just what folds are good for. Meanwhile, we use
  // map2 because it gives us a way to lift (::) into the Option context of our Option[B] and Option[List[B]].
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]])((ha, z) => map2(f(ha), z)(_ :: _))

  def sequenceReimpl[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)
}