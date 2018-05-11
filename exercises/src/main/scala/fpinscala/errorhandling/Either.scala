package fpinscala.errorhandling


import scala.{Either => _, Left => _, Option => _, Right => _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

  // I don't see any simple way to implement this without using pattern matching,
  // because we haven't defined any catamorphism to break down the extra either
  // structure introduced by the map call.
  // (Indeed, the book's solution also resorted to pattern matching.)
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(_) => this
      case Left(_) => b
    }

  // I'm basically copying the book's Option solution, but from my own memory --
  // I remember that it chains flatMap and map, with map as the more deeply nested call
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap (aa => b map (bb => f(aa, bb)))
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  // I'm doing this from memory of my folding solution for Option.traverse
  def traverse[E,A,B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil): Either[E, List[B]])((ha, z) => f(ha).map2(z)(_ :: _))
    // an alternative using a for comprehension:
//    as.foldRight(Right(Nil): Either[E, List[B]])((ha, z) =>
//      for {
//        bb <- f(ha)
//        zz <- z
//      } yield bb :: zz)

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  // Exercise 4.8 (text-only)
  // I think that the best way to go would be to use a new data type. It would be similar to
  // something like Either[List[Error], Success], but unlike either it would not short-circuit
  // in the Left case.
  // `orElse` would probably continue to work the same way, discarding its error list and
  // instead returning a success value in the error case.
  // `traverse` (and therefore `sequence`) would work a little differently, because they would
  // not short-circuit when iterating through the list if they encounter a Left result. Rather,
  // they would iterate through the entire list. On account of this, it might actually be better
  // to implement this other `traverse` in terms of foldLeft instead of foldRight; short-circuiting
  // isn't needed, but being stack-safe is a big plus.
}