package fpinscala.laziness

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Note: while the book did give my solution below as one of its solutions, this
  // solution is not tail recursive, and that means it isn't stack-safe. The book also
  // had a couple of tail recursive solutions.
  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  // Not tail recursive. I worked on a tail recursive version below, but it ends up with
  // a solution that's in reversed order, which is no good.
  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
      case _ => Stream.empty
    }

//  def takeTail(n: Int): Stream[A] = {
//    @annotation.tailrec
//    def go(rem: Int, s: Stream[A], acc: Stream[A]): Stream[A] =
//      s match {
//        case Cons(h, t) if rem > 0 => go(rem - 1, t(), Stream.cons(h(), acc))
//        case _ => acc
//      }
//
//    go(n, this, Stream())
//  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      // I think that my own answer, commented out below, is a bit less elegant than the book's.
      // The book only had two logical possibilities, while mine had three (two from the if-else
      // of the Cons match, and then a third for the catchall).
//      case Cons(_, t) => if (n > 0) t().drop(n-1) else this
//      case _ => Stream.empty
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _ => Stream.empty
    }

  // Because I'm reusing the book's pre-defined lazy foldRight, AND the body of my
  // folding function can short-circuit, we are successfully avoiding a full
  // traversal of the stream in the event that one of the stream's
  // elements returns false.
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, z) => p(h) && z)

  // The key to getting this right was not using `else z` at the very end, but rather `else Stream()`;
  // when I was using `else z`, I was forcing the traversal of the stream to continue instead of
  // short-circuiting. This unwanted continuation could actually cause bugs: for instance, a takeWhile
  // applying n < 12 to Stream(10, 11, 12, 11, 10) would return Stream(10, 11, 11, 10) instead of
  // simply Stream(10, 11). Why did the evaluation continue when I was using `else z`? If you look at
  // the book's definition of foldRight (commented out below), you'll see that the second argument to f
  // includes a recursive call to foldRight. So only by not
  // forcing evaluation of that second argument in your folding function's body can you short-circuit.
  // One more question that came up in my mind: how do I successfully accumulate some number of results,
  // when my `else` case isn't doing any accumulation? Well, I'm accumulating in the `if` case; that's
  // where the accumulation happens. Then, when I get to an `else`, I do not reference `z` and therefore
  // do not keep folding over the stream.
  // I guess one thing I'm still a little unclear on is how, given all this laziness, we actually DO
  // keep traversing the stream until we hit a p(h) that returns false. The body of my folding function
  // does reference z (the fold of the tail of the stream), but it does so in a call to the smart constructor
  // `cons`, which takes both arguments by name and assigns them to lazy vals. Oh! Actually, I think I've figured
  // out the answer to my own question, and the book might also address it soon. The truth is that we DON'T
  // keep traversing the stream -- not until something really does force the evaluation of these values. That is,
  // I was correct that passing z into `cons` would not force `z` to be evaluated. It will only be evaluated later
  // if forced -- if forced, for instance, by a call to `toList`. This is backed up by looking at the output of
  // a `takeWhileViaFold` call in the REPL -- it's still just a single Cons of two lambdas, even if the final toList
  // result would contain more than one Cons. When the intro to this chapter in the book said that streams gave
  // us a special ability to build up a series of functions and apply them on a single traversal, this must be
  // what it was referring to: the fact that we can do any number of folds over a stream without traversing it,
  // and then (maybe?) only force traversal a single time later on.
  def takeWhileViaFold(p: A => Boolean): Stream[A] =
    foldRight(Stream(): Stream[A])((h, z) => if (p(h)) Stream.cons(h, z) else Stream())
//
//  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
//    this match {
//      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
//      case _ => z
//    }

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // Note that the four below functions -- map, filter, append, and flatMap -- all ultimately
  // call the lazy smart constructor cons, and thus do not force traversal of their stream
  // (at least, beyond maybe forcing the head).
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty: Stream[B])((h, z) => Stream.cons(f(h), z))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty: Stream[A])((h, z) => if (f(h)) Stream.cons(h, z) else z)

  // My answers are a little different from the book's -- I implemented append as adding
  // one item to the end of a stream, not adding a whole stream to another stream -- but
  // my private definition of concat (within flatMap) delivers the same functionality as
  // the book's append.
  def append[B >: A](b: => B): Stream[B] =
    foldRight(Stream.cons(b, Empty): Stream[B])((h, z) => Stream.cons(h, z))

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    def concat(b0: Stream[B], b1: Stream[B]): Stream[B] = {
      b0.foldRight(b1)(Stream.cons(_, _))
    }
    foldRight(Stream.empty: Stream[B])((h, z) => concat(f(h), z))
  }

  // It's interesting that we've now defined map both via fold and unfold. This isn't too
  // crazy, though, since the 'fold' version's folding function uses cons, and thus actually
  // builds up a stream.
  // I'd be tempted to call the folding version more elegant since it doesn't use pattern
  // matching; however, I'm not sure how much more elegant calling the smart `cons` constructor
  // really is. Either way, you know something about the Stream datatype specifically.
  def mapViaUnfold[B](f: A => B): Stream[B] =
  // this is my own solution; I'm using the 'pattern matching anonymous function'
  // syntax that IntelliJ asked me to use, though.
    Stream.unfold(this){
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def takeViaUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n)){
      case (Cons(h, t), x) if x > 0 => Some(h(), (t(), x - 1))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    Stream.unfold(this){
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](sb: Stream[B])(f: (A, B) => C): Stream[C] =
    // The shorter syntax here actually seems to work -- I think Scala is inferring
    // that I'm asking for tuples.
    Stream.unfold(this, sb){
      case (Cons(ha, ta), Cons(hb, tb)) => Some(f(ha(), hb()), (ta(), tb()))
      case _ => None
    }

  def zipAll[B](sb: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold(this, sb){
      // Note: the book used an unexpected arrow syntax in place of the final two commas
      // below. I don't know what that was about, but they'll probably explain it later.
      case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
      case (Cons(ha, ta), Empty) => Some((Some(ha()), None), (ta(), Empty))
      case (Empty, Cons(hb, tb)) => Some((None, Some(hb())), (Empty, tb()))
      case _ => None
    }

  def length: Int =
    map(_ => 1).foldRight(0)(_ + _)

  // Though I initially thought that I couldn't reuse forAll here, I totally could. There's nothing
  // stopping me from comparing Option values for equality (I think) -- it should just work.
  def eq[B >: A](sb: Stream[B]): Boolean =
    zipAll(sb).foldRight(true){
      case ((Some(a), Some(b)), z) => (a == b) && z
      case _ => false
    }

  // The book's solution has all the same pieces as mine, but assembled in a more terse way.
  // The book opted to use zipAll first, THEN takeWhile. That's better than what I did -- in
  // retrospect, my logic is actually redundant, because I check for whether the 'b' stream
  // exhausted twice, once by using its length in startsWith's take and once by demanding that
  // it still return a 'some' in `eq`. The book's solution also enabled it to reuse forAll.
  // In fairness to my solution, though, I ended up with a very useful and reusable `eq` method.
  // In some sense, my solution just might be better.
  def startsWith[B >: A](sub: Stream[B]): Boolean =
    takeViaUnfold(sub.length) eq sub
  // the book's overall better solution:
  //  zipAll(s).takeWhile(!_._2.isEmpty) forAll {
  //    case (h,h2) => h == h2
  //  }

  // The book had a better solution -- instead of shimming in an Option and a third `case` match to deal
  // with the fact that we needed to add a `Stream()` to the final result, the book just directly appended
  // the empty stream. The book's solution is ALSO cleaner for not directly matching on, and then verbatim
  // repeating, `Cons(h, t)`. Instead the book just matched on the entire Cons after checking for Empty,
  // and then dropped 1 to effectively get the tail.
  def tails: Stream[Stream[A]] =
    Stream.unfold(Some(this): Option[Stream[A]]){
      case Some(Cons(h, t)) => Some(Cons(h, t), Some(t()))
      // this special 'empty' case is required to correctly include an empty stream as the final element of
      // our unfold's return stream
      case Some(Empty) => Some(Empty, None)
      case _ => None
    }
      // the book's cleaner solution:
//     Stream.unfold(this) {
//       case Empty => None
//       case s => Some((s, s drop 1))
//     } append Stream(Stream.empty)

  // I do not think that this can be implemented conveniently with unfold, because unfold builds
  // its results from left to right -- it generates more and more deeply nested elements of the
  // stream, instead of prepending each new result. So we would need to unfold and then reverse,
  // which I think would defeat the point of laziness -- that reverse would be a full additional
  // traversal of our stream.
  // Given how very similar the function signature is to foldRight, though, surely we can reuse
  // that function somehow.
  // Turns out I was right! Having now looked at the book's solution, it's extremely similar to mine;
  // the book's solution is better, though, because it accounts properly for laziness. For instance,
  // my solution should have had the folding function take its `B` arg by name in the type signature,
  // and then should have assigned the (b, bs) tuple to a lazy val. In that sense -- to make the lazy
  // val assignment convenient -- it might be better to stick with just having the whole tuple assigned
  // to a constant, instead of breaking it down with pattern matching like I did in my final solution.
  // NOTE, a lesson: you'll very often want to use lazy val in the body of a function that takes an argument
  // by name, because otherwise the by-name argument might be evaluated any number of times (more than once).
  def scanRight[B >: A](z: B)(f: (A, B) => B): Stream[B] =
    // Basically, I took the folding function passed into scanRight and wrapped it in another function
    // which will build a stream out of our intermediate results.
    // `a` is the current value from our original stream
    // `b` is the fold's z -- the tail of the original stream that we still need to fold over,
    //    but not itself a stream, just one more 'tail' value at a time
    // `bs` is just like b, except it's a stream, and we cons onto it every time -- this is my
    // accumulator that holds all intermediate results, prepending each result.
    foldRight(z, Stream(z)){
      case (a, (b, bs)) => (f(a, b), Stream.cons(f(a, b), bs))
    }._2
    // my same solution, just without pattern matching:
    //  foldRight((z, Stream(z)))((a, ztup) => (f(a, ztup._1), Stream.cons(f(a, ztup._1), ztup._2)))._2

  // The book's solution is essentially identical, yet cleaner: they used the `exists` helper function, defined
  // way above in this file. It does the very same kind of fold, but is ultimately cleaner to read.
  // NOTE, A lesson: just as we usually shouldn't manually pattern match but instead should use a higher-order function
  // like `foldRight` or `unfold`, we should often use a helper function that itself calls `foldRight` or `unfold`
  // instead of manually calling either method. That's because some aspect of the logic we're doing is probably
  // really reusable -- for instance, the fact that my fold below was actually intended to check whether any
  // part of the sequence matched a predicate.
  // NOTE, Another lesson: instead of doing some tricky logic where you iterate over a single list but feel like you
  // might need to backtrack, just iterate over a list of lists instead -- what `tails` above achieves (the book
  // told me to implement tails, so that's a pattern from them).
  def hasSubsequence[B >: A](sub: Stream[B]): Boolean =
    tails.foldRight(false)((tail, z) => tail.startsWith(sub) || z)
    // book's cleaner solution using an identical helper
//    tails exists (_ startsWith sub)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))
  // Note: the book offered a more efficient (but kind of trippy) solution, below.
  //  lazy val tail: Stream[A] = Cons(() => a, () => tail)
  //  tail

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  //  def fibs(): Stream[Int] = {
  //    @annotation.tailrec
  //    def go(x: Int, y: Int, s: Stream[Int]): Stream[Int] = {
  //      go(y, x + y, Stream.cons(x, s))
  //    }
  //
  //    go(0, 1, Stream.empty)
  // My solution, above, is commented out. It was inferior to the book's solution (below)
  // for two reasons: it unnecessarily passed around a reference to a stream, and more
  // importantly, it was unusable -- it went on forever any time it was called, even when
  // I tried to build in laziness. In retrospect, I think I could have made my solution
  // successfully avoid infinite recursion if I'd manually assigned x and y to lazy vals in
  // the body (and then assigned x + y to another lazy val). I'll try that below.
//  }
  // Book's solution -- mine was no good. (To my benefit, though, my solution -- had
  // it worked -- would have been tail recursive.)
  val fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  // Nope, still didn't work.
//  val fibsMyWorse: Stream[Int] = {
//    @annotation.tailrec
//    def go(x: () => Int, y: () => Int, s: () => Stream[Int]): Stream[Int] = {
//      lazy val xx = x()
//      lazy val yy = y()
//      lazy val z = xx + yy
//      go(() => yy, () => z, () => cons(xx, s()))
//    }
//
//    go(() => 0, () => 1, () => empty)
//  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
  }

  val onesViaUnfold: Stream[Int] =
    unfold(1)(_ => Some((1, 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  def fromViaUnfold(n: Int): Stream[Int] =
//    unfold(n)(x => {
//      val y = x + 1
//      Some(y, y)
//    })
  // My definition, commented out above, actually had a bug: it would start
  // its stream on n + 1, not n. So a call to fromViaUnfold(5) would return
  // a stream whose first element was six. The book's definition, below,
  // does not have this bug.
  unfold(n)(n => Some(n, n+1))

  val fibsViaUnfold: Stream[Int] =
    // I had to look up this syntax online for dealing with a tuple as
    // an argument to a lambda.
    unfold((0, 1))({case (x, y) => Some((x, (y, x + y)))})

}