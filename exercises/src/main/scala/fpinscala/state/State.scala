package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    // Note that this is pretty janky -- the book did it differently, and possibly
    // better, to avoid skewing the numbers generated by this function. That is,
    // my function is more likely to generate abs(Int.minValue + 1) than any
    // other number. Not by much, but it'd be a problem for a real random number generator.
    val notMin = if (i == Int.MinValue) i + 1 else i
    (math.abs(notMin), rng2)
  }

  // The book's solution was slightly better; it added 1 to Int.MaxValue after converting it
  // to a double. This way, the book ensured that it would always return a value less than one without
  // mapping both Int.MaxValue and Int.MaxValue - 1 to the same thing. Thus, the book avoided my
  // implementation's slighly skewed randomness.
  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    val notMax = if (i == Int.MaxValue) i - 1 else i
    val d = notMax.toDouble
    val under1 = d / Int.MaxValue.toDouble
    (under1, rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  // This list of ints will technically come out in reverse order, but the ints were 'random' anyway,
  // so I think that's fine. I could build the list left-to-right instead, at the cost of losing
  // tail recursiveness.
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(n: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (n < 1) (acc, rng)
      else {
        val (i, rng2) = rng.nextInt
        go(n - 1, rng2, i :: acc)
      }
    }

    go(count, rng, List())
  }

  val doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  // book's definition of the more verbose pre-map double, for comparison
//  def double(rng: RNG): (Double, RNG) = {
//    val (i, rng2) = nonNegativeInt(rng)
//    (i / (Int.MaxValue.toDouble + 1), rng2)
//  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

  // I based this solution off of my earlier solution for `ints`.
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    @annotation.tailrec
    def go(ras: List[Rand[A]], rng: RNG, acc: List[A]): (List[A], RNG) =
      ras match {
        case Nil => (acc, rng)
        case ra :: t =>
          val (a, rng2) = ra(rng)
          go(t, rng2, a :: acc)
      }

    rng => go(fs, rng, List())
  }

  // The book's MUCH more terse solution for sequence.
  def sequenceBook[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  // flatMap is like a version of map that says "hey, I'm actually going to do a
  // second state transition" -- that second state transition would be a lot more clear
  // if, instead of ending with the line my implementation does end with, it instead said:
  // val (b, rng3) = g(a)(rng2)
  // (b, rng3)
  // The cool thing about this is that it doesn't actually have to change the state on that
  // second transition -- the A => Rand[B] function passed in could just be `unit` (in which
  // case B would happen to equal A). Also, you can use flatMap to implement a recursive function,
  // as we do with nonNegativeLessThan, to actually implement an indefinite number of
  // state transitions in a way that plain old map can't. (Map can't do this because the mapping
  // function you pass it doesn't do state transitions.)
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  // The book instructed me to implement this in terms of flatMap, and also
  // provided the logic (which I do not fully understand) for dealing with i,
  // n, and mod. So really, I took the book's earlier partial implementation with
  // `map` and just had to wrap the if-case return value in `unit`, because the mapping
  // function that flatMap takes needs to return a Rand, which unit does.
  // I also borrowed this syntax from the book, where instead of opening the lambda
  // with `(i => {`, I just open it with `{ i =>`.
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  // I recall that earlier in the book we defined a map2 operation by composing flatMap
  // and map, and generally that we could define mapN by chaining a bunch of flatMaps
  // and ending with a map -- after all, that's what the for-yield syntax does under the hood.
  // Looking at this implementation, it seems that we need to call some form of 'map' for each
  // Rand argument to sort of break into its context (or rather, I suppose, 'lift' into its context).
  // While the very final lift can just be of type (all our args => final) and doesn't need to add
  // another layer of Rand, the earlier calls DO have to keep adding layers of Rand structure to
  // reach that final Rand's context. Thus, to keep from building a lot of structure that we don't
  // actually want, all of those earlier calls have to be flatMap.
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a => // applying an (A => Rand[C]) to Rand[A] -- requires flattening, else gives Rand[Rand[C]]
      mapViaFlatMap(rb)(b => f(a, b)) // making a Rand[B] a Rand[C]
    }
  }

// As best I can tell, the reason we're wrapping everything in a case class like this is to make
// State reusable. That is, without wrapping it in a case class, we wouldn't be able to generalize
// the pattern `S => (A, S)` by defining a ton of shared methods on it regardless of what actual
// type S takes. Given that we're making a case class like this, we need some way to refer to the
// `S => (A, S)` attribute, so we're apparently hewing to Haskell's convention and calling it `run`.
case class State[S,+A](run: S => (A, S)) {
  // I decided to reimplement this in terms of flatMap.
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def mapVerbose[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s1) = this.run(s)
      (f(a), s1)
    })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    this flatMap (a =>
      sb map (b => f(a, b))
    )

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = this.run(s)
      f(a).run(s1)
    })

  // `get` is a state action that simply exposes current state. It's saying, "when
  // you apply me to your existing state, the value that you'll get is actually
  // your current state, and then I'll leave that state unchanged by passing it
  // through as next state." This is kind of a neat trick: instead of returning
  // the same kind of `A` we usually expected, this is using the same sort of
  // output socket (obv a misuse of 'socket', but I'm thinking in terms of a
  // function as a machine with designated input and output locations) to return
  // a state value instead.
  def get: State[S, S] = State(s => (s, s))

  // `set` is a state action that ignores current state, just gives unit as a value,
  // and then sets our next state to be whatever the passed-in state value was.
  def set(s: S): State[S, Unit] = State(_ => ((), s))

  // Note that, while the `yield` statement only gives unit, that's because the yield
  // statement is just showing the value of `A` in `State[A, S]`. We're still returning
  // a full state action, and that state action will have our modified state.
  // NOTE, a lesson (immediately below):
  // One question that occurred to me: WHERE exactly is our "current", now-modified state? We
  // aren't using a mutable model here, where there's some object with a "current state" field
  // that keeps being changed. We aren't even using a technically-immutable version of that
  // where we copy the old object into a new object with a different "current state" field.
  // Instead, we're chaining a bunch of functions together, those functions being "state actions"
  // of type `S => (A, S)`. When I talk about the "current state", I'm talking about whatever
  // the last result._2 value is -- whatever the final state value from that return tuple would
  // be, once we ran all of our currently-chained state actions.
  // Open question: what are the benefits of this function-chaining "state action" style, versus
  // having a technically-immutable object that stores current state (and can convert that current
  // state into whatever `A` value you need)? Well, maybe I'm thinking about this wrong. That's
  // basically what state actions already are, at least when used like in the 'Machine' case below. You
  // have immutable Machine objects, and functions that advance one machine state to another, and then
  // you chain those functions together and call them on an immutable input machine. It's quite elegant.
  def modify(f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

object State {

  // Note: I absolutely could have implemented this within the case class instead; I'd have
  // only needed to tweak the type signature to no longer reference S and to specify B >: A,
  // then work with State[S, B]. However, it felt a lot more correct to define unit here --
  // I shouldn't need an existing State instance on which to call unit. That is, I shouldn't
  // have to define `val s = State(x => (1, x))` and then call `s.unit(17)`, just to build
  // some new unrelated state object out of 17.
  def unit[S,A](a: A): State[S, A] =
    State(s => (a, s))

  // I remember that the book used foldRight for Rand's sequence, so I'm doing
  // the same thing essentially from memory here.
  def sequence[S,A](as: List[State[S, A]]): State[S, List[A]] =
    as.foldRight(unit(List()): State[S,List[A]])((sa, t) =>
      sa.map2(t)(_ :: _)
    )

  type Rand[A] = State[RNG, A]
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  val outputs: (Int, Int) = (candies, coins)

  val isEmpty: Boolean = candies <= 0
}

// The book's solution actually uses an `object Candy` instead, and uses some techniques
// that it hasn't really covered yet.
object Machine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    inputs.foldRight(State(noOp(_)))((in, zAction) =>
      inputToAction(in).flatMap(_ => zAction)
    )
    // My more verbose solution is below. It's a slighly unusual use of flatMap above, but
    // it's the truth -- we ignore our (candies, coins) output tuples until until the very end.
    // I guess that's the norm -- unless you want to build a list of all your intermediate A values,
    // you really just need the last one.
//    inputs.foldRight(State(noOp(_)))((in, zAction) => {
//      val action = inputToAction(in)
//      State(m => {
//        val (_, m1) = action.run(m)
//        zAction.run(m1)
//      })
//    })
  }

  def inputToAction(i: Input): State[Machine, (Int, Int)] = {
    i match {
      case Turn => State(Machine.tryTurn)
      case Coin => State(Machine.tryCoin)
    }
  }

  def tryTurn(m: Machine): ((Int, Int), Machine) =
    if (!m.locked && !m.isEmpty) outputCandy(m)
    else noOp(m)

  private def outputCandy(m: Machine): ((Int, Int), Machine) = {
    val newM = m.copy(locked = true, candies = m.candies - 1)
    (newM.outputs, newM)
  }

  def tryCoin(m: Machine): ((Int, Int), Machine) =
    if (m.locked && !m.isEmpty) takeCoin(m)
    else noOp(m)

  private def takeCoin(m: Machine): ((Int, Int), Machine) = {
    val newM = m.copy(locked = false, coins = m.coins + 1)
    (newM.outputs, newM)
  }

  private def noOp(m: Machine): ((Int, Int), Machine) = (m.outputs, m)
}
