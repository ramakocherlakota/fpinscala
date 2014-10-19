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
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, r) = double(rng)
    val (i, r2) = r.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    if (count > 0) {
      val (i, r) = rng.nextInt
      val (ilist, r1) = ints(count - 1)(r)
      (i :: ilist, r1)
    }
    else {
      (Nil, rng)
    }

  }


  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight[Rand[List[A]]](unit[List[A]](Nil))((x,y) => map2(x,y)(_ :: _))
  }

  def intsViaSequence(count : Int) : Rand[List[Int]] =
    sequence(List.fill(count)(int))


  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
    r0 => {
      val (a, r1) = f(r0)
      g(a)(r1)
    }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2viaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    flatMap(ra)(a => map(rb)(b => f(a, b)))


  def nonNegativeLessThan(n: Int) : Rand[Int] = {
    def g(i: Int) : Rand[Int] = {
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        {rng => (mod, rng)}  // unit(mod)
      else
        nonNegativeLessThan(n)
    }

    flatMap[Int, Int](nonNegativeInt)(g)
  }



}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    new State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    new State(s => {
      val (a, s1) = run(s)
      val (b, s2) = sb.run(s1)
      (f(a, b), s2)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    new State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

  def sequence[A](fs: List[State[S, A]]): State[S, List[A]] = 
    fs.foldRight[State[S, List[A]]](State.unit[S, List[A]](Nil))((x,y) => x.map2(y)(_ :: _))

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a : A) : State[S, A] = 
    new State(s => (a, s))
}

object Candy {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

    def handleInput(input: Input, state : State[Machine, (Int, Int)]) : State[Machine, (Int, Int)] = {
      new State[Machine, (Int, Int)](machine => {
        input match {
          case Coin => {
            if (machine.locked && machine.candies > 0)
              ( (machine.candies, machine.coins + 1), new Machine(false, machine.candies, machine.coins + 1))
            else
              ((machine.candies, machine.coins), machine)
          }
          case Turn => {
            if (!machine.locked && machine.candies > 0)
              ((machine.candies - 1, machine.coins), new Machine(true, machine.candies - 1, machine.coins))
            else 
              ((machine.candies, machine.coins), machine)
          }
          case _ => ((machine.candies, machine.coins), machine)
        }
      })
    }

    def emptyRun : State[Machine, (Int, Int)] = new State[Machine, (Int, Int)]({
      machine => ((machine.candies, machine.coins), machine)
    })

    inputs.foldRight[State[Machine, (Int, Int)]](emptyRun)(handleInput)
  }

}
