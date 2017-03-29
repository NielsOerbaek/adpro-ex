trait RNG {
  def nextInt: (Int, RNG)
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

  // Exercise 1 (CB 6.1)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (randomInt, newRNG) = rng.nextInt
    if(randomInt < 0) ((-randomInt-1), newRNG)
    else (randomInt, newRNG)
  }

  // Exercise 2 (CB 6.2)

  def double(rng: RNG): (Double, RNG) = { 
    val (randomInt, newRNG) = nonNegativeInt(rng)
    val randomDouble = randomInt.toDouble/((Int.MaxValue).toDouble + 1.0)
    (randomDouble, newRNG)
  }

  // Exercise 3 (CB 6.3)

  def intDouble(rng: RNG): ((Int, Double), RNG) = { 
    val (randomInt, newRNG1) = rng.nextInt
    val (randomDouble, newRNG2) = double(newRNG1)
    ((randomInt,randomDouble), newRNG2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = { 
    val (randomDouble, newRNG1) = double(rng)
    val (randomInt, newRNG2) = newRNG1.nextInt
    ((randomDouble, randomInt), newRNG2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = { 
    val (randomDouble1, newRNG1) = double(rng)
    val (randomDouble2, newRNG2) = double(newRNG1)
    val (randomDouble3, newRNG3) = double(newRNG2)
    ((randomDouble1,randomDouble2,randomDouble3), newRNG3)
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  // Exercise 4 (CB 6.4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count == 0) (Nil, rng)
    else {
      val (randomInt, newRNG) = rng.nextInt
      val (tailOfList, nextRNG) = ints(count-1)(newRNG)
      ((randomInt::tailOfList), nextRNG)
    }
  }

  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // Exercise 5 (CB 6.5)
  val _double: Rand[Double] = map(int)(i => i.toDouble/(Int.MaxValue+1).toDouble)

  // Exercise 6 (CB 6.6)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

  // this is given in the book

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 7 (6.7)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs.foldLeft[(List[A], RNG)] (Nil, rng) ((acc, f) => {
        val (as, accRNG) = acc
        val (a, newRNG) = f(accRNG)
        (a::as, newRNG)
      })
    }

  def _ints(count: Int): Rand[List[Int]] = 
    sequence(List.fill(count)(int))

  // Exercise 8 (6.8)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
    rng => {
      val (a, rng2) = f(rng)
      val (b, rng3) = g(a)(rng2)
      (b,rng3)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = 
    flatMap (double) (d => rng => ((d * n).toInt, rng))

}

import State._

case class State[S, +A](run: S => (A, S)) {

  // Exercise 9 (6.10)

  def map[B](f: A => B): State[S, B] = 
    State( s => {
      val (a, state2) = run(s)
      (f(a), state2)
    })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = 
    State( s => {
        val (a, state2) = run(s)
        val (b, state3) = sb.run(state2)
        (f(a,b), state3)
      })

  def flatMap[B](f: A => State[S, B]): State[S, B] = 
    State(s => {
        val (a, state2) = run(s)
        val (b, state3) = f(a).run(state2)
        (b, state3)
      })

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // Exercise 9 (6.10) continued

  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] = 
    State(s => {
      sas.foldLeft[(List[A],S)] (Nil, s) ((acc, sa) => {
        val (as, accState) = acc
        val (a, newState) = sa.run(accState)
        (a::as, newState)
        })
      })
  
  // This is given in the book:

  // def modify[S](f: S => S): State[S, Unit] = for {
  //   s <- get // Gets the current state and assigns it to `s`.
  //   _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  // } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))


  def random_int :Rand[Int] =  State (_.nextInt)

  // Exercise 10

  def state2stream[S,A] (s :State[S,A]) (seed :S) :Stream[A] = {
    val (a, newState) = s.run(seed)
    Stream.cons(a, state2stream(s)(newState))
  }

  // Exercise 11

  val random_integer_stream = state2stream(random_int)(RNG.Simple(42))
  
  val random_integers = random_integer_stream.take(10).toList
  
}


// vim:cc=80:foldmethod=indent:foldenable
