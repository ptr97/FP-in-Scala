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

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, newRng) = rng.nextInt
    (if (x < 0) -(x + 1) else x, newRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(x => x / (Int.MaxValue.toDouble + 1))(rng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng1) = double(rng)
    val (i, rng2) = rng1.nextInt
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def loop(n: Int, rng1: RNG, l: List[Int]): (List[Int], RNG) = {
      if (n == 0) {
        (l, rng1)
      } else {
        val (i, nextRNG) = rng1.nextInt
        loop(n - 1, nextRNG, i :: l)
      }
    }

    loop(count, rng, Nil)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (x1, rng1) = ra(rng)
      val (x2, rng2) = rb(rng1)
      (f(x1, x2), rng2)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((a, b) => (a, b))
  }

  def betterIntDouble(rng: RNG): Rand[(Int, Double)] = {
    both(int, double)
  }

  def betterDoubleInt(rng: RNG): Rand[(Double, Int)] = {
    both(double, int)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(Nil: List[A]))((a, acc) => map2(a, acc)(_ :: _))
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (x, newRng) = f(rng)
      g(x)(newRng)
    }
  }

  def nonNegativeLessThen(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)(i => unit(i % n))
  }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(x => unit(f(x)))
  }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => {
      map(rb)(b => {
        f(a, b)
      })
    })
  }

  def main(args: Array[String]): Unit = {
    val rng = Simple(1)
    val l1 = sequence(List(unit(1), unit(2), unit(3)))(rng)._1
    println(l1)

    /** flatMap tests*/
    val coin = mapViaFlatMap(unit(3))(_ % 2)(rng)._1
    println(s"My coin = $coin")

    val sumOfCoins = map2ViaFlatMap(unit(10), unit(2))(_ + _)(rng)._1
    println(s"Sum of coins = $sumOfCoins")
  }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
