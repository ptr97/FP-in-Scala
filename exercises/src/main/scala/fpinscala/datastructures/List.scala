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
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }
  }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, xs) => Cons(h, xs)
    }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  /** not tail recursive!!! */
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def initWithBuffer[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @annotation.tailrec
    def loop(rest: List[A]): List[A] = {
      rest match {
        case Nil => Nil
        case Cons(_, Nil) => List(buf.toList: _*)
        case Cons(h, t) => buf += h; loop(t)
      }
    }

    loop(l)
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => 1 + acc)

  def foldLeftAnonymousFunction[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(l: List[A], acc: B): B = {
      l match {
        case Nil => acc
        case Cons(h, t) => loop(t, f(acc, h))
      }
    }

    loop(l, z)
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], acc: B)(f: (B, A) => B): B = {
    l match {
      case Nil => acc
      case Cons(h, t) => foldLeft(t, f(acc, h))(f)
    }
  }

  /** Exercise 3.11 */
  def sumFoldLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def productFoldLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  def lenFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)


  /** Exercise 3.12 */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))


  /** Exercise 3.13 hard */
  // TODO watch it
  def foldRightViaFoldLeft[A, B](l: List[A], acc: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), acc)((b, a) => f(a, b))
  }

  /** Exercise 3.14 */
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))

  /** Exercise 3.15 hard */
  // TODO watch it
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  /** Exercise 3.16 */
  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((el, nl) => Cons(el + 1, nl))

  /** Exercise 3.17 */
  def listOfDblToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((el, nl) => Cons(el.toString, nl))

  /** Exercise 3.18 map <3 */
  def map[A, B](l: List[A])(f: A => B): List[B] =
  /** not stack safe...
    foldRight(l, Nil: List[B])((el, nl) => Cons(f(el), nl))
  */
    foldRightViaFoldLeft(l, Nil: List[B])((el, nl) => Cons(f(el), nl))


  /** Exercise 3.19 */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((el, nl) => if (f(el)) Cons(el, nl) else nl)

  /** Exercise 3.20 */
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  /** Exercise 3.21 */
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if(f(a)) List(a) else Nil)

  /** Exercise 3.22 */
  def zipAdd(l: List[Int], r: List[Int]): List[Int] = {
    (l, r) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipAdd(t1, t2))
    }
  }

  /** Exercise 3.23 */
  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] =
    (l, r) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  /** Exercise 3.24 */
  // TODO


  def main(args: Array[String]): Unit = {

  }
}
