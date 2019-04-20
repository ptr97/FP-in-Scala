package fpinscala.datastructures

import scala.collection.immutable.{List => ScalaList}
import scala.collection.immutable.{Nil => ScalaNil}
import scala.collection.immutable._

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  /** TODO all exercises */

  /** Exercise 3.25 */


  /** Exercise 3.26 */


  /** Exercise 3.27 */


  /** Exercise 3.28 */


  /** Exercise 3.29 */

  def main(args: Array[String]): Unit = {

  }
}
