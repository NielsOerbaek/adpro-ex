// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
package fpinscala.monoids
import scala.language.higherKinds

trait Monoid[A] {

  def op(a1: A, a2: A): A
  def zero: A

}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  // I guess such objects could be useful if we combine them with implicits

  def listMonoid[A] = new Monoid[List[A]] {
    def op (a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  // Exercise 1

  val intAddition = new Monoid[Int] {
  	def op (i1: Int, i2: Int) = i1 + i2
  	val zero = 0
  }
  val intMultiplication = new Monoid[Int] {
  	def op (i1: Int, i2: Int) = i1 * i2
  	val zero = 1
  }
  val booleanOr = new Monoid[Boolean] {
  	def op (b1: Boolean, b2: Boolean) = b1 || b2
  	val zero = false
  }
  val booleanAnd = new Monoid[Boolean] {
  	def op (b1: Boolean, b2: Boolean) = b1 && b2
  	val zero = true
  }

  // Exercise 2

  def optionMonoid[A] = new Monoid[Option[A]] {
  	def op (oa1: Option[A], oa2: Option[A]) = oa1.orElse(oa2)
  	val zero = None
  }

  def dual[A] (m :Monoid[A]) = new Monoid[A] {
    def op (a1: A, a2: A) = m.op(a2,a1)
    val zero = m.zero
  }

  // Exercise 3
  // Does the order of the two functions here matter?
  // Should it be f1(f2(a))? 
  def endoMonoid[A] = new Monoid[A => A] {
  	def op (f1: A => A, f2: A => A) = a => f2(f1(a))
  	def zero = a => a
  }

  // Exercise 4 is solved in MonoidSpec.scala

  def concatenate[A] (as: List[A], m: Monoid[A]): A =
    as.foldLeft (m.zero) (m.op)

  // Exercise 7
  //
  // Implement a productMonoid that builds a monoid out of two monoids. Test it
  // with scala check for instance by composing an Option[Int] monoid with a
  // List[String] monoid and running through our monoid laws.

  def productMonoid[A,B] (ma: Monoid[A]) (mb: Monoid[B]) = new Monoid[(A,B)] {
    def op(t1: (A,B), t2: (A,B)) = (ma.op(t1._1, t2._1), mb.op(t1._2, t2._2))
    def zero = (ma.zero, mb.zero)
  }

}


trait Foldable[F[_]] {

  def foldRight[A,B] (as: F[A]) (z: B) (f: (A,B) => B): B
  def foldLeft[A,B] (as: F[A]) (z: B) (f: (B,A) => B): B
  def foldMap[A,B] (as: F[A]) (f: A => B) (mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  // Exercise 9 (CB 10.15)

  def toList[A] (fa: F[A]) :List[A] = 
    foldRight[A, List[A]] (fa) (Nil) ((a, acc) => a::acc)
}

// Exercise 8 (CB 10.12 We just do Foldable[List])

object Foldable extends Foldable[List] {

  def foldRight[A,B] (as: List[A]) (b: B) (f: (A,B) => B): B = 
    as.foldRight (b) (f)

  def foldLeft[A,B] (as: List[A]) (b: B) (f: (B,A) => B): B = 
    as.foldLeft (b) (f)

  // using foldLeft would not be a good idea for Streams (there I would use
  // foldRight)
  def foldMap[A,B] (as: List[A]) (f: A => B) (mb: Monoid[B]): B = 
    as.map(f).foldLeft(mb.zero)(mb.op)
}

// vim:cc=80:tw=80
