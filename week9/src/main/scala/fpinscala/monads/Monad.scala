// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
package fpinscala.monads
import scala.language.higherKinds

trait Functor[F[_]] {

  def map[A,B] (fa: F[A]) (f: A => B) :F[B]

  def distribute[A,B] (fab: F[(A,B)]): (F[A],F[B]) =
    (map (fab) (_._1), map (fab)(_._2))

  def codistribute[A,B] (e :Either[F[A],F[B]]): F[Either[A,B]] = e match {
    case Left(fa) => map (fa) (Left(_))
    case Right(fb) => map (fb) (Right(_))
  }

}

object Functor {

  val ListFunctor = new Functor[List] {
    def map[A,B] (as: List[A]) (f: A => B): List[B] = as.map (f)
  }

  // Exercise 10

  val OptionFunctor = new Functor[Option] {
    def map[A, B] (oa: Option[A]) (f: A => B) = oa.map (f)
  }

}

trait Monad[F[_]] {

  def unit[A]  (a: => A): F[A]
  def flatMap[A,B] (ma: F[A]) (f: A => F[B]) :F[B]

  def map[A,B] (ma: F[A]) (f: A => B) :F[B] =
    flatMap (ma) (a => unit (f(a)))

  def map2[A, B, C] (ma: F[A], mb: F[B]) (f: (A,B) => C): F[C] =
    flatMap (ma) (a => map (mb) (b => f(a,b)))

  // Exercise 13 (CB11.3)

  def sequence[A] (lfa: List[F[A]]): F[List[A]] =
    lfa.foldRight[F[List[A]]] (unit(List())) ((fa, acc) => map2(fa, acc) (_::_)) 

  // traverse seems to simply sequence results of mapping.  I do not think that
  // it appeared in our part. You can uncomment it once you have sequence.
  def traverse[A,B] (la: List[A]) (f: A => F[B]): F[List[B]] = sequence(la.map (f))

  // Exercise 14 (CB11.4)

  // It makes a something of something into a something of a list of somethings.
  // In normal parlance, it turns each element in the monad into a list of that element,
  // replicated n times. For example, replicating three times Some(3) would result in
  // Some(List(3, 3, 3)); and replicating three times a list List(1,2,3) would result in
  // List(List(1,1,1),List(2,2,2),List(3,3,3)). A similar result would be achieved when
  // replicating a stream.
  def replicateM[A] (n: Int, ma: F[A]): F[List[A]] = 
    map(ma)(a => List.fill(n)(a))

  def join[A] (mma: F[F[A]]): F[A] = flatMap (mma) (ma => ma)

  // Exercise 15 is solved in MonadSpec.scala

  // Exercise 16 (CB11.7)
  def compose[A,B,C] (f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap (f(a)) (g)

  // Just for fun: Implementing flatMap using compose.
  def flatMap1[A,B] (ma: F[A]) (f: A => F[B]) :F[B] =   
    compose[Null, A, B]((notADamnThing => ma), f) (null)

}

object Monad {

  // Exercise 12 (CB11.1)

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A,B](oa: Option[A])(f: A => Option[B]) =
      oa.flatMap(f)
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A,B](as: List[A])(f: A => List[B]) =
      as.flatMap(f)
  }

}
