// Advanced Programming
// Andrzej Wasowski, IT University of Copenhagen

package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them :)

import stream00._    // uncomment to test the book solution
// import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption

// here's our test exception that goes boom.
case class Bomb() extends java.lang.Exception("Boom!")

// pirate my pad http://piratepad.net/9t4ATxObgt

class StreamSpecNroeMaumNiec extends FlatSpec with Checkers {

  import Stream._

  //niels c really wanted to build an infinite unit stream that throws bombs, so here goes:
  def infiniteBombStream: Stream[Unit] = cons({throw new Bomb}, infiniteBombStream)

  behavior of "headOption"

  // a scenario test:

  it should "return None on an empty Stream (01)" in {
    assert(empty.headOption == None)
  }

  // An example generator of random finite non-empty streams
  def list2stream[A] (la :List[A]): Stream[A] = la.foldRight (empty[A]) (cons[A](_,_))

  // In ScalaTest we use the check method to switch to ScalaCheck's internal DSL
  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] =
    for { la <- arbitrary[List[A]] suchThat (_.nonEmpty) }
    yield list2stream (la)

  // a property test:

  it should "return the head of the stream packaged in Some (02)" in check {
    // the implict makes the generator available in the context
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    
    ("singleton" |:
      Prop.forAll { (n :Int) => cons (n,empty).headOption == Some (n) } ) &&
    ("random" |:
      Prop.forAll { (s :Stream[Int]) => s.headOption != None } )
  }

  it should "not force the tail of the stream (03)" in {
    val explodingStream = cons(1 + 0, cons({throw new Bomb; 3}, empty))
    assert(explodingStream.headOption == Some(1))
  }
  
  behavior of "take"

  it should "should not force any heads nor any tails (04)" in check {
    Prop.forAll { (n: Int) => { infiniteBombStream.take(n); empty == empty } }
  } 

  it should "not ever force the (n+1)st head ever" in check {
    def nNumbersThenBombs(n: Int): Stream[Int] = {
      if(n == 0) infiniteBombStream.map(x => 1)
      else cons(n, nNumbersThenBombs(n - 1))
    }
    
    Prop.forAll { (n: Int) => n < 0 || nNumbersThenBombs(n).take(n).toList.length == n }
  }

}
