// Advanced Programming
// Andrzej Wasowski, IT University of Copenhagen

package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary
import java.lang.Exception

// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them :)

import stream00._    // uncomment to test the book solution
// import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption

// here's our test exception that goes boom.
case class Bomb() extends java.lang.Exception("Boom!")

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
    for { la <- arbitrary[List[A]] suchThat (_.nonEmpty)}
    yield list2stream (la)

  // the implict makes the generator available in the context
  implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

  // a property test:

  it should "return the head of the stream packaged in Some (02)" in check {
    ("singleton" |:
      Prop.forAll { (n :Int) => cons (n,empty).headOption == Some (n) } ) &&
    ("random" |:
      Prop.forAll { (s :Stream[Int]) => s.headOption != None } )

  }

  // A scenario test to see if headOption makes the stream explode if there
  // is a bomb in the tail
    it should "not force the tail of the stream (03)" in {
    val explodingStream = cons(1 + 0, cons({throw new Bomb; 3}, empty))
    assert(explodingStream.headOption == Some(1))
  }
  
  behavior of "take"

  val nonNegativeInts = for (n <- Gen.choose(0, 10000)) yield n 

  it should "should not force any heads nor any tails (04)" in check {
    Prop.forAll (nonNegativeInts) { (n: Int) => infiniteBombStream.take(n); true }
  } 

  
  it should "not ever force the (n+1)st head ever" in check {
    def nNumbersThenBombs(n: Int): Stream[Int] = {
      if(n == 0) infiniteBombStream.map(x => 1)
      else cons(n, nNumbersThenBombs(n - 1))
    }
    
    Prop.forAll (nonNegativeInts) { (n: Int) => nNumbersThenBombs(n).take(n).toList.length == n }
  }

  it should "be idempotent my friend (05)" in check {
    Prop.forAll (nonNegativeInts) { (n: Int) => from(0).take(n).take(n).toList == from(0).take(n).toList }
  }
  
  behavior of "drop"

  it should "be additive my friend (06)" in check {
    Prop.forAll (nonNegativeInts, nonNegativeInts, genNonEmptyStream[Int]) { (n: Int, m: Int, s: Stream[Int]) => s.drop(n).drop(m).toList == s.drop(n + m).toList }
  } 

  it should "not force any of the dropped elements heads (07)" in check {
    Prop.forAll (nonNegativeInts) { (n: Int) => infiniteBombStream.drop(n); true }
  } 

  it should "not force any dropped element heads, even if we force the tail (08)" in check {
    def nBombsThenNumbers(n: Int): Stream[Int] = {
      if(n == 0) from(0)
      else cons({throw new Bomb; 1}, nBombsThenNumbers(n - 1))
    }
       
    Prop.forAll (nonNegativeInts, nonNegativeInts) { (n: Int, m: Int) => nBombsThenNumbers(n).drop(n).take(m).toList; true }
  }

  behavior of "map"

  it should "map to the value with the identity function (09)" in check {
    Prop.forAll { (s: Stream[Int]) => s.map(s => s).toList == s.toList }
  }
    
  it should "terminate on infinite streams (10)" in {
    Prop.forAll (nonNegativeInts) { (n: Int) => from(0).map(_ + 1).take(n).toList == from(1).take(n).toList}
  }

  behavior of "append"
    
  it should "not force any values in the first stream (11)" in {
     assert({infiniteBombStream append from(0); true})
  }
  
  it should "not force any values in the last stream (12)" in {
    assert({from(0) append infiniteBombStream; true})
  }

  it should "result in a stream with the parts concatenated (13)" in check {
    Prop.forAll (nonNegativeInts, nonNegativeInts) { (n: Int, m: Int) => (from(0).take(n) append from(n).take(m)).toList == from(0).take(n + m).toList }
  }

}
