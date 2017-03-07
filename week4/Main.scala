// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// A script meant to be loaded into REPL (scala -i Main.scala)

import fpinscala.laziness._
import fpinscala.laziness.Stream._

// this is how we do simple interactive testing

val l1 :Stream[Int] = Empty
val l2 :Stream[Int] = empty

val l3 :Stream[Int]= cons(1, cons(2, cons (3, empty)))
val l4 :Stream[Int]= cons(4, cons(5, cons (6, empty)))

//println (l1.headOption)
//println (l2.headOption)
//println (l3.headOption)

val naturals = from(1)

val l3l = l3.toList

val s1 = to(10)

val s1l = s1.toList

val n1t = naturals.take(1000000000).drop(41).take(10).toList;

val n2t = naturals.takeWhile(_<1000000000).drop(100).take(50).toList

assert(!naturals.forAll(_ < 0))

assert(!naturals.forAll(_ < 10))

val n3t = naturals.takeWhile2(_<1000000000).drop(100).take(50).toList

assert(l1.headOption2 == None)
assert(l2.headOption2 == None)
assert(l3.headOption2 == Some(1))
assert(naturals.headOption2 == Some(1))
assert(naturals.drop(5).headOption2 == Some(6))

val f1t = naturals.map (_*2).drop (30).take (50).toList
val f2t = naturals.drop(42).filter (_%2 ==0).take (30).toList
val f3t = naturals.append (naturals);
val f3t2 = naturals.take(10).append(naturals).take(20).toList
//val f4t = naturals.flatMap (to _).take (100).toList
//val f4t2 = naturals.flatMap (x => from(x)).take(100).toList

val naturals1 = unfold (1) (x => Some((x,x+1)))

assert(naturals.take(10).toList == naturals1.take(10).toList)

assert(from(1).take(1000000000).drop (41).take(10).toList == from1(1).take(1000000000).drop (41).take(10).toList)

assert(fibs1.take(100).toList ==fibs.take(100).toList)

println(naturals.map1 (_*2).drop (30).take (50).toList)

println(naturals.take1(10).toList)

println(naturals.takeWhile1(_ <= 10).toList)

println(naturals.zipWith[Int,Int] (_+_) (naturals).take(2000000000).take(20).toList)
