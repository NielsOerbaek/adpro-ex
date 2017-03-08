// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

package fpinscala.laziness

import Stream._

sealed trait Stream[+A] {

  def headOption () :Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def tail :Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
      case Empty => z
      case Cons (h,t) => f (h(), t().foldRight (z) (f))
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
      case Empty => z
      case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

  def exists (p : A => Boolean) :Boolean = this match {
      case Empty => false
      case Cons (h,t) => p(h()) || t().exists (p)
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }


  //exercise 2
  def toList :List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h()::t().toList
  }

  //exercise 3
  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => cons(h(), t().take(n-1))
    case _ => Empty
  } 

  def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => t().drop(n-1)
    case Empty => Empty
    case _ => this
  }

  //exercise 4
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  //exercise 5
  def forAll(p: A => Boolean): Boolean = 
    !this.exists(a => !p(a))

  //exercise 6
  def takeWhile2(p: A => Boolean): Stream[A] = 
    this.foldRight[Stream[A]] (Empty) ((a, acc) => if(p(a)) cons(a, acc) else Empty) 

  //exercise 7
  def headOption2 (): Option[A] = 
    this.foldRight[Option[A]] (None) ((a, acc) => Some(a))

  //exercise 8
  def map[B](f: A => B): Stream[B] =
    this.foldRight[Stream[B]] (Empty) ((a, acc) => cons(f(a), acc))

  def filter(p: A => Boolean): Stream[A] = 
    this.foldRight[Stream[A]] (Empty) ((a,acc) => if(p(a)) cons(a,acc) else acc)

  def append[B >: A](that: => Stream[B]) = 
    this.foldRight[Stream[B]] (that) ((a, acc) => cons(a,acc))

  def flatMap[B](f: A => Stream[B]) = 
    this.foldRight[Stream[B]] (Empty) ((a,acc) => f(a) append acc)

  //exercise 9
  def find (p :A => Boolean) :Option[A] = this.filter (p).headOption

  //On lists, filter runs through the entire list, because it is eager (strict).
  //For streams, filter is lazy and so no computation is performed until headOption
  // is called, and then only the needed values (until first match) computes.

  //exercise 13
  def map1[B](f: A => B): Stream[B] = 
    unfold (this) (xs => xs.headOption.map(h => (f(h), xs.tail)))

  //Can we do this with out the if-statement?
  def take1(n: Int): Stream[A] = 
    unfold[A, (Stream[A], Int)] (this,n) (state => 
        if(state._2 > 0) 
          state._1.headOption.map(h => (h, (state._1.tail, state._2 - 1))) 
        else None
      )

  def takeWhile1(p: A => Boolean): Stream[A] = 
    unfold (this) (xs => xs.headOption.filter(p).map(h => (h, xs.tail)))

  def zipWith[B,C] (f: (=> A, => B) => C) (that: Stream[B]) : Stream[C] =
    unfold (this, that) (state =>
        for {
          thisH <- state._1.headOption
          thatH <- state._2.headOption
        } yield (f(thisH, thatH), (state._1.tail, state._2.tail))
      )
      
  //The result of the computation in exercise 13 should be a list of 10 x true
}


case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]


object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq

  def to(n: Int): Stream[Int] = {
    def go(i: Int): Stream[Int] = {
      if(i == n) empty
      else cons(i, go(i+1))
    }
    go(1);
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  //exercise 10
  def fibs() : Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] = {
      cons(n2, go(n2, n1+n2))
    }
    cons(0, go(0,1))
  }

  //exercise 11
  def unfold[A,S] (state: S) (f: S => Option[(A,S)]) : Stream[A] =
    f(state).map( x => cons(x._1, unfold (x._2) (f)) ).getOrElse(Empty)

  //exercise 12
  def from1(n: Int): Stream[Int] = 
    unfold (n) (x => Some((x,x+1)))

  def fibs1(): Stream[Int] =
    unfold (0,1) (x => Some( x._1, (x._2, (x._1 + x._2) ) ) )

}

// vim:tw=0:cc=80:nowrap
