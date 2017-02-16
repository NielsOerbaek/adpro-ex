// Advanced Programming 2017,
// A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1: Niels Ørbæk Christensen, niec@itu.dk
// AUTHOR2:
//
// Write names and ITU email addresses of both group members that contributed to
// the solution of the exercise (in alphabetical order by family name).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled as follows:
//
// scalac Exercises.scala
//
// or
//
// fsc Exercises.scala
//
// To run the compiled file do "scala Exercises"
//
// To load the file int the REPL start the 'scala' interpreter and issue the
// command ':load Exercises.scala'. Now you can interactively experiment with
// your code.
//
// Continue solving exercises in the order presented in the PDF file. Large
// parts of the file are commented in order to make sure that the exercise
// compiles.  Uncomment and complete fragments as you proceed.  The file shall
// always compile and run after you are done with each exercise (if you do them
// in order).  Please compile and test frequently.

// An ADT of Lists

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  // Exercise 1
  // The third case matches and the result will be (1 + 2) = 3

  // override function application to provide a factory of lists (convenience)

  def apply[A](as: A*): List[A] = // Variadic function
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(h,t) => t
  }

  // Exercise 3

  def setHead[A](xs: List[A], newHead: A): List[A] = xs match {
    case Nil => Cons(newHead, Nil)
    case Cons(h,t) => Cons(newHead, t)
  }


  // Exercise 4

  def drop[A](xs: List[A], n: Int): List[A] = (xs, n) match {
    case (Nil, _) => Nil
    case (ys, 0) => ys
    case (Cons(h,t),  i) => drop(t, i-1)
  }

  // Exercise 5

  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case Cons(h,t) => {
      if(f(h)) dropWhile(t, f)
      else Cons(h,t)
    }
  }

  // Exercise 6

  def init[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h,t) => Cons(h, init(t))
  }

  // Exercise 7

  def foldRight[A,B] (as :List[A], z: B) (f : (A,B)=> B) :B = as match {
    case Nil => z
    case Cons (x,xs) => f (x, foldRight (xs,z) (f))
  }

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((x,y) => y + 1)

  // Exercise 8

  @annotation.tailrec
  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B) : B = as match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  // Exercise 9

  def sumL(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def productL(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def lengthL[A](as: List[A]): Int =
    foldLeft(as, 0)((x,y) => x + 1)

  // Exercise 10

  def reverse[A] (as :List[A]) :List[A] = 
    foldLeft(as, List[A]())((x, y) => Cons(y, x))

  // Exercise 11

  def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B) : B = 
    foldLeft(reverse(as),z)((x,y) => f(y,x))

  def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B) : B = 
    foldRight[A, B=>B](as, (b => b))((x,g) => y => g(f(y,x)))(z)

  // Exercise 12

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def concat[A] (as: List[List[A]]) :List[A] = 
    foldRight1(as, List[A]())((a1, a2) => append(a1,a2))

  // Exercise 13

  def filter[A] (as: List[A]) (f: A => Boolean) : List[A] = as match {
    case Nil => Nil
    case Cons(h,t) if f(h) => Cons(h,filter(t)(f))
    case Cons(h,t) => filter(t)(f)
  }

  def filterTest(): Unit = {
    val l = List(1,2,3,4,5,6,7)
    println(filter(l)(x => (x%2) == 0))
  }
  
  // Exercise 14

  def flatMap[A,B](as: List[A])(f: A => List[B]) : List[B] = 
    foldRight1(as, List[B]())((a1, a2) => append(f(a1),a2))

  def flatMapTest(): Unit = 
    println( flatMap (List(1,2,3)) (i =>List(i,i)))

  // Exercise 15

  def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] = 
    flatMap(l)(x => {
      if(p(x)) List(x)
      else Nil
      })

  def filter1Test(): Unit = {
    val l = List(1,2,3,4,5,6,7)
    println(filter1(l)(x => (x%2) == 0))
  }

  // Exercise 16

  def add (l: List[Int]) (r: List[Int]): List[Int] = (l,r) match {
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, add(t1)(t2))
    case _ => Nil
  }

  def addTest() : Unit = 
    println(add(List(1,2,3))(List(4,5,6,7)))

  // Exercise 17

  def zipWith[A,B,C] (f : (A,B)=>C) (l: List[A], r: List[B]) : List[C] = 
    (l,r) match {
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(f)(t1,t2))
      case _ => Nil
    }

  def zipWithTest() : Unit = 
    println(zipWith((x:Int,y:Int) => (x+y))(List(1,2,3),List(4,5,6,7)))

  // Exercise 18

  def hasSubsequence[A] (sup: List[A], sub: List[A]) : Boolean = {
    def checkSeq(a: List[A], b: List[A]): Boolean = (a,b) match {
      case (_, Nil) => true
      case (Cons(h1,t1), Cons(h2,t2)) if (h1 == h2) =>
        checkSeq(t1,t2)
      case _ => false
    }
    sup match {
      case x if checkSeq(x, sub) => true
      case Cons(h,t) => hasSubsequence(t,sub)
      case Nil => false 
    }
  }

  // Exercise 19

  def pascal (n :Int) : List[Int] = {
    def go(thisList: List[Int], lastList: List[Int], nLeft: Int): List[Int] = 
      (thisList, lastList, nLeft) match {
        case (x, Cons(a, Cons(b, c)), _) => 
          go(Cons((a+b),x), Cons(b,c), nLeft)
        case (x, Cons(a, b), 0) => Cons(a, thisList)
        case (x, Cons(a,b), _) => 
          go(List(1), Cons(a,x), nLeft-1)
        case(x, Nil, 0) => x
        case (x, Nil, n) => 
          go(List(1), x, n-1) 
      }
    if(n==0) Nil
    else go(List(1),List[Int](),n-1)
  }

  // a test: pascal (4) = Cons(1,Cons(3,Cons(3,Cons(1,Nil))))

}
