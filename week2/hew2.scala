package fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x,xs) => x + sum(xs)
	}
	
	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x,xs) => x * product(xs)
	}
	
	def apply[A](as: A*): List[A] =
	if (as.isEmpty) Nil
	else Cons(as.head, apply(as.tail: _*))
	
	def tail[A](xs: List[A]): List[A] = xs match {
		case Nil => Nil
		case Cons(h,t) => t
	}

	def setHead[A](xs: List[A], newHead: A): List[A] = xs match {
		case Nil => Cons(newHead, Nil)
		case Cons(h,t) => Cons(newHead, t)
	}

	def drop[A](xs: List[A], n: Int): List[A] = (xs, n) match {
		case (Nil, _) => Nil
		case (ys, 0) => ys
		case (Cons(h,t),  i) => drop(t, i-1)
	}

	def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
		case Nil => Nil
		case Cons(h,t) => {
			if(f(h)) dropWhile(t, f)
			else Cons(h,t)
		}
	}

	def init[A](xs: List[A]): List[A] = xs match {
		case Nil => Nil
		case Cons(h, Nil) => Nil
		case Cons(h,t) => Cons(h, init(t))
	}

	def dropWhile2[A](xs: List[A])(f: A => Boolean): List[A] = xs match {
		case Nil => Nil
		case Cons(h,t) if f(h) => dropWhile2(t)(f)
		case _ => xs
	}

	def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
	as match {
		case Nil => z
		case Cons(x, xs) => f(x, foldRight(xs, z)(f))
	}

	def sum2(ns: List[Int]) =
		foldRight(ns, 0)((x,y) => x + y)

	def product2(ns: List[Double]) =
		foldRight(ns, 1.0)(_ * _)

	def length[A](as: List[A]): Int =
		foldRight(as, 0)((x,y) => y + 1)

	def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
		@annotation.tailrec
		def go(bs: List[A], res: B): B = bs match {
			case Nil => res
			case Cons(h,t) => go(t, f(res, h))
		}
		go(as, z)
	}

	def sum3(ns: List[Int]) =
		foldLeft(ns, 0)((x,y) => x + y)

	def product3(ns: List[Double]) =
		foldLeft(ns, 1.0)(_ * _)

	def length3[A](as: List[A]): Int =
		foldLeft(as, 0)((x,y) => x + 1)

}