import java.util.concurrent._
import scala.language.implicitConversions

// Work through the file top-down, following the exercises from the week's
// sheet.  Uncomment and complete code fragments.

object Par {

  type Par[A] = ExecutorService => Future[A]
  def run[A] (s: ExecutorService) (a: Par[A]) : Future[A] = a(s)


  case class UnitFuture[A] (get: A) extends Future[A] {
    def isDone = true
    def get (timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel (evenIfRunning: Boolean) : Boolean = false
  }

  def unit[A] (a: A) :Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A,B,C] (a: Par[A], b: Par[B]) (f: (A,B) => C) : Par[C] =
    (es: ExecutorService) => {
      val af = a (es)
      val bf = b (es)
      UnitFuture (f(af.get, bf.get))
    }

  def fork[A] (a: => Par[A]) : Par[A] = es => es.submit(
    new Callable[A] { def call = a(es).get }
  )

  def lazyUnit[A] (a: =>A) : Par[A] = fork(unit(a))

  // Exercise 1 (CB7.4)

  def asyncF[A,B] (f: A => B) : A => Par[B] = 
    a => lazyUnit(f(a))

  // map is shown in the book

  def map[A,B] (pa: Par[A]) (f: A => B) : Par[B] =
    map2 (pa,unit (())) ((a,_) => f(a))

  // Exercise 2 (CB7.5)

  def sequence[A] (ps: List[Par[A]]): Par[List[A]] = 
    ps.foldRight[Par[List[A]]] (unit(Nil)) ((p, acc) => map2(p,acc)(_::_))

  // Exercise 3 (CB7.6)

  // this is shown in the book:

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  //I looked in the answers online to get this solution, because i get trying to
  //figure out a way to do it with the functions we've had presented, but couldn't
  //make the types work. But then apparently it was because you needed to use flatten
  //in the list of lists. 
  //So in the first line we make the list into a List[Par[List[A]]], and then
  //we use sequence to make it to a Par[List[List[A]]], and finally we use
  //flatten to get a Par[List[A]].
  //It doesn't feel very elegant.
  //I like that the name of your variable is pls.
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val pls = as.map(asyncF(a => if(f(a)) List(a) else List()))
    map (sequence(pls)) (_.flatten)
  }

  // oohhhh bros i just got it ! !!! ! !! magic flatten is magic
  // oh. I had to rewrite it a bunch now it is not as cool...
  // hmm now i got it slightly shorter again
  def parFilterMaybe[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    map (parMap (as) (a => Some(a).filter(f))) (_.flatten)
  }

  //From lecture 16/03/2017
  def parForall[A] (as: List[A]) (p: A => Boolean): Par[Boolean] = fork {
    var bools = as.map(asyncF(p))
    map (sequence(bools)) ( _.foldLeft (true) (_ && _))
  }
      
  // Exercise 4: implement map3 using map2

  def map3[A,B,C,D] (pa :Par[A], pb: Par[B], pc: Par[C]) (f: (A,B,C) => D) = {
    val pcurriedf = map2[A, B, C => D] (pa, pb) ((a,b) => c => f(a,b,c))
    map2 (pcurriedf, pc) (_(_)) //This looks weeeeeird: (_(_))
  }

  // shown in the book

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  // Exercise 5 (CB7.11)

  def choiceN[A] (n: Par[Int]) (choices: List[Par[A]]) :Par[A] = 
    es => choices (run(es)(n).get) (es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    choiceN ( map(cond)(if(_) 0 else 1) ) ( List(t,f) )
  }

  // Exercise 6 (CB7.13)

  def chooser[A,B] (pa: Par[A]) (choices: A => Par[B]): Par[B] = 
    es => choices (run(es)(pa).get) (es)

  def choiceNViaChooser[A] (n: Par[Int]) (choices: List[Par[A]]): Par[A] =
    chooser (n) (choices(_))

  def choiceViaChooser[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]): Par[A] =
    chooser (cond) (if(_) t else f)

  // Exercise 7 (CB7.14)

  //For good orders sake
  def flatMap[A,B] (pa: Par[A]) (f: A => Par[B]): Par[B] = 
    es => f (run(es)(pa).get) (es)

  //Does this work? How do i test it?
  // Yeah this one confuses me a bit too...
  def join[A] (a : Par[Par[A]]) :Par[A] = 
    es => run (es) ( run(es)(a).get() )

  // flatMap via join
  def flatMapViaJoin[A,B] (pa: Par[A]) (f: A => Par[B]): Par[B] = fork {
    join(map(pa)(a => f(a)))
  }

  // join via flatMap
  def joinViaFlatMap[A] (ppa: Par[Par[A]]): Par[A] = fork {
    flatMap(ppa)(pa => pa)
  }

  class ParOps[A](p: Par[A]) {

  }

  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)
}
