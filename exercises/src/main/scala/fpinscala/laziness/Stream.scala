package fpinscala.laziness

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = ???
  def takeViaUnfold(n: Int): Stream[A] = ???

  def drop(n: Int): Stream[A] = ???

  def takeWhile(p: A => Boolean): Stream[A] = ???
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = ???
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = ???
  def zipWithViaUnfold[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = ???
  def tails: Stream[Stream[A]] = ???
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = ???

  def forAll(p: A => Boolean): Boolean = ???
  def map[B](f: A => B): Stream[B] = ???
  def mapViaUnfold[B](f: A => B): Stream[B] = ???
  def filter(f: A => Boolean): Stream[A] = ???
  def append[B>:A](s: => Stream[B]): Stream[B] = ???

  def headOption: Option[A] = ???
  def headOptionViaFoldRight: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def flatMap[B](f: A => Stream[B]): Stream[B] = ???

  def startsWith[B](s: Stream[B]): Boolean = ???

  def toList: List[A] = ???
  def toListRecursive: List[A] = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  val fibs:Stream[Int] = ???
  val fibsViaUnfold: Stream[Int] = ???
  def fromViaUnfold(n: Int): Stream[Int] = ???

  def constantViaUnfold[A](a: A): Stream[Int] = ???

  // could also of course be implemented as constant(1)
  val onesViaUnfold:Stream[Int] = ???

  def empty[A]: Stream[A] = Empty
  def constant[A](a: A): Stream[A] = ???

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}