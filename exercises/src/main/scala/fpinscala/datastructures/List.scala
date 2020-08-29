package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead of empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
     case Nil => Nil
     case Cons(_, rst) => drop(rst, n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if(f(x)) dropWhile(xs, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons (x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, i) => i +1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B) : B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z,h))(f)
    }

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))
    //foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)


  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def appendViaFoldRight[A](l: List[A], l2: List[A]): List[A] = foldRight(l, l2)((a,b) => Cons(a, b))

  def appendViaFoldLeft[A](l: List[A], l2: List[A]): List[A] = foldLeft(reverse(l), l2)((a,b) => Cons(b, a))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  def transform(l: List[Int]): List[Int] = foldRight(l, Nil:List[Int])((b,l) => Cons(b + 1, l))

  def convertToStringList[A](l: List[A]): List[String] = foldRight(l, Nil:List[String])((a,b) => Cons(a.toString,b))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((a,b) => Cons(f(a),b))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, Nil:List[A])((a,b) => if(f(a)) Cons(a,b) else b)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil:List[B])((a, b) => append(f(a), b))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if(f(a)) List(a) else Nil)

  def addTwoList(l: List[Int], l2: List[Int]): List[Int] = (l, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(a, ta), Cons(b, tb)) => Cons(a + b, addTwoList(ta, tb))
  }

  def zipWith[A,B,C](l: List[A], l2: List[B])(f: (A,B) => C): List[C] = (l, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(a, ta), Cons(b, tb)) => Cons(f(a,b), zipWith(ta, tb)(f))
  }

  def hasSubsequence[A](l: List[A], l2: List[A]): Boolean = ???

}
