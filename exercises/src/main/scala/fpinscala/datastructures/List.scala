package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

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


  def tail[A](l: List[A]): List[A] = sys.error("todo")

  def setHead[A](l: List[A], h: A): List[A] = sys.error("todo")

  def drop[A](l: List[A], n: Int): List[A] = sys.error("todo")

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = sys.error("todo")

  def init[A](l: List[A]): List[A] = sys.error("todo")

  def length[A](l: List[A]): Int = sys.error("todo")

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {

    @annotation.tailrec
    def _foldLeft(ll: List[A], acc: B) : B = ll match {
      case Nil => acc
      case Cons(x, xs) => _foldLeft(xs, f(acc, x))
    }

    _foldLeft(l, z)
  }

  def head[A] (l : List[A]) : A = l match {
    case Nil => throw new Exception("oops, no head")
    case Cons(x, xs) => x
  }

  def reverse[A](l : List[A]) : List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => append(reverse(xs), List(x))
  }

  def reverse2[A](l : List[A]) : List[A] = {
    foldRight(l, Nil:List[A])((x :A , y : List[A]) => append(y, List(x)))
  }

  def sumLeft(l : List[Int]) : Int =
    foldLeft(l, 0)(_ + _)

  def lengthLeft[A](l : List[A]) : Int =
    foldLeft(l, 0)((x,y) => x + 1)

  def foldRightViaLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((x, y) => f(y, x))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")

  def zipWith[A, B, C](as : List[A], bs : List[B])(f : (A, B) => C) : List[C] = {

    def _zipWith(xs : List[A], ys : List[B], zs : List[C]) : List[C] = (xs, ys) match {
      case (_, Nil) => reverse(zs)
      case (Nil, _) => reverse(zs)
      case (Cons(x, xt), Cons(y, yt)) => _zipWith(xt, yt, Cons(f(x, y), zs))
    }

    _zipWith(as, bs, Nil : List[C])
  }

  def addPairwise(as : List[Int], bs : List[Int]) : List[Int] = {

    def _addPairwise(xs : List[Int], ys : List[Int], zs : List[Int]) : List[Int] = (xs, ys) match {
      case (_, Nil) => reverse(zs)
      case (Nil, _) => reverse(zs)
      case (Cons(x, xt), Cons(y, yt)) => _addPairwise(xt, yt, Cons(x + y, zs))
    }

    _addPairwise(as, bs, Nil : List[Int])
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]) : Boolean = {

    def isPrefix(all: List[A], prefix: List[A]) : Boolean = all match {
      val check = zipWith(all, prefix)(_ == _)

    }
  }
}
