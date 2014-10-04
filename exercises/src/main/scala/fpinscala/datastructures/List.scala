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

  def appendNotFold[A](a1: List[A], a2: List[A]): List[A] =
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
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => List(h)
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => {
      if (n <= 1) 
        xs
      else
        drop(xs, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => {
      if (f(x)) 
        dropWhile(xs, f)
      else
        l
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil 
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((x,y) => 1+y)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {

    def _foldLeft(ll : List[A], acc : B) : B = ll match {
      case Nil => acc
      case Cons(x, xs) => _foldLeft(xs, f(acc, x))
    }

    _foldLeft(l, z)
  }

  def reverse[A](l : List[A]) = {
    foldLeft(l, Nil:List[A])((x, y) => Cons(y, x))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((x, y) => Cons(y, x))

  def appendRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x, y) => Cons(x, y))

  def concat[A](ls : List[List[A]]) : List[A] = {
    foldLeft(ls, Nil: List[A])((l1, l2) => append(l1, l2))
  }

  def concatRight[A](ls : List[List[A]]) : List[A] = {
    foldRight(ls, Nil: List[A])(appendRight)
  }

  def mapAdd1(as : List[Int]) = {
    foldRight(as, Nil: List[Int]) ((x, y) => Cons(x+1, y))

//    as match {
//      case Nil => Nil
//      case Cons(x, xs) => Cons(x + 1, mapAdd1(xs))
//    }
  }

  def mapToString(as : List[Double]) = {
    foldRight(as, Nil: List[String]) ((x, y) => Cons(x.toString, y))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((x, y) => Cons(f(x), y))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A]) ((x, xs) => if (f(x)) Cons(x, xs) else xs)
  }

  def flatMap[A,B](as: List[A]) (f: A=>List[B]) : List[B] = {
    foldRight(as, Nil: List[B]) ((x, xs) => append(f(x), xs))
  }

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(x => if (f(x)) List(x) else Nil)
  }

  def zipWith[A, B, C](xs : List[A], ys : List[B])(f:(A, B) => C) : List[C] =  {

    @annotation.tailrec
    def _zipWith(as : List[A], bs : List[B], cs : List[C]) : List[C] = (as, bs) match {
      case (Nil, _) => cs
      case (_, Nil) => cs
      case (Cons(a, at), Cons(b, bt)) => _zipWith(at, bt, Cons(f(a, b), cs))
    }

    _zipWith(xs, ys, Nil: List[C])
  }

  @annotation.tailrec
  def isSubList[A](sub:List[A], sup: List[A]) : Boolean = {

      def initMatchCount[B](bs : List[B], b : B) : Int = {

        @annotation.tailrec
        def _initMatchCount(ys : List[B], count : Int) : Int = ys match {
          case Nil => count
          case Cons(y, yt) => {
            if (y == b) {
              _initMatchCount(yt, count+1)
            }
            else {
              count
            }
          }
        }
      _initMatchCount(bs, 0)
    }

    def isPrefix(sub1:List[A], sup1: List[A]) : Boolean = {
      val agrees : List[Boolean] = zipWith(sub1, sup1) ( _ == _ )
      initMatchCount(agrees, true) == length(sub1)
    }

    if (isPrefix(sub, sup)) {
      true
    }
    else {
      sup match {
        case Nil => false
        case Cons(h, t) => isSubList(sub, t)
      }
    }
  }

}
