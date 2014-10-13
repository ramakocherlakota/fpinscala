package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {

    def _scanRight(as: Stream[A], acc : Stream[B]) : Stream[B] = as match {
      case Cons(h, t) => _scanRight(t(), foldRight(z)(f), acc)
      case _ => acc
    }

    _scanRight(this, empty[B])
  }

  def map[B](f: A => B) : Stream[B] = 
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def mapViaUnfold[B](f: A => B) : Stream[B] = {
    def _mapViaUnfold(l : Stream[A]) : Option[(B, Stream[A])] = l match {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

    unfold[B, Stream[A]](this)(_mapViaUnfold)
  }

  def takeWhileViaUnfold(p:A=>Boolean) : Stream[A] = {
    def _takeWhileViaUnfold(l: Stream[A]) : Option[(A, Stream[A])] = l match {
      case Cons(h, t) => if (p(h())) 
        Some((h(), t()))
      else
        None
      case _ => None
    }

    unfold[A, Stream[A]](this)(_takeWhileViaUnfold)
  }

  def zipWithAll[B, C](bs: Stream[B])(f: (Option[A], Option[B]) => C) : Stream[C] = {

    def _zipWithAll(pair : (Stream[A], Stream[B]) ) : Option[(C, (Stream[A], Stream[B]))] = pair match {
      case (xs, ys) => xs match {
        case Cons(xh, xt) => ys match {
          case Cons(yh, yt) => Some((f(Some(xh()), Some(yh())), (xt(), yt())))
          case _ => Some((f(Some(xh()), Option.empty[B]), (xt(), empty[B])))
        }
        case _ => ys match {
          case Cons(yh, yt) => Some((f(Option.empty[A], Some(yh())), (empty[A], yt())))
          case _ => None
        }
      }
    }

    unfold[C, (Stream[A], Stream[B])]((this, bs))(_zipWithAll)
  }

  def tails() : Stream[Stream[A]] = {
    def _tails(as: Stream[A]) : Option[(Stream[A], Stream[A])] = as match {
      case Cons(h, t) => Some((t(), t()))
      case _ => None
    }

    unfold[Stream[A], Stream[A]](this)(_tails)
  }

  def zipAll[B](bs: Stream[B]) : Stream[(Option[A], Option[B])] = {
    def f(ao : Option[A], bo : Option[B]) : (Option[A], Option[B]) = ao match {
      case Some(a) => bo match {
        case Some(b) => (Some(a), Some(b))
        case _ => (Some(a), Option.empty[B])
      }
      case _ => bo match {
        case Some(b) => (Option.empty[A], Some(b))
        case _ => (Option.empty[A], Option.empty[B])
      }
    }

    zipWithAll[B, (Option[A], Option[B])](bs)(f)
  }

  // this is wrong because it quits as soon as it runs out of elements from either stream
  def incorrectzipAll[B](bs: Stream[B]) : Stream[(Option[A], Option[B])] = {

    def f(a : A, b  : B) : (Option[A], Option[B]) = (Some(a), Some(b))

    zipWith[B, (Option[A], Option[B])](bs)(f)
  }

  def zipWith[B, C](bs : Stream[B])(f:(A, B) => C) : Stream[C] =  {

    // S == Stream[A[, Stream[B]
    def _zipWith(pair : (Stream[A], Stream[B]) ) : Option[(C, (Stream[A], Stream[B]))] = pair match {
      case (xs, ys) => xs match {
        case Cons(xh, xt) => ys match {
          case Cons(yh, yt) => Some((f(xh(), yh()), (xt(), yt())))
          case _ => None
        }
        case _ => None
      }
      case _ => None
    }

    unfold[C, (Stream[A], Stream[B])]((this, bs))(_zipWith)
  }

  def takeViaUnfold(n: Int) : Stream[A] = {
    def _takeViaUnfold(pair : (Int, Stream[A])) : Option[(A, (Int, Stream[A]))] = pair match {
      case (k, l) =>
        if (k <= 0)
          None
        else l match {
          case Cons(h, t) => Some((h(), (k-1, t())))
          case _ => None
      }
    }

    unfold[A, (Int, Stream[A])](n, this)(_takeViaUnfold)
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight[Stream[A]](empty[A]) ((h : A, t)  => if (p(h)) cons(h, t) else t)
  }

  def append[B >: A](b: Stream[B]) : Stream[B] = {
    foldRight[Stream[B]](b) ((h, t) => cons(h, t))
  }

  def flatMap[B] (f: A=>Stream[B]) : Stream[B] = {
    foldRight(empty[B]) ((x, xs) => f(x).append(xs))
  }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = {
    if (n <= 0)
      Empty
    else this match {
      case Empty => Empty
      case Cons(h, t) => cons(h(), t().take(n-1))
    }
  }

  def drop(n: Int): Stream[A] = {
    if (n <= 0)
      this
    else this match {
      case Empty => Empty
      case Cons(h, t) =>  t().drop(n-1)
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => {
      if (p(h())) {
        Cons(h, () => t().takeWhile(p))
      } else {
        empty
      }
    }
  }

  def takeWhileViaFoldRight(p: A=>Boolean) : Stream[A] = 
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean = {
    ! exists(x => !p(x))
  }

  def startsWith[B](s: Stream[B]): Boolean = s match {
    case Empty => true
    case Cons(h, t) => this match {
      case Empty => false
      case Cons(hh, tt) => if (hh() != h()) false else tt().startsWith(t())
    }
  }

  def headOption(): Option[A] = this match {
    case Cons(h, t) => Some(h())
    case _ => None
  }

  def tailOption() : Option[Stream[A]] = this match {
    case Cons(h, t) => Some(t())
    case _ => None
  }

  def headOptionViaFoldRight(): Option[A] = {
    def simple(a: A, b: => Option[A]):Option[A] = Some(a)
    foldRight(None:Option[A])(simple)
  }


}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def fibs(a0 : Int, a1: Int):Stream[Int] = {
    cons(a0, fibs(a1, a0 + a1))
  }

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }


  def my_unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {

    def _unfold(s: S) : Stream[(A, S)] = f(s) match {
      case Some((a, s2)) => cons((a, s), _unfold(s2))
      case _ => empty[(A, S)]
    }

    _unfold(z).map(_._1)
  }

  def fibsViaUnfold(a0 : Int, a1: Int) : Stream[Int] = {
    unfold[Int, (Int, Int)](a0, a1)(s => Some(s._1, (s._2, s._1 + s._2)))
  }

  def fromViaUnfold(n : Int) : Stream[Int] = {
    unfold[Int, Int](n)(k => Some(k, k+1))
  }

  def constantViaUnfold(n : Int) : Stream[Int] = {
    unfold[Int, Int](n)(k => Some(n, k))
  }

  def isPrime(n: Int) : Boolean =
    ! from(2).takeWhile(x => x * x <= n).exists(n % _ == 0)

  def primes: Stream[Int] = 
    from(2).filter(isPrime)

  def test() : Unit = {
    println(from(4).take(10).filter((i:Int) => i % 2 == 0).toList)
  }
}
