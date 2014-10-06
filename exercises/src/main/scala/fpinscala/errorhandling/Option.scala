package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = 
    map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => this
    case None => ob
  }


  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) => {
      if (f(a)) 
        this
      else
        None
    }
    case None => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]



object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = sys.error("todo")

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {

    def _sequence(l : List[Option[A]],  acc : Option[List[A]]) : Option[List[A]] = l match {
      case Nil => acc
      case h :: t => _sequence(t, map2(h, acc)((x,y)=> x :: y))
    }

    _sequence(a, Some(Nil))
  }

  // from hints, equivalent to
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

  def traverse[A, B](alist: List[A])(f: A => Option[B]): Option[List[B]] = alist match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t) (f)) (_ :: _)
  }


  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =     a flatMap (aa => b map (bb => f(aa, bb)))

  def test1() : Unit = {
    val option1 = Some("Rama")
    val option2 = Some("Rao")
    val option3 = Some("Kocherlakota")
    val optionList = List(option1, option2, option3)
    println(sequence(optionList))
    val optionList2 = List(option1, option2, None, option3)
    println(sequence(optionList2))
  }

  def Try[A](a: => A) : Option[A] = {
    try {
      Some(a)
    }
    catch { 
      case e:Exception=>None
    }
  }

  def test2() : Unit = {
    val optionList = List("1", "2", "3");
    println(traverse(optionList)(s  =>Try(s.toInt)))
    val optionList2 = List("1", "2", "Rama", "3");
    println(traverse(optionList2)(s  =>Try(s.toInt)))
  }

}
