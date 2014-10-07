package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Left(e) => Left(e)
   case Right(a) => Right(f(a))
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Left(ee) => Left(ee)
   case Right(a) => f(a) 
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Left(ee) => b
   case Right(a) => Right(a)
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
   case Left(e) => Left(e)
   case Right(a) => b map (bb => f(a, bb))
 }

}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def traverse[E, A, B](alist: List[A])(f: A => Either[E, B]): Either[E, List[B]] = alist match {
   case Nil => Right(Nil)
   case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
  }

  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] = as match {
    case Nil => Right(Nil)
    case h :: t => h.map2(sequence(t))(_ :: _)
  }

  def sequenceViaTraverse[E, A](as: List[Either[E, A]]): Either[E, List[A]] = {
    traverse[E, Either[E, A], A](as)((x : Either[E, A]) => x)
  }

  def test1() : Unit = {
    val e1 = Right("Rama")
    val e2 = Right("Rao")
    val e3 = Right("Kocherlakota")
    val eList = List(e1, e2, e3)
    println(sequence(eList))
    val eList2 = List(e1, e2, Left("oops"), e3)
    println(sequenceViaTraverse(eList2))
  }

}
