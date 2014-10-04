package fpinscala.datastructures


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A]( t : Tree[A] ) : Int = t match {
    case Leaf(value) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def maximum( t : Tree[Int] ) : Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  def depth[A]( t : Tree[A] ) : Int = t match {
    case Leaf(value) => 0
    case Branch(left, right) => 1 + depth(left).max(depth(right))
  }

  def map[A,B]( t : Tree[A] ) (f:A=>B): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A,B] (t: Tree[A]) (leafFunc:A => B) (branchFunc : (B, B) => B) : B = t match {
    case Leaf(value) => leafFunc(value)
    case Branch(left, right) => branchFunc(fold(left)(leafFunc)(branchFunc), fold(right)(leafFunc)(branchFunc))
  }

  def sizeViaFold[A](t : Tree[A]) = 
    fold(t)(x=>1) ((x,y) => x + y + 1)

  def depthViaFold[A](t : Tree[A]) : Int = 
    fold(t) (x=>0) ((x,y) => x.max(y) + 1)

  def maximumViaFold(t : Tree[Int]) = 
    fold(t) (x=>x) ((x,y) => x.max(y))

  def mapViaFold[A,B]( t : Tree[A] ) (f:A=>B): Tree[B] =  {
    def branchFunc (x : Tree[B], y : Tree[B]) : Tree[B] = Branch(x, y)
    def leafFunc(x : A) : Tree[B] = Leaf(f(x))
    fold(t) (leafFunc) (branchFunc)
  }

}
