sealed trait LList[+A]
case object NNil extends LList[Nothing]
case class Cons[+A](head: A, tail: LList[A]) extends LList[A]

object LList {

  def apply[A](as: A*): LList[A] =
    if (as.isEmpty) 
      NNil
    else 
      Cons(as.head, apply(as.tail: _*))

  def concat[A](l1: LList[A],l2: LList[A]): LList[A] = l1 match {
    case NNil => l2
    case Cons(elem, tail) => Cons(elem, concat(tail,l2))
  }

  def append[A](a1: LList[A], a2: LList[A]): LList[A] = a1 match {
    case NNil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def foldRight[A,B](l: LList[A], z: B)(f: (A, B) => B): B = l match {
    case NNil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  @annotation.tailrec
  def foldLeft[A,B](l: LList[A], z: B)(f: (B, A) => B): B = l match {
    case NNil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def map[A,B](l: LList[A])(f: A => B): LList[B] = 
    foldRight(l,NNil:LList[B])( (a,b) => Cons(f(a), b) )

  def flatMap[A,B](l: LList[A])(f: A => LList[B]): LList[B] =
    foldRight(l,NNil:LList[B])( (a,b) => concat(f(a), b) )  

}

import LList._
