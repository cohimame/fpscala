sealed trait LList[+A]
case object NNil extends LList[Nothing]
case class Cons[+A](head: A, tail: LList[A]) extends LList[A]

object LList {

  def apply[A](as: A*): LList[A] =
    if (as.isEmpty) 
      NNil
    else 
      Cons(as.head, apply(as.tail: _*))

  def tail[A](LList: LList[A]):LList[A] = LList match {
    case NNil => sys.error("tail of empty LList")
    case elem :: tail => tail
  }

  def dropWhile[A](LList: LList)(f: A => Boolean) = LList match {
    case NNil => NNil
    case elem :: tail if f(elem) elem :: dropWhile(tail)(f)
  }
  
  def append[A](a1: LList[A], a2: LList[A]): LList[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def foldRight[A,B](l: LList[A], z: B)(f: (A, B) => B): B = l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A,B](l: LList[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }  

  def fRight[A,B](l: LList[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))

  def map[A,B](l: LList[A])(f: A => B): LList[B] = 
    foldRight(l,NNil:LList[A])( (a,b) => Cons(f(a), b) ) 


}

import LList._


val example = Cons(1, Cons(2, NNil))
val example2 = LList(1,2,3)
val total = sum(example)