sealed trait LList[+A]
case object NNil extends LList[Nothing]
case class Cons[+A](head: A, tail: LList[A]) extends LList[A]

object LList {

  def apply[A](as: A*): LList[A] =
    if (as.isEmpty) 
      NNil
    else 
      Cons(as.head, apply(as.tail: _*))

  def tail[A](list: LList[A]):LList[A] = list match {
    case NNil => sys.error("tail of empty list")
    case elem :: tail => tail
  }

  def dropWhile[A](list: LList)(f: A => Boolean) = list match {
    case NNil => NNil
    case elem :: tail if f(elem) elem :: dropWhile(tail)(f)
  }
  
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }  

  def fRight[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l,NNil:List[A])( (a,b) => f(a) :: b ) 


}

import LList._


val example = Cons(1, Cons(2, NNil))
val example2 = LList(1,2,3)
val total = sum(example)