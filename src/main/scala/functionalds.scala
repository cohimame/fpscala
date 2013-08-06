sealed trait LList[+A]
case object NNil extends LList[Nothing]
case class Cons[+A](head: A, tail: LList[A]) extends LList[A]

object LList {

  def apply[A](as: A*): LList[A] =
    if (as.isEmpty) 
      NNil
    else 
      Cons(as.head, apply(as.tail: _*))


	def sum(ints: LList[Int]): Int = ints match {
  	case NNil => 0
  	case Cons(x,xs) => x + sum(xs)
	}

	def product(ds: LList[Double]): Double = ds match {
  	case NNil => 1.0
  	case Cons(0.0, _) => 0.0
  	case Cons(x,xs) => x * product(xs)
	}

}

import LList._


val example = Cons(1, Cons(2, NNil))
val example2 = LList(1,2,3)
val total = sum(example)