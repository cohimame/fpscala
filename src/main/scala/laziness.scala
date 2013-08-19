
trait SStream[+A] {

  import SStream._

  def uncons: Option[(A, SStream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  def take(n: Int): SStream[A] = uncons match {
    case Some((a, sa)) if(n>0) => cons(a, sa.take(n-1))
    case _ => empty  
  }

  def takeWhile(f: A => Boolean): SStream[A] = uncons match {
    case Some((h,t)) if f(h) => cons(h, t takeWhile f)
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def map[B](f: A => B): SStream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t))

  def append[B>:A](s: SStream[B]): SStream[B] =
    foldRight(s)((h,t) => cons(h,t))
  
  def flatMap[B](f: A => SStream[B]): SStream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

  def filter[B](f: A => Boolean): SStream[A] =
    foldRight(empty[A])(
      (h,t) =>
              if (f(h)) cons(h, t)
              else t
    ) 

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)  
  
}

object SStream {
  def empty[A]: SStream[A] =
    new SStream[A] { def uncons = None }
  
  def cons[A](hd: => A, tl: => SStream[A]): SStream[A] =
    new SStream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): SStream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}

import SStream._

val stream: SStream[Int] = 
  cons(
    {println("1!");1}
    ,
    cons(
      {println("2!");2}
      ,
      cons(
        {println("3!");3}
        ,
        cons(
          {println("4!");4}
          ,
          cons(
            {println("5!");5}
            ,
            empty[Int]
          )
        )
      )
    )
  )


val scalaStream = {println("1!");1} #:: {println("2!");2}

//val processedS = stream.map(_ + 10).filter(_ % 2 == 0)


val ones: SStream[Int] = cons(1, ones)
