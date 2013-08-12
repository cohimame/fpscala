trait Optional[+A] {

  def foreach(f: A => Unit): Unit = this match {
    case Certain(a) => f(a)
    case Empty => () 
  }  

  def map[B](f: A => B): Optional[B] = this match {
    case Certain(a) => Certain(f(a))
    case Empty => Empty:Optional[B] 
  }  

  def flatMap[B](f: A => Optional[B]): Optional[B] = this match {
    case Certain(a) => f(a)
    case Empty => Empty:Optional[B]
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Certain(a) => a
    case Empty => default
  }

  def orElse[B >: A](ob: => Optional[B]): Optional[B] = this match {
    case some @ Certain(a) => some
    case Empty => ob
  }

  def filter(f: A => Boolean): Optional[A] = this match {
    case some @ Certain(a) if(f(a)) => some
    case Empty => Empty
  }
}
case class Certain[+A](get: A) extends Optional[A]
case object Empty extends Optional[Empty]

object Optional {

  def lift[A,B](f: A => B):Optional[A] =>Optional[B] = 
    optA => optA map f

  def lift2[A,B,C](f: (A,B) => C): (Option[A],Option[B]) =>Optional[C] =
    (optA, optB) => optA flatMap (oa => (optB map (ob => f(oa,ob))))

  def map2[A,B,C](a:Optional[A], b:Optional[B])(f: (A, B) => C):Optional[C] = lift2(f)(a,b)

  def sequence[A](a: List[Option[A]]):Optional[List[A]] = a match {
    case Nil =>Certain(Nil) 
    case head :: tail => head flatMap ( h => sequence(tail) map ( h :: _) ) 
  }

  def traverse[A, B](a: List[A])(f: A =>Optional[B]):Optional[List[B]] = 
    sequence (a map f) 

}



class Animal{def sound = "nothin' special"} 
class Dog extends Animal{override def sound = "bark"} 
class Cat extends Animal{override def sound = "mew"}

val noDog: Optional[Dog] = Empty
val someCat: Optional[Cat] = Certain(new Cat())

val pet = noDog orElse someCat // Optional[Animal]

noDog.foreach(s => println(s.sound))
pet.foreach(s => println(s.sound))



sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case l @ Left(_) => l
  }
  
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => f(a)
      case l @ Left(_) => l
    }


  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = 
    this match {
      case r @ Right(a) => r
      case l @ _ => b
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):Either[EE, C] =
    this flatMap(e1 => b map (e2 => f(e1,e2)) )
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


object Either {

  def sequence[E,R](a: List[Either[E,R]]):Either[E,List[R]] = a match {
    case Nil => Right(Nil) 
    case head :: tail => head flatMap ( h => sequence(tail) map ( h :: _) ) 
  }

  def traverse[A, B](a: List[A])(f: A =>Optional[B]):Optional[List[B]] = 
    sequence (a map f)
}

val num1: Either[String, Double] = Left("num1 arg err")
val num2: Either[String, Double] = Right(4.0)
val num3: Either[String, Double] = Right(2.5)

val map2apply = num1.map2(num2)((a,b)=>a*b)

import Either._

val eitherSeq = sequence List(num1,num2)


/*
sealed trait Partial[+A,+B]{
  ...

  def map2[EE >: E, B, C](b: Partial[EE, B])(f: (A, B) => C):Partial[EE, C] =
    this match {
      case suc @ Success(_) => suc flatMap(s1 => b map (s2 => f(e1,e2)) )
      case err @ Errors(_) => b1 map 
    }

  def map[B](f: A => B): Partial[E, B] = this match {
    case Success(a) => Success(f(a))
    case l @ Errors(_) => l
  }  
    

}
case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
case class Success[+B](get: B) extends Partial[Nothing,B]

object Partial {

  def sequence[E,R](a: List[Either[E,R]]):Either[E,List[R]] = a match {
    case Nil => Right(Nil) 
    case head :: tail => head flatMap ( h => sequence(tail) map ( h :: _) ) 
  }

  def traverse[A, B](a: List[A])(f: A =>Optional[B]):Optional[List[B]] = 
    sequence (a map f)
}


*/
