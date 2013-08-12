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



sealed trait Either[+E, +A]
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
