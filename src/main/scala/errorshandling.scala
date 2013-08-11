trait OOption[+A] {

  def foreach(f: A => Unit): Unit = this match {
    case SSome(a) => f(a)
    case NNone => () 
  }  

  def map[B](f: A => B): OOption[B] = this match {
    case SSome(a) => SSome(f(a))
    case NNone => NNone:OOption[B] 
  }  

  def flatMap[B](f: A => OOption[B]): OOption[B] = this match {
    case SSome(a) => f(a)
    case NNone => NNone:OOption[B]
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case SSome(a) => a
    case NNone => default
  }

  def orElse[B >: A](ob: => OOption[B]): OOption[B] = this match {
    case some @ SSome(a) => some
    case NNone => ob
  }

  def filter(f: A => Boolean): OOption[A] = this match {
    case some @ SSome(a) if(f(a)) => some
    case NNone => NNone
  }
}

case class SSome[+A](get: A) extends OOption[A]
case object NNone extends OOption[Nothing]


class Animal{def sound = "nothin' special"} 
class Dog extends Animal{override def sound = "bark"} 
class Cat extends Animal{override def sound = "mew"}

val noDog: OOption[Dog] = NNone
val someCat: OOption[Cat] = SSome(new Cat())

val pet = noDog orElse someCat // OOption[Animal]

noDog.foreach(s => println(s.sound))
pet.foreach(s => println(s.sound))



def lift[A,B](f: A => B): Option[A] => Option[B] = 
  optA => optA map f

def lift2[A,B,C](f: (A,B) => C): (Option[A],Option[B]) => Option[C] =
  (optA, optB) => optA flatMap (oa => (optB map (ob => f(oa,ob))))

def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = lift2(f)(a,b)

def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
  case None :: t => None
  case Some
}
