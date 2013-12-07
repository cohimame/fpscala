package algebra

trait Monoid[A] {
  def op(a1:A,a2:A): A
  def zero:A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1:String,a2:String) = a1 + a2
    val zero = ""
  }

  def optMonoid[A] =
    new Monoid[Option[A]] {
      def op(a1:Option[A],a2:Option[A]) = a1 orElse a2
      def zero: Option[A] = None
    }

  def endoMonoid[A] =
    new Monoid[A => A] {
      def op(f1: A=>A, f2: A=>A) = f1 andThen f2
      def zero: A=>A = a => a
    }


  def concatenate[A](as: List[A], m: Monoid[A]):A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b,a) => m.op(b, f(a)))
}

class MonoidApp extends App {
  import Monoid._

  val s = List("qw","er","ty").foldRight(stringMonoid.zero)(stringMonoid.op)

}

