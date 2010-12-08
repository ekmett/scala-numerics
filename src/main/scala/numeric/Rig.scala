package numeric

trait Rig[@specialized A] extends Semiring[A] {
  def plus: Abelian.Monoid[A]
  def times: Monoid[A]
  def zero: A = plus.e
  def one: A = times.e
  implicit override def multiplication(a: A) = times.multiplication(a) // no monoid ops
}

object Rig { 
  def apply[A](a: Abelian.Monoid[A], m: Monoid[A]): Rig[A] = new Rig[A] {
    def plus = a
    def times = m
  }
}
