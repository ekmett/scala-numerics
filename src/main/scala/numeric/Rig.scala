package numeric

trait Rig[@specialized A] extends Semiring[A] {
  def plus: Abelian.Monoid[A]
  def times: Monoid[A]
  def zero: A = plus.e
  def one: A = times.e
  implicit override def ops(a: A) = times.ops(a) // no monoid ops
}

object Rig { 
  def apply[A](addition: Abelian.Monoid[A], multiplication : Monoid[A]): Rig[A] = new Rig[A] {
    def plus = addition
    def times = multiplication
  }
}

