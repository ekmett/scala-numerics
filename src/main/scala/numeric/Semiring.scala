package numeric

trait Semiring[@specialized A] extends Ringoid[A] {
  def plus: Abelian.Semigroup[A]
  def times: Semigroup[A]
  implicit override def addition(a: A) = times.addition(a)
}

object Semiring {
  def apply[A](a: Abelian.Semigroup[A], m: Semigroup[A]): Semiring[A] = new Semiring[A] {
    def plus = a
    def times = m
  }
}

