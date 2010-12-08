package numeric

trait Semiring[@specialized A] extends Ringoid[A] { 
  def plus: Abelian.Semigroup[A]
  def times: Semigroup[A]
  implicit override def ops(a: A) = times.ops(a) // no semigroup ops
}

object Semiring { 
  def apply[A](addition: Abelian.Semigroup[A], multiplication: Semigroup[A]): Semiring[A] = new Semiring[A] {
    def plus = addition
    def times = multiplication
  }
}

