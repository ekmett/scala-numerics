package numeric

trait Ringoid[@specialized A] {
  def plus: Magma[A]
  def times: Magma[A]
  implicit def multiplication(a: A) = times.multiplication(a)
  implicit def addition(a: A) = plus.addition(a)
}

object Ringoid {
  def apply[A](a: Magma[A], m: Magma[A]): Ringoid[A] = new Ringoid[A] {
    def plus = a
    def times = m
  }
}
