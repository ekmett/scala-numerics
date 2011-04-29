package numeric

trait RingoidLike[@specialized A, +P <: Magma[A], +T <: Magma[A]] {
  def plus: P
  def times: T
  implicit def addition(a: A)       : P#AdditionOps       = plus.addition(a)
  implicit def multiplication(a: A) : T#MultiplicationOps = times.multiplication(a)
}

object RingoidLike {
  def apply[A, P <: Magma[A], T <: Magma[A] ](a: S, m: T): Ringoid[A] = new Ringoid[A] {
    def plus = a
    def times = m
  }
}
