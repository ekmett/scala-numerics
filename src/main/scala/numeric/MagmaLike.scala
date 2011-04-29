package numeric

trait MagmaLike[@specialized A, +R <: Magma[A]] {
  def dual: R
}

trait Additive[@specialized A, +R <: Magma[A], +P <: Ops[R,A]] extends MagmaLike[A,R] {
  type AdditionOps = P
  def addition(a: A) : P 
}

trait Multiplicative[@specialized A, +R <: Magma[A], +M <: Ops[R,M]] {
  type MultiplicationOps = M
  implicit def multiplication(m: M) : P 
}
