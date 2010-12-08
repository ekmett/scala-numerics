package numeric

trait Ringoid[@specialized A] { 
  def plus: Magma[A]
  def times: Magma[A]
  implicit def ops(a: A) = times.ops(a)
  implicit def additiveOps(a: A) = plus.additive(a)
}

object Ringoid { 
  def apply[A](addition: Magma[A], multiplication: Magma[A]): Ringoid[A] = new Ringoid[A] {
    def plus = addition
    def times = multiplication
  }
}

