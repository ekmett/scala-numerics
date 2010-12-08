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

trait Ring[@specialized A] extends Rig[A] {
  def plus: Abelian.Group[A]
  def minus(a: A, b: A): A = plus.over(a,b)
  def negate(a: A) = plus.inverse(a)
  implicit override def ops(a: A) = times.ops(a)
  implicit override def additiveOps(a: A) = plus.additive(a)
}

object Ring { 
  def apply[A](addition: Abelian.Group[A], multiplication : Monoid[A]): Ring[A] = new Ring[A] {
    def plus = addition
    def times = multiplication
  }
}

