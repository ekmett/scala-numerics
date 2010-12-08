package numeric

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

