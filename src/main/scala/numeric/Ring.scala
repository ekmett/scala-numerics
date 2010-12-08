package numeric

trait Ring[@specialized A] extends Rig[A] {
  def plus: Abelian.Group[A]
  def minus(a: A, b: A): A = plus.over(a,b)
  def negate(a: A) = plus.inverse(a)
  implicit override def multiplication(a: A) = times.multiplication(a)
  implicit override def addition(a: A) = plus.addition(a)
}

object Ring {
  def apply[A](a: Abelian.Group[A], m: Monoid[A]): Ring[A] = new Ring[A] {
    def plus = a
    def times = m
  }
}

