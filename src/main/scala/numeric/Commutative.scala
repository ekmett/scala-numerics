package numeric

trait Commutative
object Commutative {

  trait Ring[@specialized A] extends numeric.Ring[A] {
    def times: Abelian.Monoid[A]
    implicit override def addition(a: A) = plus.addition(a)
  }

  object Ring { 
    def apply[A](a: Abelian.Group[A], m: Monoid[A]): Ring[A] = new Ring[A] {
      def plus = a
      def times = m
    }
  }
}
