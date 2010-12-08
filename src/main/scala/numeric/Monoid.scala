package numeric

/**
 * A semigroup with an identity such that: <code>x*e = x = e*x</code>
 */

trait Monoid[@specialized A] extends Unital[A] with Semigroup[A] {
  override def dual: Monoid[A] = new Monoid.Dual[A] {
    override def dual: Monoid[A] = Monoid.this
  }
  def *[B](that: Monoid[B]) = new Monoid.Product[A,B] {
    def _1: Monoid[A] = Monoid.this
    def _2: Monoid[B] = that
  }
}

object Monoid {
  def apply[A](
    f: (A,A) => A,
    id: A
  ) : Monoid[A] = new Monoid[A] {
    def apply(a: A, b: A): A = f(a,b)
    def e: A = id
  }
  trait Dual[@specialized A] extends Semigroup.Dual[A] with Unital.Dual[A] with Monoid[A]
  trait Product[A,B] extends Semigroup.Product[A,B] with Unital.Product[A,B] with Monoid[(A,B)] {
    def _1: Monoid[A]
    def _2: Monoid[B]
    override def dual: Product[A,B] = new Product.Dual[A,B] {
      override def dual: Product[A,B] = Product.this
    }
  }
  object Product {
    trait Dual[A,B] extends Product[A,B] {
      def _1: Monoid[A] = dual._1.dual
      def _2: Monoid[B] = dual._2.dual
    }
  }
}

