package numeric

trait Semigroup[@specialized A] extends Magma[A] {
  override def dual: Semigroup[A] = new Semigroup.Dual[A] {
    override def dual: Semigroup[A] = Semigroup.this
  }
  def *[B](that: Semigroup[B]) = new Semigroup.Product[A,B] {
    def _1: Semigroup[A] = Semigroup.this
    def _2: Semigroup[B] = that
  }
}

object Semigroup {
  def apply[A](f:(A,A) => A) : Semigroup[A] = new Semigroup[A] {
    def apply(a: A, b: A): A = f(a,b)
  }
  trait Dual[@specialized A] extends Magma.Dual[A] with Semigroup[A]
  trait Product[A,B] extends Magma.Product[A,B] with Semigroup[(A,B)] {
    def _1: Semigroup[A]
    def _2: Semigroup[B]
    override def dual: Product[A,B] = new Product.Dual[A,B] {
      override def dual: Product[A,B] = Product.this
    }
  }
  object Product {
    trait Dual[A,B] extends Product[A,B] {
      def _1: Semigroup[A] = dual._1.dual
      def _2: Semigroup[B] = dual._2.dual
    }
  }
}


