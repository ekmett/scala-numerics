package numeric

trait Magma[@specialized A] extends Additive[A, Magma[A], Magma.Addition[A]] 
                               with Multiplicative[A, Magma[A], Magma.Multiplication[A]] {
  def apply(a: A, b: A): A
  def dual: Magma[A] = new Magma.Dual[A] {
    override def dual: Magma[A] = Magma.this
  }
  def *[B](that: Magma[B]) = new Magma.Product[A,B] {
    def _1: Magma[A] = Magma.this
    def _2: Magma[B] = that
  }
  implicit def multiplication(a: A): Magma.Multiplication[A] = new Magma.Multiplication[A] {
    def lhs: A = a
    def numeric: Magma[A] = Magma.this
  }
  def addition(a: A): Magma.Addition[A] = new Magma.Addition[A] {
    def lhs: A = a
    def numeric: Magma[A] = Magma.this
  }
}

object Magma {
  def apply[A](f:(A,A) => A) : Magma[A] = new Magma[A] {
    def apply(a: A, b: A): A = f(a,b)
  }
  trait Multiplication[@specialized A] extends Ops[Magma[A],A] {
    def *(rhs: A): A = numeric(lhs,rhs)
  }
  trait Addition[@specialized A] extends Ops[Magma[A],A] {
    def +(rhs: A): A = numeric(lhs,rhs)
  }
  trait Dual[@specialized A] extends Magma[A] {
    def apply(a: A, b: A) = dual(b,a)
  }
  trait Product[A,B] extends Magma[(A,B)] with ProductLike[Magma,A,B] {
    def apply(a: (A,B), b: (A,B)) = (_1(a._1,b._1),_2(a._2,b._2))
    override def dual: Product[A,B] = new Product.Dual[A,B] {
      override def dual: Product[A,B] = Product.this
    }
  }
  object Product {
    trait Dual[A,B] extends Product[A,B] {
      def _1: Magma[A] = dual._1.dual
      def _2: Magma[B] = dual._2.dual
    }
  }
}
