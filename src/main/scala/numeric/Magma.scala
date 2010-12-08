package numeric

trait Magma[@specialized A] {
  def apply(a: A, b: A): A
  def dual: Magma[A] = new Magma.Dual[A] {  
    override def dual: Magma[A] = Magma.this
  }
  def *[B](that: Magma[B]) = new Magma.Product[A,B] {
    def _1: Magma[A] = Magma.this
    def _2: Magma[B] = that
  }
  implicit def ops(a: A): Magma.Ops[A] = new Magma.Ops[A] {
    def lhs: A = a
    def numeric: Magma[A] = Magma.this
  }
  def additive(a: A): Magma.AdditiveOps[A] = new Magma.AdditiveOps[A] {
    def lhs: A = a
    def numeric: Magma[A] = Magma.this
  }
}

object Magma { 
  def apply[A](f:(A,A) => A) : Magma[A] = new Magma[A] {
    def apply(a: A, b: A): A = f(a,b)
  }
  trait Ops[@specialized A] { 
    protected def lhs: A
    protected def numeric: Magma[A]
    def *(rhs: A): A = numeric(lhs,rhs)
  }
  trait AdditiveOps[@specialized A] { 
    protected def lhs: A
    protected def numeric: Magma[A]
    def +(rhs: A): A = numeric(lhs,rhs)
  }
  trait Dual[@specialized A] extends Magma[A] { 
    def apply(a: A, b: A) = dual(b,a)
  }
  trait Product[A,B] extends Magma[(A,B)] {
    def _1: Magma[A]
    def _2: Magma[B]
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
