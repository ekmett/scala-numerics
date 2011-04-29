package numeric

/**
 * A binary operation with 'left and right division' Laws:
 * <ol>
 * <li><code>y = x * (x \ y)</code>
 * <li><code>y = x \ (x * y)</code>
 * <li><code>y = (y / x) * x</code>
 * <li><code>y = (y * x) / x</code>
 * </ol>
 */

trait Quasigroup[@specialized A] extends Magma[A] 
                                    with Additive[A,Quasigroup[A],Quasigroup.Addition[A]] 
                                    with Multiplicative[A,Quasigroup[A],Quasigroup.Multiplication[A]] {
  /** a / b */
  def over(a: A, b: A): A
  /** a \ b */
  def under(a: A, b: A): A
  override def dual: Quasigroup[A] = new Quasigroup.Dual[A] {  
    override def dual: Quasigroup[A] = Quasigroup.this
  }
  def *[B](that: Quasigroup[B]) = new Quasigroup.Product[A,B] {
    def _1: Quasigroup[A] = Quasigroup.this
    def _2: Quasigroup[B] = that
  }
  implicit override def multiplication(a: A): Quasigroup.Multiplication[A] = new Quasigroup.Multiplication[A] { 
    def lhs: A = a
    def numeric: Quasigroup[A] = Quasigroup.this
  }
  override def addition(a: A): Quasigroup.Addition[A] = new Quasigroup.Addition[A] { 
    def lhs: A = a
    def numeric: Quasigroup[A] = Quasigroup.this
  }
}

object Quasigroup { 
  def apply[A](
    f: (A,A) => A,
    o: (A,A) => A,
    u: (A,A) => A
  ) : Quasigroup[A] = new Quasigroup[A] {
    def apply(a: A, b: A): A = f(a,b)
    def over(a: A, b: A): A = o(a,b)
    def under(a: A, b: A): A = u(a,b)
  }
  trait Dual[@specialized A] extends Magma.Dual[A] with Quasigroup[A] { 
    // a \ b
    def over(a: A, b: A): A = dual.over(b,a)
    def under(a: A, b: A): A = dual.under(b,a)
  }
  trait Multiplication[@specialized A] extends Magma.Multiplication[A] with Ops[A, Quasigroup[A]] {
    def /(rhs: A): A = numeric.over(lhs,rhs)
    def \(rhs: A): A = numeric.under(lhs,rhs)
  }
  trait Addition[@specialized A] extends Magma.Addition[A] with Ops[A, Quasigroup[A]] {
    def -(rhs: A): A = numeric.over(lhs,rhs)
    def subtract(rhs: A): A = numeric.under(lhs,rhs)
  }
  trait Product[A,B] extends Magma.Product[A,B] with Quasigroup[(A,B)] with ProductLike[Quasigroup,A,B] {
    def over(a: (A,B), b: (A,B)): (A,B) = (_1.over(a._1,b._1), _2.over(a._2,b._2))
    def under(a: (A,B), b: (A,B)): (A,B) = (_1.under(a._1,b._1), _2.under(a._2,b._2))
    override def dual: Product[A,B] = new Product.Dual[A,B] {
      override def dual: Product[A,B] = Product.this
    }
  }
  object Product {
    trait Dual[A,B] extends Product[A,B] {
      def _1: Quasigroup[A] = dual._1.dual
      def _2: Quasigroup[B] = dual._2.dual
    }
  }
}

