package numeric

/**
 * A quasigroup with and an identity: <code>x*e = x = e*x</code>
 */

trait Loop[@specialized A] extends Unital[A] 
                              with Quasigroup[A] 
                              with Multiplicative[A,Loop[A],Loop.Multiplication[A]]
                              with Additive[A,Loop[A],Loop.Addition[A]] {
  def leftInverse(x: A): A = over(e,x)
  def rightInverse(x: A): A = under(x,e)
  override def dual: Loop[A] = new Loop.Dual[A] {
    override def dual: Loop[A] = Loop.this
  }
  def *[B](that: Loop[B]) = new Loop.Product[A,B] {
    def _1: Loop[A] = Loop.this
    def _2: Loop[B] = that
  }
  implicit override def multiplication(a: A): Loop.Multiplication[A] = new Loop.Multiplication[A] {
    def lhs: A = a
    def numeric: Loop[A] = Loop.this
  }
  override def addition(a: A): Loop.Addition[A] = new Loop.Addition[A] {
    def lhs: A = a
    def numeric: Loop[A] = Loop.this
  }
}


object Loop {
  def apply[A](
    f:(A,A) => A,
    o:(A,A) => A,
    u:(A,A) => A,
    id: A
  ) : Loop[A] = new Loop[A] {
    def apply(a: A, b: A): A = f(a,b)
    def over(a: A, b: A): A = o(a,b)
    def under(a: A, b: A): A = u(a,b)
    def e: A = id
  }
  trait Dual[@specialized A] extends Quasigroup.Dual[A] with Unital.Dual[A] with Loop[A]
  trait Multiplication[@specialized A] extends Quasigroup.Multiplication[A] with Ops[A,Loop[A]]{
    def leftInverse: A = numeric.leftInverse(lhs)
    def rightInverse: A = numeric.rightInverse(lhs)
  }
  trait Addition[@specialized A] extends Quasigroup.Addition[A] with Ops[A,Loop[A]] {
    def leftNegation: A = numeric.leftInverse(lhs)
    def rightNegation: A = numeric.rightInverse(lhs)
  }
  trait Product[A,B] extends Quasigroup.Product[A,B] 
                        with Unital.Product[A,B] 
                        with ProductLike[Loop,A,B]
                        with Loop[(A,B)] {
    override def dual: Product[A,B] = new Product.Dual[A,B] {
      override def dual: Product[A,B] = Product.this
    }
  }
  object Product {
    trait Dual[A,B] extends Product[A,B] {
      def _1: Loop[A] = dual._1.dual
      def _2: Loop[B] = dual._2.dual
    }
  }
}

