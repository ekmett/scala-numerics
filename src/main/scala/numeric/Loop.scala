package numeric

/**
 * A quasigroup with and an identity: <code>x*e = x = e*x</code>
 */

trait Loop[@specialized A] extends Unital[A] with Quasigroup[A] {
  def left_inverse(x: A): A = over(e,x)
  def right_inverse(x: A): A = under(x,e)
  override def dual: Loop[A] = new Loop.Dual[A] {  
    override def dual: Loop[A] = Loop.this
  }
  def *[B](that: Loop[B]) = new Loop.Product[A,B] {
    def _1: Loop[A] = Loop.this
    def _2: Loop[B] = that
  }
  implicit override def ops(a: A): Loop.Ops[A] = new Loop.Ops[A] { 
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
  trait Ops[@specialized A] extends Quasigroup.Ops[A] {
    protected def numeric: Loop[A]
    def leftInverse: A = numeric.left_inverse(lhs)
    def rightInverse: A = numeric.right_inverse(lhs)
  }
  trait Product[A,B] extends Quasigroup.Product[A,B] with Unital.Product[A,B] with Loop[(A,B)] {
    def _1: Loop[A]
    def _2: Loop[B]
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

