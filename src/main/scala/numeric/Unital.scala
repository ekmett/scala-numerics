package numeric

trait Unital[@specialized A] {
  def e: A
  def dual: Unital[A] = new Unital.Dual[A] {
    override def dual: Unital[A] = Unital.this
  }
  def *[B](that: Unital[B]) = new Unital.Product[A,B] {
    def _1: Unital[A] = Unital.this
    def _2: Unital[B] = that
  }
}

object Unital {
  def apply[A](id: A) : Unital[A] = new Unital[A] {
    def e: A = id
  }
  trait Dual[@specialized A] extends Unital[A] {
    def e: A = dual.e
  }
  trait Product[A,B] extends Unital[(A,B)] {
    def _1: Unital[A]
    def _2: Unital[B]
    def e: (A,B)  = (_1.e,_2.e)
    override def dual: Product[A,B] = new Product.Dual[A,B] {
      override def dual: Product[A,B] = Product.this
    }
  }
  object Product {
    trait Dual[A,B] extends Product[A,B] {
      def _1: Unital[A] = dual._1.dual
      def _2: Unital[B] = dual._2.dual
    }
  }
}
