package numeric

// TODO: try specializing?
trait ProductLike[+Repr[_],A,B] extends Magma[(A,B)] with MagmaLike[(A,B),ProductLike[Repr,A,B]] {
  def _1: Repr[A]
  def _2: Repr[B] 
  def dual: ProductLike[Repr,A,B]
}
