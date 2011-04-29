package numeric

trait ModuleLike[R,M,+S<:Ringoid[R] ,+V<:Magma[M]] {
  def scalar: S
  def vector: V
}

trait LeftModuleLike[R,M,+S<:Ringoid[R],+V<:Magma[M]] extends ModuleLike[R,M,S,V] {
  def times(r: R, m: M): M
}

trait RightModuleLike[R,M,+S<:Ringoid[R],+V<:Magma[M]] extends ModuleLike[R,M,S,V]{
  def times(r: M, r: R): M 
} 

trait BimoduleLike[R,M,+S<:Ringoid[R],+V<:Magma[M]] extends LeftModule[R,M,S,V] with RightModule[R,M,S,V]

trait LeftModule[R,M]  extends LeftModuleLike [R,M,Ring[R], Abelian.Group[M]]
trait RightModule[R,M] extends RightModuleLike[R,M,Ring[R], Abelian.Group[M]]
trait Bimodule[R,M]    extends BimoduleLike   [R,M,Ring[R], Abelian.Group[M]]
