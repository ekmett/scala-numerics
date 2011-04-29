package numeric

trait Ops[@specialized A, +Repr] { 
  protected def numeric: Repr
  protected def lhs: A
}

