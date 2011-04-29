package numeric.ad

import scala.collection.mutable.Buffer
import numeric._

case class Reverse[@specialized A](primal: A, slot: Int)

object Reverse { 
  class Tape[@specialized A]()(implicit val A: Numeric[A]) {
    val buffer = Buffer[Entry[A]](Zero)
    implicit val ops : Numeric.Ops[A] = A.ops
    def sensitivities(top : Int): Array[A] = { 
      var result = Array.tabulate[A](top + 1)(n => if (n == top) A.one else A.zero)
      // now iterate backwards across the tape
      top max 1 to 1 by -1 foreach { 
        n => buffer(n) match { 
          case Zero => ()
          case Var => ()
          case Unary(dadb, bix) => result.update(bix, result(bix) + dadb * result(n))
          case Binary(dadb, bix, dadc, cix) => {
            result.update(bix, result(bix) + dadb * result(n))
            result.update(cix, result(cix) + dadc * result(n))
          }
        }
      }
      (x : Reverse[A]) => result(x.slot)
    }
    def pushSlot(e: Entry[A]) : Int = synchronized { 
      val len = buffer.length
      buffer += e
      len
    }
    def push(a: A, e: Entry[A]): Int = Reverse[A](a, pushEntry(e))

    def unary(f: A => A, dadb : => A, b: Reverse[A]) = 
      Reverse[A]( f(b.primal), 
        if (b.slot == 0) 0 
        else push(Unary[A](dadb, b.slot))
      )

    def binary(f: (A,A) => A, dadb: => A, dadc: => A, b: Reverse[A], c: Reverse[A]) = 
      Reverse[A]( f(b.primal,c.primal), 
        if (b.slot == 0) {
          if (c.slot == 0) 0
          else pushSlot(Unary[A](dadc, c.slot))
        } else {
          if (c.slot == 0) pushSlot(Unary[A](dadb, b.slot))
          else pushSlot(Binary[A](dadb, b.slot, dadc, c.slot))
        }
      )
    def fresh(a: A): Reverse[A] = push(a, Var)
  }

  object Tape { 
    trait Entry[@specialized +A] 
    case object Zero extends Entry[Nothing]
    case object Var extends Entry[Nothing]
    case class Unary[@specialized A](di: A, i: Int) extends Entry[A]
    case class Binary[@specialized A](di: A, i: Int, dj: A, j: Int) extends Entry[A]
  }

  implicit def lift[A:Numeric](a: A) = Reverse[A](a, implicitly[Numeric[A]].zero[A])

  def diff[A:Numeric](f: Reverse[A] => Reverse[A]) : A => (A, A) = (a: A) => {
    val tape = new Tape()
    val x = tape.fresh(a)
    val y = f(x)
    val ybar = tape.sensitivities(y.slot)
    (y.primal, ybar(x))
  }

  class ReverseNumeric[@specialized A](tape: Tape[A]) extends Numeric[Reverse[A]] with Mode[A] {
    def num: Numeric[A]
    def minus_one = num.negate(num.one)
    def plus(a: Reverse[A], b: Reverse[A]): Reverse[A] = tape.binary(num.plus(_,_),num.one,num.one,a,b)
    def times(a: Reverse[A], b: Reverse[A]): Reverse[A] = tape.binary(num.times(_,_),b.primal,a.primal,a,b)
    def minus(a: Reverse[A], b: Reverse[A]): Reverse[A] = tape.binary(num.minus(_,_),num.one,minus_one,a,b)
    // def abs(a: Reverse[A]): Reverse[A] = unary(num.abs(_),
  }

}
