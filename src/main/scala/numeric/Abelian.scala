package numeric

/** An Abelian {Magma,Semigroup,Monoid,Quasigroup,Loop,Group} is commutative. Not enforced, but this is a hint. */
trait Abelian

trait AbelianCompanion {
  trait Magma[@specialized A] extends numeric.Magma[A] with Abelian {
    override def dual: Magma[A] = new Magma.Dual[A] {
      override def dual: Magma[A] = Magma.this
    }
    def *[B](that: Magma[B]) = new Magma.Product[A,B] {
      def _1: Magma[A] = Magma.this
      def _2: Magma[B] = that
    }
  }
  object Magma {
    def apply[A](f:(A,A) => A) : Magma[A] = new Magma[A] {
      def apply(a: A, b: A): A = f(a,b)
    }
    trait Dual[@specialized A] extends numeric.Magma.Dual[A] with Magma[A]
    trait Product[A,B] extends numeric.Magma.Product[A,B] with Magma[(A,B)] with ProductLike[Magma,A,B] {
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


  trait Semigroup[@specialized A] extends numeric.Semigroup[A] with Magma[A] {
    override def dual: Semigroup[A] = new Semigroup.Dual[A] {
      override def dual: Semigroup[A] = Semigroup.this
    }
    def *[B](that: Semigroup[B]) = new Semigroup.Product[A,B] {
      def _1: Semigroup[A] = Semigroup.this
      def _2: Semigroup[B] = that
    }
  }
  object Semigroup {
    def apply[A](f:(A,A) => A) : Semigroup[A] = new Semigroup[A] {
      def apply(a: A, b: A): A = f(a,b)
    }
    trait Dual[@specialized A] extends numeric.Semigroup.Dual[A] with Semigroup[A]
    trait Product[A,B] extends numeric.Semigroup.Product[A,B] with Semigroup[(A,B)] with ProductLike[Semigroup,A,B] {
      override def dual: Product[A,B] = new Product.Dual[A,B] {
        override def dual: Product[A,B] = Product.this
      }
    }
    object Product {
      trait Dual[A,B] extends Product[A,B] {
        def _1: Semigroup[A] = dual._1.dual
        def _2: Semigroup[B] = dual._2.dual
      }
    }
  }


  trait Monoid[@specialized A] extends numeric.Monoid[A] with Semigroup[A] {
    override def dual: Monoid[A] = new Monoid.Dual[A] {
      override def dual: Monoid[A] = Monoid.this
    }
    def *[B](that: Monoid[B]) = new Monoid.Product[A,B] {
      def _1: Monoid[A] = Monoid.this
      def _2: Monoid[B] = that
    }
  }
  object Monoid {
    def apply[A](f:(A,A) => A, id: A) : Monoid[A] = new Monoid[A] {
      def apply(a: A, b: A): A = f(a,b)
      def e: A = id
    }
    trait Dual[@specialized A] extends numeric.Monoid.Dual[A] with Monoid[A]
    trait Product[A,B] extends numeric.Monoid.Product[A,B] with Monoid[(A,B)] with ProductLike[Monoid,A,B] {
      override def dual: Product[A,B] = new Product.Dual[A,B] {
        override def dual: Product[A,B] = Product.this
      }
    }
    object Product {
      trait Dual[A,B] extends Product[A,B] {
        def _1: Monoid[A] = dual._1.dual
        def _2: Monoid[B] = dual._2.dual
      }
    }
  }

  trait Quasigroup[@specialized A] extends numeric.Quasigroup[A] with Magma[A] {
    override def dual: Quasigroup[A] = new Quasigroup.Dual[A] {
      override def dual: Quasigroup[A] = Quasigroup.this
    }
    def *[B](that: Quasigroup[B]) = new Quasigroup.Product[A,B] {
      def _1: Quasigroup[A] = Quasigroup.this
      def _2: Quasigroup[B] = that
    }
  }
  object Quasigroup {
    def apply[A](f:(A,A) => A, o: (A,A) => A, u: (A,A) => A) : Quasigroup[A] = new Quasigroup[A] {
      def apply(a: A, b: A): A = f(a,b)
      def over(a: A, b: A): A = o(a,b)
      def under(a: A, b: A): A = u(a,b)
    }
    trait Dual[@specialized A] extends numeric.Quasigroup.Dual[A] with Quasigroup[A]
    trait Product[A,B] extends numeric.Quasigroup.Product[A,B] with Quasigroup[(A,B)] with ProductLike[Quasigroup,A,B] {
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

  trait Loop[@specialized A] extends numeric.Loop[A] with Quasigroup[A] {
    override def dual: Loop[A] = new Loop.Dual[A] {
      override def dual: Loop[A] = Loop.this
    }
    def *[B](that: Loop[B]) = new Loop.Product[A,B] {
      def _1: Loop[A] = Loop.this
      def _2: Loop[B] = that
    }
  }
  object Loop {
    def apply[A](f:(A,A) => A, o: (A,A) => A, u: (A,A) => A, id: A) : Loop[A] = new Loop[A] {
      def apply(a: A, b: A): A = f(a,b)
      def over(a: A, b: A): A = o(a,b)
      def under(a: A, b: A): A = u(a,b)
      def e: A = id
    }
    trait Dual[@specialized A] extends numeric.Loop.Dual[A] with Loop[A]
    trait Product[A,B] extends numeric.Loop.Product[A,B] with Loop[(A,B)] with ProductLike[Loop,A,B] {
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

  trait Group[@specialized A] extends numeric.Group[A] with Loop[A] with Monoid[A] {
    override def dual: Group[A] = new Group.Dual[A] {
      override def dual: Group[A] = Group.this
    }
    def *[B](that: Group[B]) = new Group.Product[A,B] {
      def _1: Group[A] = Group.this
      def _2: Group[B] = that
    }
  }
  object Group {
    def apply[A](f:(A,A) => A, inv: A => A, id: A) : Group[A] = new Group[A] {
      def apply(a: A, b: A): A = f(a,b)
      def inverse(a: A):A = inv(a)
      def e: A = id
    }
    trait Dual[@specialized A] extends numeric.Group.Dual[A] with Group[A]
    trait Product[A,B] extends numeric.Group.Product[A,B] with Group[(A,B)] with ProductLike[Group,A,B] {
      override def dual: Product[A,B] = new Product.Dual[A,B] {
        override def dual: Product[A,B] = Product.this
      }
    }
    object Product {
      trait Dual[A,B] extends Product[A,B] {
        def _1: Group[A] = dual._1.dual
        def _2: Group[B] = dual._2.dual
      }
    }
  }
}

// aliasing
object Abelian extends AbelianCompanion
