package com.scalaz.config

private[config] trait ProductBuilder[F[_], A, B] {
  implicit val C: ConfigSyntax[F]
  val a: F[A]
  val b: F[B]

  def apply[C](f: (A, B) => C): F[C] = C.map(C.product(a, b))({ case (aa, bb) => f(aa, bb) })

  def ~ [C](cc: F[C]): ProductBuilder3[C] = new ProductBuilder3[C] {
    val c: F[C] = cc
  }

  sealed abstract class ProductBuilder3[C] {
    val c: F[C]

    def apply[D](f: (A, B, C) => D): F[D] =
      C.map(C.product(C.product(a, b), c))({ case ((aa, bb), cc) => f(aa, bb, cc) })

    def ~ [D](dd: F[D]): ProductBuilder4[D] = new ProductBuilder4[D] {
      val d: F[D] = dd
    }

    sealed abstract class ProductBuilder4[D] {
      val d: F[D]

      def apply[E](f: (A, B, C, D) => E): F[E] =
        C.map(C.product(C.product(a, b), C.product(c, d))) {
          case ((aa, bb), (cc, dd)) => f(aa, bb, cc, dd)
        }

      def ~ [E](ee: F[E]): ProductBuilder5[E] = new ProductBuilder5[E] {
        val e: F[E] = ee
      }

      sealed abstract class ProductBuilder5[E] {
        val e: F[E]

        def apply[FF](f: (A, B, C, D, E) => FF): F[FF] =
          C.map(C.product(C.product(C.product(a, b), C.product(c, d)), e)) {
            case (((aa, bb), (cc, dd)), ee) => f(aa, bb, cc, dd, ee)
          }

      }

    }

  }

}
