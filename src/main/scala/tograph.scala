package generic

import graph._
import shapeless._
import labelled._
import ops.record._
import cats.implicits._

trait ToGraph[C, G[_], Label] {
  def toGraph(parent: Label, c: C ): G[Label] => G[Label]
}

trait Bottom {
  implicit def base[A, G[_]](
      implicit
      G: NewEdge[G]
    ) = new ToGraph[A, G, Symbol] {
    def toGraph(parent: Symbol, c: A ): G[Symbol] => G[Symbol] = identity
  }
}
object ToGraph extends Bottom {
  def apply[C, G[_]](root: Symbol, c: C )(implicit G: ToGraph[C, G, Symbol], C: CreateGraph[G] ) =
    G.toGraph( root, c )( C.create( root ) )

  implicit def ccons[H, K <: Symbol, T <: HList, G[_]](
      implicit
      key: Witness.Aux[K],
      A: NewEdge[G],
      C: Lazy[ToGraph[H, G, Symbol]],
      N: Lazy[ToGraph[T, G, Symbol]]
    ) = new ToGraph[FieldType[K, H] :: T, G, Symbol] {
    def toGraph(parent: Symbol, c: labelled.FieldType[K, H] :: T ): G[Symbol] => G[Symbol] = { graph =>
      N.value.toGraph( parent, c.tail )(
        C.value.toGraph( key.value, c.head )( A.newEdge( graph )( parent, key.value ) )
      )
    }
  }
  implicit def hnil[G[_]] = new ToGraph[HNil, G, Symbol] {
    def toGraph(parent: Symbol, c: HNil ): G[Symbol] => G[Symbol] = identity
  }
  implicit def generic[P, C, G[_]](
      implicit
      gen: LabelledGeneric.Aux[P, C],
      G: Lazy[ToGraph[C, G, Symbol]]
    ): ToGraph[P, G, Symbol] = new ToGraph[P, G, Symbol] {
    def toGraph(parent: Symbol, p: P ): G[Symbol] => G[Symbol] = G.value.toGraph( parent, gen.to( p ) )
  }
}
