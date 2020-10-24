package tograph

import graph._
import shapeless._
import labelled._
import ops.record._
import cats.implicits._

trait ToGraph[C, G[_], Label] {
  def toGraph(parent: Label, c: C ): G[Label] => G[Label]
}

trait Bottom {
  implicit def base[A, G[_]] = new ToGraph[A, G, Symbol] {
    def toGraph(parent: Symbol, c: A ): G[Symbol] => G[Symbol] = identity
  }
}
object ToGraph extends Bottom {
  def create[C, G[_]](root: Symbol, c: C )(implicit G: ToGraph[C, G, Symbol], C: CreateGraph[G] ) =
    G.toGraph( root, c )( C.create( root ) )

  implicit def ccons[H, K <: Symbol, T <: HList, G[_]](
      implicit
      key: Witness.Aux[K],
      A: NewEdge[G],
      C: ToGraph[H, G, Symbol],
      N: ToGraph[T, G, Symbol]
    ) = new ToGraph[FieldType[K, H] :: T, G, Symbol] {
    def toGraph(parent: Symbol, c: labelled.FieldType[K, H] :: T ): G[Symbol] => G[Symbol] = { graph =>
      N.toGraph( parent, c.tail )(
        C.toGraph( key.value, c.head )( A.newEdge( graph )( parent, key.value ) )
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
