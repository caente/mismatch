package tograph

import graph._
import shapeless._
import labelled._
import ops.record._
import cats.implicits._
import cats.data.NonEmptyList
import cats.Show
import cats.kernel.Eq

sealed trait Labelled[A]
object Labelled {
  implicit def eq[A: Eq] = new Eq[Labelled[A]] {
    def eqv(x: Labelled[A], y: Labelled[A] ): Boolean =
      ( x, y ) match {
        case ( Leaf( x ), Leaf( y ) ) => x === y
        case ( Node( x ), Node( y ) ) => x === y
        case _                        => false
      }
  }
  implicit def ord[A: Ordering]: Ordering[Labelled[A]] = new Ordering[Labelled[A]] {
    def compare(x: Labelled[A], y: Labelled[A] ): Int = {
      ( x, y ) match {
        case ( Leaf( _ ), Node( _ ) ) => -1
        case ( Node( _ ), Leaf( _ ) ) => 1
        case ( Leaf( x ), Leaf( y ) ) => Ordering[A].compare( x, y )
        case ( Node( x ), Node( y ) ) => x.compare( y )
      }
    }
  }
  type AsString = Labelled[String]
  implicit def show[A: Show]: Show[Labelled[A]] = new Show[Labelled[A]] {
    def show(t: Labelled[A] ): String = t match {
      case Leaf( x )     => x.show
      case Node( label ) => label.toString
    }
  }
}
case class Node[A](label: Symbol ) extends Labelled[A]
case class Leaf[A](a: A ) extends Labelled[A]

trait ToGraph[C, G[_], Label] {
  def toGraph(parent: NonEmptyList[Label], c: C ): G[Label] => G[Label]
}
trait Bottom {
  implicit def cconsNot[H, K <: Symbol, T <: HList, G[_]](
      implicit
      key: Witness.Aux[K],
      A: Connect[G],
      C: Show[H],
      N: ToGraph[T, G, Labelled.AsString]
    ) = new ToGraph[FieldType[K, H] :: T, G, Labelled.AsString] {
    def toGraph(
        parent: NonEmptyList[Labelled.AsString],
        c: FieldType[K, H] :: T
      ): G[Labelled.AsString] => G[Labelled.AsString] = { graph =>
      val label: Labelled.AsString = Node( key.value )
      val graphWithLabel = A.connect( graph )( parent, label )
      val graphWithLeaf = A.connect( graphWithLabel )( label :: parent, Leaf( Show[H].show( c.head ) ) )
      N.toGraph( parent, c.tail )( graphWithLeaf )
    }
  }
}

trait Hlists extends Bottom {
  implicit def ccons[H, K <: Symbol, T <: HList, G[_]](
      implicit
      key: Witness.Aux[K],
      A: Connect[G],
      C: ToGraph[H, G, Labelled.AsString],
      N: ToGraph[T, G, Labelled.AsString]
    ) = new ToGraph[FieldType[K, H] :: T, G, Labelled.AsString] {
    def toGraph(
        parent: NonEmptyList[Labelled.AsString],
        c: FieldType[K, H] :: T
      ): G[Labelled.AsString] => G[Labelled.AsString] = { graph =>
      N.toGraph( parent, c.tail )(
        C.toGraph( Node[String]( key.value ) :: parent, c.head )( A.connect( graph )( parent, Node( key.value ) ) )
      )
    }
  }

  implicit def hnil[G[_]] = new ToGraph[HNil, G, Labelled.AsString] {
    def toGraph(parent: NonEmptyList[Labelled.AsString], c: HNil ): G[Labelled.AsString] => G[Labelled.AsString] =
      identity
  }
  implicit def cnil[G[_]]: ToGraph[CNil, G, Labelled.AsString] = new ToGraph[CNil, G, Labelled.AsString] {
    def toGraph(parent: NonEmptyList[Labelled.AsString], c: CNil ): G[Labelled.AsString] => G[Labelled.AsString] =
      identity
  }

  implicit def coproduct[G[_], H, T <: Coproduct, K <: Symbol](
      implicit
      key: Witness.Aux[K],
      C: ToGraph[H, G, Labelled.AsString],
      N: ToGraph[T, G, Labelled.AsString]
    ): ToGraph[FieldType[K, H] :+: T, G, Labelled.AsString] =
    new ToGraph[FieldType[K, H] :+: T, G, Labelled.AsString] {
      def toGraph(
          parent: NonEmptyList[Labelled.AsString],
          c: FieldType[K, H] :+: T
        ): G[Labelled.AsString] => G[Labelled.AsString] = { graph =>
        c match {
          case Inl( h ) =>
            C.toGraph( parent, h )( graph )
          case Inr( tail ) =>
            N.toGraph( parent, tail )( graph )
        }
      }
    }
}
object ToGraph extends Hlists {

  def create[C, G[_]](
      root: Symbol,
      c: C
    )(implicit G: ToGraph[C, G, Labelled.AsString],
      C: CreateGraph[G]
    ): G[Labelled.AsString] =
    G.toGraph( NonEmptyList.one( Node( root ) ), c )( C.create( Node[String]( root ) ) )

  implicit def option[P, G[_]](
      implicit
      T: ToGraph[P, G, Labelled.AsString]
    ): ToGraph[Option[P], G, Labelled.AsString] = new ToGraph[Option[P], G, Labelled.AsString] {
    def toGraph(
        parent: NonEmptyList[Labelled.AsString],
        c: Option[P]
      ): G[Labelled.AsString] => G[Labelled.AsString] = { graph =>
      c match {
        case Some( p ) => T.toGraph( parent, p )( graph )
        case None      => graph
      }
    }
  }

  implicit def generic[P, C, G[_]](
      implicit
      gen: LabelledGeneric.Aux[P, C],
      G: Lazy[ToGraph[C, G, Labelled.AsString]]
    ): ToGraph[P, G, Labelled.AsString] = new ToGraph[P, G, Labelled.AsString] {
    def toGraph(parent: NonEmptyList[Labelled.AsString], p: P ): G[Labelled.AsString] => G[Labelled.AsString] =
      G.value.toGraph( parent, gen.to( p ) )
  }
}
