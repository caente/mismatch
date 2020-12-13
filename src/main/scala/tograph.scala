package tograph

import graph._
import cats.implicits._
import cats.data.NonEmptyList
import cats._
import algorithm.Diff
import algorithm.Mismatches
import scala.annotation.implicitNotFound
import magnolia._
import scala.language.experimental.macros

object `package` {
  implicit object symbolOrd extends Ordering[Symbol] {
    def compare(x: Symbol, y: Symbol ): Int = x.toString.compare( y.toString )
  }
}

sealed trait Labelled[A]
object Labelled {

  implicit def eq[A: Eq] = new Eq[Labelled[A]] {
    def eqv(x: Labelled[A], y: Labelled[A] ): Boolean =
      ( x, y ) match {
        case ( Leaf( x ), Leaf( y ) )   => x === y
        case ( Node( x ), Node( y ) )   => x === y
        case ( Index( x ), Index( y ) ) => x === y
        case _                          => false
      }
  }
  implicit def ord[A: Ordering]: Ordering[Labelled[A]] = new Ordering[Labelled[A]] {
    def compare(x: Labelled[A], y: Labelled[A] ): Int = {
      ( x, y ) match {
        case ( Index( x ), Index( y ) ) => x.compare( y )
        case ( Index( _ ), _ )          => -1
        case ( _, Index( _ ) )          => 1
        case ( Leaf( x ), Leaf( y ) )   => Ordering[A].compare( x, y )
        case ( Leaf( _ ), _ )           => -1
        case ( _, Leaf( _ ) )           => 1
        case ( Node( x ), Node( y ) )   => x.compare( y )
      }
    }
  }
  type AsString = Labelled[String]
  implicit def show[A: Show]: Show[Labelled[A]] = new Show[Labelled[A]] {
    def show(t: Labelled[A] ): String = t match {
      case Leaf( x )     => x.show
      case Index( x )    => x.show
      case Node( label ) => label.show
    }
  }
}
case class Node[A](label: String ) extends Labelled[A]
case class Leaf[A](a: A ) extends Labelled[A]
case class Index[A](i: Int ) extends Labelled[A]

@implicitNotFound( "Implicit not found for ToGraph[${C}, ${G}, ${Label}]" )
trait ToGraph[C, G[_], Label] {
  def toGraph(parent: NonEmptyList[Label], c: C ): G[Label] => G[Label]
}

/*
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
      val label: Labelled.AsString = Node( key.value.toString.drop( 1 ) )
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
        C.toGraph( Node[String]( key.value.toString.drop( 1 ) ) :: parent, c.head )(
          A.connect( graph )( parent, Node( key.value.toString.drop( 1 ) ) )
        )
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
  implicit def generic[P, C, G[_]](
      implicit
      gen: LabelledGeneric.Aux[P, C],
      G: Lazy[ToGraph[C, G, Labelled.AsString]]
    ): ToGraph[P, G, Labelled.AsString] = new ToGraph[P, G, Labelled.AsString] {
    def toGraph(parent: NonEmptyList[Labelled.AsString], p: P ): G[Labelled.AsString] => G[Labelled.AsString] =
      G.value.toGraph( parent, gen.to( p ) )
  }

}
 */
trait Bottom {
  implicit def fromShow[A, G[_]](implicit A: Show[A], G: Connect[G] ) = new ToGraph[A, G, Labelled[String]] {
    def toGraph(parent: NonEmptyList[Labelled[String]], c: A ): G[Labelled[String]] => G[Labelled[String]] = { graph =>
      G.connect( graph )( parent, Leaf( c.show ) )
    }
  }
}
trait Magnolia[G[_]] extends Bottom{
  type Typeclass[T] = ToGraph[T, G, Labelled[String]]
  def connect: Connect[G]
  def combine[T](caseClass: CaseClass[Typeclass, T] ): Typeclass[T] =
    new ToGraph[T, G, Labelled[String]] {
      def toGraph(parent: NonEmptyList[Labelled[String]], c: T ): G[Labelled[String]] => G[Labelled[String]] = {
        graph =>
          caseClass.parameters.foldLeft( graph ) { ( graph, p ) =>
            p.typeclass.toGraph( Node[String]( p.label ) :: parent, p.dereference( c ) )(
              connect.connect( graph )( parent, Node( p.label ) )
            )
          }
      }
    }
  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T] ): Typeclass[T] =
    new ToGraph[T, G, Labelled[String]] {
      def toGraph(parent: NonEmptyList[Labelled[String]], c: T ): G[Labelled[String]] => G[Labelled[String]] = {
        graph =>
          sealedTrait.dispatch( c ) { subType =>
            subType.typeclass.toGraph( parent, subType.cast( c ) )( graph )
          }
      }
    }
  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}
trait ToGraphInstances[G[_]] extends Magnolia[G] {

  def create[C, G[_]](
      root: String,
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
  implicit def listGraph[P, G[_]](
      implicit
      T: ToGraph[P, G, Labelled.AsString],
      A: Connect[G]
    ): ToGraph[List[P], G, Labelled.AsString] = new ToGraph[List[P], G, Labelled.AsString] {
    def toGraph(parent: NonEmptyList[Labelled.AsString], c: List[P] ): G[Labelled.AsString] => G[Labelled.AsString] = {
      graph =>
        c.foldLeft( ( graph, 0 ) ) {
            case ( ( graph, index ), p ) =>
              val indexLeaf = Index[String]( index )
              val indexed = A.connect( graph )( parent, indexLeaf )
              ( T.toGraph( indexLeaf :: parent, p )( indexed ), index + 1 )
          }
          ._1
    }
  }
 //implicit def listShow[P, G[_]](
 //    implicit
 //    A: Connect[G],
 //    S: Show[P]
 //  ): ToGraph[List[P], G, Labelled.AsString] = new ToGraph[List[P], G, Labelled.AsString] {
 //  def toGraph(parent: NonEmptyList[Labelled.AsString], c: List[P] ): G[Labelled.AsString] => G[Labelled.AsString] = {
 //    graph =>
 //      c.foldLeft( graph ) { ( graph, p ) =>
 //        A.connect( graph )( parent, Leaf( p.show ) )
 //      }
 //  }
 //}
  implicit def mapGraph[K: Ordering, V, G[_]](
      implicit
      A: Connect[G],
      K: Show[K],
      V: ToGraph[V, G, Labelled.AsString]
    ): ToGraph[Map[K, V], G, Labelled.AsString] = new ToGraph[Map[K, V], G, Labelled.AsString] {
    def toGraph(
        parent: NonEmptyList[Labelled.AsString],
        c: Map[K, V]
      ): G[Labelled.AsString] => G[Labelled.AsString] = { graph =>
      c.keySet.toList.sorted
        .foldLeft( ( graph, 0 ) ) {
          case ( ( graph, index ), k ) =>
            val indexLeaf = Leaf( k.show )
            val indexed = A.connect( graph )( parent, indexLeaf )
            ( V.toGraph( indexLeaf :: parent, c( k ) )( indexed ), index + 1 )
        }
        ._1
    }
  }
}

object ToGraph extends ToGraphInstances[AdjacentGraph] {
  def connect: Connect[AdjacentGraph] = implicitly[Connect[AdjacentGraph]]
}
