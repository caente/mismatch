package ai.x
package toMatrix

import scala.language.implicitConversions
import shapeless._
import labelled._
import ops.record._
import simulacrum._

@typeclass trait ExtractPaths[C] {
  def extractPaths(c: C ): List[Symbol]
}

@typeclass trait IsLeaf[A] {
  def leaf(a: A ): Symbol
}

trait ExtractPathsLow {
// implicit def leaf[K <: Symbol, L](implicit key: Witness.Aux[K] ): ExtractPaths[FieldType[K, L]] =
//   new ExtractPaths[FieldType[K, L]] {
//     def extractPaths(c: FieldType[K, L] ): List[K] = List( key.value )
//   }
}
object ExtractPaths extends ExtractPathsLow {
  implicit def generic[A, G](implicit gen: LabelledGeneric.Aux[A, G], sg: Lazy[ExtractPaths[G]] ): ExtractPaths[A] =
    new ExtractPaths[A] {
      def extractPaths(a: A ) = sg.value.extractPaths( gen.to( a ) )
    }
  implicit def hnil: ExtractPaths[HNil] = new ExtractPaths[HNil] { def extractPaths(c: HNil ): List[Symbol] = Nil }
  implicit def cnil: ExtractPaths[CNil] = new ExtractPaths[CNil] { def extractPaths(c: CNil ): List[Symbol] = Nil }
  implicit def hcons[K <: Symbol, H, T <: HList](
      implicit
      key: Witness.Aux[K],
      head: ExtractPaths[FieldType[K, H]],
      tail: ExtractPaths[T]
    ): ExtractPaths[FieldType[K, H] :: T] =
    new ExtractPaths[FieldType[K, H] :: T] {
      def extractPaths(c: FieldType[K, H] :: T ) = head.extractPaths( c.head ) ::: tail.extractPaths( c.tail )
    }
  implicit def leaf[K <: Symbol, L](implicit key: Witness.Aux[K], L: ExtractPaths[L] ): ExtractPaths[FieldType[K, L]] =
    new ExtractPaths[FieldType[K, L]] {
      def extractPaths(c: FieldType[K, L] ) = List( key.value ) ::: L.extractPaths( c )
    }
// implicit def list[L](implicit L: IsLeaf[L] ): ExtractPaths[List[L]] =
//   new ExtractPaths[List[L]] {
//     def extractPaths(cs: List[L] ): List[Symbol] = cs.map( L.leaf )
//   }
// implicit def option[L](implicit L: IsLeaf[L] ): ExtractPaths[Option[L]] =
//   new ExtractPaths[Option[L]] {
//     def extractPaths(cs: Option[L] ): List[Symbol] = cs.map( L.leaf ).toList
//   }
}

case class X()
case class Y()
case class Z()
case class B(x: X )
case class C(y: Y, z: Z )
case class A(b: B, c: C )

object Main extends App {
  val a = A( B( X() ), C( Y(), Z() ) )
  pprint.pprintln( ExtractPaths[A].extractPaths( a ) )
}
