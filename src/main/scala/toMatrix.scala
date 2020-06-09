package ai.x
package toMatrix

object paths {
  import shapeless._
  import labelled._
  import ops.record._
  import scala.language.implicitConversions
  import simulacrum._

  @typeclass trait ExtractPaths[C] {
    def extractPaths(c: C ): List[Symbol]
  }

  @typeclass trait Leaf[A] {
    def leaf(a: A ): Symbol
  }

  trait ExtractPathsLow {
    implicit def leaf[K <: Symbol, L](implicit key: Witness.Aux[K], L: ExtractPaths[L] ): ExtractPaths[FieldType[K, L]] =
      new ExtractPaths[FieldType[K, L]] {
        def extractPaths(c: FieldType[K, L] ) = List( key.value ) ::: L.extractPaths( c )
      }
    implicit def optionLeaf[L, K <: Symbol](implicit key: Witness.Aux[K] ): ExtractPaths[FieldType[K, Option[L]]] =
      new ExtractPaths[FieldType[K, Option[L]]] {
        def extractPaths(cs: FieldType[K, Option[L]] ): List[Symbol] = cs.map( _ => key.value ).toList
      }
    implicit def hconsLeaf[K <: Symbol, H, T <: HList](
        implicit
        key: Witness.Aux[K],
        tail: ExtractPaths[T]
      ): ExtractPaths[FieldType[K, H] :: T] =
      new ExtractPaths[FieldType[K, H] :: T] {
        def extractPaths(c: FieldType[K, H] :: T ) = key.value :: tail.extractPaths( c.tail )
      }
    implicit def list[K <: Symbol, L](implicit key: Witness.Aux[K], L: ExtractPaths[L] ): ExtractPaths[FieldType[K, List[L]]] =
      new ExtractPaths[FieldType[K, List[L]]] {
        def extractPaths(c: FieldType[K, List[L]] ) = c.flatMap( L.extractPaths )
      }

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

    implicit def leafList[K <: Symbol, L](implicit key: Witness.Aux[K], L: Leaf[L] ): ExtractPaths[FieldType[K, List[L]]] =
      new ExtractPaths[FieldType[K, List[L]]] {
        def extractPaths(c: FieldType[K, List[L]] ) = c.map( L.leaf )
      }

    implicit def option[L, K <: Symbol](implicit key: Witness.Aux[K], L: ExtractPaths[L] ): ExtractPaths[FieldType[K, Option[L]]] =
      new ExtractPaths[FieldType[K, Option[L]]] {
        def extractPaths(cs: FieldType[K, Option[L]] ): List[Symbol] = cs.toList.map( _ => key.value ) ::: cs.toList.flatMap( L.extractPaths )
      }
  }
}

case class X()
case class Y(i: Int )
case class Z(i: Int )
case class B(y: Option[Y], x: X )
case class C(z: List[Z], y: Y )
case class A(b: B, c: C )

object Main extends App {
  import paths._
  implicit def leafZ = new Leaf[Z] {
    def leaf(z: Z ): Symbol = Symbol( z.i.toString )
  }
  val a1 = A( B( None, X() ), C( List( Z( 1 ), Z( 2 ) ), Y( 1 ) ) )
  val a2 = A( B( Some( Y( 3 ) ), X() ), C( List(), Y( 2 ) ) )
  val a1ls = ExtractPaths[A].extractPaths( a1 )
  val a2ls = ExtractPaths[A].extractPaths( a2 )
  pprint.pprintln( a1ls )
  pprint.pprintln( a2ls )
}
