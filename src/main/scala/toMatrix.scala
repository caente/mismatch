package ai.x
package toMatrix

object paths {
  import shapeless._
  import labelled._
  import ops.record._
  import scala.language.implicitConversions
  import simulacrum._

  sealed trait Path {
    def append(o: Path ): Path
  }
  object Path {
    def empty: Path = Empty
  }
  case class Node(children: Set[Path], tag: Symbol ) extends Path {
    def append(o: Path ): Path =
      o match {
        case Empty => this
        case _     => Node( children + o, tag )
      }
  }
  case class End(tag: Symbol ) extends Path {
    def append(o: Path ): Path =
      o match {
        case Empty => this
        case _     => Node( Set( o ), tag )
      }
  }
  case object Empty extends Path {
    def append(o: Path ): Path = o
  }

  @typeclass trait ExtractPaths[C] {
    def extractPaths(c: C ): Path
  }

  @typeclass trait Leaf[A] {
    def leaf(a: A ): Symbol
  }

  trait ExtractPathsLow {
    implicit def leaf[K <: Symbol, L](implicit key: Witness.Aux[K], L: ExtractPaths[L] ): ExtractPaths[FieldType[K, L]] =
      new ExtractPaths[FieldType[K, L]] {
        def extractPaths(c: FieldType[K, L] ) = End( key.value ).append( L.extractPaths( c ) )
      }
    implicit def optionLeaf[L, K <: Symbol](implicit key: Witness.Aux[K] ): ExtractPaths[FieldType[K, Option[L]]] =
      new ExtractPaths[FieldType[K, Option[L]]] {
        def extractPaths(cs: FieldType[K, Option[L]] ) = cs.map( _ => End( key.value ) ).foldLeft( Path.empty )( _ append _ )
      }
    implicit def hconsLeaf[K <: Symbol, H, T <: HList](
        implicit
        key: Witness.Aux[K],
        tail: ExtractPaths[T]
      ): ExtractPaths[FieldType[K, H] :: T] =
      new ExtractPaths[FieldType[K, H] :: T] {
        implicit def hnil: ExtractPaths[HNil] = new ExtractPaths[HNil] { def extractPaths(c: HNil ) = Path.empty }
        implicit def cnil: ExtractPaths[CNil] = new ExtractPaths[CNil] { def extractPaths(c: CNil ) = Path.empty }
        implicit def hcons[K <: Symbol, H, T <: HList](
            implicit
            key: Witness.Aux[K],
            head: ExtractPaths[FieldType[K, H]],
            tail: ExtractPaths[T]
          ): ExtractPaths[FieldType[K, H] :: T] =
          new ExtractPaths[FieldType[K, H] :: T] {
            def extractPaths(c: FieldType[K, H] :: T ) = head.extractPaths( c.head ).append( tail.extractPaths( c.tail ) )
          }

        implicit def leafList[K <: Symbol, L](implicit key: Witness.Aux[K], L: Leaf[L] ): ExtractPaths[FieldType[K, List[L]]] =
          new ExtractPaths[FieldType[K, List[L]]] {
            def extractPaths(c: FieldType[K, List[L]] ) = {
              c.map( c => End( L.leaf( c ) ) ).foldLeft( Path.empty )( _ append _ )
            }
          }

        implicit def option[L, K <: Symbol](implicit key: Witness.Aux[K], L: ExtractPaths[L] ): ExtractPaths[FieldType[K, Option[L]]] =
          new ExtractPaths[FieldType[K, Option[L]]] {
            def extractPaths(cs: FieldType[K, Option[L]] ) =
              (cs.toList.map( _ => End( key.value ) ) ::: cs.toList.map( L.extractPaths )).foldLeft( Path.empty )( _ append _ )
          }
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
