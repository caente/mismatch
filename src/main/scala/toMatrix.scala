package ai.x
package toMatrix

object paths {
  import shapeless._
  import labelled._
  import ops.record._
  import scala.language.implicitConversions
  import scala.language.higherKinds
  import simulacrum._

  trait HasChildren[N[_], C] {
    def children[A](node: N[A] ): Set[N[C]]
  }
  trait HasParent[N[_], P] {
    def parent[A](node: N[A] ): N[P]
  }
  trait Path[N[_]] {
    //def returns `this` as parent of `o`
    //`o` cannot have a parent
    def append[P[_], A, O](node1: N[P[A]], node2: N[O] ): N[P[O]] 
    //returns the parent of both object, whith the nodes as its children
    //both object need the same parent, or only one object will have a parent
    def addSibling[O, P](node1: N, node2: O ): P
  }
  object Path {
    def empty: Path = Empty
  }

  case class Start(tag: Symbol, children: Set[Path] ) extends Path {
    def append(o: Path ): Path = ???
    def addSibling(o: Path ): Path
  }
  case class Node(parent: Path, children: Set[Path], tag: Symbol ) extends Path {
    def append(o: Path ): Path =
      o match {
        case Empty => this
        case _     => Node( children + o, tag )
      }
  }
  case class End(parent: Path, tag: Symbol ) extends Path {
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
        def extractPaths(c: FieldType[K, H] :: T ) = End( key.value ).append( tail.extractPaths( c.tail ) )
      }
    implicit def list[K <: Symbol, L](implicit key: Witness.Aux[K], L: ExtractPaths[L] ): ExtractPaths[FieldType[K, List[L]]] =
      new ExtractPaths[FieldType[K, List[L]]] {
        def extractPaths(c: FieldType[K, List[L]] ) = c.map( L.extractPaths ).foldLeft( Path.empty )( _ append _ )
      }

  }
  object ExtractPaths extends ExtractPathsLow {
    implicit def generic[A, G](implicit gen: LabelledGeneric.Aux[A, G], sg: Lazy[ExtractPaths[G]] ): ExtractPaths[A] =
      new ExtractPaths[A] {
        def extractPaths(a: A ) = sg.value.extractPaths( gen.to( a ) )
      }

    implicit def hcons[K <: Symbol, H, T <: HList](
        implicit
        key: Witness.Aux[K],
        head: ExtractPaths[FieldType[K, H]],
        tail: ExtractPaths[T]
      ): ExtractPaths[FieldType[K, H] :: T] =
      new ExtractPaths[FieldType[K, H] :: T] {
        def extractPaths(c: FieldType[K, H] :: T ) = head.extractPaths( c.head ).append( tail.extractPaths( c.tail ) )
      }
    implicit def hnil: ExtractPaths[HNil] = new ExtractPaths[HNil] { def extractPaths(c: HNil ) = Path.empty }
    implicit def cnil: ExtractPaths[CNil] = new ExtractPaths[CNil] { def extractPaths(c: CNil ) = Path.empty }

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
  pprint.pprintln( a1ls, width = 60 )
  pprint.pprintln( a2ls, width = 60 )
}
