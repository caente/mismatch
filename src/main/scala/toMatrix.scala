import simulacrum._
import shapeless._
import labelled._
import ops.record._
import scala.language.implicitConversions
import scala.language.higherKinds
import cats.{ Applicative, Eq }
import cats.implicits._

object paths {
  case class Edge[A, B](start: A, end: B )
  case class Siblings[A, B](s1: A, s2: B )
  case class Node[A](node: A )
  case class Empty[A]()
  implicit def eqEdge[A: Eq, B: Eq]: Eq[Edge[A, B]] = Eq.fromUniversalEquals
  implicit def eqSiblings[A, B]: Eq[Siblings[A, B]] = Eq.fromUniversalEquals
  implicit def eqNode[A]: Eq[Node[A]] = Eq.fromUniversalEquals
  implicit def eqEmpty[A]: Eq[Empty[A]] = Eq.fromUniversalEquals
  @typeclass trait Leaf[Parent] {
    type LeafOut
    def leaf(a: Parent ): LeafOut
  }
  object Leaf {
    import Leaf.ops._
    implicit def node[Parent] = new Leaf[Node[Parent]] {
      type LeafOut = Node[Parent]
      def leaf(a: Node[Parent] ) = a
    }
    type Aux[Parent, L] = Leaf[Parent] { type LeafOut = L }
    implicit def edge[Start, End, L](implicit B: Leaf.Aux[End, L] ) = new Leaf[Edge[Start, End]] {
      type LeafOut = Edge[Start, Edge[End, L]]
      def leaf(e: Edge[Start, End] ) = Edge( e.start, Edge( e.end, e.end.leaf ) )
    }

  }

  import paths.Leaf.ops._

  @typeclass trait Path[N[_]] {
    def node[A](a: A ): N[Node[A]]
    def empty[A]: N[Empty[A]]
    def append[A, B](node1: N[A], node2: N[B] ): N[Edge[A, B]]
    def join[A, B, C](node1: N[Edge[A, B]], node2: N[C] ): N[Edge[A, Siblings[B, C]]]
  }

  import paths.Path.ops._
  trait ExtractPaths[C, N[_]] {
    type Out
    def extractPaths(c: C ): N[Out]
  }

  /*
  trait ExtractPathsLow {
    implicit def leaf[K <: Symbol, A, N[_]](implicit key: Witness.Aux[K], L: ExtractPaths[A, N], Pth: Path[N] ): ExtractPaths[FieldType[K, A], N] =
      new ExtractPaths[FieldType[K, A], N] {
        type Out = Edge[Node[K], Edge[Node[K], L.Out]]
        def extractPaths(c: FieldType[K, A] ) = Pth.node( key.value ).append( L.extractPaths( c ) )
      }
    implicit def optionLeaf[L, K <: Symbol, N[_]](implicit key: Witness.Aux[K], Pth: Path[N], A: Applicative[N] ): ExtractPaths[FieldType[K, Option[L]], N] =
      new ExtractPaths[FieldType[K, Option[L]], N] {
        type Out = Option[Node[K]]
        def extractPaths(cs: FieldType[K, Option[L]] ) = cs.map( _ => Pth.node( key.value ) ).traverse( identity )
      }
    implicit def hconsNode[K <: Symbol, H, T <: HList, N[_], G, P](
        implicit
        key: Witness.Aux[K],
        tail: ExtractPaths.Aux[T, N, G],
        Pth: Path[N]
      ): ExtractPaths[FieldType[K, H] :: T, N] =
      new ExtractPaths[FieldType[K, H] :: T, N] {
        type Out = Siblings[Node[K], G]
        def extractPaths(c: FieldType[K, H] :: T ) = Pth.node( key.value ).join( tail.extractPaths( c.tail ) )
      }
    implicit def list[K <: Symbol, L, N[_], P, G](
        implicit
        key: Witness.Aux[K],
        L: ExtractPaths.Aux[L, N, G],
        A: Applicative[N],
        Pth: Path[N]
      ): ExtractPaths[FieldType[K, List[L]], N] =
      new ExtractPaths[FieldType[K, List[L]], N] {
        type Out = Edge[Node[K], Edge[Node[K], List[G]]]
        def extractPaths(c: FieldType[K, List[L]] ) = Pth.node( key.value ).append( c.map( L.extractPaths ).traverse( identity ) )
      }
  }
  object ExtractPaths extends ExtractPathsLow {
    type Aux[C, N[_], O] = ExtractPaths[C, N] { type Out = O }
    def apply[C, N[_]](implicit E: ExtractPaths[C, N] ) = E
    implicit def generic[A, G, N[_], P](implicit gen: LabelledGeneric.Aux[A, G], sg: Lazy[ExtractPaths.Aux[G, N, P]] ): ExtractPaths[A, N] =
      new ExtractPaths[A, N] {
        type Out = P
        def extractPaths(a: A ) = sg.value.extractPaths( gen.to( a ) )
      }
    implicit def hcons[K <: Symbol, H, T <: HList, N[_], Tail, Head, P](
        implicit
        key: Witness.Aux[K],
        head: ExtractPaths.Aux[FieldType[K, H], N, Head],
        tail: ExtractPaths.Aux[T, N, Tail],
        L1: Leaf.Aux[P, Head],
        L2: Leaf.Aux[P, Tail],
        Pth: Path[N]
      ): ExtractPaths[FieldType[K, H] :: T, N] =
      new ExtractPaths[FieldType[K, H] :: T, N] {
        type Out = Siblings[Head, Tail]
        def extractPaths(c: FieldType[K, H] :: T ) = head.extractPaths( c.head ).join( tail.extractPaths( c.tail ) )
      }
    implicit def hnil[N[_]](implicit P: Path[N] ): ExtractPaths[HNil, N] = new ExtractPaths[HNil, N] {
      type Out = Empty[Nothing]
      def extractPaths(c: HNil ) = P.empty[Nothing]
    }
    //implicit def cnil[N[_]]: ExtractPaths[CNil, N] = new ExtractPaths[CNil, N] {
    //  def extractPaths(c: CNil ) = Path.empty
    //}
   */
  /*
    implicit def leafList[K <: Symbol, L, N[_], P](implicit key: Witness.Aux[K] ): ExtractPaths[FieldType[K, List[L]], N] =
      new ExtractPaths[FieldType[K, List[L]], N] {
        type Out = Edge[P, Edge[Node[K], List[L]]]
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
 */
}

case class X()
case class Y(i: Int )
case class Z(i: Int )
case class B(y: Option[Y], x: X )
case class C(z: List[Z], y: Y )
case class A(b: B, c: C )

object Main {
// import paths._
// import Leaf.ops._
// import paths.Path.ops._
// case class R[A](unR: A )
// implicit def eqR[A]: Eq[R[A]] = Eq.fromUniversalEquals
// implicit object RPath extends Path[R] {
//   def node[A](a: A ): R[Node[A]] = R( Node( a ) )
//   def empty[A]: R[Empty[A]] = R( Empty[A]() )
//   def append[A, B](node1: R[A], node2: R[B] ): R[paths.Edge[A, B]] = {
//     R( Edge( node1.unR, node2.unR ) )
//   }
//   def join[A, B](node1: R[A], node2: R[B] ): R[paths.Siblings[A, B]] = {
//     R( Siblings( node1.unR, node2.unR ) )
//   }
  //}
  //implicit def leafZ = new Leaf[Z] {
  //  def leaf(z: Z ): Symbol = Symbol( z.i.toString )
  //}
  //val a1 = A( B( None, X() ), C( List( Z( 1 ), Z( 2 ) ), Y( 1 ) ) )
  //val a2 = A( B( Some( Y( 3 ) ), X() ), C( List(), Y( 2 ) ) )
  //def node[A](a: A ) = Path[R].node( a )
  //pprint.pprintln( node( 1 ).append( node( 2 ) ).append( node( 3 ) ) )
  //pprint.pprintln( node( 1 ).append( node( 2 ) ).join( node( 3 ) ) )
  //assert( R( Edge( Node( 1 ), Edge( Node( 2 ), Node( 3 ) ) ) ) === node( 1 ).append( node( 2 ) ).append( node( 3 ) ) )
  //assert( R( Edge( Node( 1 ), Siblings( Node( 2 ), Node( 3 ) ) ) ) === node( 1 ).append( node( 2 ) ).join( node( 3 ) ) )
  //assert( R( Edge( Node( 1 ), Siblings( Node( 2 ), Siblings( Node( 3 ), Node( 4 ) ) ) ) ) === node( 1 ).append( node( 2 ) ).join( node( 3 ) ).join( node( 4 ) ) )
  //pprint.pprintln( ExtractPaths[A, R].extractPaths( a1 ) )
  //val a1ls = ExtractPaths[A].extractPaths( a1 )
  //val a2ls = ExtractPaths[A].extractPaths( a2 )
  //pprint.pprintln( a1ls, width = 60 )
  //pprint.pprintln( a2ls, width = 60 )
}
