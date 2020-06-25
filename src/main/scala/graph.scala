import simulacrum._
import shapeless._
import shapeless.nat._
import shapeless.ops.nat._
import labelled._
import ops.record._
import scala.language.implicitConversions
import scala.language.higherKinds
import cats.{ Applicative, Eq }
import cats.implicits._

sealed trait IGraph[I, G, O]
object IGraph {
  case class Nodes[I, G, O](graph: G ) extends IGraph[I, G, O]
  object Nodes {
    def node[A](a: A ): IGraph.Nodes[_1, A, _1] = IGraph.Nodes( a )
  }
  case class Empty[I, G, O]() extends IGraph[I, G, O]
  object Empty {
    def empty[A]: Empty[_0, A, _0] = Empty()
  }
}

trait Juxtapose[A, B] {
  type Out
  def juxtapose(g1: A, g2: B ): Out
}

trait Concat[A, B] {
  type Out
  def concat(a: A, b: B ): Out
}

object Juxtapose {
  type Aux[A, B, C] = Juxtapose[A, B] { type Out = C }
  def apply[A, B, C](implicit J: Juxtapose.Aux[A, B, C] ) = J

  implicit def nodes[A, B, IA <: Nat, OA <: Nat, IB <: Nat, OB <: Nat, C](implicit R: Juxtapose.Aux[A, B, C], SI: Sum[IA, IB], SO: Sum[OA, OB] ) =
    new Juxtapose[IGraph.Nodes[IA, A, OA], IGraph.Nodes[IB, B, OB]] {
      type Out = IGraph[SI.Out, C, SO.Out]
      def juxtapose(g1: IGraph.Nodes[IA, A, OA], g2: IGraph.Nodes[IB, B, OB] ) =
        IGraph.Nodes( R.juxtapose( g1.graph, g2.graph ) )
    }
}
object Concat {
  type Aux[A, B, C] = Concat[A, B] { type Out = C }
  def apply[A, B, C](implicit C: Concat.Aux[A, B, C] ) = C
  implicit def nodes[A, B, I <: Nat, O <: Nat, OA <: Nat, IB <: Nat, R <: Nat](implicit C: Concat.Aux[A, B, C], gte: GTEq[OA, IB], R: Diff.Aux[OA, IB, R], S: Sum[O, R] ) =
    new Concat[IGraph.Nodes[I, A, OA], IGraph.Nodes[IB, B, O]] {
      type Out = IGraph.Nodes[I, C, S.Out]
      def concat(start: IGraph.Nodes[I, A, OA], end: IGraph.Nodes[IB, B, O] ) =
        IGraph.Nodes( C.concat( start.graph, end.graph ) )
    }
}

trait Graph[N[_]] {
  def node[A](a: A ): N[IGraph[_1, A, _1]]
  def concat[A, B, I <: Nat, O <: Nat](start: N[IGraph[_, A, O]], end: N[IGraph[I, B, _]] )(implicit R: Concat.Aux[A, B, C] ): N[IGraph[I, C, O]]
  def juxtapose[A, B, IA <: Nat, OA <: Nat, IB <: Nat, OB <: Nat, C](
      g1: N[IGraph[IA, A, OA]],
      g2: N[IGraph[IB, B, OB]]
    )(implicit R: Juxtapose.Aux[A, B, C],
      SI: Sum[IA, IB],
      SO: Sum[OA, OB]
    ): N[IGraph[SI.Out, C, SO.Out]]
  def empty[A](a: N[A] ): N[IGraph.Empty[_0, A, _0]]
}

object test extends App {
  implicit class JuxtaposeSyntax[A, B, IA <: Nat, OA <: Nat, IB <: Nat, OB <: Nat](a: IGraph[IA, A, OA] ) {
    def juxtapose(b: IGraph[IB, B, OB] )(implicit J: Juxtapose[IGraph[IA, A, OA], IGraph[IB, B, OB]] ) = J.juxtapose( a, b )
  }
  case class R[A](unR: A )
  val g1 = IGraph.Nodes.node( 1 )
  val g2 = IGraph.Nodes.node( 2 )
  implicit def ints = new Juxtapose[Int, Int] {
    type Out = Int
    def juxtapose(g1: Int, g2: Int ): Int = g1 + g2
  }
  pprint.pprintln( g1.juxtapose( g2 ) )
// implicit def eqR[A]: Eq[R[A]] = Eq.fromUniversalEquals
// implicit def graphR = new Graph[R] {
//   def node[A](a: A ): R[A] = ???
//   def edge[A, B](start: R[A], end: R[B] ): R[Edge[A, B]] = ???
//   def join[A, B, C](edge: Edge[A, B], node: R[C] ): R[Join[A, B, C]] = ???
//   def empty[A](a: R[A] ): R[Empty[A]] = ???
// }
// val RG = Graph[R]
// import RG._
// import Graph.ops._
// val g1 = node( 1 ).edge( node( 2 ) ).edge( node( 3 ) )
// val g2 = node( 1 ).edge( node( 2 ) ).join( node( 3 ) )
}
