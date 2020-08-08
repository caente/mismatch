package graph_recursive

import shapeless.Nat
import shapeless.nat._
import shapeless.ops.nat.Sum
import shapeless.syntax.typeable
import shapeless.Witness
import simulacrum._
import cats._
import shapeless.Succ
import cats.implicits._
import higherkindness.droste._
import higherkindness.droste.data._

sealed trait IGraph[I, O, G]
object IGraph {
  def node[I <: Nat, O <: Nat, A](a: A )(implicit I: Witness.Aux[I], O: Witness.Aux[O] ): IGraph[I, O, A] = IGraph.Node( I.value, a, O.value )
  case class Node[I, O, N](in: I, graph: N, out: O ) extends IGraph[I, O, N]
  //case class Adjacents[I, O, A](in: I, up: A, down: A, out: O ) extends IGraph[I, O, A]
  //case class Connected[I, O, C](in: I, left: C, right: C, out: O ) extends IGraph[I, O, C]
  case class Empty[I, O, E](in: I, out: O ) extends IGraph[I, O, E]
  implicit def IGraphF[I, O] = new Functor[IGraph[I, O, *]] {
    def map[A, B](fa: IGraph[I, O, A] )(f: A => B ): IGraph[I, O, B] =
      fa match {
        case Node( in, graph, out ) =>
          pprint.pprintln( graph )
          throw new RuntimeException( "stopped" )
          pprint.pprintln( f( graph ) )
          Node( in, f( graph ), out )
        // case Adjacents( in, up, down, out )    => Adjacents( in, f( up ), f( down ), out )
        // case Connected( in, left, right, out ) => Connected( in, f( left ), f( right ), out )
        case Empty( in, out ) => Empty( in, out )
      }
  }
}

object schemes {
  case class Adjacent[N, S](up: N, down: S )
  case class Connected[E, W](left: E, right: W )
  def juxtapose[A, B, IA <: Nat, OA <: Nat, IB <: Nat, OB <: Nat, RI <: Nat, RO <: Nat](
      g1: IGraph[IA, OA, A],
      g2: IGraph[IB, OB, B]
    )(implicit
      SI: Sum.Aux[IA, IB, RI],
      SO: Sum.Aux[OA, OB, RO]
    ): IGraph[RI, RO, Adjacent[A, B]] = ???
}

object Test extends App {
  implicit def showSucc[P <: Nat](implicit S: shapeless.ops.nat.ToInt[Succ[P]] ): Show[Succ[P]] = Show.show( s => s.toInt.toString )
  implicit def showIGraph[I <: Nat, O <: Nat, A](implicit I: Show[I], A: Show[A], O: Show[O] ): Show[IGraph[I, O, A]] =
    Show.show {
      case IGraph.Node( in, graph, out ) => s"IGraph.Node(${in.show}, ${graph.show}, ${out.show})"
      case IGraph.Empty( in, out )       => s"IGraph.Empty(${in.show}, ${out.show})"
    }
  type Single[A] = IGraph[_1, _1, A]
  val g1: Single[Symbol] = IGraph.node( 'a )
  println( g1.show )
}
