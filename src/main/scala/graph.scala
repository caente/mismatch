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

case class IGraph[I, G, O](graph: G )
object IGraph {
  def node[A](a: A ): IGraph[_1, A, _1] = IGraph( a )
}
trait Juxtapose[A, B] {
  type Out
  def juxtapose(g1: A, g2: B ): Out
}

trait Concat[A, B] {
  type Out
  def concat(a: A, b: B ): Out
}

trait JuxtaposeBase {
  implicit def sum[A, B] = new Juxtapose[A, B] {
    type Out = Connector.Vertical[A, B]
    def juxtapose(g1: A, g2: B ) = Connector.Vertical( g1, g2 )
  }
}
sealed trait Connector[N, S, E, W]
object Connector {
  case class Vertical[N, S](north: N, south: S ) extends Connector[N, S, Nothing, Nothing]
  case class Horizontal[E, W](east: E, west: W ) extends Connector[Nothing, Nothing, E, W]
}

object Juxtapose extends JuxtaposeBase {
  type Aux[A, B, C] = Juxtapose[A, B] { type Out = C }
  def apply[A, B, C](implicit J: Juxtapose.Aux[A, B, C] ) = J

  implicit def nodes[A, B, IA <: Nat, OA <: Nat, IB <: Nat, OB <: Nat, C](implicit R: Juxtapose.Aux[A, B, C], SI: Sum[IA, IB], SO: Sum[OA, OB] ) =
    new Juxtapose[IGraph[IA, A, OA], IGraph[IB, B, OB]] {
      type Out = IGraph[SI.Out, C, SO.Out]
      def juxtapose(g1: IGraph[IA, A, OA], g2: IGraph[IB, B, OB] ) =
        IGraph( R.juxtapose( g1.graph, g2.graph ) )
    }
  implicit def verticalLeft[A, B, C, JA, JB] =
    new Juxtapose[Connector.Vertical[A, B], C] {
      type Out = Connector.Vertical[A, Connector.Vertical[B, C]]
      def juxtapose(g1: Connector.Vertical[A, B], g2: C ) = Connector.Vertical( g1.north, Connector.Vertical( g1.south, g2 ) )
    }
}
trait ConcatBase {
  implicit def prod[A, B] = new Concat[A, B] {
    type Out = Connector.Horizontal[A, B]
    def concat(g1: A, g2: B ) = Connector.Horizontal( g1, g2 )
  }
}
object Concat extends ConcatBase {
  type Aux[A, B, C] = Concat[A, B] { type Out = C }
  def apply[A, B, C](implicit C: Concat.Aux[A, B, C] ) = C
  implicit def nodes[A, B, I <: Nat, O <: Nat, OA <: Nat, IB <: Nat, R <: Nat](implicit C: Concat.Aux[A, B, C], gte: GTEq[OA, IB], R: Diff.Aux[OA, IB, R], S: Sum[O, R] ) =
    new Concat[IGraph[I, A, OA], IGraph[IB, B, O]] {
      type Out = IGraph[I, C, S.Out]
      def concat(start: IGraph[I, A, OA], end: IGraph[IB, B, O] ) =
        IGraph( C.concat( start.graph, end.graph ) )
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
}

object test extends App {
  implicit class JuxtaposeSyntax[A, B](a: A ) {
    def juxtapose(b: B )(implicit J: Juxtapose[A, B] ) = J.juxtapose( a, b )
  }
  implicit class ConcatSyntax[A, B, IA <: Nat, OA <: Nat, IB <: Nat, OB <: Nat](a: IGraph[IA, A, OA] ) {
    def concat(b: IGraph[IB, B, OB] )(implicit J: Concat[IGraph[IA, A, OA], IGraph[IB, B, OB]] ) = J.concat( a, b )
  }
  case class R[A](unR: A )
  val g1: IGraph[_1, Int, _1] = IGraph.node( 1 )
  val g2: IGraph[_1, Int, _1] = IGraph.node( 2 )
  val g3: IGraph[_1, Int, _1] = IGraph.node( 3 )
  implicit def ints = new Juxtapose[Int, Int] {
    type Out = Connector.Vertical[Int, Int]
    def juxtapose(g1: Int, g2: Int ) = Connector.Vertical( g1, g2 )
  }
  val g12: IGraph[_2, Connector.Vertical[Int, Int], _2] = g1.juxtapose( g2 )
  val g123: IGraph[_3, Connector.Vertical[Int, Connector.Vertical[Int, Int]], _3] = g12.juxtapose( g3 )
  val g312: IGraph[_3, Connector.Vertical[Int, Connector.Vertical[Int, Int]], _3] = g3.juxtapose( g12 )
  val g1231: IGraph[_4, Connector.Vertical[Int, Connector.Vertical[Connector.Vertical[Int, Int], Int]], _4] = g12.juxtapose( g3 ).juxtapose( g1 )
  val g1123: IGraph[_4, Connector.Vertical[Int, Connector.Vertical[Int, Connector.Vertical[Int, Int]]], _4] = g1.juxtapose( g12.juxtapose( g3 ) )
  pprint.pprintln( g1 )
  pprint.pprintln( g123 )
  pprint.pprintln( g312 )
  pprint.pprintln( g1231 )
  pprint.pprintln( g1123 )
}
