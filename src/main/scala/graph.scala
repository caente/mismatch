import shapeless.Nat
import shapeless.nat._
import shapeless.ops.nat.Sum

case class IGraph[I, G, O](graph: G )
object IGraph {
  def node[I <: Nat, A, O <: Nat](a: A ): IGraph[I, A, O] = IGraph( a )
}
sealed trait Connector[N, S, E, W]
object Connector {
  case class Vertical[N, S](up: N, down: S ) extends Connector[N, S, Nothing, Nothing]
  case class Horizontal[E, W](left: E, right: W ) extends Connector[Nothing, Nothing, E, W]
}

trait Juxtapose[A, B] {
  type Out
  def juxtapose(g1: A, g2: B ): Out
}

trait JuxtaposeBase {
  implicit def sum[A, B] = new Juxtapose[A, B] {
    type Out = Connector.Vertical[A, B]
    def juxtapose(g1: A, g2: B ) = Connector.Vertical( g1, g2 )
  }
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
      def juxtapose(g1: Connector.Vertical[A, B], g2: C ) = Connector.Vertical( g1.up, Connector.Vertical( g1.down, g2 ) )
    }
  implicit def verticalRight[A, B, C, JA, JB] =
    new Juxtapose[A, Connector.Vertical[B, C]] {
      type Out = Connector.Vertical[A, Connector.Vertical[B, C]]
      def juxtapose(g1: A, g2: Connector.Vertical[B, C] ) = Connector.Vertical( g1, g2 )
    }
}

trait Concat[A, B] {
  type Out
  def concat(g1: A, g2: B ): Out
}

trait ConcatBase {}
object Concat extends ConcatBase {
  type Aux[A, B, C] = Concat[A, B] { type Out = C }
  def apply[A, B, C](implicit J: Concat.Aux[A, B, C] ) = J

  implicit def nodes[A, B, I <: Nat, O <: Nat, R <: Nat, C](implicit R: Concat.Aux[A, B, C] ) =
    new Concat[IGraph[I, A, R], IGraph[R, B, O]] {
      type Out = IGraph[I, C, O]
      def concat(g1: IGraph[I, A, R], g2: IGraph[R, B, O] ) =
        IGraph( R.concat( g1.graph, g2.graph ) )
    }
  implicit def horizontalLeft[A, B, C, JA, JB] =
    new Concat[Connector.Horizontal[A, B], C] {
      type Out = Connector.Horizontal[A, Connector.Horizontal[B, C]]
      def concat(g1: Connector.Horizontal[A, B], g2: C ) = Connector.Horizontal( g1.left, Connector.Horizontal( g1.right, g2 ) )
    }
  implicit def horizontalRight[A, B, C, JA, JB] =
    new Concat[A, Connector.Horizontal[B, C]] {
      type Out = Connector.Horizontal[A, Connector.Horizontal[B, C]]
      def concat(g1: A, g2: Connector.Horizontal[B, C] ) = Connector.Horizontal( g1, g2 )
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
  implicit class ConcatSyntax[A, B](a: A ) {
    def concat(b: B )(implicit J: Concat[A, B] ) = J.concat( a, b )
  }
  case class R[A](unR: A )
  val g1: IGraph[_1, Int, _1] = IGraph.node( 1 )
  val g2: IGraph[_1, Int, _1] = IGraph.node( 2 )
  val g3: IGraph[_1, Int, _1] = IGraph.node( 3 )
  implicit def intsC = new Concat[Int, Int] {
    type Out = Connector.Horizontal[Int, Int]
    def concat(g1: Int, g2: Int ) = Connector.Horizontal( g1, g2 )
  }

  val v12: IGraph[_2, Connector.Vertical[Int, Int], _2] = g1.juxtapose( g2 )
  val v123: IGraph[_3, Connector.Vertical[Int, Connector.Vertical[Int, Int]], _3] = v12.juxtapose( g3 )
  val v312: IGraph[_3, Connector.Vertical[Int, Connector.Vertical[Int, Int]], _3] = g3.juxtapose( v12 )
  val v1231: IGraph[_4, Connector.Vertical[Int, Connector.Vertical[Connector.Vertical[Int, Int], Int]], _4] = v12.juxtapose( g3 ).juxtapose( g1 )
  val v1123: IGraph[_4, Connector.Vertical[Int, Connector.Vertical[Int, Connector.Vertical[Int, Int]]], _4] = g1.juxtapose( v12.juxtapose( g3 ) )
  pprint.pprintln( g1 )
  pprint.pprintln( v12 )
  pprint.pprintln( v123 )
  pprint.pprintln( v312 )
  pprint.pprintln( v1231 )
  pprint.pprintln( v1123 )
  println( "-" * 30 )
  val h12: IGraph[_1, Connector.Horizontal[Int, Int], _1] = g1.concat( g2 )
  val h123: IGraph[_1, Connector.Horizontal[Int, Connector.Horizontal[Int, Int]], _1] = h12.concat( g3 )
  val h312: IGraph[_1, Connector.Horizontal[Int, Connector.Horizontal[Int, Int]], _1] = g3.concat( h12 )
  val h1231: IGraph[_1, Connector.Horizontal[Int, Connector.Horizontal[Connector.Horizontal[Int, Int], Int]], _1] = h12.concat( g3 ).concat( g1 )
  val h1123: IGraph[_1, Connector.Horizontal[Int, Connector.Horizontal[Int, Connector.Horizontal[Int, Int]]], _1] = g1.concat( h12.concat( g3 ) )
  pprint.pprintln( h12 )
  pprint.pprintln( h123 )
  pprint.pprintln( h312 )
  pprint.pprintln( h1231 )
  pprint.pprintln( h1123 )
  println( "-" * 30 )
  val v12h3 = h12.juxtapose( h1123 )
  pprint.pprintln( v12h3 )
}
