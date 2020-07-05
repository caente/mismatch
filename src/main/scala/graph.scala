import shapeless.Nat
import shapeless.nat._
import shapeless.ops.nat.Sum

case class IGraph[I, G, O](graph: G )
object IGraph {
  def node[I <: Nat, A, O <: Nat](a: A ): IGraph[I, A, O] = IGraph( a )
}
sealed trait Connector[N, S, E, W]
object Connector {
  case class Adjacent[N, S](up: N, down: S ) extends Connector[N, S, Nothing, Nothing]
  case class Connected[E, W](left: E, right: W ) extends Connector[Nothing, Nothing, E, W]
}

trait Juxtapose[A, B] {
  type Out
  def juxtapose(g1: A, g2: B ): Out
}

trait JuxtaposeBase {
  implicit def nodesBase[A, B, IA <: Nat, OA <: Nat, IB <: Nat, OB <: Nat, C](implicit SI: Sum[IA, IB], SO: Sum[OA, OB] ) =
    new Juxtapose[IGraph[IA, A, OA], IGraph[IB, B, OB]] {
      type Out = IGraph[SI.Out, Connector.Adjacent[IGraph[IA, A, OA], IGraph[IB, B, OB]], SO.Out]
      def juxtapose(g1: IGraph[IA, A, OA], g2: IGraph[IB, B, OB] ) =
        IGraph( Connector.Adjacent( g1, g2 ) )
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
}

trait Concat[A, B] {
  type Out
  def concat(g1: A, g2: B ): Out
}

trait ConcatBase {
  implicit def nodesBase[A, B, I <: Nat, O <: Nat, R <: Nat] =
    new Concat[IGraph[I, A, R], IGraph[R, B, O]] {
      type Out = IGraph[I, Connector.Connected[IGraph[I, A, R], IGraph[R, B, O]], O]
      def concat(g1: IGraph[I, A, R], g2: IGraph[R, B, O] ) =
        IGraph( Connector.Connected( g1, g2 ) )
    }
}
object Concat extends ConcatBase {
  type Aux[A, B, C] = Concat[A, B] { type Out = C }
  def apply[A, B, C](implicit J: Concat.Aux[A, B, C] ) = J

  implicit def nodes[A <: IGraph[_, _, _], B <: IGraph[_, _, _], I <: Nat, O <: Nat, R <: Nat, C](implicit R: Concat.Aux[A, B, C] ) =
    new Concat[IGraph[I, A, R], IGraph[R, B, O]] {
      type Out = IGraph[I, C, O]
      def concat(g1: IGraph[I, A, R], g2: IGraph[R, B, O] ) =
        IGraph( R.concat( g1.graph, g2.graph ) )
    }
}

trait Graph[N[_]] {
  def node[I, A, O](a: A ): N[IGraph[I, A, O]]
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
  type Single[A] = IGraph[_1, A, _1]
  val g1: Single[Int] = IGraph.node( 1 )
  val g2: Single[Int] = IGraph.node( 2 )
  val g3: Single[Int] = IGraph.node( 3 )
  val g4: Single[Int] = IGraph.node( 4 )

  val v12: IGraph[_2, Connector.Adjacent[Single[Int], Single[Int]], _2] = g1.juxtapose( g2 )
  val v123: IGraph[_3, Connector.Adjacent[IGraph[_2, Connector.Adjacent[Single[Int], Single[Int]], _2], Single[Int]], _3] = v12.juxtapose( g3 )
  val v312: IGraph[_3, Connector.Adjacent[Single[Int], IGraph[_2, Connector.Adjacent[Single[Int], Single[Int]], _2]], _3] = g3.juxtapose( v12 )
  val v1231: IGraph[_4, Connector.Adjacent[IGraph[_3, Connector.Adjacent[IGraph[_2, Connector.Adjacent[Single[Int], Single[Int]], _2], Single[Int]], _3], Single[Int]], _4] =
    v12.juxtapose( g3 ).juxtapose( g1 )
  val v1123: IGraph[_4, Connector.Adjacent[Single[Int], IGraph[_3, Connector.Adjacent[IGraph[_2, Connector.Adjacent[Single[Int], Single[Int]], _2], Single[Int]], _3]], _4] =
    g1.juxtapose( v12.juxtapose( g3 ) )
  pprint.pprintln( g1 )
  pprint.pprintln( v12 )
  pprint.pprintln( v123 )
  pprint.pprintln( v312 )
  pprint.pprintln( v1231 )
  pprint.pprintln( v1123 )
  println( "-" * 30 )
  val h12: Single[Connector.Connected[Single[Int], Single[Int]]] = g1.concat( g2 )
  val h123: Single[Connector.Connected[Single[Connector.Connected[Single[Int], Single[Int]]], Single[Int]]] = h12.concat( g3 )
  val h312: Single[Connector.Connected[Single[Int], Single[Connector.Connected[Single[Int], Single[Int]]]]] = g3.concat( h12 )
  //val h1231: IGraph[_1, Connector.Horizontal[Int, Connector.Horizontal[Connector.Horizontal[Int, Int], Int]], _1] = h12.concat( g3 ).concat( g1 )
  //val h1123: IGraph[_1, Connector.Horizontal[Int, Connector.Horizontal[Int, Connector.Horizontal[Int, Int]]], _1] = g1.concat( h12.concat( g3 ) )
  pprint.pprintln( h12 )
  pprint.pprintln( h123 )
  pprint.pprintln( h312 )
  //pprint.pprintln( h1231 )
  //pprint.pprintln( h1123 )
  println( "-" * 30 )
  val head = IGraph.node[_1, Single[Int], _3]( g1 )
  pprint.pprintln( head )
  pprint.pprintln( head.concat( v123 ).juxtapose( g4.concat( g2 ).concat( head ).concat( v123 ) ) )
}
