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

case class IGraph[I, G, O](in: I, graph: G, out: O )

object IGraph {
  def node[I <: Nat, A, O <: Nat](a: A )(implicit I: Witness.Aux[I], O: Witness.Aux[O] ): IGraph[I, A, O] = IGraph( I.value, a, O.value )
}
case class Adjacent[N, S](up: N, down: S )
case class Connected[E, W](left: E, right: W )

trait Juxtapose[A, B] {
  type Out
  def juxtapose(g1: A, g2: B ): Out
}

object Juxtapose {
  implicit class JuxtaposeSyntax[A, B](a: A ) {
    def juxtapose(b: B )(implicit J: Juxtapose[A, B] ) = J.juxtapose( a, b )
  }
  type Aux[A, B, C] = Juxtapose[A, B] { type Out = C }
  def apply[A, B, C](implicit J: Juxtapose.Aux[A, B, C] ) = J
  implicit def nodesBase[A, B, IA <: Nat, OA <: Nat, IB <: Nat, OB <: Nat, RI <: Nat, RO <: Nat, C](
      implicit SI: Sum.Aux[IA, IB, RI],
      SO: Sum.Aux[OA, OB, RO],
      RI: Witness.Aux[RI],
      RO: Witness.Aux[RO]
    ) =
    new Juxtapose[IGraph[IA, A, OA], IGraph[IB, B, OB]] {
      type Out = IGraph[RI, Adjacent[IGraph[IA, A, OA], IGraph[IB, B, OB]], RO]
      def juxtapose(g1: IGraph[IA, A, OA], g2: IGraph[IB, B, OB] ) =
        IGraph( RI.value, Adjacent( g1, g2 ), RO.value )
    }
  implicit def nodes[A, B, IA <: Nat, OA <: Nat, IB <: Nat, OB <: Nat, RI <: Nat, RO <: Nat, C](
      implicit R: Juxtapose.Aux[A, B, C],
      SI: Sum.Aux[IA, IB, RI],
      SO: Sum.Aux[OA, OB, RO],
      RI: Witness.Aux[RI],
      RO: Witness.Aux[RO]
    ) =
    new Juxtapose[IGraph[IA, A, OA], IGraph[IB, B, OB]] {
      type Out = IGraph[RI, C, RO]
      def juxtapose(g1: IGraph[IA, A, OA], g2: IGraph[IB, B, OB] ) =
        IGraph( RI.value, R.juxtapose( g1.graph, g2.graph ), RO.value )
    }
}

trait Concat[A, B] {
  type Out
  def concat(g1: A, g2: B ): Out
}

object Concat {
  implicit class ConcatSyntax[A, B](a: A ) {
    def concat(b: B )(implicit J: Concat[A, B] ) = J.concat( a, b )
  }

  type Aux[A, B, C] = Concat[A, B] { type Out = C }
  def apply[A, B, C](implicit J: Concat.Aux[A, B, C] ) = J
  implicit def nodesBase[A, B, I <: Nat, O <: Nat, R <: Nat](
      implicit
      I: Witness.Aux[I],
      O: Witness.Aux[O]
    ) =
    new Concat[IGraph[I, A, R], IGraph[R, B, O]] {
      type Out = IGraph[I, Connected[IGraph[I, A, R], IGraph[R, B, O]], O]
      def concat(g1: IGraph[I, A, R], g2: IGraph[R, B, O] ) =
        IGraph( I.value, Connected( g1, g2 ), O.value )
    }
  implicit def nodes[A <: IGraph[_, _, _], B <: IGraph[_, _, _], I <: Nat, O <: Nat, R <: Nat, C](
      implicit
      R: Concat.Aux[A, B, C],
      I: Witness.Aux[I],
      O: Witness.Aux[O]
    ) =
    new Concat[IGraph[I, A, R], IGraph[R, B, O]] {
      type Out = IGraph[I, C, O]
      def concat(g1: IGraph[I, A, R], g2: IGraph[R, B, O] ) =
        IGraph( I.value, R.concat( g1.graph, g2.graph ), O.value )
    }
}

@typeclass trait Graph[N[_]] {
  def node[A](a: A ): N[A]
  def concat[A, B, C](start: N[A], end: N[B] )(implicit C: Concat.Aux[A, B, C] ): N[C]
  def juxtapose[A, B, J](g1: N[A], g2: N[B] )(implicit J: Juxtapose.Aux[A, B, J] ): N[J]
}

object test extends App {
  import Concat.ConcatSyntax
  import Juxtapose.JuxtaposeSyntax

  implicit def showSucc[P <: Nat](implicit S: shapeless.ops.nat.ToInt[Succ[P]] ): Show[Succ[P]] = Show.show( s => s.toInt.toString )
  implicit def showConnected[A: Show, B: Show]: Show[Connected[A, B]] = Show.show( c => s"Connected(${c.left.show}, ${c.right.show})" )
  implicit def showAdjacent[A: Show, B: Show]: Show[Adjacent[A, B]] = Show.show( c => s"Adjacent(${c.up.show}, ${c.down.show})" )
  implicit def showIGraph[I <: Nat, O <: Nat, A](implicit I: Show[I], A: Show[A], O: Show[O] ): Show[IGraph[I, A, O]] =
    Show.show( gr => s"IGraph(${gr.in.show}, ${gr.graph.show}, ${gr.out.show})" )
  def manOf[T: Manifest](t: T ): Manifest[T] = manifest[T]
  type Single[A] = IGraph[_1, A, _1]
  val g1: Single[Int] = IGraph.node( 1 )
  val g2: Single[Int] = IGraph.node( 2 )
  val g3: Single[Int] = IGraph.node( 3 )
  val g4: Single[Int] = IGraph.node( 4 )

  val v12: IGraph[_2, Adjacent[Single[Int], Single[Int]], _2] = g1.juxtapose( g2 )
  val v123: IGraph[_3, Adjacent[IGraph[_2, Adjacent[Single[Int], Single[Int]], _2], Single[Int]], _3] = v12.juxtapose( g3 )
  val v312: IGraph[_3, Adjacent[Single[Int], IGraph[_2, Adjacent[Single[Int], Single[Int]], _2]], _3] = g3.juxtapose( v12 )
  val v1231: IGraph[_4, Adjacent[IGraph[_3, Adjacent[IGraph[_2, Adjacent[Single[Int], Single[Int]], _2], Single[Int]], _3], Single[Int]], _4] =
    v12.juxtapose( g3 ).juxtapose( g1 )
  val v1123: IGraph[_4, Adjacent[Single[Int], IGraph[_3, Adjacent[IGraph[_2, Adjacent[Single[Int], Single[Int]], _2], Single[Int]], _3]], _4] =
    g1.juxtapose( v12.juxtapose( g3 ) )
// pprint.pprintln( g1.show )
// pprint.pprintln( v12.show )
// pprint.pprintln( v123.show )
// pprint.pprintln( v312.show )
// pprint.pprintln( v1231.show )
// pprint.pprintln( v1123.show )
// println( "-" * 30 )
  val h12: Single[Connected[Single[Int], Single[Int]]] = g1.concat( g2 )
  val h123: Single[Connected[Single[Connected[Single[Int], Single[Int]]], Single[Int]]] = h12.concat( g3 )
  val h312: Single[Connected[Single[Int], Single[Connected[Single[Int], Single[Int]]]]] = g3.concat( h12 )
  //val h1231: IGraph[_1, Connector.Horizontal[Int, Connector.Horizontal[Connector.Horizontal[Int, Int], Int]], _1] = h12.concat( g3 ).concat( g1 )
  //val h1123: IGraph[_1, Connector.Horizontal[Int, Connector.Horizontal[Int, Connector.Horizontal[Int, Int]]], _1] = g1.concat( h12.concat( g3 ) )
  //pprint.pprintln( h12.show )
  //pprint.pprintln( h123.show )
  //pprint.pprintln( h312.show )
  //pprint.pprintln( h1231 .show)
  //pprint.pprintln( h1123 .show)
  //println( "-" * 30 )
  val head = IGraph.node[_1, String, _3]( "a" )
  val tail = IGraph.node[_1, Double, _2]( 2.0 )
  //pprint.pprintln( head.concat( v123 ).show )
  val g = head.juxtapose( head.concat( v123 ) )
  //pprint.pprintln( g.show )
  case class R[A](unR: A )
  implicit def RGraph = new Graph[R] {
    def node[A](a: A ): R[A] = R( a )
    def concat[A, B, C](start: R[A], end: R[B] )(implicit C: Concat.Aux[A, B, C] ): R[C] = R( start.unR.concat( end.unR ) )
    def juxtapose[A, B, J](g1: R[A], g2: R[B] )(implicit J: Juxtapose.Aux[A, B, J] ): R[J] = R( g1.unR.juxtapose( g2.unR ) )
  }
  implicit def showR[A: Show]: Show[R[A]] = Show.show( r => r.unR.show )
  import Graph.ops._
  case class S[A](unS: A, string: Show[A] => String )
  implicit def showS[A](implicit A: Show[A] ): Show[S[A]] = Show.show( s => s.string( A ) )
  implicit def SGraph = new Graph[S] {
    def node[A](a: A ): S[A] = S( a, _.show( a ) )
    def concat[A, B, C](start: S[A], end: S[B] )(implicit C: Concat.Aux[A, B, C] ): S[C] = {
      val node = start.unS.concat( end.unS )
      S( node, s => s.show( start.unS.concat( end.unS ) ) )
    }
    def juxtapose[A, B, J](g1: S[A], g2: S[B] )(implicit J: Juxtapose.Aux[A, B, J] ): S[J] =
      S( g1.unS.juxtapose( g2.unS ), _.show( g1.unS.juxtapose( g2.unS ) ) )
  }
  def graph[N[_]](implicit G: Graph[N] ) = {
    val gn1 = G.node( g1 )
    val gn2 = G.node( g2 )
    val gn3 = G.node( g3 )
    val gn4 = G.node( g4 )
    val t = G.node( tail )
    t.concat( gn1.juxtapose( gn2 ) )
  }
  //println( graph[R].show )
  //println( graph[S].show )
  case class Str[G](s: String )
  implicit val FStr = new Functor[Str] {
    def map[A, B](fa: Str[A] )(f: A => B ): Str[B] = Str( fa.s )
  }
  implicit def IGraphF[I, O] = new Functor[IGraph[I, ?, O]] {
    def map[A, B](fa: IGraph[I, A, O] )(f: A => B ): IGraph[I, B, O] = IGraph( fa.in, f( fa.graph ), fa.out )
  }
  def graphToString[I: Show, G: Show, O: Show]: Coalgebra[Str, IGraph[I, G, O]] = Coalgebra {
    case IGraph( in, g, out ) => Str( s"(${in.show},${g.show},${out.show})" )
  }
  def printGraph[I: Show, G: Show, O: Show](g: IGraph[I, G, O] ) = {
    scheme.ana( graphToString[I, G, O] ).apply( g )
  }
  println( printGraph( graph[R].unR ) )

}
