package graph

import cats._
import cats.implicits._
import scala.collection.immutable.SortedSet

object AdjacentGraph {
  def single[Label: Eq: Ordering](node: Label ): AdjacentGraph[Label] =
    new AdjacentGraph( node, Map( node -> SortedSet.empty[Label] ) ) {}

  implicit def bfs[Label] = new BFS[AdjacentGraph] {
    def bfs[F[_], Label, B](
        g: AdjacentGraph[Label]
      )(start: Label,
        acc: F[B],
        initiallyVisited: Set[Label]
      )(combine: (Label, Label, F[B] ) => F[B],
        stop: F[B] => Boolean
      ): GraphVisitation[F, Label, B] = {
      val visitedAdjacents =
        g.adjacents( start ).foldLeft( GraphVisitation( acc, initiallyVisited ) ) {
          case ( GraphVisitation( graph, visited ), adj ) if visited.contains( adj ) || stop( graph ) =>
            GraphVisitation( graph, visited )
          case ( GraphVisitation( graph, visited ), adj ) =>
            GraphVisitation( combine( start, adj, graph ), visited + adj )
        }

      g.adjacents( start )
        .foldLeft( visitedAdjacents ) {
          case ( GraphVisitation( graph, visited ), adj ) =>
            bfs( g )( adj, graph, visited )( combine )
        }
    }
  }
  implicit def dfs = new DFS[AdjacentGraph] {
    def dfs[F[_], Label, B](
        g: AdjacentGraph[Label]
      )(start: Label,
        acc: F[B],
        initiallyVisited: Set[Label]
      )(combine: (Label, Label, F[B] ) => F[B],
        stop: F[B] => Boolean
      ): GraphVisitation[F, Label, B] =
      g.adjacents( start )
        .foldLeft( GraphVisitation( acc, initiallyVisited ) ) {
          case ( GraphVisitation( acc, visited ), adj ) if visited.contains( adj ) || stop( acc ) =>
            GraphVisitation( acc, visited )
          case ( GraphVisitation( acc, visited ), adj ) =>
            dfs( g )( adj, combine( start, adj, acc ), visited + start )( combine, stop )
        }
  }
  implicit def createGraph = new CreateGraph[AdjacentGraph] {
    def create[Label: Eq: Ordering](l: Label ): AdjacentGraph[Label] = AdjacentGraph.single( l )
  }
  implicit def addEdge(implicit R: Root[AdjacentGraph] ) =
    new NewEdge[AdjacentGraph] {
      def newEdge[Label: Eq: Ordering](g: AdjacentGraph[Label] )(start: Label, end: Label ): AdjacentGraph[Label] = {
        val root = R.root( g )
        if (g.data.keySet.contains( start )) {
          if (start === end) {
            g
          } else {
            val newData = g.data.updated( start, g.data( start ) + end ).updated( end, g.adjacents( end ) )
            new AdjacentGraph( root, newData ) {}
          }
        } else
          throw new IllegalArgumentException( s"At least one node must exist; start:$start end:$end" )
      }
    }
  implicit def rootAdjacent = new Root[AdjacentGraph] {
    def root[Label](g: AdjacentGraph[Label] ): Label = g.root
  }
}

case class GraphVisitation[F[_], Label, B](result: F[B], visited: Set[Label] )

trait DFS[G[_]] {
  def dfs[F[_], Label, B](
      g: G[Label]
    )(start: Label,
      acc: F[B],
      initiallyVisited: Set[Label]
    )(combine: (Label, Label, F[B] ) => F[B],
      stop: F[B] => Boolean = (_: F[B]) => false
    ): GraphVisitation[F, Label, B]
}

trait BFS[G[_]] {
  def bfs[F[_], Label, B](
      g: G[Label]
    )(start: Label,
      acc: F[B],
      initiallyVisited: Set[Label]
    )(combine: (Label, Label, F[B] ) => F[B],
      stop: F[B] => Boolean = (_: F[B]) => false
    ): GraphVisitation[F, Label, B]
}

trait CreateGraph[G[_]] {
  def create[Label: Eq: Ordering](l: Label ): G[Label]
}

trait NewEdge[G[_]] {
  def newEdge[Label: Eq: Ordering](g: G[Label] )(start: Label, end: Label ): G[Label]
}

trait Root[G[_]] {
  def root[Label](g: G[Label] ): Label
}

object GraphOps {
  def topological[Label, G[_]](g: G[Label] )(start: Label )(implicit G: DFS[G] ): List[Label] =
    G.dfs( g )( start, List( start ), Set() )( ( _, child, acc ) => child :: acc ).result.reverse

  def allNodes[Label, G[_]](g: G[Label] )(start: Label )(implicit G: BFS[G] ): List[Label] =
    G.bfs( g )( start, List( start ), Set() )( ( _, child, acc ) => child :: acc ).result.reverse

  def uniqueNames[Label, G[_]](g: G[Label] )(start: Label )(implicit G: DFS[G] ): Map[Label, List[Label]] =
    G.dfs( g )( start, Map( start -> List( start ) ), Set() ) { ( parent, child, acc ) =>
        acc.updated( child, child :: acc.getOrElse( parent, List( parent ) ) )
      }
      .result

  def subGraph[Label, G[_]](
      g: G[Label]
    )(fromNode: Label
    )(implicit G: DFS[G],
      C: CreateGraph[G],
      A: NewEdge[G],
      E: Eq[Label],
      O: Ordering[Label]
    ): G[Label] =
    G.dfs( g )( fromNode, C.create( fromNode ), Set() ) { ( parent, child, newGraph ) =>
        A.newEdge( newGraph )( parent, child )
      }
      .result

  def parents[G[_], Label](g: G[Label] )(implicit G: DFS[G], R: Root[G] ): Map[Label, Label] = {
    val root = R.root( g )
    G.dfs( g )( root, Map( root -> root ), Set() ) { ( parent, child, ps ) =>
        ps.updated( child, parent )
      }
      .result
  }

  def addSubGraph[G[_], Label](
      current: G[Label]
    )(node: Label,
      graph: G[Label]
    )(implicit G: DFS[G],
      R: Root[G],
      A: NewEdge[G],
      E: Eq[Label],
      O: Ordering[Label]
    ): G[Label] = {
    val root = R.root( current )
    G.dfs( graph )( root, A.newEdge( current )( node, root ), Set() )(
        ( parent, child, newGraph ) => A.newEdge( newGraph )( parent, child )
      )
      .result
  }

  def map[G[_], Label, B](
      g: G[Label]
    )(f: Label => B
    )(implicit G: DFS[G],
      R: Root[G],
      C: CreateGraph[G],
      A: NewEdge[G],
      E: Eq[B],
      O: Ordering[B]
    ): G[B] = {
    val root = R.root( g )
    G.dfs( g )( root, C.create( f( root ) ), Set() )(
        ( parent, child, graph ) => A.newEdge( graph )( f( parent ), f( child ) )
      )
      .result
  }

  def find[G[_], Label](
      g: G[Label]
    )(f: Label => Boolean
    )(implicit G: BFS[G],
      R: Root[G],
      A: NewEdge[G],
      E: Eq[Label],
      O: Ordering[Label]
    ): GraphVisitation[Option, Label, Label] = {
    val root = R.root( g )
    if (f( root ))
      GraphVisitation( Some( root ), Set() )
    else
      G.bfs( g )( root, Option.empty[Label], Set() )(
        combine = ( parent, node, result ) =>
          result.orElse( Option.when( f( parent ) )( parent ) ).orElse( Option.when( f( node ) )( node ) ),
        stop = (result) => result.isDefined
      )
  }
  def findUnsafe[G[_], Label](
      g: G[Label]
    )(f: Label => Boolean
    )(implicit G: BFS[G],
      R: Root[G],
      A: NewEdge[G],
      E: Eq[Label],
      O: Ordering[Label]
    ): Label =
    find( g )( f ).result.getOrElse( throw new RuntimeException( "Label not found" ) )

}

sealed abstract case class AdjacentGraph[Label: Eq: Ordering](root: Label, val data: Map[Label, SortedSet[Label]] ) {
  def adjacents(a: Label ): SortedSet[Label] = data.getOrElse( a, SortedSet.empty[Label] )
  def addEdge(start: Label, end: Label ): AdjacentGraph[Label] = AdjacentGraph.addEdge.newEdge( this )( start, end )
  def uniqueNames(start: Label ): Map[Label, List[Label]] = GraphOps.uniqueNames( this )( start )
}
