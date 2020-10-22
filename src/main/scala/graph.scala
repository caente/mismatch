package graph

import cats._
import cats.implicits._
import scala.collection.immutable.SortedSet

object AdjacentGraph {
  def single[Label: Eq: Ordering](node: Label ): AdjacentGraph[Label] =
    new AdjacentGraph( node, Map( node -> SortedSet.empty[Label] ) ) {}

  implicit def bfs[Label] = new BFS[AdjacentGraph, Label] {
    def bfs[F[_], B](
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
  implicit def dfs[Label] = new DFS[AdjacentGraph, Label] {
    def dfs[F[_], B](
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
  implicit def createGraph[Label: Eq: Ordering] = new CreateGraph[AdjacentGraph, Label] {
    def create(l: Label ): AdjacentGraph[Label] = AdjacentGraph.single( l )
  }
  implicit def addEdge[Label: Eq: Ordering](implicit R: Root[AdjacentGraph, Label] ) =
    new AddEdge[AdjacentGraph, Label] {
      def addEdge(g: AdjacentGraph[Label] )(start: Label, end: Label ): AdjacentGraph[Label] = {
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
  implicit def rootAdjacent[Label] = new Root[AdjacentGraph, Label] {
    def root(g: AdjacentGraph[Label] ): Label = g.root
  }
}

case class GraphVisitation[F[_], Label, B](result: F[B], visited: Set[Label] )

trait DFS[G[_], Label] {
  def dfs[F[_], B](
      g: G[Label]
    )(start: Label,
      acc: F[B],
      initiallyVisited: Set[Label]
    )(combine: (Label, Label, F[B] ) => F[B],
      stop: F[B] => Boolean = (_: F[B]) => false
    ): GraphVisitation[F, Label, B]
}

trait BFS[G[_], Label] {
  def bfs[F[_], B](
      g: G[Label]
    )(start: Label,
      acc: F[B],
      initiallyVisited: Set[Label]
    )(combine: (Label, Label, F[B] ) => F[B],
      stop: F[B] => Boolean = (_: F[B]) => false
    ): GraphVisitation[F, Label, B]
}

trait CreateGraph[G[_], Label] {
  def create(l: Label ): G[Label]
}

trait AddEdge[G[_], Label] {
  def addEdge(g: G[Label] )(start: Label, end: Label ): G[Label]
}

trait Root[G[_], Label] {
  def root(g: G[Label] ): Label
}

object GraphOps {
  def topological[Label, G[_]](g: G[Label] )(start: Label )(implicit G: DFS[G, Label] ): List[Label] =
    G.dfs( g )( start, List( start ), Set() )( ( _, child, acc ) => child :: acc ).result.reverse

  def uniqueNames[Label, G[_]](g: G[Label] )(start: Label )(implicit G: DFS[G, Label] ): Map[Label, List[Label]] =
    G.dfs( g )( start, Map( start -> List( start ) ), Set() ) { ( parent, child, acc ) =>
        acc.updated( child, child :: acc.getOrElse( parent, List( parent ) ) )
      }
      .result

  def subGraph[Label, G[_]](
      g: G[Label]
    )(fromNode: Label
    )(implicit G: DFS[G, Label],
      C: CreateGraph[G, Label],
      Add: AddEdge[G, Label]
    ): G[Label] =
    G.dfs( g )( fromNode, C.create( fromNode ), Set() ) { ( parent, child, newGraph ) =>
        Add.addEdge( newGraph )( parent, child )
      }
      .result

  def parents[G[_], Label](g: G[Label] )(implicit G: DFS[G, Label], R: Root[G, Label] ): Map[Label, Label] = {
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
    )(implicit G: DFS[G, Label],
      R: Root[G, Label],
      A: AddEdge[G, Label]
    ): G[Label] = {
    val root = R.root( current )
    G.dfs( graph )( root, A.addEdge( current )( node, root ), Set() )(
        ( parent, child, newGraph ) => A.addEdge( newGraph )( parent, child )
      )
      .result
  }

  def map[G[_], Label, B](
      g: G[Label]
    )(f: Label => B
    )(implicit G: DFS[G, Label],
      R: Root[G, Label],
      C: CreateGraph[G, B],
      Add: AddEdge[G, B]
    ): G[B] = {
    val root = R.root( g )
    G.dfs( g )( root, C.create( f( root ) ), Set() )(
        ( parent, child, graph ) => Add.addEdge( graph )( f( parent ), f( child ) )
      )
      .result
  }

  def find[G[_], Label](
      g: G[Label]
    )(f: Label => Boolean
    )(implicit G: BFS[G, Label],
      R: Root[G, Label],
      A: AddEdge[G, Label]
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
    )(implicit G: BFS[G, Label],
      R: Root[G, Label],
      A: AddEdge[G, Label]
    ): Label =
    find( g )( f ).result.getOrElse( throw new RuntimeException( "Label not found" ) )

}

sealed abstract case class AdjacentGraph[Label: Eq: Ordering](root: Label, val data: Map[Label, SortedSet[Label]] ) {
  def adjacents(a: Label ): SortedSet[Label] = data.getOrElse( a, SortedSet.empty[Label] )
  def addEdge(start: Label, end: Label ): AdjacentGraph[Label] = AdjacentGraph.addEdge.addEdge( this )( start, end )
  def uniqueNames(start: Label ): Map[Label, List[Label]] = GraphOps.uniqueNames( this )( start )
}
