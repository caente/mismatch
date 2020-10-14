package graph

import cats._
import cats.implicits._
import scala.collection.immutable.SortedSet

object AdjacentGraph {
  def single[Label: Eq: Ordering](node: Label ): AdjacentGraph[Label] =
    new AdjacentGraph( node, Map( node -> SortedSet.empty[Label] ) ) {}
}
case class GraphVisitation[F[_], Label, B](result: F[B], visited: Set[Label] )

sealed abstract case class AdjacentGraph[Label: Eq: Ordering](root: Label, val data: Map[Label, SortedSet[Label]] ) {
  def topological(start: Label ): List[Label] =
    dfs( start, List( start ), Set() )( ( _, child, acc ) => child :: acc ).result.reverse

  def adjacents(a: Label ): SortedSet[Label] = data.getOrElse( a, SortedSet.empty[Label] )

  def subGraph(start: Label ): AdjacentGraph[Label] =
    dfs( start, AdjacentGraph.single( start ), Set() ) { ( parent, child, newGraph ) =>
      newGraph.addEdge( parent, child )
    }.result

  val parent: Map[Label, Label] = {
    dfs( root, Map( root -> root ), Set() ) { ( parent, child, ps ) =>
      ps.updated( child, parent )
    }.result
  }
  def addSubGraph(node: Label, graph: AdjacentGraph[Label] ): AdjacentGraph[Label] =
    graph
      .dfs( graph.root, addEdge( node, graph.root ), Set() )(
        ( parent, child, newGraph ) => newGraph.addEdge( parent, child )
      )
      .result

  def map[B: Eq: Ordering](f: Label => B ): AdjacentGraph[B] =
    dfs( root, AdjacentGraph.single( f( root ) ), Set() )(
      ( parent, child, graph ) => graph.addEdge( f( parent ), f( child ) )
    ).result

  def find(f: Label => Boolean ): GraphVisitation[Option, Label, Label] =
    if (f( root ))
      GraphVisitation( Some( root ), Set() )
    else
      bfs( root, Option.empty[Label], Set() )(
        combine = ( parent, node, result ) =>
          result.orElse( Option.when( f( parent ) )( parent ) ).orElse( Option.when( f( node ) )( node ) ),
        stop = (result) => result.isDefined
      )

  def findUnsafe(f: Label => Boolean ): Label =
    find( f ).result.getOrElse( throw new RuntimeException( "Label not found" ) )

  def addEdge(start: Label, end: Label ): AdjacentGraph[Label] = {
    if (data.keySet.contains( start )) {
      if (start === end) {
        this
      } else {
        val newData = data.updated( start, data( start ) + end ).updated( end, adjacents( end ) )
        new AdjacentGraph( root, newData ) {}
      }
    } else
      throw new IllegalArgumentException( s"At least one node must exist; start:$start end:$end" )
  }

  def dfs[F[_], B](
      start: Label,
      acc: F[B],
      initiallyVisited: Set[Label]
    )(combine: (Label, Label, F[B] ) => F[B],
      stop: F[B] => Boolean = (_: F[B]) => false
    ): GraphVisitation[F, Label, B] =
    adjacents( start )
      .foldLeft( GraphVisitation( acc, initiallyVisited ) ) {
        case ( GraphVisitation( acc, visited ), adj ) if visited.contains( adj ) || stop( acc ) =>
          GraphVisitation( acc, visited )
        case ( GraphVisitation( acc, visited ), adj ) =>
          dfs( adj, combine( start, adj, acc ), visited + start )( combine, stop )
      }

  def bfs[F[_], B](
      start: Label,
      acc: F[B],
      initiallyVisited: Set[Label]
    )(combine: (Label, Label, F[B] ) => F[B],
      stop: F[B] => Boolean = (_: F[B]) => false
    ): GraphVisitation[F, Label, B] = {
    val visitedAdjacents =
      adjacents( start ).foldLeft( GraphVisitation( acc, initiallyVisited ) ) {
        case ( GraphVisitation( graph, visited ), adj ) if visited.contains( adj ) || stop( graph ) =>
          GraphVisitation( graph, visited )
        case ( GraphVisitation( graph, visited ), adj ) =>
          GraphVisitation( combine( start, adj, graph ), visited + adj )
      }

    adjacents( start )
      .foldLeft( visitedAdjacents ) {
        case ( GraphVisitation( graph, visited ), adj ) =>
          bfs( adj, graph, visited )( combine )
      }
  }
}
