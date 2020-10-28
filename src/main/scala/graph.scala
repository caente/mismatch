package graph

import cats._
import cats.implicits._
import scala.collection.immutable.SortedSet
import cats.data.NonEmptyList

object AdjacentGraph {
  def single[Label: Eq: Ordering](node: Label ): AdjacentGraph[Label] =
    new AdjacentGraph( node, Map( NonEmptyList.one( node ) -> SortedSet.empty[Label] ) ) {}

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
  implicit def bfsConnect[Label] = new BFSConnect[AdjacentGraph] {
    def bfs[F[_], Label, B](
        g: AdjacentGraph[Label]
      )(start: NonEmptyList[Label],
        acc: F[B],
        initiallyVisited: Set[NonEmptyList[Label]]
      )(combine: (NonEmptyList[Label], Label, F[B] ) => F[B],
        stop: F[B] => Boolean
      ): GraphVisitation[F, NonEmptyList[Label], B] = {
      val visitedAdjacents =
        g.adjacentsPath( start ).foldLeft( GraphVisitation( acc, initiallyVisited ) ) {
          case ( GraphVisitation( graph, visited ), adj ) if visited.contains( adj :: start ) || stop( graph ) =>
            GraphVisitation( graph, visited )
          case ( GraphVisitation( graph, visited ), adj ) =>
            GraphVisitation( combine( start, adj, graph ), visited + (adj :: start) )
        }

      g.adjacentsPath( start )
        .foldLeft( visitedAdjacents ) {
          case ( GraphVisitation( graph, visited ), adj ) =>
            bfs( g )( adj :: start, graph, visited )( combine )
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

  implicit def dfsConnect = new DFSConnect[AdjacentGraph] {
    def dfs[F[_], Label, B](
        g: AdjacentGraph[Label]
      )(start: NonEmptyList[Label],
        acc: F[B],
        initiallyVisited: Set[NonEmptyList[Label]]
      )(combine: (NonEmptyList[Label], Label, F[B] ) => F[B],
        stop: F[B] => Boolean
      ): GraphVisitation[F, NonEmptyList[Label], B] = {
      g.adjacentsPath( start ).foldLeft( GraphVisitation( acc, initiallyVisited ) ) {
        case ( GraphVisitation( acc, visited ), adj ) if visited.contains( adj :: start ) || stop( acc ) =>
          GraphVisitation( acc, visited )
        case ( GraphVisitation( acc, visited ), adj ) =>
          dfs( g )( adj :: start, combine( start, adj, acc ), visited + start )( combine, stop )
      }
    }
  }

  implicit def createGraph = new CreateGraph[AdjacentGraph] {
    def create[Label: Eq: Ordering](l: Label ): AdjacentGraph[Label] = AdjacentGraph.single( l )
  }
  implicit def addEdge(implicit R: Root[AdjacentGraph] ) =
    new NewEdge[AdjacentGraph] {
      def newEdge[Label: Eq: Ordering](g: AdjacentGraph[Label] )(start: Label, end: Label ): AdjacentGraph[Label] = {
        val root = R.root( g )
        val path = g.data.keySet.find( _.head === start ).getOrElse( NonEmptyList.one( start ) )
        if (g.data.keySet.contains( path )) {
          if (start === end) {
            g
          } else {
            val newData = g.data.updated( path, g.data( path ) + end ).updated( end :: path, g.adjacents( end ) )
            new AdjacentGraph( root, newData ) {}
          }
        } else
          throw new IllegalArgumentException( s"At least one node must exist; start:$start end:$end" )
      }
    }
  implicit def rootAdjacent = new Root[AdjacentGraph] {
    def root[Label](g: AdjacentGraph[Label] ): Label = g.root
  }
  implicit def connect =
    new Connect[AdjacentGraph] {
      def connect[Label: Eq: Ordering](
          g: AdjacentGraph[Label]
        )(path: NonEmptyList[Label],
          end: Label
        ): AdjacentGraph[Label] = {
        if (g.data.keySet.contains( path )) {
          if (path.head === end) {
            g
          } else {
            val newData = g.data
              .updated( path, g.data( path ) + end )
              .updated( end :: path, g.data.getOrElse( end :: path, SortedSet.empty[Label] ) )
            new AdjacentGraph( g.root, newData ) {}
          }
        } else
          throw new IllegalArgumentException( s"At least one node must exist; path:$path end:$end" )
      }
    }
}

sealed abstract case class AdjacentGraph[Label: Eq: Ordering](
    root: Label,
    val data: Map[NonEmptyList[Label], SortedSet[Label]]) {
  def adjacents(a: Label ): SortedSet[Label] =
    data.keySet.find( _.head === a ).flatMap( data.get ).getOrElse( SortedSet.empty[Label] )
  def adjacentsPath(a: NonEmptyList[Label] ): SortedSet[Label] =
    data.getOrElse( a, SortedSet.empty[Label] )
  def addEdge(start: Label, end: Label ): AdjacentGraph[Label] = AdjacentGraph.addEdge.newEdge( this )( start, end )
  def uniqueNames(start: Label ): Map[Label, List[Label]] = GraphOps.uniqueNames( this )( start )
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

trait DFSConnect[G[_]] {
  def dfs[F[_], Label, B](
      g: G[Label]
    )(start: NonEmptyList[Label],
      acc: F[B],
      initiallyVisited: Set[NonEmptyList[Label]]
    )(combine: (NonEmptyList[Label], Label, F[B] ) => F[B],
      stop: F[B] => Boolean = (_: F[B]) => false
    ): GraphVisitation[F, NonEmptyList[Label], B]
}

trait BFSConnect[G[_]] {
  def bfs[F[_], Label, B](
      g: G[Label]
    )(start: NonEmptyList[Label],
      acc: F[B],
      initiallyVisited: Set[NonEmptyList[Label]]
    )(combine: (NonEmptyList[Label], Label, F[B] ) => F[B],
      stop: F[B] => Boolean = (_: F[B]) => false
    ): GraphVisitation[F, NonEmptyList[Label], B]
}

trait CreateGraph[G[_]] {
  def create[Label: Eq: Ordering](l: Label ): G[Label]
}

trait NewEdge[G[_]] {
  def newEdge[Label: Eq: Ordering](g: G[Label] )(start: Label, end: Label ): G[Label]
}

trait Connect[G[_]] {
  def connect[Label: Eq: Ordering](g: G[Label] )(path: NonEmptyList[Label], node: Label ): G[Label]
}

trait Root[G[_]] {
  def root[Label](g: G[Label] ): Label
}

object GraphOps {
  //def topological[Label, G[_]](g: G[Label] )(start: Label )(implicit G: DFS[G] ): List[Label] =
  //  G.dfs( g )( start, List( start ), Set() )( ( _, child, acc ) => child :: acc ).result.reverse

  def allNodes[Label, G[_]](g: G[Label] )(implicit G: BFS[G], R: Root[G] ): List[Label] =
    G.bfs( g )( R.root( g ), List( R.root( g ) ), Set() )( ( _, child, acc ) => child :: acc ).result.reverse

  def nodes[Label, G[_]](g: G[Label] )(implicit G: BFSConnect[G], R: Root[G] ): List[NonEmptyList[Label]] =
    G.bfs( g )( NonEmptyList.one( R.root( g ) ), List( NonEmptyList.one( R.root( g ) ) ), Set() )(
        ( parent, child, acc ) => (child :: parent) :: acc
      )
      .result
      .reverse

  def uniqueNames[Label, G[_]](g: G[Label] )(start: Label )(implicit G: DFS[G] ): Map[Label, List[Label]] =
    G.dfs( g )( start, Map( start -> List( start ) ), Set() ) { ( parent, child, acc ) =>
        acc.updated( child, child :: acc.getOrElse( parent, List( parent ) ) )
      }
      .result

  //def subGraph[Label, G[_]](
  //    g: G[Label]
  //  )(fromNode: Label
  //  )(implicit G: DFS[G],
  //    C: CreateGraph[G],
  //    A: NewEdge[G],
  //    E: Eq[Label],
  //    O: Ordering[Label]
  //  ): G[Label] =
  //  G.dfs( g )( fromNode, C.create( fromNode ), Set() ) { ( parent, child, newGraph ) =>
  //      A.newEdge( newGraph )( parent, child )
  //    }
  //    .result

  def parents[G[_], Label](g: G[Label] )(implicit G: DFS[G], R: Root[G] ): Map[Label, Label] = {
    val root = R.root( g )
    G.dfs( g )( root, Map( root -> root ), Set() ) { ( parent, child, ps ) =>
        ps.updated( child, parent )
      }
      .result
  }

  //def addSubGraph[G[_], Label](
  //    current: G[Label]
  //  )(node: Label,
  //    graph: G[Label]
  //  )(implicit G: DFS[G],
  //    R: Root[G],
  //    A: NewEdge[G],
  //    E: Eq[Label],
  //    O: Ordering[Label]
  //  ): G[Label] = {
  //  val root = R.root( current )
  //  G.dfs( graph )( root, A.newEdge( current )( node, root ), Set() )(
  //      ( parent, child, newGraph ) => A.newEdge( newGraph )( parent, child )
  //    )
  //    .result
  //}

  def map[G[_], Label, B](
      g: G[Label]
    )(f: Label => B
    )(implicit G: DFSConnect[G],
      R: Root[G],
      C: CreateGraph[G],
      A: Connect[G],
      E: Eq[B],
      O: Ordering[B]
    ): G[B] = {
    val root = R.root( g )
    G.dfs( g )( NonEmptyList.one( root ), C.create( f( root ) ), Set() )(
        ( parent, child, graph ) => A.connect( graph )( parent.map( f ), f( child ) )
      )
      .result
  }

  def find[G[_], Label](
      g: G[Label]
    )(f: Label => Boolean
    )(implicit G: BFS[G],
      R: Root[G]
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

  def findUnsafe[G[_], Label](g: G[Label] )(f: Label => Boolean )(implicit G: BFS[G], R: Root[G] ): Label =
    find( g )( f ).result.getOrElse( throw new RuntimeException( "Label not found" ) )

  def findPath[G[_], Label](
      g: G[Label]
    )(f: NonEmptyList[Label] => Boolean
    )(implicit G: BFSConnect[G],
      R: Root[G]
    ): GraphVisitation[Option, NonEmptyList[Label], NonEmptyList[Label]] = {
    val root = R.root( g )
    val rootPath = NonEmptyList.one( root )
    if (f( rootPath ))
      GraphVisitation( Some( rootPath ), Set() )
    else
      G.bfs( g )( rootPath, Option.empty[NonEmptyList[Label]], Set() )(
        combine = ( parent, node, result ) =>
          result
            .orElse( Option.when( f( parent ) )( parent ) )
            .orElse( Option.when( f( node :: parent ) )( node :: parent ) ),
        stop = (result) => result.isDefined
      )
  }
  def findPathUnsafe[G[_], Label](
      g: G[Label]
    )(f: NonEmptyList[Label] => Boolean
    )(implicit G: BFSConnect[G],
      R: Root[G]
    ): NonEmptyList[Label] =
    findPath( g )( f ).result.getOrElse( throw new RuntimeException( "Label not found" ) )
}

sealed trait Tree[Label] {
  def node: Label
}
object Tree {
  case class Branch[Label](node: Label, adj: NonEmptyList[Tree[Label]] ) extends Tree[Label]
  case class Leaf[Label](node: Label ) extends Tree[Label]

  implicit def dfs[Label] = new DFS[Tree] {
    def dfs[F[_], Label, B](
        g: Tree[Label]
      )(start: Label,
        acc: F[B],
        initiallyVisited: Set[Label]
      )(combine: (Label, Label, F[B] ) => F[B],
        stop: F[B] => Boolean
      ): GraphVisitation[F, Label, B] = {
      g match {
        case Branch( node, adjacents ) =>
          adjacents
            .foldLeft( GraphVisitation( acc, initiallyVisited ) ) {
              case ( GraphVisitation( acc, visited ), adj ) if visited.contains( adj.node ) || stop( acc ) =>
                GraphVisitation( acc, visited )
              case ( GraphVisitation( acc, visited ), adj ) =>
                dfs( g )( adj.node, combine( start, adj.node, acc ), visited + start )( combine, stop )
            }
        case Leaf( node ) =>
          GraphVisitation( combine( start, node, acc ), initiallyVisited + start + node )
      }
    }
  }
  implicit def dfsConnect[Label] = new DFSConnect[Tree] {
    def dfs[F[_], Label, B](
        g: Tree[Label]
      )(start: NonEmptyList[Label],
        acc: F[B],
        initiallyVisited: Set[NonEmptyList[Label]]
      )(combine: (NonEmptyList[Label], Label, F[B] ) => F[B],
        stop: F[B] => Boolean
      ): GraphVisitation[F, NonEmptyList[Label], B] = ???
  }
  implicit def bfs[Label] = new BFS[Tree] {
    def bfs[F[_], Label, B](
        g: Tree[Label]
      )(start: Label,
        acc: F[B],
        initiallyVisited: Set[Label]
      )(combine: (Label, Label, F[B] ) => F[B],
        stop: F[B] => Boolean
      ): GraphVisitation[F, Label, B] = {
      g match {
        case Branch( node, adjacents ) =>
          val visitedAdjacents =
            adjacents.foldLeft( GraphVisitation( acc, initiallyVisited ) ) {
              case ( GraphVisitation( graph, visited ), adj ) if visited.contains( adj.node ) || stop( graph ) =>
                GraphVisitation( graph, visited )
              case ( GraphVisitation( graph, visited ), adj ) =>
                GraphVisitation( combine( start, adj.node, graph ), visited + adj.node )
            }
          adjacents
            .foldLeft( visitedAdjacents ) {
              case ( GraphVisitation( graph, visited ), adj ) =>
                bfs( g )( adj.node, graph, visited )( combine )
            }
        case Leaf( node ) =>
          GraphVisitation( combine( start, node, acc ), initiallyVisited + start + node )
      }
    }
  }
  implicit def createGraph = new CreateGraph[Tree] {
    def create[Label: Eq: Ordering](l: Label ): Tree[Label] = Leaf( l )
  }
  implicit def rootTree = new Root[Tree] {
    def root[Label](g: Tree[Label] ): Label = g.node
  }
  implicit def connect =
    new Connect[Tree] {
      def connect[Label: Eq: Ordering](g: Tree[Label] )(path: NonEmptyList[Label], end: Label ): Tree[Label] = ???
    }
}
