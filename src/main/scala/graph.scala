package graph

import cats._
import cats.implicits._
import scala.collection.immutable.SortedSet
import cats.data.NonEmptyList
import algorithm.Diff
import simulacrum._
import tograph.Labelled

object AdjacentGraph {
  def single[Label: Eq: Ordering](node: Label ): AdjacentGraph[Label] =
    new AdjacentGraph( node, Map( NonEmptyList.one( node ) -> SortedSet.empty[Label] ) ) {}

  implicit def bfs[Label] = new BFS[AdjacentGraph] {
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
          val newData = g.data
            .updated( path, g.data( path ) + end )
            .updated( end :: path, g.data.getOrElse( end :: path, SortedSet.empty[Label] ) )
          new AdjacentGraph( g.root, newData ) {}
        } else
          throw new IllegalArgumentException( s"At least one node must exist; path:$path end:$end" )
      }
    }
}

@typeclass
trait Representation[Label] {
  def representation(label: Label ): String
  def length(label: Label ): Int
}
object Representation {
  implicit def diff[Label: Show] = new Representation[Diff[Label]] {
    def representation(label: Diff[Label] ): String = label.show

    def length(label: Diff[Label] ): Int = label.value.show.length
  }
  implicit def symbol = new Representation[Symbol] {
    def representation(label: Symbol ): String = label.toString

    def length(label: Symbol ): Int = representation( label ).length
  }
  implicit def labelled[Label: Show] = new Representation[Labelled[Label]] {
    def representation(label: Labelled[Label] ): String = label.show

    def length(label: Labelled[Label] ): Int = label.show.length

  }
}
import Representation.ops._
case class Printed(col: Int, string: List[String] )
sealed abstract case class AdjacentGraph[Label: Eq: Ordering](
    root: Label,
    val data: Map[NonEmptyList[Label], SortedSet[Label]]) {
  def adjacents(a: Label ): SortedSet[Label] =
    data.keySet.find( _.head === a ).flatMap( data.get ).getOrElse( SortedSet.empty[Label] )
  def adjacentsPath(a: NonEmptyList[Label] ): SortedSet[Label] =
    data.getOrElse( a, SortedSet.empty[Label] )
  def connect(path: NonEmptyList[Label], end: Label ) = AdjacentGraph.connect.connect( this )( path, end )
  def addEdge(start: Label, end: Label ): AdjacentGraph[Label] = {
    val path = data.keySet.find( _.head === start ).getOrElse( NonEmptyList.one( start ) )
    if (data.keySet.contains( path )) {
      if (start === end) {
        this
      } else {
        val newData = data.updated( path, data( path ) + end ).updated( end :: path, adjacents( end ) )
        new AdjacentGraph( root, newData ) {}
      }
    } else
      throw new IllegalArgumentException( s"At least one node must exist; start:$start end:$end" )
  }
  def print(implicit S: Representation[Label] ): String = {
    val arrow = " -> "
    def traverse(
        from: NonEmptyList[Label],
        visitation: GraphVisitation[Id, NonEmptyList[Label], Printed]
      ): GraphVisitation[Id, NonEmptyList[Label], Printed] = {
      val extraCols = " " * visitation.result.col
      val newLines = s"\n$extraCols|"
      adjacentsPath( from ).foldLeft( visitation ) {
        case ( GraphVisitation( acc, visited ), adj ) if visited.contains( adj :: from ) =>
          GraphVisitation( acc, visited )
        case ( GraphVisitation( Printed( col, string ), visited ), adj ) =>
          val adjString = arrow + adj.representation
          val branch =
            traverse(
              adj :: from,
              GraphVisitation[Id, NonEmptyList[Label], Printed](
                Printed( col + adj.length + arrow.length, string :+ newLines :+ adjString ),
                visited + (adj :: from)
              )
            )
          branch.copy[Id, NonEmptyList[Label], Printed](
            result = branch.result.copy( col = col )
          )
      }
    }
    traverse(
      NonEmptyList.one( root ),
      GraphVisitation[Id, NonEmptyList[Label], Printed]( Printed( 0, List( root.representation ) ), Set() )
    ).result.string.reduce( _ + _ )
  }
}

case class GraphVisitation[F[_], Label, B](result: F[B], visited: Set[Label] )

trait DFS[G[_]] {
  def dfs[F[_], Label, B](
      g: G[Label]
    )(start: NonEmptyList[Label],
      acc: F[B],
      initiallyVisited: Set[NonEmptyList[Label]]
    )(combine: (NonEmptyList[Label], Label, F[B] ) => F[B],
      stop: F[B] => Boolean = (_: F[B]) => false
    ): GraphVisitation[F, NonEmptyList[Label], B]
}

trait BFS[G[_]] {
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

trait Connect[G[_]] {
  def connect[Label: Eq: Ordering](g: G[Label] )(path: NonEmptyList[Label], node: Label ): G[Label]
}

trait Root[G[_]] {
  def root[Label](g: G[Label] ): Label
}

object GraphOps {
  def nodes[Label, G[_]](g: G[Label] )(implicit G: BFS[G], R: Root[G] ): List[NonEmptyList[Label]] =
    G.bfs( g )( NonEmptyList.one( R.root( g ) ), List( NonEmptyList.one( R.root( g ) ) ), Set() )(
        ( parent, child, acc ) => (child :: parent) :: acc
      )
      .result
      .reverse

  def map[G[_], Label, B](
      g: G[Label]
    )(f: Label => B
    )(implicit G: DFS[G],
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

  def subGraph[Label: Eq: Ordering, G[_]](
      g: G[Label]
    )(fromNode: NonEmptyList[Label]
    )(implicit G: DFS[G],
      C: CreateGraph[G],
      Add: Connect[G]
    ): G[Label] =
    G.dfs( g )( fromNode, C.create( fromNode.head ), Set() ) { ( parent, child, newGraph ) =>
        Add.connect( newGraph )( parent, child )
      }
      .result

  def addSubGraph[G[_], Label: Eq: Ordering](
      current: G[Label]
    )(node: NonEmptyList[Label],
      graph: G[Label]
    )(implicit G: DFS[G],
      R: Root[G],
      A: Connect[G]
    ): G[Label] = {
    val root = R.root( current )
    G.dfs( graph )( NonEmptyList.one( root ), A.connect( current )( node, root ), Set() )(
        ( parent, child, newGraph ) => A.connect( newGraph )( parent, child )
      )
      .result
  }

  def dfs[F[_], Label, B, G[_]](
      g: G[Label]
    )(start: NonEmptyList[Label],
      acc: F[B],
      initiallyVisited: Set[NonEmptyList[Label]]
    )(combine: (NonEmptyList[Label], Label, F[B] ) => F[B],
      stop: F[B] => Boolean = (_: F[B]) => false
    )(implicit DFS: DFS[G]
    ): GraphVisitation[F, NonEmptyList[Label], B] = DFS.dfs( g )( start, acc, initiallyVisited )( combine, stop )

  def bfs[F[_], Label, B, G[_]](
      g: G[Label]
    )(start: NonEmptyList[Label],
      acc: F[B],
      initiallyVisited: Set[NonEmptyList[Label]]
    )(combine: (NonEmptyList[Label], Label, F[B] ) => F[B],
      stop: F[B] => Boolean = (_: F[B]) => false
    )(implicit BFS: BFS[G]
    ): GraphVisitation[F, NonEmptyList[Label], B] = BFS.bfs( g )( start, acc, initiallyVisited )( combine, stop )

  def filter[G[_], Label](
      g: G[Label]
    )(f: Label => Boolean
    )(implicit G: DFS[G],
      R: Root[G],
      C: CreateGraph[G],
      A: Connect[G],
      E: Eq[Label],
      O: Ordering[Label]
    ): G[Label] = {
    val root = R.root( g )
    if (!f( root )) {
      throw new IllegalArgumentException( "don't know what to do here yet" )
    } else {
      G.dfs( g )( NonEmptyList.one( root ), C.create( root ), Set() )(
          ( parent, child, graph ) =>
            if (f( child )) {
              A.connect( graph )( parent, child )
            } else {
              graph
            }
        )
        .result
    }
  }

  def findPath[G[_], Label](
      g: G[Label]
    )(f: NonEmptyList[Label] => Boolean
    )(implicit G: BFS[G],
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
    )(implicit G: BFS[G],
      R: Root[G]
    ): NonEmptyList[Label] =
    findPath( g )( f ).result.getOrElse( throw new RuntimeException( "Label not found" ) )
}
