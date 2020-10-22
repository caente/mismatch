package algorithm

import matrices._
import graph._
import cats._
import cats.implicits._
import scala.reflect.ClassTag
import algorithm.NeedlemanWunsch.Alignment

sealed trait Diff[A] { def value: A }
case class Added[A](value: A ) extends Diff[A]
case class Removed[A](value: A ) extends Diff[A]
case class Same[A](value: A ) extends Diff[A]

object Diff {
  def same[A](a: A ): Diff[A] = Same( a )
  def added[A](a: A ): Diff[A] = Added( a )
  def removed[A](a: A ): Diff[A] = Removed( a )
  implicit def eq[A: Eq]: Eq[Diff[A]] = Eq.fromUniversalEquals[Diff[A]]
  implicit def ord[A](implicit A: Ordering[A] ) = new Ordering[Diff[A]] {
    def compare(x: Diff[A], y: Diff[A] ): Int = A.compare( x.value, y.value )
  }
}
object Mismatches {
  def compare[Label: Eq: Ordering: Show, G[_]](
      A: G[Label],
      B: G[Label]
    )(implicit Dfs: DFS[G, Label],
      bfs: BFS[G, Diff[Label]],
      C: CreateGraph[G, Diff[Label]],
      R: Root[G, Label],
      Rdiff: Root[G, Diff[Label]],
      AddDiff: AddEdge[G, Diff[Label]]
    ): G[Diff[Label]] = {
    val labelToPathA: Map[Label, String] = GraphOps.uniqueNames( A )( R.root( A ) ).map {
      case ( label, path ) => label -> path.map( _.show ).reduce( _ + _ )
    }
    val pathToLabelA: Map[String, Label] = labelToPathA.map {
      case ( label, path ) => path -> label
    }
    val labelToPathB: Map[Label, String] = GraphOps.uniqueNames( B )( R.root( B ) ).map {
      case ( label, path ) => label -> path.map( _.show ).reduce( _ + _ )
    }
    val pathToLabelB: Map[String, Label] = labelToPathB.map {
      case ( label, path ) => path -> label
    }
    val placeholder = "-"
    val alignments =
      NeedlemanWunsch(
        placeholder,
        GraphOps.topological( A )( R.root( A ) ).map( labelToPathA ).toArray,
        GraphOps.topological( B )( R.root( B ) ).map( labelToPathB ).toArray
      )
    val parentsA = GraphOps.parents( A )
    val parentsB = GraphOps.parents( B )
    alignments
      .foldLeft( GraphVisitation( C.create( Diff.same( R.root( A ) ) ), Set.empty[Label] ) ) {
        case ( visitation, Alignment( left, right ) ) =>
          left.zip( right ).foldLeft( visitation ) {
            case ( GraphVisitation( result, visited ), ( `placeholder`, r ) ) =>
              val b = pathToLabelB( r )
              val parent = parentsB( b )
              val parentDiff = GraphOps.findUnsafe( result )( _.value === parent )
              GraphVisitation( AddDiff.addEdge( result )( parentDiff, Diff.added( b ) ), visited + b )
            case ( GraphVisitation( result, visited ), ( l, `placeholder` ) ) =>
              val a = pathToLabelA( l )
              val parent = parentsA( a )
              val parentDiff = GraphOps.findUnsafe( result )( _.value === parent )
              GraphVisitation( AddDiff.addEdge( result )( parentDiff, Diff.removed( a ) ), visited + a )
            case ( GraphVisitation( result, visited ), ( l, r ) ) if l === r =>
              val a = pathToLabelA( l )
              val parent = parentsA( a )
              val parentDiff = GraphOps.findUnsafe( result )( _.value === parent )
              GraphVisitation( AddDiff.addEdge( result )( parentDiff, Diff.same( a ) ), visited + a )
            case ( GraphVisitation( result, visited ), ( l, r ) ) if l =!= r =>
              val a = pathToLabelA( l )
              val b = pathToLabelB( r )
              val parentL = parentsA( a )
              val parentR = parentsB( b )
              val parentDiffL = GraphOps.findUnsafe( result )( _.value === parentL )
              val parentDiffR = GraphOps.findUnsafe( result )( _.value === parentR )
              GraphVisitation(
                AddDiff.addEdge( AddDiff.addEdge( result )( parentDiffL, Diff.removed( a ) ) )(
                  parentDiffR,
                  Diff.added( b )
                ),
                visited + a + b
              )
          }
      }
      .result
  }
}

object MismatchesTest extends App {
  val A =
    AdjacentGraph
      .single( 'Foo )
      .addEdge( 'Foo, 'b )
      .addEdge( 'Foo, 'a )
      .addEdge( 'a, 'c )
      .addEdge( 'a, 'd )
      .addEdge( 'b, 'h )
      .addEdge( 'b, 'g )
      .addEdge( 'g, 'k )
      .addEdge( 'c, 'e )
  val Ap =
    AdjacentGraph
      .single( 'Foo )
      .addEdge( 'Foo, 'a )
      .addEdge( 'Foo, 'b )
      .addEdge( 'a, 'c )
      .addEdge( 'a, 'd )
      .addEdge( 'b, 'h )
      .addEdge( 'b, 'g )
      .addEdge( 'g, 'k )
      .addEdge( 'c, 'e )
  val B =
    AdjacentGraph
      .single( 'Foo )
      .addEdge( 'Foo, 'l )
      .addEdge( 'Foo, 'a )
      .addEdge( 'a, 'c )
      .addEdge( 'a, 'd )
      .addEdge( 'l, 'x )
      .addEdge( 'c, 'j )
      .addEdge( 'x, 'i )
  val miniB =
    AdjacentGraph
      .single( 'Foo )
  //pprint.pprintln( A.parents( A.root ) )
  //pprint.pprintln( A.topological( A.root ) )
  //pprint.pprintln( B.topological( B.root ) )

  val newGraph = Mismatches.compare( A, B )
  val named = A.uniqueNames( 'Foo )
  pprint.pprintln( newGraph )
}
