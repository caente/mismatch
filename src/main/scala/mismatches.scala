package algorithm

import matrices._
import graph._
import cats._
import cats.implicits._
import scala.reflect.ClassTag
import algorithm.NeedlemanWunsch.Alignment

sealed trait Diff[A] {
  def value: A
  def map[B](f: A => B ): Diff[B] =
    this match {
      case Added( value )   => Added( f( value ) )
      case Removed( value ) => Removed( f( value ) )
      case Same( value )    => Same( f( value ) )
    }
}
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
  implicit def show[A: Show]: Show[Diff[A]] = new Show[Diff[A]] {
    def show(t: Diff[A] ): String = t match {
      case Added( a )   => s"Added(${a.show})"
      case Removed( a ) => s"Removed(${a.show})"
      case Same( a )    => s"Same(${a.show})"
    }
  }
}
object Mismatches {
  def compare[Label: Eq: Show, G[_]](
      A: G[Label],
      B: G[Label]
    )(implicit
      DFS: DFS[G],
      BFS: BFS[G],
      R: Root[G],
      C: CreateGraph[G],
      E: NewEdge[G],
      O: Ordering[Label]
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

    val ANamed = GraphOps.map( A )( labelToPathA )
    val BNamed = GraphOps.map( B )( labelToPathB )
    val placeholder = "-"
    val alignments =
      NeedlemanWunsch(
        placeholder,
        GraphOps.allNodes( ANamed )( labelToPathA( R.root( A ) ) ).toArray,
        GraphOps.allNodes( BNamed )( labelToPathB( R.root( B ) ) ).toArray
      )

    val parentsA = GraphOps.parents( ANamed )
    val parentsB = GraphOps.parents( BNamed )
    val comparedGraphs =
      alignments
        .foldLeft( GraphVisitation( C.create( Diff.same( labelToPathA( R.root( A ) ) ) ), Set.empty[String] ) ) {
          case ( visitation, Alignment( left, right ) ) =>
            left.zip( right ).foldLeft( visitation ) {
              case ( GraphVisitation( result, visited ), ( `placeholder`, r ) ) =>
                val parent = parentsB( r )
                val parentDiff = GraphOps.findUnsafe( result )( _.value === parent )
                GraphVisitation( E.newEdge( result )( parentDiff, Diff.added( r ) ), visited + r )
              case ( GraphVisitation( result, visited ), ( l, `placeholder` ) ) =>
                val parent = parentsA( l )
                val parentDiff = GraphOps.findUnsafe( result )( _.value === parent )
                GraphVisitation( E.newEdge( result )( parentDiff, Diff.removed( l ) ), visited + l )
              case ( GraphVisitation( result, visited ), ( l, r ) ) if l === r =>
                val parent = parentsA( l )
                val parentDiff = GraphOps.findUnsafe( result )( _.value === parent )
                GraphVisitation( E.newEdge( result )( parentDiff, Diff.same( l ) ), visited + l )
              case ( GraphVisitation( result, visited ), ( l, r ) ) if l =!= r =>
                val parentL = parentsA( l )
                val parentR = parentsB( r )
                val parentDiffL = GraphOps.findUnsafe( result )( _.value === parentL )
                val parentDiffR = GraphOps.findUnsafe( result )( _.value === parentR )
                GraphVisitation(
                  E.newEdge( E.newEdge( result )( parentDiffL, Diff.removed( l ) ) )(
                    parentDiffR,
                    Diff.added( r )
                  ),
                  visited + l + r
                )
            }
        }
        .result
    GraphOps.map( comparedGraphs )( _.map( n => pathToLabelA.get( n ).getOrElse( pathToLabelB( n ) ) ) )
  }
}
