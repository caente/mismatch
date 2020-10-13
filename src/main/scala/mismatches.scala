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
  def compare[Label: Eq: ClassTag: Ordering](
      A: AdjacentGraph[Label],
      B: AdjacentGraph[Label],
      placeholder: Label
    ): GraphVisitation[AdjacentGraph, Label, Diff[Label]] = {
    val alignments =
      NeedlemanWunsch(
        placeholder,
        A.topological( A.root ).toArray,
        B.topological( B.root ).toArray
      )
    alignments.foldLeft( GraphVisitation( AdjacentGraph.single( Diff.same( A.root ) ), Set.empty[Label] ) ) {
      case ( visitation, Alignment( left, right ) ) =>
        left.zip( right ).foldLeft( visitation ) {
          case ( GraphVisitation( result, visited ), ( `placeholder`, r ) ) =>
            val parent = B.parent( r )
            val parentDiff = result.findUnsafe( _.value === parent )
            GraphVisitation( result.addEdge( parentDiff, Diff.added( r ) ), visited + r )
          case ( GraphVisitation( result, visited ), ( l, `placeholder` ) ) =>
            val parent = A.parent( l )
            val parentDiff = result.findUnsafe( _.value === parent )
            GraphVisitation( result.addEdge( parentDiff, Diff.removed( l ) ), visited + l )
          case ( GraphVisitation( result, visited ), ( l, r ) ) if l === r =>
            val parent = A.parent( l )
            val parentDiff = result.findUnsafe( _.value === parent )
            GraphVisitation( result.addEdge( parentDiff, Diff.same( l ) ), visited + l )
          case ( GraphVisitation( result, visited ), ( l, r ) ) if l =!= r =>
            val parentL = A.parent( l )
            val parentR = B.parent( r )
            val parentDiffL = result.findUnsafe( _.value === parentL )
            val parentDiffR = result.findUnsafe( _.value === parentR )
            GraphVisitation(
              result
                .addEdge( parentDiffL, Diff.removed( l ) )
                .addEdge( parentDiffR, Diff.added( r ) ),
              visited + l + r
            )
        }
    }
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

  //pprint.pprintln( A.parents( A.root ) )
  //pprint.pprintln( A.topological( A.root ) )
  //pprint.pprintln( B.topological( B.root ) )

  val newGraph = Mismatches.compare( A, B, '- )
  pprint.pprintln( newGraph )
}
