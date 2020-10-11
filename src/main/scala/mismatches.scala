package algorithm

import matrices._
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
}
object Mismatches {
  var count = 0
  def compare[Label: Eq: ClassTag](
      A: AdjacentGraph[Label],
      B: AdjacentGraph[Label],
      placeholder: Label
    ): GraphVisitation[AdjacentGraph, Label, Diff[Label]] = {
    val parentsA = A.parents
    val parentsB = B.parents
    val alignments =
      NeedlemanWunsch(
        placeholder,
        A.topological( A.root ).toArray,
        B.topological( B.root ).toArray
      )
    alignments.foldLeft( GraphVisitation( AdjacentGraph.single( Diff.same( A.root ) ), Set.empty[Label] ) ) {
      case ( visitation, Alignment( left, right ) ) =>
        count += 1
        left.zip( right ).foldLeft( visitation ) {
          case ( GraphVisitation( result, visited ), ( `placeholder`, r ) ) =>
            val parent = parentsB( r )
            val parentDiff = result.find( _.value === parent )
            count += 1 + parentDiff.visited.size
            GraphVisitation( result.addEdge( parentDiff.result.get, Diff.added( r ) ), visited + r )
          case ( GraphVisitation( result, visited ), ( l, `placeholder` ) ) =>
            val parent = parentsA( l )
            val parentDiff = result.find( _.value === parent )
            count += 1 + parentDiff.visited.size
            GraphVisitation( result.addEdge( parentDiff.result.get, Diff.removed( l ) ), visited + l )
          case ( GraphVisitation( result, visited ), ( l, r ) ) if l === r =>
            count += 1
            pprint.pprintln(parentsA)
            pprint.pprintln(l)
            val parent = parentsA( l )
            val parentDiff = result.find( _.value === parent )
            count += 1 + parentDiff.visited.size
            GraphVisitation( result.addEdge( parentDiff.result.get, Diff.same( l ) ), visited + l )
          case ( GraphVisitation( result, visited ), ( l, r ) ) if l =!= r =>
            val parentL = parentsA( l )
            val parentR = parentsB( r )
            val parentDiffL = result.find( _.value === parentL )
            val parentDiffR = result.find( _.value === parentR )
            count += 1 + parentDiffL.visited.size + parentDiffR.visited.size
            GraphVisitation(
              result
                .addEdge( parentDiffL.result.get, Diff.removed( l ) )
                .addEdge( parentDiffR.result.get, Diff.added( r ) ),
              visited + l + r
            )
        }
    }
  }
}

object MismatchesTest extends App {
  val A =
    matrices.AdjacentGraph
      .single( 'Foo )
      .addEdge( 'Foo, 'b )
      .addEdge( 'Foo, 'a )
      .addEdge( 'a, 'c )
      .addEdge( 'a, 'd )
      .addEdge( 'b, 'h )
      .addEdge( 'b, 'g )
      .addEdge( 'g, 'k )
      .addEdge( 'c, 'e )

  val B =
    matrices.AdjacentGraph
      .single( 'Foo )
      .addEdge( 'Foo, 'l )
      .addEdge( 'Foo, 'a )
      .addEdge( 'a, 'c )
      .addEdge( 'a, 'd )
      .addEdge( 'l, 'x )
      .addEdge( 'c, 'j )
      .addEdge( 'x, 'i )

  pprint.pprintln( A.parents( A.root ) )
  pprint.pprintln( A.topological( A.root ) )
  pprint.pprintln( B.topological( B.root ) )

  val newGraph = Mismatches.compare( A, B, '- )
  pprint.pprintln( newGraph )
  pprint.pprintln( Mismatches.count )
  pprint.pprintln( A )
  pprint.pprintln( B )
}
