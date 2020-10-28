package algorithm

import matrices._
import graph._
import cats._
import cats.implicits._
import scala.reflect.ClassTag
import algorithm.NeedlemanWunsch.Alignment
import cats.data.NonEmptyList

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
      E: Connect[G],
      O: Ordering[Label]
    ): G[Diff[Label]] = {

    val nodesA: List[NonEmptyList[Label]] = GraphOps.nodes( A )
    val nodesB: List[NonEmptyList[Label]] = GraphOps.nodes( B )

    val pathToLabelA: Map[String, NonEmptyList[Label]] = nodesA.map( n => n.show -> n ).toMap
    val pathToLabelB: Map[String, NonEmptyList[Label]] = nodesB.map( n => n.show -> n ).toMap

    val placeholder = "-"
    val alignments: Set[Alignment[String]] =
      NeedlemanWunsch(
        placeholder,
        nodesA.map( _.show ).toArray,
        nodesB.map( _.show ).toArray
      )
    alignments
      .foldLeft( GraphVisitation( C.create( Diff.same( R.root( A ) ) ), Set.empty[String] ) ) {
        case ( visitation, Alignment( left, right ) ) =>
          left.zip( right ).foldLeft( visitation ) {
            case ( GraphVisitation( result, visited ), ( `placeholder`, r ) ) =>
              val rLabel = pathToLabelB( r )
              val parent = rLabel.tail.toNel.getOrElse( NonEmptyList.one( R.root( B ) ) )
              val parentDiff = GraphOps.findPathUnsafe( result )( _.map( _.value ) === parent )
              GraphVisitation( E.connect( result )( parentDiff, Diff.added( rLabel.head ) ), visited + r )
            case ( GraphVisitation( result, visited ), ( l, `placeholder` ) ) =>
              val lLabel = pathToLabelA( l )
              val parent = lLabel.tail.toNel.getOrElse( NonEmptyList.one( R.root( A ) ) )
              val parentDiff = GraphOps.findPathUnsafe( result )( _.map( _.value ) === parent )
              GraphVisitation( E.connect( result )( parentDiff, Diff.removed( lLabel.head ) ), visited + l )
            case ( GraphVisitation( result, visited ), ( l, r ) ) if l === r =>
              val lLabel = pathToLabelA( l )
              lLabel.tail.toNel match {
                case Some( parent ) =>
                  val parentDiff = GraphOps.findPathUnsafe( result )( _.map( _.value ) === parent )
                  GraphVisitation( E.connect( result )( parentDiff, Diff.same( lLabel.head ) ), visited + l )
                case None => visitation
              }
            case ( GraphVisitation( result, visited ), ( l, r ) ) if l =!= r =>
              val lLabel = pathToLabelA( l )
              val parentL = lLabel.tail.toNel.getOrElse( NonEmptyList.one( R.root( A ) ) )
              val rLabel = pathToLabelB( r )
              val parentR = rLabel.tail.toNel.getOrElse( NonEmptyList.one( R.root( B ) ) )
              val parentDiffL = GraphOps.findPathUnsafe( result )( _.map( _.value ) === parentL )
              val parentDiffR = GraphOps.findPathUnsafe( result )( _.map( _.value ) === parentR )
              GraphVisitation(
                E.connect( E.connect( result )( parentDiffL, Diff.removed( lLabel.head ) ) )(
                  parentDiffR,
                  Diff.added( rLabel.head )
                ),
                visited + l + r
              )
          }
      }
      .result
  }
}
