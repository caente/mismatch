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
  def compare[Label: Eq, G[_]](
      A: G[Label],
      B: G[Label],
      placeholder: Label
    )(implicit
      DFS: DFS[G],
      BFS: BFS[G],
      R: Root[G],
      C: CreateGraph[G],
      E: Connect[G],
      O: Ordering[Label]
    ): G[Diff[Label]] = {
    val alignments: List[Alignment[NonEmptyList[Label]]] =
      NeedlemanWunsch.findAlignments(
        NonEmptyList.one( placeholder ),
        GraphOps.nodes( A ).toArray,
        GraphOps.nodes( B ).toArray
      )
    alignments
      .foldLeft( GraphVisitation( C.create( Diff.same( R.root( A ) ) ), Set.empty[NonEmptyList[Label]] ) ) {
        case ( visitation, Alignment( left, right ) ) =>
          left.zip( right ).foldLeft( visitation ) {
            case ( GraphVisitation( result, visited ), ( NonEmptyList( `placeholder`, Nil ), r ) ) =>
              val parent = r.tail.toNel
              val parentDiff = GraphOps.findPathUnsafe( result )( n => parent.exists( _ === n.map( _.value ) ) )
              GraphVisitation( E.connect( result )( parentDiff, Diff.added( r.head ) ), visited + r )
            case ( GraphVisitation( result, visited ), ( l, NonEmptyList( `placeholder`, Nil ) ) ) =>
              val parent = l.tail.toNel
              val parentDiff = GraphOps.findPathUnsafe( result )( n => parent.exists( _ === n.map( _.value ) ) )
              GraphVisitation( E.connect( result )( parentDiff, Diff.removed( l.head ) ), visited + l )
            case ( GraphVisitation( result, visited ), ( l, r ) ) if l === r =>
              // the only case when lLabel.tail.toNel can be empty, is when both l and r are the root
              l.tail.toNel match {
                case Some( parent ) =>
                  val parentDiff = GraphOps.findPathUnsafe( result )( _.map( _.value ) === parent )
                  GraphVisitation( E.connect( result )( parentDiff, Diff.same( l.head ) ), visited + l )
                case None => visitation
              }
            case ( GraphVisitation( result, visited ), ( l, r ) ) if l =!= r =>
              val parentL = l.tail.toNel
              val parentR = r.tail.toNel
              val parentDiffL = GraphOps.findPathUnsafe( result )( n => parentL.exists( _ === n.map( _.value ) ) )
              val parentDiffR = GraphOps.findPathUnsafe( result )( n => parentR.exists( _ === n.map( _.value ) ) )
              GraphVisitation(
                E.connect( E.connect( result )( parentDiffL, Diff.removed( l.head ) ) )(
                  parentDiffR,
                  Diff.added( r.head )
                ),
                visited + l + r
              )
          }
      }
      .result
  }
}
