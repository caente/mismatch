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

object Mismatches {
  trait GraphOps[Graph[_]] {
    def single[Label](label: Label ): Graph[Label]
    def addEdge[Label](graph: Graph[Label] )(start: Label, end: Label ): Graph[Label]
    def leaves[Label](graph: Graph[Label] ): Array[List[Label]]
  }
  case class Leaves[Label](leaves: Map[Int, List[Label]], indexes: Array[Int] )
  object Leaves {
    def apply[Label](leaves: Array[List[Label]] ): Leaves[Label] = {
      val indexedLeaves = leaves.map( l => l.hashCode -> l )
      val leavesMap = indexedLeaves.toMap
      val leafIndexes = indexedLeaves.map( _._1 )
      Leaves( leavesMap, leafIndexes )
    }
  }
  def same[Label: ClassTag: Eq, Graph[_]](
      graph: Graph[Label],
      previous: Label,
      nodes: List[Label]
    )(implicit Graph: GraphOps[Graph]
    ): Same[Graph[Label]] =
    nodes.foldLeft( ( Same( graph ), previous ) ) {
      case ( ( graph, previous ), label ) => Same( Graph.addEdge( graph )( previous, label ) ) -> label
    }
  def apply[Label: ClassTag: Eq, Graph[_]](
      placeholder: Label,
      root: Label,
      left: Graph[Label],
      right: Graph[Label]
    )(implicit Graph: GraphOps[Graph]
    ): Diff[Graph[Label]] = {
    val leftLeaves: Leaves[Label] = Leaves( Graph.leaves( left ) )
    val rightLeaves: Leaves[Label] = Leaves( Graph.leaves( right ) )
    val graph: Diff[Graph[Label]] = Same( Graph.single( root ) )
    NeedlemanWunsch( -1, leftLeaves.indexes, rightLeaves.indexes )
      .foldLeft( ( graph, root ) ) {
        case ( ( graph, previous ), Alignment( left, right ) ) =>
          left.zip( right ).foldLeft( ( graph, previous ) ) {
            case ( ( graph, previous ), ( l, r ) ) if l === r =>
              same[Label, Graph]( graph.value, previous, leftLeaves.leaves( l ) )
              leftLeaves.leaves( l ).foldLeft( ( graph, previous ) ) {
                case ( ( graph, previous ), label ) => Same( Graph.addEdge( graph.value )( previous, label ) -> label )
              }
            case ( ( graph, previous ), ( -1, r ) ) =>
              rightLeaves.leaves( r ).foldLeft( ( graph, previous ) ) {
                case ( ( graph, previous ), label ) => Added( Graph.addEdge( graph.value )( previous, label ) ) -> label
              }
            case ( ( graph, previous ), ( l, -1 ) ) =>
              leftLeaves.leaves( l ).foldLeft( ( graph, previous ) ) {
                case ( ( graph, previous ), label ) => Removed( Graph.addEdge( graph.value )( previous, label ) ) -> label
              }
            case ( ( graph, previous ), ( l, r ) ) =>
              NeedlemanWunsch[Label]( placeholder, leftLeaves.leaves( l ).toArray, rightLeaves.leaves( r ).toArray ).foldLeft( ( graph, previous ) ) {
                case ( ( graph, previous ), Alignment( left, right ) ) =>
                  ???
              }
          }
      }
      ._1
  }
  private def diffGraph[A: Eq, Graph[_]](graph: Graph[A], previous: A )(alignment: Alignment[A] ): Diff[Graph[A]] = ???
}
