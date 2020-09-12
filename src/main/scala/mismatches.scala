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
  trait GraphOps[Graph[_]] {
    def single[Label: ClassTag](label: Label ): Graph[Label]
    def addEdge[Label: ClassTag: Eq](graph: Graph[Label] )(start: Label, end: Label ): Graph[Label]
    def leaves[Label](graph: Graph[Label] ): Array[List[Label]]
    def print[Label](graph: Graph[Label] ): Unit
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
  private def addBranch[A: Eq](
      graph: GraphMatrix[Diff[A]],
      previous: Diff[A],
      nodes: List[A]
    )(f: A => Diff[A]
    ): ( GraphMatrix[Diff[A]], Diff[A] ) = {
    nodes
      .foldLeft( ( graph, previous ) ) {
        case ( ( graph, previous ), node ) =>
          val nodeK = f( node )
          // println( "addBranch" )
          // graph.print
          // println( graph.indexedColLabels )
          // println( s"$previous -> $nodeK" )
          val g =
            graph.addEdge( previous, nodeK ) -> f( node )
          g
      }

  }
  def apply[Label: ClassTag: Eq](placeholder: Label, left: AdjacentGraph[Label], right: AdjacentGraph[Label] ): AdjacentGraph[Diff[Label]] = {
    val leftLeaves: Leaves[Label] = Leaves( left.branches( left.root ).map( _.drop( 1 ) ).toArray ) //dropping the root node
    val rightLeaves: Leaves[Label] = Leaves( right.branches( right.root ).map( _.drop( 1 ) ).toArray ) //dropping the root node
    val leavesDiffs =
      NeedlemanWunsch( -1, leftLeaves.indexes, rightLeaves.indexes )
    leavesDiffs.foreach {
      case Alignment( left, right ) =>
        println( "-" * 30 )
        pprint.pprintln( left.map( leftLeaves.leaves.getOrElse( _, placeholder ) ) )
        pprint.pprintln( right.map( rightLeaves.leaves.getOrElse( _, placeholder ) ) )
    }
    ???
  }
  def apply[Label: ClassTag: Eq](placeholder: Label, root: Label, left: GraphMatrix[Label], right: GraphMatrix[Label] ): GraphMatrix[Diff[Label]] = {
    val leftLeaves: Leaves[Label] = Leaves( left.leaves.map( _.drop( 1 ) ) ) //dropping the root node
    val rightLeaves: Leaves[Label] = Leaves( right.leaves.map( _.drop( 1 ) ) ) //dropping the root node
    val leavesDiffs =
      NeedlemanWunsch( -1, leftLeaves.indexes, rightLeaves.indexes )
    leavesDiffs.foreach {
      case Alignment( left, right ) =>
        pprint.pprintln( left.map( leftLeaves.leaves.getOrElse( _, placeholder ) ) )
        pprint.pprintln( right.map( rightLeaves.leaves.getOrElse( _, placeholder ) ) )
        println( "-" * 30 )
    }
    leavesDiffs
      .foldLeft( ( GraphMatrix.single( Diff.same( root ) ), Diff.same( root ) ) ) {
        case ( ( graph, previous ), Alignment( left, right ) ) =>
          //println( "left:" )
          //pprint.pprintln( left )
          //println( "right:" )
          //pprint.pprintln( right )
          left.zip( right ).foldLeft( ( graph, previous ) ) {
            case ( ( graph, previous ), ( l, r ) ) if l === r =>
              //println( "l === r" )
              //pprint.pprintln( leftLeaves.leaves( l ) )
              addBranch( graph, previous, leftLeaves.leaves( l ) )( Same( _ ) )
            case ( ( graph, previous ), ( l, -1 ) ) =>
              //println( "only l" )
              val removed = leftLeaves
                .leaves( l )
                .filter( label => !graph.indexedColLabels.keySet.exists( _.value === label ) )
              //pprint.pprintln( leftLeaves.leaves( l ) )
              addBranch( graph, previous, removed )( Removed( _ ) )
            case ( ( graph, previous ), ( -1, r ) ) =>
              //println ("only r")
              val added = rightLeaves
                .leaves( r )
                .filter( label => !graph.indexedColLabels.keySet.exists( _.value === label ) )
              //pprint.pprintln( rightLeaves.leaves( r ) )
              addBranch( graph, previous, added )( Added( _ ) )
            case ( ( graph, previous ), ( l, r ) ) =>
              //println( "l =/= r" )
              //pprint.pprintln( leftLeaves.leaves( l ) )
              //pprint.pprintln( rightLeaves.leaves( r ) )
              NeedlemanWunsch[Label]( placeholder, leftLeaves.leaves( l ).toArray, rightLeaves.leaves( r ).toArray ).foldLeft( ( graph, previous ) ) {
                case ( ( graph, previous ), Alignment( left, right ) ) =>
                  left.zip( right ).foldLeft( ( graph, previous ) ) {
                    case ( ( graph, previous ), ( l, r ) ) if l === r =>
                      graph.addEdge( previous, Same( l ) ) -> Same( l )
                    case ( ( graph, previous ), ( l, -1 ) ) =>
                      graph.addEdge( previous, Removed( l ) ) -> Removed( l )
                    case ( ( graph, previous ), ( -1, r ) ) =>
                      graph.addEdge( previous, Added( r ) ) -> Added( r )
                    case ( ( graph, previous ), ( l, r ) ) =>
                      graph.addEdge( previous, Removed( l ) ) -> Removed( l )
                      graph.addEdge( previous, Added( r ) ) -> Added( r )
                  }
              }
          }
      }
      ._1
  }
}

object MismatchesTest extends App {
  val left =
    matrices.AdjacentGraph
      .single( 'Foo )
      .addEdge( 'Foo, 'b )
      .addEdge( 'Foo, 'a )
      .addEdge( 'a, 'c )
      .addEdge( 'a, 'd )
      .addEdge( 'b, 'f )
      .addEdge( 'b, 'g )
      .addEdge( 'c, 'e )
  val right =
    matrices.AdjacentGraph
      .single( 'Foo )
      .addEdge( 'Foo, 'a )
      .addEdge( 'Foo, 'b )
      .addEdge( 'a, 'c )
      .addEdge( 'a, 'd )
      .addEdge( 'b, 'h )
      .addEdge( 'c, 'j )
      .addEdge( 'h, 'i )

  pprint.pprintln( left.branches( left.root ) )
  pprint.pprintln( right.branches( right.root ) )
  //implicit val gr = new Mismatches.GraphOps[GraphMatrix] {
  //  def single[Label: ClassTag](label: Label ): GraphMatrix[Label] = GraphMatrix.single( label )
  //  def addEdge[Label: ClassTag: Eq](graph: GraphMatrix[Label] )(start: Label, end: Label ): GraphMatrix[Label] =
  //    graph.addEdge( start, end )
  //  def leaves[Label](graph: GraphMatrix[Label] ): Array[List[Label]] = graph.leaves
  //  def print[Label](graph: GraphMatrix[Label] ): Unit = graph.print

  //}

  //ammonite
  //  .Main()
  //  .run(
  //    "m" -> left.matrix.matrix,
  //    "g" -> left
  //  )
  //pprint.pprintln( left.leaves.map( _.drop( 1 ) ).toList )
  
  pprint.pprintln(left.data)

  Mismatches( '-, left, right )
}
