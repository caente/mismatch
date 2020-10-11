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
  //def apply[Label: ClassTag: Eq](
  //    placeholder: Label,
  //    left: AdjacentGraph[Label],
  //    right: AdjacentGraph[Label]
  //  ): AdjacentGraph[Diff[Label]] = {
  //  val leftLeaves: Leaves[Label] = Leaves( left.branches( left.root ).toArray )
  //  val rightLeaves: Leaves[Label] = Leaves( right.branches( right.root ).toArray )
  //  val leavesDiffs =
  //    NeedlemanWunsch( -1, leftLeaves.indexes, rightLeaves.indexes )
  //  leavesDiffs.foreach {
  //    case Alignment( left, right ) =>
  //      println( "-" * 30 )
  //      val l = left.map( leftLeaves.leaves.getOrElse( _, Nil ) )
  //      val r = right.map( rightLeaves.leaves.getOrElse( _, Nil ) )
  //      val comparisons: List[Diff[Label]] =
  //        l.zip( r )
  //          .flatMap {
  //            case ( Nil, Nil ) => Nil
  //            case ( left, Nil ) =>
  //              left.map( Removed( _ ) )
  //            case ( Nil, right ) =>
  //              right.map( Added( _ ) )
  //            case notEmpty @ ( left, right ) =>
  //              NeedlemanWunsch( placeholder, left.toArray, right.toArray ).flatMap {
  //                case Alignment( left, right ) => compare( left, right )
  //              }
  //          }
  //      pprint.pprintln( comparisons.distinct )
  //    //  .unzip match {
  //    //  case ( left, right ) =>
  //    //    pprint.pprintln( left, width = 10000 )
  //    //    pprint.pprintln( right, width = 10000 )
  //    //}
  //  }
  //  ???
  //}
  //def compare[Label: Eq](left: List[Label], right: List[Label] ) = {
  //  left.zip( right ).map {
  //    case ( left, right ) if left === right => Same( left )
  //    case ( left, right )                   => Exchanged( left, right )
  //  }
  //}
  /*
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
  }*/
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

// pprint.pprintln( left.branches( left.root ) )
// pprint.pprintln( right.branches( right.root ) )
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

  val alignments =
    NeedlemanWunsch(
      '-,
      A.topological( A.root ).toArray,
      B.topological( B.root ).toArray
    )
  case class Exchange[Label](exchanged: Set[Label] )
  val compared: Map[Symbol, Exchange[Symbol]] =
    alignments.foldLeft( Map.empty[Symbol, Exchange[Symbol]] ) {
      case ( boths, Alignment( left, right ) ) =>
        left.zip( right ).foldLeft( boths ) {
          case ( boths, ( left, right ) ) =>
            val both = boths.getOrElse( left, Exchange( Set.empty[Symbol] ) )
            boths.updated(
              left,
              both.copy(
                exchanged = both.exchanged + right
              )
            )
        }
    }

  lazy val newGraph: GraphVisitation[AdjacentGraph, Symbol, Diff[Symbol]] =
    alignments.foldLeft( GraphVisitation( AdjacentGraph.single( Diff.same( A.root ) ), Set.empty[Symbol] ) ) {
      case ( visitation, Alignment( left, right ) ) =>
        left.zip( right ).foldLeft( visitation ) {
          case ( GraphVisitation( result, visited ), ( '-, r ) ) =>
            val parent = B.findParentUnsafe( _ === r )
            val parentDiff = result.findUnsafe( _.value === parent )
            GraphVisitation( result.addEdge( parentDiff, Diff.added( r ) ), visited + r )
          case ( GraphVisitation( result, visited ), ( l, '- ) ) =>
            val parent = A.findParentUnsafe( _ === l )
            val parentDiff = result.findUnsafe( _.value === parent )
            GraphVisitation( result.addEdge( parentDiff, Diff.removed( l ) ), visited + l )
          case ( GraphVisitation( result, visited ), ( l, r ) ) if l === r =>
            val parent = A.findParentUnsafe( _ === l )
            val parentDiff = result.findUnsafe( _.value === parent )
            GraphVisitation( result.addEdge( parentDiff, Diff.same( l ) ), visited + l )
          case ( GraphVisitation( result, visited ), ( l, r ) ) if l =!= r =>
            val parentL = A.findParentUnsafe( _ === l )
            val parentR = B.findParentUnsafe( _ === r )
            val parentDiffL = result.findUnsafe( _.value === parentL )
            val parentDiffR = result.findUnsafe( _.value === parentR )
            GraphVisitation(
              result.addEdge( parentDiffL, Diff.removed( l ) ).addEdge( parentDiffR, Diff.added( r ) ),
              visited + l + r
            )
        }
    }

  pprint.pprintln( A.topological( A.root ) )
  pprint.pprintln( B.topological( B.root ) )
  pprint.pprintln( compared )
  var visited = Set.empty[Symbol]
  A.dfs( A.root, AdjacentGraph.single( Diff.same( A.root ) ), Set() ) { ( parent, child, newGraph ) =>
    compared( child ) match {
      case Exchange( exch ) if exch.contains( child ) =>
        val parentDiff =
          newGraph
            .find( _.value === parent )
            .result
            .getOrElse( throw new RuntimeException( s"Parent not found: $parent + $newGraph" ) )
        newGraph.addEdge( parentDiff, Diff.same( child ) ) //same
      case Exchange( exch ) if !exch.contains( child ) =>
        val parentDiff =
          newGraph
            .find( _.value === parent )
            .result
            .getOrElse( throw new RuntimeException( s"Parent not found: $parent + $newGraph" ) )
        exch.foldLeft( newGraph.addEdge( parentDiff, Diff.removed( child ) ) ) { //deleted/added
          case ( g, '- )                         => g
          case ( g, r ) if visited.contains( r ) => g
          case ( g, r ) =>
            visited += r
            //TODO: filter out the visited from right.subGraph(r)
            g.addSubGraph( parentDiff, B.subGraph( r ).map( Diff.added( _ ) ) )
        }
    }
  }
  //compared( '- ).exchanged.foldLeft( newGraph ) { ( g, r ) =>
  //  right.dfs( right.root, g.result, Set() )(
  //    combine = ( parent, child, visitation ) =>
  //      if (child === r) {
  //        val commonParent: Diff[Symbol] = ???
  //        val extra: AdjacentGraph[Diff[Symbol]] = right.subGraph( commonParent.value ).map( Diff.added )
  //        visitation.addSubGraph( commonParent, extra )
  //      } else visitation
  //  )
  //}
  pprint.pprintln( newGraph )
  pprint.pprintln( A )
  pprint.pprintln( B )
  //alignments
  //  .foreach {
  //    case Alignment( left, right ) =>
  //      println( "-" * 30 )
  //      pprint.pprintln( left, width = 10000 )
  //      pprint.pprintln( right, width = 10000 )
  //  }
//  Mismatches( '-, left, right )
}
