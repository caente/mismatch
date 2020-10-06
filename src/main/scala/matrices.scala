package matrices

import simulacrum._
import cats._
import cats.implicits._
import higherkindness.droste._
import higherkindness.droste.data._
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import scala.reflect.ClassTag
import breeze.storage.Zero
import breeze.linalg.CanPadRight
import breeze.math.Semiring

object utils {
  def prepend[A: ClassTag: Zero](matrix: DenseMatrix[A], extraRows: Int, extraCols: Int ): DenseMatrix[A] = {
    val newColsMatrix = DenseMatrix.zeros( matrix.rows, extraCols )
    val newRowsMatrix = DenseMatrix.zeros( extraRows, matrix.cols + extraCols )
    val matrixExtraCols = DenseMatrix.horzcat( newColsMatrix, matrix )
    val padded = DenseMatrix.vertcat( matrixExtraCols, matrixExtraCols )
    padded
  }
  def append[A: ClassTag: Zero](matrix: DenseMatrix[A], extraRows: Int, extraCols: Int ): DenseMatrix[A] = {
    val newColsMatrix = DenseMatrix.zeros( matrix.rows, extraCols )
    val newRowsMatrix = DenseMatrix.zeros( extraRows, matrix.cols + extraCols )
    val matrixExtraCols = DenseMatrix.horzcat( matrix, newColsMatrix )
    val padded = DenseMatrix.vertcat( matrixExtraCols, newRowsMatrix )
    padded
  }
}

object LabelledMatrix {
  def zeros[LabelRow: ClassTag, LabelCol: ClassTag, A: ClassTag: Zero: Semiring](
      rowLabels: Array[LabelRow],
      colLabels: Array[LabelCol]
    ): LabelledMatrix[LabelRow, LabelCol, A] = {
    val matrix: DenseMatrix[A] = DenseMatrix.zeros[A]( rowLabels.length, colLabels.length )
    new LabelledMatrix( rowLabels, colLabels, matrix ) {}
  }
}
abstract sealed case class LabelledMatrix[LabelRow: ClassTag, LabelCol: ClassTag, A: ClassTag: Zero: Semiring](
    rowLabels: Array[LabelRow],
    colLabels: Array[LabelCol],
    val matrix: DenseMatrix[A]) {
  def print = println( matrix )
  def column(col: Int ) = matrix( ::, col ).toDenseVector
  def row(row: Int ) = matrix( row, :: ).inner
  def prepend(newRowLabels: Array[LabelRow], newColLabels: Array[LabelCol] ): LabelledMatrix[LabelRow, LabelCol, A] = {
    val paddedMatrix = utils.append( matrix, newRowLabels.length, newColLabels.length )
    new LabelledMatrix( newRowLabels ++ rowLabels, newColLabels ++ colLabels, paddedMatrix ) {}
  }
  def append(newRowLabels: Array[LabelRow], newColLabels: Array[LabelCol] ): LabelledMatrix[LabelRow, LabelCol, A] = {
    val paddedMatrix = utils.append( matrix, newRowLabels.length, newColLabels.length )
    new LabelledMatrix( rowLabels ++ newRowLabels, colLabels ++ newColLabels, paddedMatrix ) {}
  }
  def update(row: Int, col: Int, v: A ): Unit = matrix.update( row, col, v )
  def apply(row: Int, col: Int ): A = matrix( row, col )
  def apply(r1: Range, r2: Range ) = matrix( r1, r2 )
  def rows: Int = matrix.rows
  def cols: Int = matrix.cols
  def columnSum = matrix.t * DenseVector.ones[A]( matrix.rows )
  override def toString = s"$matrix"
}

abstract sealed case class NeedlemanWunschMatrix[Label: ClassTag](
    private val labelledMatrix: matrices.LabelledMatrix[Label, Label, Int],
    defaultLabel: Label) {
  val rowLabels = labelledMatrix.rowLabels
  val colLabels = labelledMatrix.colLabels
  val columnVector: DenseVector[Int] = DenseVector(
    0 +: labelledMatrix.rowLabels.zipWithIndex.map( v => (v._2 + 1) * -1 )
  )
  val horizontalVector = DenseVector( 0 +: labelledMatrix.colLabels.zipWithIndex.map( v => (v._2 + 1) * -1 ) )
  val matrix = labelledMatrix.prepend( Array( defaultLabel ), Array( defaultLabel ) )
  columnVector.mapPairs {
    case ( index, v ) => matrix.update( index, 0, v )
  }
  horizontalVector.mapPairs {
    case ( index, v ) => matrix.update( 0, index, v )
  }
}

object NeedlemanWunschMatrix {
  def apply[Label: ClassTag: Eq](
      placeholder: Label,
      left: Array[Label],
      right: Array[Label]
    ): NeedlemanWunschMatrix[Label] =
    new NeedlemanWunschMatrix( matrices.LabelledMatrix.zeros( left, right ), placeholder ) {}
}

abstract sealed case class GraphMatrix[Label](
    val matrix: LabelledMatrix[Int, Label, Int],
    private val adjacents: Map[Label, List[Label]]) {
  val labels = matrix.colLabels
  ///val edges: Set[( Label, Label )] = ???
  val indexedColLabels = matrix.colLabels.zipWithIndex.toMap
  def addEdge(start: Label, end: Label )(implicit E: Eq[Label], C: ClassTag[Label] ): GraphMatrix[Label] = {
    if (start === end && indexedColLabels.keySet.contains( start )) {
      println( s"$start === $end" )
      this
    } else if (start =!= end && startOfEdge( end ) === Some( start )) this
    else {
      val startIndex: Option[Int] = indexedColLabels.get( start )
      val endIndex: Option[Int] = indexedColLabels.get( end )
      val newEdge = matrix.rowLabels.lastOption.map( _ + 1 ).getOrElse( 1 )
      val newNodes = ( startIndex, endIndex ) match {
        case ( Some( _ ), Some( _ ) ) => Array()
        case ( Some( _ ), None )      => Array( end )
        case ( None, Some( _ ) )      => Array( start )
        case _                        => throw new IllegalArgumentException( s"At least one node must exist; start:$start end:$end" )
      }
      val newMatrix: LabelledMatrix[Int, Label, Int] = matrix.append( Array( newEdge ), newNodes )
      val startCol = startIndex.getOrElse( matrix.colLabels.length - 1 )
      val endCol = endIndex.getOrElse( newMatrix.colLabels.length - 1 )
      newMatrix.update( newEdge - 1, startCol, -1 )
      newMatrix.update( newEdge - 1, endCol, 1 )
      new GraphMatrix( newMatrix, adjacents.updated( start, end :: adjacents.getOrElse( start, Nil ) ) ) {}
    }
  }

  private def dfsLeaves(m: DenseMatrix[Int], start: Label ): List[List[Label]] = {
    val col = indexedColLabels( start )
    val nextLabels: List[Option[Label]] =
      matrix
        .column( col )
        .mapPairs {
          case ( index, -1 ) =>
            val row = matrix.row( index )
            row.update( col, 0 )
            val colFinder: DenseVector[Int] = DenseVector( 0.until( row.length ).toArray )
            val nextCol = row.dot( colFinder )
            Some( matrix.colLabels( nextCol ) )
          case _ => None
        }
        .toArray
        .toList
    val m2 = DenseMatrix.horzcat( m( ::, 0 until col ), m( ::, col + 1 until m.cols ) )
    nextLabels.flatMap {
      case Some( nextLabel ) => dfsLeaves( m2, nextLabel ).map( start :: _ )
      case _                 => Nil
    }
  }

  //find the row vector corresponding to the row where there is a 1
  //in the column of label
  private def incomingRowVector(label: Label ): Option[DenseVector[Int]] = {
    for {
      col <- indexedColLabels.get( label ).toList
      data = matrix
        .column( col ) // column corresponding to the label
        .toDenseVector
        .data
        .toList
        .zipWithIndex
      ( 1, index ) <- data
    } yield matrix.row( index )
  }.headOption // there can only be a single row with a 1

  private def startOfEdge(label: Label ): Option[Label] = {
    // this vector will be of the form [0, -1, -2, -3, 0, -4....], where 0 is the current column
    // the goal is to find the dot product of this vector with the row vector corresponding to the
    // outgoing edge
    val colFinder: DenseVector[Int] = DenseVector( 0.until( matrix.cols ).toArray ).map( v => -v )
    for {
      col <- indexedColLabels.get( label )
      _ = colFinder.update( col, 0 )
      row <- incomingRowVector( label )
      nextCol = row.dot( colFinder ) // the column where the value is -1, i.e. an outoging edge
    } yield matrix.colLabels( nextCol )
  }

  private def pathToRoot(label: Label ): List[Label] =
    startOfEdge( label ).toList.flatMap( pathToRoot ) :+ label

  val leaves: Array[List[Label]] = {
    matrix.columnSum.toArray
      .zip( matrix.colLabels )
      .collect {
        case ( 1, label ) => pathToRoot( label )
      }
  }

  def print = matrix.print
}

object GraphMatrix {
  def single[Label: ClassTag](node: Label ): GraphMatrix[Label] =
    new GraphMatrix( LabelledMatrix.zeros( Array(), Array( node ) ), Map( node -> Nil ) ) {}
}

object AdjacentGraph {
  def single[Label: Eq](node: Label ): AdjacentGraph[Label] =
    new AdjacentGraph( node, Map( node -> List.empty[Label] ) ) {}
}
case class GraphVisitation[F[_], Label](result: F[Label], visited: Set[Label] )

sealed abstract case class AdjacentGraph[Label: Eq](root: Label, val data: Map[Label, List[Label]] ) {
  def branches(start: Label ): List[List[Label]] = {
    if (adjacents( start ).isEmpty) List( List( start ) )
    else {
      adjacents( start ).flatMap( a => branches( a ).map( start :: _ ) )
    }
  }

  def topological(start: Label ): List[Label] =
    dfs( start, List( start ), Set() )( ( _, child, acc ) => child :: acc ).result.reverse

  def adjacents(a: Label ): List[Label] = data.getOrElse( a, List() )

  def subGraph(start: Label ): AdjacentGraph[Label] = {
    dfs( start, AdjacentGraph.single( start ), Set() ) { ( parent, child, newGraph ) =>
      newGraph.addEdge( parent, child )
    }.result
  }

  def addGraph(node: Label, graph: AdjacentGraph[Label] ): AdjacentGraph[Label] = {
    ???
  }

  def addEdge(start: Label, end: Label ): AdjacentGraph[Label] = {
    if (data.keySet.contains( start )) {
      if (start === end) {
        this
      } else {
        val newData = data.updated( start, (end :: data( start )).distinct ).updated( end, adjacents( end ) )
        new AdjacentGraph( root, newData ) {}
      }
    } else
      throw new IllegalArgumentException( s"At least one node must exist; start:$start end:$end" )
  }

  private def dfs[F[_]](
      start: Label,
      acc: F[Label],
      initiallyVisited: Set[Label]
    )(combine: (Label, Label, F[Label] ) => F[Label]
    ): GraphVisitation[F, Label] =
    adjacents( start )
      .foldLeft( GraphVisitation( acc, initiallyVisited ) ) {
        case ( GraphVisitation( acc, visited ), adj ) if visited.contains( adj ) => GraphVisitation( acc, visited )
        case ( GraphVisitation( acc, visited ), adj ) =>
          dfs( adj, combine( start, adj, acc ), visited + start )( combine )
      }

  private def bfs[F[_]](
      start: Label,
      acc: F[Label],
      initiallyVisited: Set[Label]
    )(combine: (Label, Label, F[Label] ) => F[Label]
    ): GraphVisitation[F, Label] = {
    val visitedAdjacents =
      adjacents( start ).foldLeft( GraphVisitation( acc, initiallyVisited ) ) {
        case ( GraphVisitation( graph, visited ), adj ) if visited.contains( adj ) => GraphVisitation( graph, visited )
        case ( GraphVisitation( graph, visited ), adj ) =>
          GraphVisitation( combine( start, adj, graph ), visited + adj )
      }

    adjacents( start )
      .foldLeft( visitedAdjacents ) {
        case ( GraphVisitation( graph, visited ), adj ) =>
          bfs( adj, graph, visited )( combine )
      }
  }
}
