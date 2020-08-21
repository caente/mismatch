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
    private val matrix: DenseMatrix[A]) {
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
  val columnVector: DenseVector[Int] = DenseVector( 0 +: labelledMatrix.rowLabels.zipWithIndex.map( v => (v._2 + 1) * -1 ) )
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
  def apply[Label: ClassTag: Eq](placeholder: Label, left: Array[Label], right: Array[Label] ): NeedlemanWunschMatrix[Label] =
    new NeedlemanWunschMatrix( matrices.LabelledMatrix.zeros( left, right ), placeholder ) {}
}

abstract sealed case class GraphMatrix[Label: ClassTag: Eq](private val matrix: LabelledMatrix[Int, Label, Int] ) {
  private val indexedColLabels = matrix.colLabels.zipWithIndex.toMap
  def addEdge(start: Label, end: Label ): GraphMatrix[Label] =
    if (isEdge( start, end )) this
    else {
      val startIndex: Option[Int] = indexedColLabels.get( start )
      val endIndex: Option[Int] = indexedColLabels.get( end )
      val newEdge = matrix.rowLabels.lastOption.map( _ + 1 ).getOrElse( 1 )
      val newNodes = ( startIndex, endIndex ) match {
        case ( Some( _ ), Some( _ ) ) => Array()
        case ( Some( _ ), None )      => Array( end )
        case ( None, Some( _ ) )      => Array( start )
        case _                        => throw new IllegalArgumentException( "At least one node must exist" )
      }
      val newMatrix: LabelledMatrix[Int, Label, Int] = matrix.append( Array( newEdge ), newNodes )
      val startCol = startIndex.getOrElse( matrix.colLabels.length - 1 )
      val endCol = endIndex.getOrElse( newMatrix.colLabels.length - 1 )
      newMatrix.update( newEdge - 1, startCol, -1 )
      newMatrix.update( newEdge - 1, endCol, 1 )
      new GraphMatrix( newMatrix ) {}
    }
  def isEdge(start: Label, end: Label ): Boolean = startOfEdge( end ) === Some( start )

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
    label :: startOfEdge( label ).toList.flatMap( pathToRoot )

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
  def single[Label: ClassTag: Eq](node: Label ): GraphMatrix[Label] =
    new GraphMatrix( LabelledMatrix.zeros( Array(), Array( node ) ) ) {}
}
