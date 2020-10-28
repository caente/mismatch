package matrices

import cats.Eq
import breeze.linalg.{ DenseMatrix, DenseVector }
import scala.reflect.ClassTag
import breeze.storage.Zero
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
