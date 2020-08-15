package matrices

import shapeless.Nat
import shapeless.nat._
import shapeless.ops.nat.Sum
import shapeless.syntax.typeable
import shapeless.Witness
import simulacrum._
import cats._
import shapeless.Succ
import cats.implicits._
import higherkindness.droste._
import higherkindness.droste.data._
import shapeless._
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import scala.reflect.ClassTag
import breeze.storage.Zero
import breeze.linalg.CanPadRight

object utils {
  def prepend[A: ClassTag: Zero](matrix: DenseMatrix[A], extraRows: Int, extraCols: Int ): DenseMatrix[A] = {
    val newRowsMatrix: DenseVector[A] = DenseVector.zeros( matrix.rows + extraRows )
    val horizontalVector: DenseVector[A] = DenseVector.zeros( matrix.cols + extraCols - 1 )
    val matrixExtraCols: DenseMatrix[A] = DenseMatrix.vertcat( horizontalVector.toDenseMatrix, matrix )
    DenseMatrix.horzcat( newRowsMatrix.toDenseMatrix.t, matrixExtraCols )
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
  def zeros[LabelRow: ClassTag, LabelCol: ClassTag, A: ClassTag: Zero](rowLabels: Array[LabelRow], colLabels: Array[LabelCol] ): LabelledMatrix[LabelRow, LabelCol, A] = {
    val matrix: DenseMatrix[A] = DenseMatrix.zeros[A]( rowLabels.length, colLabels.length )
    LabelledMatrix( rowLabels, colLabels, matrix )
  }
}
case class LabelledMatrix[LabelRow: ClassTag, LabelCol: ClassTag, A: ClassTag: Zero](rowLabels: Array[LabelRow], colLabels: Array[LabelCol], private val matrix: DenseMatrix[A] ) {
  def print = println( matrix )
  def prepend(newRowLabels: Array[LabelRow], newColLabels: Array[LabelCol] ): LabelledMatrix[LabelRow, LabelCol, A] = {
    val paddedMatrix = utils.append( matrix, newRowLabels.length, newColLabels.length )
    LabelledMatrix( newRowLabels ++ rowLabels, newColLabels ++ colLabels, paddedMatrix )
  }
  def append(newRowLabels: Array[LabelRow], newColLabels: Array[LabelCol] ): LabelledMatrix[LabelRow, LabelCol, A] = {
    val paddedMatrix = utils.append( matrix, newRowLabels.length, newColLabels.length )
    LabelledMatrix( rowLabels ++ newRowLabels, colLabels ++ newColLabels, paddedMatrix )
  }
  def update(row: Int, col: Int, v: A ): Unit = matrix.update( row, col, v )
  def apply(row: Int, col: Int ): A = matrix( row, col )
  def apply(r1: Range, r2: Range ) = matrix( r1, r2 )
  def rows: Int = matrix.rows
  def cols: Int = matrix.cols
  override def toString = s"$matrix"
}

case class NeedlemanWunschMatrix[Label: ClassTag](private val labelledMatrix: matrices.LabelledMatrix[Label, Label, Int], defaultLabel: Label ) {
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

case class GraphMatrix[Label: ClassTag: Eq](private val matrix: LabelledMatrix[Int, Label, Int] ) {
  val indexedColLabels = matrix.colLabels.zipWithIndex.toMap
  def addEdge(start: Label, end: Label ): GraphMatrix[Label] = {
    val startIndex: Option[Int] = indexedColLabels.get( start )
    val endIndex: Option[Int] = indexedColLabels.get( end )
    val newEdge = matrix.rowLabels.lastOption.map( _ + 1 ).getOrElse( 1 )
    val newNodes = ( startIndex, endIndex ) match {
      case ( Some( _ ), Some( _ ) ) => Array()
      case ( Some( _ ), None )      => Array( end )
      case ( None, Some( _ ) )      => Array( start )
      case _                        => Array()
    }
    val newMatrix: LabelledMatrix[Int, Label, Int] = matrix.append( Array( newEdge ), newNodes )
    val startCol = startIndex.getOrElse( matrix.colLabels.length - 1 )
    val endCol = endIndex.getOrElse( newMatrix.colLabels.length - 1 )
    newMatrix.update( newEdge - 1, startCol, -1 )
    newMatrix.update( newEdge - 1, endCol, 1 )
    GraphMatrix( newMatrix )
  }
  def print = matrix.print
}

object GraphMatrix {
  def single[Label: ClassTag: Eq](node: Label ): GraphMatrix[Label] =
    GraphMatrix( LabelledMatrix.zeros( Array(), Array( node ) ) )
}
