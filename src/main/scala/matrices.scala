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

object utils {
  def padded[A: ClassTag: Zero](matrix: DenseMatrix[A], extraRows: Int, extraCols: Int ): DenseMatrix[A] = {
    val rowsVector: DenseVector[A] = DenseVector.zeros( matrix.rows + extraRows )
    val colsVector: DenseVector[A] = DenseVector.zeros( matrix.cols + extraCols - 1 )
    val matrixWithCols: DenseMatrix[A] = DenseMatrix.vertcat( colsVector.toDenseMatrix, matrix )
    val matrixWithRows = DenseMatrix.horzcat( rowsVector.toDenseMatrix.t, matrixWithCols )
    matrixWithRows
  }
}

case class LabelledMatrix[Label: ClassTag, A: ClassTag: Zero](rowLabels: Array[Label], colLabels: Array[Label] ) {
  private val matrix: DenseMatrix[A] = DenseMatrix.zeros[A]( rowLabels.length, colLabels.length )
  def prepend(newLabels: Array[Label] ): LabelledMatrix[Label, A] = {
    val paddedMatrix = utils.padded( matrix, newLabels.length, newLabels.length )
    LabelledMatrix( newLabels ++ rowLabels, newLabels ++ colLabels )
  }
  def update(row: Int, col: Int, v: A ): Unit = matrix.update( row, col, v )
  def apply(row: Int, col: Int ): A = matrix( row, col )
  def apply(r1: Range, r2: Range ) = matrix( r1, r2 )
  def rows: Int = matrix.rows
  def cols: Int = matrix.cols
}

object M {
  def addNode[A](n: A, m: DenseMatrix[A] ): DenseMatrix[A] = {
    m.reshape( m.rows + 1, m.cols + 1 )
  }
}
