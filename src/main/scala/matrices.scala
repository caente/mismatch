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
  def padded[A](matrix: DenseMatrix[Int], extraRows: Int, extraCols: Int ): DenseMatrix[Int] = {
    val rowsVector: DenseVector[Int] = DenseVector.zeros( matrix.rows + extraRows )
    val colsVector: DenseVector[Int] = DenseVector.zeros( matrix.cols + extraCols - 1 )
    val matrixWithCols: DenseMatrix[Int] = DenseMatrix.vertcat( colsVector.toDenseMatrix, matrix )
    val matrixWithRows = DenseMatrix.horzcat( rowsVector.toDenseMatrix.t, matrixWithCols )
    matrixWithRows
  }
}

case class LabelledMatrix[A](rowLabels: Array[A], colLabels: Array[A] ) {
  val matrix: DenseMatrix[Int] = DenseMatrix.zeros[Int]( rowLabels.length, colLabels.length )
}

object M {
  def addNode[A](n: A, m: DenseMatrix[A] ): DenseMatrix[A] = {
    m.reshape( m.rows + 1, m.cols + 1 )
  }
}
