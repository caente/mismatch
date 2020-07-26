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
import breeze.linalg._
import scala.reflect.ClassTag
import breeze.storage.Zero

object NeedlemanWunsch extends App {
  case class Neighbors(left: Int, top: Int, diag: Int )
  sealed trait Section[+A]
  case class Border[+A](score: Int ) extends Section[A]
  case class Node[+A](fromRows: A, fromCols: A, score: Int, neighbors: Neighbors ) extends Section[A]
  case class NWMatrix[A: Zero: Eq: ClassTag](rows: Array[A], cols: Array[A] ) {

    def initializedMatrix = {
      val matrix = DenseMatrix.zeros[Int]( rows.length, cols.length )
      addHeaders( matrix, identity )
    }

    def sectionedMatrix(matrix: DenseMatrix[Int] ): DenseMatrix[Section[A]] = {
      val updated =
        matrix( 1 until matrix.rows, 1 until matrix.cols ).mapActivePairs {
          case ( ( _row, _col ), value ) =>
            val row = _row + 1
            val col = _col + 1
            val s = section( matrix, row, col )
            s
        }
      addHeaders( updated, i => Border( i ) )
    }

    def alignments(matrix: DenseMatrix[Section[A]] ): Set[Vector[( A, A )]] = {
      ???
    }
    private val rowsVector: DenseVector[Int] = DenseVector( 0 +: rows.zipWithIndex.map( v => (v._2 + 1) * -1 ) )
    private val colsVector = DenseVector( cols.zipWithIndex.map( v => (v._2 + 1) * -1 ) )
    private def addHeaders[O: ClassTag: Zero](matrix: DenseMatrix[O], f: Int => O ) = {
      val matrixWithCols = DenseMatrix.vertcat( colsVector.mapValues( f ).toDenseMatrix, matrix )
      val matrixWithRows = DenseMatrix.horzcat( rowsVector.mapValues( f ).toDenseMatrix.t, matrixWithCols )
      matrixWithRows
    }
    private def section(matrix: DenseMatrix[Int], row: Int, col: Int ): Section[A] = {
      val fromRows = rows( row - 1 )
      val fromCols = cols( col - 1 )
      val d = if (fromCols === fromRows) 1 else -1
      val left = matrix( row, col - 1 )
      val top = matrix( row - 1, col )
      val diag = matrix( row - 1, col - 1 )
      Node( fromRows = fromRows, fromCols = fromCols, score = List( left + d, top + d, diag + d ).max, neighbors = Neighbors( left, top, diag ) )
    }
  }
  val matrix = NWMatrix( Array( 'a, 'b, 'c, 'd ), Array( 'e, 'b, 'f ) )
  println( matrix.initializedMatrix )
  println( "-" * 30 )
  println( matrix.sectionedMatrix( matrix.initializedMatrix ) )
}
