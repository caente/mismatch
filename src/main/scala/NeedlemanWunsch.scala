package algorithm

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
  private case class Left(i: Int )
  private case class Top(i: Int )
  private case class Diag(i: Int )
  private case class Neighbors(left: Left, diag: Diag, top: Top )
  private sealed trait Section[+A] {
    def score: Int
  }
  private case class Border[+A](score: Int ) extends Section[A]
  private case class Node[+A](fromRows: A, fromCols: A, score: Int, neighbors: Neighbors ) extends Section[A]
  private case class NWMatrix[A: Zero: Eq: ClassTag](rows: Array[A], cols: Array[A] ) {
    val rowsVector: DenseVector[Int] = DenseVector( 0 +: rows.zipWithIndex.map( v => (v._2 + 1) * -1 ) )
    val colsVector = DenseVector( cols.zipWithIndex.map( v => (v._2 + 1) * -1 ) )
    val matrix = {
      val matrix = DenseMatrix.zeros[Int]( rows.length, cols.length )
      addHeaders( matrix, rowsVector, colsVector, identity )
    }
  }

  private def section[A: Eq](m: NWMatrix[A], row: Int, col: Int ): Section[A] = {
    val fromRows = m.rows( row - 1 )
    val fromCols = m.cols( col - 1 )
    val d = if (fromCols === fromRows) 1 else -1
    val left = m.matrix( row, col - 1 ) + d
    val top = m.matrix( row - 1, col ) + d
    val diag = m.matrix( row - 1, col - 1 ) + d
    val score = List( left, top, diag ).max
    m.matrix.update( row, col, score )
    Node( fromRows = fromRows, fromCols = fromCols, score = score, neighbors = Neighbors( Left( left ), Diag( diag ), Top( top ) ) )
  }
  private def addHeaders[O: ClassTag: Zero](matrix: DenseMatrix[O], rowsVector: DenseVector[Int], colsVector: DenseVector[Int], f: Int => O ) = {
    val matrixWithCols = DenseMatrix.vertcat( colsVector.mapValues( f ).toDenseMatrix, matrix )
    val matrixWithRows = DenseMatrix.horzcat( rowsVector.mapValues( f ).toDenseMatrix.t, matrixWithCols )
    matrixWithRows
  }
  private def sectionedMatrix[A: Eq](m: NWMatrix[A] ): DenseMatrix[Section[A]] = {
    val updated: DenseMatrix[Section[A]] =
      m.matrix( 1 until m.matrix.rows, 1 until m.matrix.cols ).mapPairs {
        case ( ( _row, _col ), value ) =>
          val row = _row + 1
          val col = _col + 1
          val s = section( m, row, col )
          s
      }
    addHeaders( updated, m.rowsVector, m.colsVector, i => Border( i ) )
  }

  private def alignments[A: Eq](matrix: DenseMatrix[Section[A]] ): Set[( Array[A], Array[A] )] = {
    /*
     * 1 - Start at the bottom left
     * 2 - pick the neighbors with the biggest score
     * 3 - for each neighbor with the same biggest score: goto 2)
     */
    ???
  }
  private val m = NWMatrix( Array( 'a, 'b, 'c, 'd ), Array( 'e, 'b, 'f ) )
  //println( m.matrix )
  //println( "-" * 30 )
  println( sectionedMatrix( m ) )
}
