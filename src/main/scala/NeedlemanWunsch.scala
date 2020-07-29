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
  private sealed trait Side {
    def score: Int
  }
  private case class Left(score: Int ) extends Side
  private case class Top(score: Int ) extends Side
  private case class Diag(score: Int ) extends Side
  private case class Neighbors(left: Left, diag: Diag, top: Top )
  private sealed trait Section[+A] {
    def score: Int
  }
  private case class Border[+A](score: Int ) extends Section[A]
  private case class Node[+A](rowHeader: A, colHeader: A, score: Int, neighbors: Neighbors ) extends Section[A]
  private case class NWMatrix[A: Zero: Eq: ClassTag](rowsHeaders: Array[A], colsHeaders: Array[A] ) {
    val rowsVector: DenseVector[Int] = DenseVector( 0 +: rowsHeaders.zipWithIndex.map( v => (v._2 + 1) * -1 ) )
    val colsVector = DenseVector( colsHeaders.zipWithIndex.map( v => (v._2 + 1) * -1 ) )
    val matrix = {
      val matrix = DenseMatrix.zeros[Int]( rowsHeaders.length, colsHeaders.length )
      addHeaders( matrix, rowsVector, colsVector, identity )
    }
  }

  private def section[A: Eq](m: NWMatrix[A], row: Int, col: Int ): Section[A] = {
    val rowHeader = m.rowsHeaders( row - 1 )
    val colHeader = m.colsHeaders( col - 1 )
    val d = if (colHeader === rowHeader) 1 else -1
    val left = m.matrix( row, col - 1 ) + d
    val top = m.matrix( row - 1, col ) + d
    val diag = m.matrix( row - 1, col - 1 ) + d
    val score = List( left, top, diag ).max
    m.matrix.update( row, col, score )
    Node( rowHeader = rowHeader, colHeader = colHeader, score = score, neighbors = Neighbors( Left( left ), Diag( diag ), Top( top ) ) )
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
  case class Alignment[A](left: List[A], right: List[A] )
  private def accAppl[A](row: A, col: A, acc: Set[Alignment[A]] ): Set[Alignment[A]] = {
    if (acc.isEmpty) Set( Alignment( List( row ), List( col ) ) )
    else
      acc.map {
        case Alignment( left, right ) =>
          Alignment( row :: left, col :: right )
      }
  }

  private def alignments[A: ClassTag](placeholder: A, row: Int, col: Int, matrix: DenseMatrix[Section[A]], acc: Set[Alignment[A]] ): Set[Alignment[A]] = {
    val last = matrix( row, col )
    val next = last match {
      case Border( score ) => acc
      case Node( rowHeader, colHeader, _, Neighbors( left, diag, top ) ) =>
        val directions: Set[Side] = List( left, diag, top ).groupBy( _.score ).maxBy( _._1 )._2.toSet
        directions.flatMap {
          case Diag( _ ) =>
            alignments(
              placeholder = placeholder,
              row = row - 1,
              col = col - 1,
              matrix = matrix,
              acc = accAppl( rowHeader, colHeader, acc )
            )
          case Top( _ ) =>
            alignments(
              placeholder = placeholder,
              row = row - 1,
              col = col,
              matrix = matrix,
              acc = accAppl( rowHeader, placeholder, acc )
            )
          case Left( _ ) =>
            alignments(
              placeholder = placeholder,
              row = row,
              col = col - 1,
              matrix = matrix,
              acc = accAppl( placeholder, colHeader, acc )
            )
        }
    }
    next
  }
  def apply[A: ClassTag: Zero: Eq](placeholder: A, left: Array[A], right: Array[A] ): Set[Alignment[A]] = {
    val m = NWMatrix( left, right )
    val matrix = sectionedMatrix( m )
    alignments(
      placeholder = placeholder,
      row = matrix.rows - 1,
      col = matrix.cols - 1,
      matrix = matrix,
      acc = Set.empty[Alignment[A]]
    )
  }
}
