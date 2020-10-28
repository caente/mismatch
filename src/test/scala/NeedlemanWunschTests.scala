package algorithm
package tests

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalactic.TypeCheckedTripleEquals
import cats.instances.SymbolInstances
import NeedlemanWunsch._

class NeedlemanWunschTests extends AnyFunSuite with TypeCheckedTripleEquals with Matchers with SymbolInstances {
  test( "first case ever" ) {
    val left = Array( 'a, 'b, 'c, 'd )
    val right = Array( 'e, 'b, 'f )

    assert(
      NeedlemanWunsch.findAlignments( '-, left, right ) === List(
        NeedlemanWunsch.Alignment( List( 'a, 'b, 'c, 'd ), List( 'e, 'b, '-, 'f ) ),
        NeedlemanWunsch.Alignment( List( 'a, 'b, 'c, 'd ), List( 'e, 'b, 'f, '- ) )
      )
    )
  }
  test( "one vs all different" ) {
    val left = Array( 'a )
    val right = Array( 'e, 'b, 'x )

    assert(
      NeedlemanWunsch.findAlignments( '-, left, right ) === List(
        NeedlemanWunsch.Alignment( List( 'a, '-, '- ), List( 'e, 'b, 'x ) ),
        NeedlemanWunsch.Alignment( List( '-, 'a, '- ), List( 'e, 'b, 'x ) ),
        NeedlemanWunsch.Alignment( List( '-, '-, 'a ), List( 'e, 'b, 'x ) )
      )
    )
  }
  test( "one vs many, first equal" ) {
    val left = Array( 'a )
    val right = Array( 'a, 'b, 'x )

    assert(
      NeedlemanWunsch.findAlignments( '-, left, right ) === List(
        NeedlemanWunsch.Alignment( List( 'a, '-, '- ), List( 'a, 'b, 'x ) )
      )
    )
  }
  test( "one vs many, last equal" ) {
    val left = Array( 'a )
    val right = Array( 'e, 'b, 'a )

    assert(
      NeedlemanWunsch.findAlignments( '-, left, right ) === List(
        NeedlemanWunsch.Alignment( List( '-, '-, 'a ), List( 'e, 'b, 'a ) )
      )
    )
  }
  test( "one vs many, middle equal" ) {
    val left = Array( 'a )
    val right = Array( 'e, 'a, 'x )

    assert(
      NeedlemanWunsch.findAlignments( '-, left, right ) === List(
        NeedlemanWunsch.Alignment( List( '-, 'a, '- ), List( 'e, 'a, 'x ) )
      )
    )
  }
  test( "two vs many, extrems equal" ) {
    val left = Array( 'e, 'x )
    val right = Array( 'e, 'a, 'x )

    assert(
      NeedlemanWunsch.findAlignments( '-, left, right ) === List(
        NeedlemanWunsch.Alignment( List( 'e, '-, 'x ), List( 'e, 'a, 'x ) )
      )
    )
  }
}
