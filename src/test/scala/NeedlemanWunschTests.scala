package algorithm
package tests

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalactic.TypeCheckedTripleEquals
import cats.instances.SymbolInstances

class NeedlemanWunschTests extends AnyFunSuite with TypeCheckedTripleEquals with Matchers with SymbolInstances {
  import NeedlemanWunsch._
  test( "first case ever" ) {
    val left = Array( 'a, 'b, 'c, 'd )
    val right = Array( 'e, 'b, 'f )

    assert(
      NeedlemanWunsch( '-, left, right ) === Set(
        NeedlemanWunsch.Alignment( List( 'a, 'b, 'c, 'd ), List( 'e, 'b, '-, 'f ) ),
        NeedlemanWunsch.Alignment( List( 'a, 'b, 'c, 'd ), List( 'e, 'b, 'f, '- ) )
      )
    )
  }
}
