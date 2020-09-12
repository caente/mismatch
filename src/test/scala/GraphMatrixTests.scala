package algorithm
package tests

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalactic.TypeCheckedTripleEquals
import cats.instances.SymbolInstances

import matrices._

class GraphMatrixTests extends AnyFunSuite with TypeCheckedTripleEquals with Matchers with SymbolInstances {
  test( "one edge" ) {
    val g = GraphMatrix.single( 'a ).addEdge( 'a, 'b )
    assert(
      g.labels === Array( 'a, 'b )
    )
    assert(
      g.leaves === Array( List( 'a, 'b ) )
    )
    assert(
      g.indexedColLabels === Map(
        'a -> 0,
        'b -> 1
      )
    )
  }
  test( "two sequential edges" ) {
    val g = GraphMatrix.single( 'a ).addEdge( 'a, 'b ).addEdge( 'b, 'c )
    assert(
      g.labels === Array( 'a, 'b, 'c )
    )
    assert(
      g.leaves === Array( List( 'a, 'b, 'c ) )
    )
    assert(
      g.indexedColLabels === Map(
        'a -> 0,
        'b -> 1,
        'c -> 2
      )
    )
  }
  test( "two forked edges" ) {
    val g = GraphMatrix.single( 'a ).addEdge( 'a, 'b ).addEdge( 'a, 'c )
    assert(
      g.labels === Array( 'a, 'b, 'c )
    )
    assert(
      g.leaves === Array( List( 'a, 'b ), List( 'a, 'c ) )
    )
    assert(
      g.indexedColLabels === Map(
        'a -> 0,
        'b -> 1,
        'c -> 2
      )
    )
  }
  test( "two forked edges, and a sequential" ) {
    val g = GraphMatrix.single( 'a ).addEdge( 'a, 'b ).addEdge( 'a, 'c ).addEdge( 'b, 'd )
    assert(
      g.labels === Array( 'a, 'b, 'c, 'd )
    )
    assert(
      g.leaves === Array( List( 'a, 'b, 'd ), List( 'a, 'c ) )
    )
    assert(
      g.indexedColLabels === Map(
        'a -> 0,
        'b -> 1,
        'c -> 2,
        'd -> 3
      )
    )
  }
}
