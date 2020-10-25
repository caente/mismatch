package algorithm
package tests

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalactic.TypeCheckedTripleEquals
import cats.instances.SymbolInstances
import graph._
import cats.implicits._

class MismatchesTests extends AnyFunSuite with TypeCheckedTripleEquals with Matchers with SymbolInstances {
  test( "the first test" ) {
    val A = AdjacentGraph.single( 'Foo ).addEdge( 'Foo, 'a )
    val B = AdjacentGraph.single( 'Foo ).addEdge( 'Foo, 'a )
    val result = Mismatches.compare( A, B )
    assert( result === AdjacentGraph.single( Diff.same( 'Foo ) ).addEdge( Diff.same( 'Foo ), Diff.same( 'a ) ) )
  }
  test( "the first mismatch" ) {
    val A = AdjacentGraph.single( 'Foo ).addEdge( 'Foo, 'a )
    val B = AdjacentGraph.single( 'Foo ).addEdge( 'Foo, 'b )
    val result = Mismatches.compare( A, B )
    val expected = AdjacentGraph
      .single( Diff.same( 'Foo ) )
      .addEdge( Diff.same( 'Foo ), Diff.removed( 'a ) )
      .addEdge( Diff.same( 'Foo ), Diff.added( 'b ) )
    assert( result === expected )
  }
  test( "one to many" ) {
    val A = AdjacentGraph.single( 'Foo ).addEdge( 'Foo, 'a ).addEdge( 'a, 'b )
    val B = AdjacentGraph.single( 'Foo )
    val result = Mismatches.compare( A, B )
    val expected = AdjacentGraph
      .single( Diff.same( 'Foo ) )
      .addEdge( Diff.same( 'Foo ), Diff.removed( 'a ) )
      .addEdge( Diff.removed( 'a ), Diff.removed( 'b ) )
    assert( result === expected )
  }
  test( "a node with the same name is different if it's in a different branch" ) {
    val A = AdjacentGraph.single( 'Foo ).addEdge( 'Foo, 'a ).addEdge( 'a, 'c ).addEdge( 'Foo, 'b )
    val B = AdjacentGraph.single( 'Foo ).addEdge( 'Foo, 'a ).addEdge( 'Foo, 'b ).addEdge( 'b, 'c )
    val result = Mismatches.compare( A, B )
    assert(
      result === AdjacentGraph
        .single( Diff.same( 'Foo ) )
        .addEdge( Diff.same( 'Foo ), Diff.same( 'a ) )
        .addEdge( Diff.same( 'a ), Diff.removed( 'c ) )
        .addEdge( Diff.same( 'Foo ), Diff.same( 'b ) )
        .addEdge( Diff.same( 'b ), Diff.added( 'c ) )
    )
  }
  test( "missing path" ) {
    val A = AdjacentGraph.single( 'Foo ).addEdge( 'Foo, 'a ).addEdge( 'a, 'i ).addEdge( 'Foo, 'b )
    val B = AdjacentGraph.single( 'Foo ).addEdge( 'Foo, 'a ).addEdge( 'a, 'i ).addEdge( 'Foo, 'b ).addEdge( 'b, 'i )
    val result = Mismatches.compare( A, B )
    val expected = AdjacentGraph
      .single( Diff.same( 'Foo ) )
      .addEdge( Diff.same( 'Foo ), Diff.same( 'a ) )
      .addEdge( Diff.same( 'Foo ), Diff.same( 'b ) )
      .addEdge( Diff.same( 'a ), Diff.removed( 'i ) )
      .addEdge( Diff.same( 'b ), Diff.added( 'i ) )
    assert( result === expected )
  }
}
