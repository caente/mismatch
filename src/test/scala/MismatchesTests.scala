package algorithm
package tests

import cats.data.NonEmptyList
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalactic.TypeCheckedTripleEquals
import cats.instances.SymbolInstances
import graph._
import cats.implicits._
import cats.Eq
import algorithm.Mismatches.onlyMismatches
import utils.NodeNames._
import tograph._

class MismatchesTests extends AnyFunSuite with TypeCheckedTripleEquals with SymbolInstances {
  test( "the first test" ) {
    val A = AdjacentGraph.single( 'Foo ).addEdge( 'Foo, 'a )
    val B = AdjacentGraph.single( 'Foo ).addEdge( 'Foo, 'a )
    val result = Mismatches.compare( A, B, '- )
    assert( result === AdjacentGraph.single( Diff.same( 'Foo ) ).addEdge( Diff.same( 'Foo ), Diff.same( 'a ) ) )
  }
  test( "the first mismatch" ) {
    val A = AdjacentGraph.single( 'Foo ).addEdge( 'Foo, 'a )
    val B = AdjacentGraph.single( 'Foo ).addEdge( 'Foo, 'b )
    val result = Mismatches.compare( A, B, '- )
    val expected = AdjacentGraph
      .single( Diff.same( 'Foo ) )
      .addEdge( Diff.same( 'Foo ), Diff.removed( 'a ) )
      .addEdge( Diff.same( 'Foo ), Diff.added( 'b ) )
    assert( result === expected )
  }
  test( "one to many" ) {
    val A = AdjacentGraph.single( 'Foo ).addEdge( 'Foo, 'a ).addEdge( 'a, 'b )
    val B = AdjacentGraph.single( 'Foo )
    val result = Mismatches.compare( A, B, '- )
    val expected = AdjacentGraph
      .single( Diff.same( 'Foo ) )
      .addEdge( Diff.same( 'Foo ), Diff.removed( 'a ) )
      .addEdge( Diff.removed( 'a ), Diff.removed( 'b ) )
    assert( result === expected )
  }
  test( "a node with the same name is different if it's in a different branch" ) {
    val A = AdjacentGraph.single( 'Foo ).addEdge( 'Foo, 'a ).addEdge( 'a, 'c ).addEdge( 'Foo, 'b )
    val B = AdjacentGraph.single( 'Foo ).addEdge( 'Foo, 'a ).addEdge( 'Foo, 'b ).addEdge( 'b, 'c )
    val result = Mismatches.compare( A, B, '- )
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
    val result = Mismatches.compare( A, B, '- )
    val expected = AdjacentGraph
      .single( Diff.same( 'Foo ) )
      .addEdge( Diff.same( 'Foo ), Diff.same( 'a ) )
      .addEdge( Diff.same( 'Foo ), Diff.same( 'b ) )
      .addEdge( Diff.same( 'a ), Diff.same( 'i ) )
      .addEdge( Diff.same( 'b ), Diff.added( 'i ) )
    assert(
      result === expected,
      s"""
      result: ${result.data.map {
        case ( path, adjs ) => path.head -> adjs
      }}
      expected: ${expected.data.map {
        case ( path, adjs ) => path.head -> adjs
      }}
    """
    )
  }
  test( "show only mismatches" ) {
    val diff = AdjacentGraph
      .single( Diff.same( 'Foo ) )
      .addEdge( Diff.same( 'Foo ), Diff.same( 'a ) )
      .addEdge( Diff.same( 'Foo ), Diff.same( 'b ) )
      .addEdge( Diff.same( 'a ), Diff.removed( 'i ) )
      .addEdge( Diff.same( 'a ), Diff.added( 'c ) )
      .addEdge( Diff.same( 'a ), Diff.same( 'd ) )
      .addEdge( Diff.same( 'a ), Diff.added( 'c ) )
      .addEdge( Diff.same( 'b ), Diff.removed( 'i ) )
      .addEdge( Diff.same( 'b ), Diff.same( 'x ) )
    val diffsOnly = onlyMismatches( diff )
    val expected = AdjacentGraph
      .single( Diff.same( 'Foo ) )
      .addEdge( Diff.same( 'Foo ), Diff.same( 'a ) )
      .addEdge( Diff.same( 'Foo ), Diff.same( 'b ) )
      .addEdge( Diff.same( 'a ), Diff.removed( 'i ) )
      .addEdge( Diff.same( 'a ), Diff.added( 'c ) )
      .addEdge( Diff.same( 'b ), Diff.removed( 'i ) )

    assert( diffsOnly === expected )
  }
  test( "comparing graphs with index" ) {
    val A = AdjacentGraph
      .single( f )
      .connect( NonEmptyList.one( f ), ls )
      .connect( NonEmptyList.of( ls, f ), index( 0 ) )
      .connect( NonEmptyList.of( index( 0 ), ls, f ), b )
      .connect( NonEmptyList.of( b, index( 0 ), ls, f ), i )
      .connect( NonEmptyList.of( i, b, index( 0 ), ls, f ), Leaf( "1" ) )
      .connect( NonEmptyList.of( ls, f ), index( 1 ) )
      .connect( NonEmptyList.of( index( 1 ), ls, f ), b )
      .connect( NonEmptyList.of( b, index( 1 ), ls, f ), s )
      .connect( NonEmptyList.of( s, b, index( 1 ), ls, f ), Leaf( "a" ) )
    val B = AdjacentGraph
      .single( f )
      .connect( NonEmptyList.one( f ), ls )
      .connect( NonEmptyList.of( ls, f ), index( 0 ) )
      .connect( NonEmptyList.of( index( 0 ), ls, f ), b )
      .connect( NonEmptyList.of( b, index( 0 ), ls, f ), i )
      .connect( NonEmptyList.of( i, b, index( 0 ), ls, f ), Leaf( "1" ) )
      .connect( NonEmptyList.of( ls, f ), index( 1 ) )
      .connect( NonEmptyList.of( index( 1 ), ls, f ), b )
      .connect( NonEmptyList.of( b, index( 1 ), ls, f ), s )
      .connect( NonEmptyList.of( s, b, index( 1 ), ls, f ), Leaf( "b" ) )
    val diff = Mismatches.compare[Labelled.AsString, AdjacentGraph]( A, B, pl )
    val expected =
      AdjacentGraph
        .single( Diff.same( f ) )
        .connect( NonEmptyList.one( Diff.same( f ) ), Diff.same( ls ) )
        .connect( NonEmptyList.of( Diff.same( ls ), Diff.same( f ) ), Diff.same( index( 0 ) ) )
        .connect( NonEmptyList.of( Diff.same( index( 0 ) ), Diff.same( ls ), Diff.same( f ) ), Diff.same( b ) )
        .connect(
          NonEmptyList.of( Diff.same( b ), Diff.same( index( 0 ) ), Diff.same( ls ), Diff.same( f ) ),
          Diff.same( i )
        )
        .connect(
          NonEmptyList.of( Diff.same( i ), Diff.same( b ), Diff.same( index( 0 ) ), Diff.same( ls ), Diff.same( f ) ),
          Diff.same( Leaf( "1" ) )
        )
        .connect( NonEmptyList.of( Diff.same( ls ), Diff.same( f ) ), Diff.same( index( 1 ) ) )
        .connect( NonEmptyList.of( Diff.same( index( 1 ) ), Diff.same( ls ), Diff.same( f ) ), Diff.same( b ) )
        .connect(
          NonEmptyList.of( Diff.same( b ), Diff.same( index( 1 ) ), Diff.same( ls ), Diff.same( f ) ),
          Diff.same( s )
        )
        .connect(
          NonEmptyList.of( Diff.same( s ), Diff.same( b ), Diff.same( index( 1 ) ), Diff.same( ls ), Diff.same( f ) ),
          Diff.removed( Leaf( "a" ) )
        )
        .connect(
          NonEmptyList.of( Diff.same( s ), Diff.same( b ), Diff.same( index( 1 ) ), Diff.same( ls ), Diff.same( f ) ),
          Diff.added( Leaf( "b" ) )
        )
    assert( diff === expected )
  }
}
