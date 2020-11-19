package generic
package tests

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalactic.TypeCheckedTripleEquals
import cats.instances.SymbolInstances
import graph._
import algorithm._
import tograph._
import cats.data.NonEmptyList
import utils.NodeNames._
import cats.instances.string._

class PrintGraph extends AnyFunSuite with TypeCheckedTripleEquals with SymbolInstances {
  test( "print graph" ) {
    val g = AdjacentGraph
      .single( 'Foo )
      .addEdge( 'Foo, 'a )
      .addEdge( 'a, 'c )
      .addEdge( 'a, 'd )
      .addEdge( 'c, 'x )
      .addEdge( 'c, 'i )
      .addEdge( 'Foo, 'b )
      .addEdge( 'b, 'h )
      .addEdge( 'b, 'g )
      .addEdge( 'g, 'r )
      .addEdge( 'r, 'z )
    val expected =
      """'Foo
| -> 'a
      | -> 'c
            | -> 'i
            | -> 'x
      | -> 'd
| -> 'b
      | -> 'g
            | -> 'r
                  | -> 'z
      | -> 'h"""
    val result = g.print
    assert( result === expected )
  }
  test( "print path graph" ) {
    val gr = AdjacentGraph
      .single( f )
      .connect( NonEmptyList.one( f ), a )
      .connect( NonEmptyList.one( f ), l )
      .connect( NonEmptyList.of( l, f ), g )
      .connect( NonEmptyList.of( g, l, f ), Leaf( 'k, "1" ) )
      .connect( NonEmptyList.of( l, f ), Leaf( 'h, "1" ) )
      .connect( NonEmptyList.of( a, f ), Leaf( 'd, "1" ) )
      .connect( NonEmptyList.of( a, f ), Leaf( 'd, "2" ) )
      .connect( NonEmptyList.of( a, f ), c )
      .connect( NonEmptyList.of( c, a, f ), Leaf( 'e, "1" ) )
      .connect( NonEmptyList.of( c, a, f ), Leaf( 'j, "1" ) )
      .connect( NonEmptyList.one( f ), b )
      .connect( NonEmptyList.of( b, f ), g )
      .connect( NonEmptyList.of( g, b, f ), Leaf( 'k, "1" ) )
      .connect( NonEmptyList.of( b, f ), Leaf( 'h, "1" ) )
    val expected = """'Foo
| -> 'a
      | -> 'c
            | -> 'e -> 1
            | -> 'j -> 1
      | -> 'd -> 1
| -> 'b
      | -> 'g
            | -> 'k -> 1
      | -> 'h -> 1
| -> 'l
      | -> 'g
            | -> 'k -> 1
      | -> 'h -> 1"""

    val result = gr.print
    assert( result === expected )
  }
  test( "print diff graph" ) {
    val diff = AdjacentGraph
      .single( Diff.same( f ) )
      .connect( NonEmptyList.one( Diff.same( f ) ), Diff.same( a ) )
      .connect( NonEmptyList.one( Diff.same( f ) ), Diff.added( l ) )
      .connect( NonEmptyList.of( Diff.added( l ), Diff.same( f ) ), Diff.added( g ) )
      .connect( NonEmptyList.of( Diff.added( g ), Diff.added( l ), Diff.same( f ) ), Diff.added( Leaf( 'k, "1" ) ) )
      .connect( NonEmptyList.of( Diff.added( l ), Diff.same( f ) ), Diff.added( Leaf( 'h, "1" ) ) )
      .connect( NonEmptyList.of( Diff.same( a ), Diff.same( f ) ), Diff.removed( Leaf( 'd, "1" ) ) )
      .connect( NonEmptyList.of( Diff.same( a ), Diff.same( f ) ), Diff.added( Leaf( 'd, "2" ) ) )
      .connect( NonEmptyList.of( Diff.same( a ), Diff.same( f ) ), Diff.same( c ) )
      .connect( NonEmptyList.of( Diff.same( c ), Diff.same( a ), Diff.same( f ) ), Diff.removed( Leaf( 'e, "1" ) ) )
      .connect( NonEmptyList.of( Diff.same( c ), Diff.same( a ), Diff.same( f ) ), Diff.added( Leaf( 'j, "1" ) ) )
      .connect( NonEmptyList.one( Diff.same( f ) ), Diff.removed( b ) )
      .connect( NonEmptyList.of( Diff.removed( b ), Diff.same( f ) ), Diff.removed( g ) )
      .connect(
        NonEmptyList.of( Diff.removed( g ), Diff.removed( b ), Diff.same( f ) ),
        Diff.removed( Leaf( 'k, "1" ) )
      )
      .connect( NonEmptyList.of( Diff.removed( b ), Diff.same( f ) ), Diff.removed( Leaf( 'h, "1" ) ) )
    val expected = s"""'Foo
| -> 'a
      | -> 'c
            | -> ${Diff.red( "'e -> 1" )}
            | -> ${Diff.green( "'j -> 1" )}
      | -> ${Diff.red( "'d -> 1" )}
| -> ${Diff.red( "'b" )}
      | -> ${Diff.red( "'g" )}
            | -> ${Diff.red( "'k -> 1" )}
      | -> ${Diff.red( "'h -> 1" )}
| -> ${Diff.green( "'l" )}
      | -> ${Diff.green( "'g" )}
            | -> ${Diff.green( "'k -> 1" )}
      | -> ${Diff.green( "'h -> 1" )}"""
    val result = diff.print
    assert( result === expected )
  }
}
