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
  test( "print path graph with leafs at the end" ) {
    val gr = AdjacentGraph
      .single( f )
      .connect( NonEmptyList.one( f ), a )
      .connect( NonEmptyList.of( a, f ), d )
      .connect( NonEmptyList.of( d, a, f ), k )
      .connect( NonEmptyList.of( a, f ), c )
      .connect( NonEmptyList.of( c, a, f ), j )
      .connect( NonEmptyList.of( j, c, a, f ), x )
    val expected = """'Foo
| -> 'a
      | -> 'c
            | -> 'j
                  | -> 'x
      | -> 'd
            | -> 'k"""

    val result = gr.print
    assert( result === expected )
  }
  test( "print diff graph" ) {
    val diff = AdjacentGraph
      .single( Diff.same( f ) )
      .connect( NonEmptyList.one( Diff.same( f ) ), Diff.same( a ) )
      .connect( NonEmptyList.one( Diff.same( f ) ), Diff.added( l ) )
      .connect( NonEmptyList.of( Diff.added( l ), Diff.same( f ) ), Diff.added( g ) )
      .connect( NonEmptyList.of( Diff.added( g ), Diff.added( l ), Diff.same( f ) ), Diff.added( k ) )
      .connect(
        NonEmptyList.of( Diff.added( k ), Diff.added( g ), Diff.added( l ), Diff.same( f ) ),
        Diff.added( Leaf( "1" ) )
      )
      .connect( NonEmptyList.of( Diff.added( l ), Diff.same( f ) ), Diff.added( h ) )
      .connect( NonEmptyList.of( Diff.added( h ), Diff.added( l ), Diff.same( f ) ), Diff.added( Leaf( "1" ) ) )
      .connect( NonEmptyList.of( Diff.same( a ), Diff.same( f ) ), Diff.same( d ) )
      .connect( NonEmptyList.of( Diff.same( d ), Diff.same( a ), Diff.same( f ) ), Diff.removed( Leaf( "1" ) ) )
      .connect( NonEmptyList.of( Diff.same( d ), Diff.same( a ), Diff.same( f ) ), Diff.added( Leaf( "2" ) ) )
      .connect( NonEmptyList.of( Diff.same( a ), Diff.same( f ) ), Diff.same( c ) )
      .connect( NonEmptyList.of( Diff.same( c ), Diff.same( a ), Diff.same( f ) ), Diff.removed( e ) )
      .connect(
        NonEmptyList.of( Diff.removed( e ), Diff.same( c ), Diff.same( a ), Diff.same( f ) ),
        Diff.removed( Leaf( "1" ) )
      )
      .connect( NonEmptyList.of( Diff.same( c ), Diff.same( a ), Diff.same( f ) ), Diff.added( j ) )
      .connect(
        NonEmptyList.of( Diff.added( j ), Diff.same( c ), Diff.same( a ), Diff.same( f ) ),
        Diff.added( Leaf( "1" ) )
      )
      .connect( NonEmptyList.one( Diff.same( f ) ), Diff.removed( b ) )
      .connect( NonEmptyList.of( Diff.removed( b ), Diff.same( f ) ), Diff.removed( g ) )
      .connect(
        NonEmptyList.of( Diff.removed( g ), Diff.removed( b ), Diff.same( f ) ),
        Diff.removed( k )
      )
      .connect(
        NonEmptyList.of( Diff.removed( k ), Diff.removed( g ), Diff.removed( b ), Diff.same( f ) ),
        Diff.removed( Leaf( "1" ) )
      )
      .connect( NonEmptyList.of( Diff.removed( b ), Diff.same( f ) ), Diff.removed( h ) )
      .connect( NonEmptyList.of( Diff.removed( h ), Diff.removed( b ), Diff.same( f ) ), Diff.removed( Leaf( "1" ) ) )
    val expected = s"""'Foo
| -> 'a
      | -> 'c
            | -> ${Diff.red( "'e" )}
                  | -> ${Diff.red( "1" )}
            | -> ${Diff.green( "'j" )}
                  | -> ${Diff.green( "1" )}
      | -> 'd
            | -> ${Diff.red( "1" )}
            | -> ${Diff.green( "2" )}
| -> ${Diff.red( "'b" )}
      | -> ${Diff.red( "'g" )}
            | -> ${Diff.red( "'k" )}
                  | -> ${Diff.red( "1" )}
      | -> ${Diff.red( "'h" )}
            | -> ${Diff.red( "1" )}
| -> ${Diff.green( "'l" )}
      | -> ${Diff.green( "'g" )}
            | -> ${Diff.green( "'k" )}
                  | -> ${Diff.green( "1" )}
      | -> ${Diff.green( "'h" )}
            | -> ${Diff.green( "1" )}"""
    val result = diff.print
    assert( result === expected )
  }
}
