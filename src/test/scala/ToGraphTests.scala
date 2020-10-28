package generic
package tests

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalactic.TypeCheckedTripleEquals
import cats.instances.SymbolInstances
import graph._
import algorithm._
import tograph._
import cats.implicits._
import cats.data.NonEmptyList

class ToGraphTests extends AnyFunSuite with TypeCheckedTripleEquals with Matchers with SymbolInstances {
  test( "manual vs generic" ) {
    val manual =
      AdjacentGraph
        .single( 'Foo )
        .addEdge( 'Foo, 'b )
        .addEdge( 'Foo, 'a )
        .addEdge( 'a, 'c )
        .addEdge( 'a, 'd )
        .addEdge( 'b, 'h )
        .addEdge( 'b, 'g )
        .addEdge( 'g, 'k )
        .addEdge( 'c, 'e )
    case class C(e: Int )
    case class A(c: C, d: Int )
    case class G(k: Int )
    case class B(h: Int, g: G )
    case class Foo(a: A, b: B )
    val foo = Foo(
      a = A(
        c = C(
          e = 1
        ),
        d = 1
      ),
      B(
        h = 1,
        g = G(
          k = 1
        )
      )
    )
    val generated = ToGraph.create[Foo, AdjacentGraph]( 'Foo, foo )
    assert( generated === manual )
  }
  test( "compare two different classes" ) {
    case class C_Left(e: Int )
    case class A_Left(c: C_Left, d: Int )
    case class G_Left(k: Int )
    case class B_Left(h: Int, g: G_Left )
    case class Foo_Left(a: A_Left, b: B_Left )
    val foo_Left = Foo_Left(
      a = A_Left(
        c = C_Left(
          e = 1
        ),
        d = 1
      ),
      B_Left(
        h = 1,
        g = G_Left(
          k = 1
        )
      )
    )
    case class C_Right(j: Int )
    case class A_Right(c: C_Right, d: Int )
    case class G_Right(k: Int )
    case class L_Right(h: Int, g: G_Right )
    case class Foo_Right(a: A_Right, l: L_Right )
    val foo_Right = Foo_Right(
      a = A_Right(
        c = C_Right(
          j = 1
        ),
        d = 1
      ),
      l = L_Right(
        h = 1,
        g = G_Right(
          k = 1
        )
      )
    )
    val generated_Left = ToGraph.create[Foo_Left, AdjacentGraph]( 'Foo, foo_Left )
    val generated_Right = ToGraph.create[Foo_Right, AdjacentGraph]( 'Foo, foo_Right )
    val compared = Mismatches.compare( generated_Left, generated_Right, '- )
    val expected = AdjacentGraph
      .single( Diff.same( 'Foo ) )
      .connect( NonEmptyList.one( Diff.same( 'Foo ) ), Diff.same( 'a ) )
      .connect( NonEmptyList.one( Diff.same( 'Foo ) ), Diff.added( 'l ) )
      .connect( NonEmptyList.of( Diff.added( 'l ), Diff.same( 'Foo ) ), Diff.added( 'g ) )
      .connect( NonEmptyList.of( Diff.added( 'g ), Diff.added( 'l ), Diff.same( 'Foo ) ), Diff.added( 'k ) )
      .connect( NonEmptyList.of( Diff.added( 'l ), Diff.same( 'Foo ) ), Diff.added( 'h ) )
      .connect( NonEmptyList.of( Diff.same( 'a ), Diff.same( 'Foo ) ), Diff.same( 'd ) )
      .connect( NonEmptyList.of( Diff.same( 'a ), Diff.same( 'Foo ) ), Diff.same( 'c ) )
      .connect( NonEmptyList.of( Diff.same( 'c ), Diff.same( 'a ), Diff.same( 'Foo ) ), Diff.removed( 'e ) )
      .connect( NonEmptyList.of( Diff.same( 'c ), Diff.same( 'a ), Diff.same( 'Foo ) ), Diff.added( 'j ) )
      .connect( NonEmptyList.one( Diff.same( 'Foo ) ), Diff.removed( 'b ) )
      .connect( NonEmptyList.of( Diff.removed( 'b ), Diff.same( 'Foo ) ), Diff.removed( 'g ) )
      .connect( NonEmptyList.of( Diff.removed( 'g ), Diff.removed( 'b ), Diff.same( 'Foo ) ), Diff.removed( 'k ) )
      .connect( NonEmptyList.of( Diff.removed( 'b ), Diff.same( 'Foo ) ), Diff.removed( 'h ) )
    assert( GraphOps.nodes( compared ) == GraphOps.nodes( expected ) )
  }
  test( "generate option/None" ) {
    case class A(i: Int )
    case class B(i: Int )
    case class Foo(a: Option[A], b: Option[B] )
    val foo = Foo( Some( A( 1 ) ), None )
    val generated = ToGraph.create[Foo, AdjacentGraph]( 'Foo, foo )
    val expected = AdjacentGraph
      .single( 'Foo )
      .addEdge( 'Foo, 'a )
      .addEdge( 'a, 'i )
      .addEdge( 'Foo, 'b )
    assert( generated === expected )
  }
  test( "generate option/Some" ) {
    case class A(i: Int )
    case class B(i: Int )
    case class Foo(a: Option[A], b: Option[B] )
    val foo = Foo( Some( A( 1 ) ), Some( B( 1 ) ) )
    val generated = ToGraph.create[Foo, AdjacentGraph]( 'Foo, foo )
    val expected = AdjacentGraph
      .single( 'Foo )
      .addEdge( 'Foo, 'a )
      .addEdge( 'a, 'i )
      .addEdge( 'Foo, 'b )
      .addEdge( 'b, 'i )
    assert( generated === expected )
  }
  test( "generate sealed trait" ) {
    sealed trait A
    case class A1(i: Int ) extends A
    case class A2(i: Int ) extends A
    case class Foo(a: A, b: A )
    val foo = Foo( a = A2( 1 ), b = A1( 1 ) )
    val generated = ToGraph.create[Foo, AdjacentGraph]( 'Foo, foo )
    val expected = AdjacentGraph
      .single( 'Foo )
      .addEdge( 'Foo, 'a )
      .addEdge( 'Foo, 'b )
      .addEdge( 'a, 'i )
      .addEdge( 'b, 'i )
    assert( generated === expected )
  }
  test( "same name in two branches" ) {
    case class A(b: B )
    case class B(i: Int )
    case class Foo(a: A, b: A )
    val foo = Foo(
      a = A(
        b = B( i = 1 )
      ),
      b = A(
        b = B( i = 1 )
      )
    )
    val generated = ToGraph.create[Foo, AdjacentGraph]( 'Foo, foo )
    val expected = AdjacentGraph
      .single( 'Foo )
      .connect( NonEmptyList.one( 'Foo ), 'a )
      .connect( NonEmptyList.one( 'Foo ), 'b )
      .connect( NonEmptyList.of( 'a, 'Foo ), 'b )
      .connect( NonEmptyList.of( 'b, 'a, 'Foo ), 'i )
      .connect( NonEmptyList.of( 'b, 'Foo ), 'b )
      .connect( NonEmptyList.of( 'b, 'b, 'Foo ), 'i )
    assert(
      generated === expected,
      s"""
      result: ${generated.data.map {
        case ( path, adjs ) => path.head -> adjs
      }}
      expected: ${expected.data.map {
        case ( path, adjs ) => path.head -> adjs
      }}
    """
    )
  }
}
