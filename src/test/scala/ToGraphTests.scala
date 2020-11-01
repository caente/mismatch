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

class ToGraphTests extends AnyFunSuite with TypeCheckedTripleEquals with SymbolInstances {
  val f: Labelled.AsString = Node( 'Foo )
  val a: Labelled.AsString = Node( 'a )
  val b: Labelled.AsString = Node( 'b )
  val c: Labelled.AsString = Node( 'c )
  val d: Labelled.AsString = Node( 'd )
  val h: Labelled.AsString = Node( 'h )
  val g: Labelled.AsString = Node( 'g )
  val k: Labelled.AsString = Node( 'k )
  val e: Labelled.AsString = Node( 'e )
  val i: Labelled.AsString = Node( 'i )
  val l: Labelled.AsString = Node( 'l )
  val j: Labelled.AsString = Node( 'j )

  test( "manual vs generic" ) {
    val manual =
      AdjacentGraph
        .single( f )
        .connect( NonEmptyList.one( f ), b )
        .connect( NonEmptyList.one( f ), a )
        .connect( NonEmptyList.of( a, f ), c )
        .connect( NonEmptyList.of( a, f ), c )
        .connect( NonEmptyList.of( a, f ), Leaf( 'd, "1" ) )
        .connect( NonEmptyList.of( b, f ), Leaf( 'h, "1" ) )
        .connect( NonEmptyList.of( b, f ), g )
        .connect( NonEmptyList.of( g, b, f ), Leaf( 'k, "1" ) )
        .connect( NonEmptyList.of( c, a, f ), Leaf( 'e, "1" ) )

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
        d = 2
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
    val compared = Mismatches.compare( generated_Left, generated_Right, Node[String]( '- ) )
    val expected = AdjacentGraph
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
    assert( GraphOps.nodes( compared ).toSet == GraphOps.nodes( expected ).toSet )
  }
  test( "generate option/None" ) {
    case class A(i: Int )
    case class B(i: Int )
    case class Foo(a: Option[A], b: Option[B] )
    val foo = Foo( Some( A( 1 ) ), None )
    val generated = ToGraph.create[Foo, AdjacentGraph]( 'Foo, foo )
    val expected = AdjacentGraph
      .single( f )
      .connect( NonEmptyList.one( f ), a )
      .connect( NonEmptyList.one( f ), b )
      .connect( NonEmptyList.of( a, f ), Leaf( 'i, "1" ) )
    assert( generated === expected )
  }
  test( "generate option/Some" ) {
    case class A(i: Int )
    case class B(i: Int )
    case class Foo(a: Option[A], b: Option[B] )
    val foo = Foo( Some( A( 1 ) ), Some( B( 1 ) ) )
    val generated = ToGraph.create[Foo, AdjacentGraph]( 'Foo, foo )
    val expected = AdjacentGraph
      .single( f )
      .connect( NonEmptyList.one( f ), a )
      .connect( NonEmptyList.one( f ), b )
      .connect( NonEmptyList.of( a, f ), Leaf( 'i, "1" ) )
      .connect( NonEmptyList.of( b, f ), Leaf( 'i, "1" ) )
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
      .single( f )
      .connect( NonEmptyList.one( f ), a )
      .connect( NonEmptyList.one( f ), b )
      .connect( NonEmptyList.of( a, f ), Leaf( 'i, "1" ) )
      .connect( NonEmptyList.of( b, f ), Leaf( 'i, "1" ) )
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
        b = B( i = 2 )
      )
    )
    val generated = ToGraph.create[Foo, AdjacentGraph]( 'Foo, foo )
    val expected = AdjacentGraph
      .single( f )
      .connect( NonEmptyList.one( f ), a )
      .connect( NonEmptyList.one( f ), b )
      .connect( NonEmptyList.of( a, f ), b )
      .connect( NonEmptyList.of( b, a, f ), Leaf( 'i, "1" ) )
      .connect( NonEmptyList.of( b, f ), b )
      .connect( NonEmptyList.of( b, b, f ), Leaf( 'i, "2" ) )
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
