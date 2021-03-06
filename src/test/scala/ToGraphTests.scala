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
import utils.NodeNames._

class ToGraphTests extends AnyFunSuite with TypeCheckedTripleEquals with SymbolInstances {
  test( "manual vs generic" ) {
    val manual =
      AdjacentGraph
        .single( f )
        .connect( NonEmptyList.one( f ), b )
        .connect( NonEmptyList.one( f ), a )
        .connect( NonEmptyList.of( a, f ), c )
        .connect( NonEmptyList.of( a, f ), c )
        .connect( NonEmptyList.of( a, f ), d )
        .connect( NonEmptyList.of( d, a, f ), Leaf( "1" ) )
        .connect( NonEmptyList.of( b, f ), h )
        .connect( NonEmptyList.of( h, b, f ), Leaf( "1" ) )
        .connect( NonEmptyList.of( b, f ), g )
        .connect( NonEmptyList.of( g, b, f ), k )
        .connect( NonEmptyList.of( k, g, b, f ), Leaf( "1" ) )
        .connect( NonEmptyList.of( c, a, f ), e )
        .connect( NonEmptyList.of( e, c, a, f ), Leaf( "1" ) )

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
    val generated = AdjacentGraph.create[Foo]( "Foo", foo )
    assert( generated === manual, generated.print )
  }
  test( "with case objects" ) {
    case object A
    case class Foo(i: Int, a: A.type = A )
    val foo = Foo( 1 )
    val generated = AdjacentGraph.create[Foo]( "Foo", foo )
    val expected = AdjacentGraph
      .single( f )
      .addEdge( f, a )
      .addEdge( f, i )
      .addEdge( i, Leaf( "1" ) )
    assert( generated === expected )
  }
  test( "compare two different classes" ) {
    case class Z(i: Int )
    case class J(z: Z )
    case class C_Left(e: Int )
    case class A_Left(c: C_Left, d: Int )
    case class G_Left(k: Int )
    case class B_Left(h: Int, g: G_Left )
    case class Foo_Left(a: A_Left, b: B_Left, j: J )
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
      ),
      j = J( Z( 1 ) )
    )
    case class C_Right(j: Int )
    case class A_Right(c: C_Right, d: Int )
    case class G_Right(k: Int )
    case class L_Right(h: Int, g: G_Right )
    case class Foo_Right(a: A_Right, l: L_Right, j: J )
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
      ),
      j = J( Z( 1 ) )
    )
    val generated_Left = AdjacentGraph.create[Foo_Left]( "Foo", foo_Left )
    val generated_Right = AdjacentGraph.create[Foo_Right]( "Foo", foo_Right )
    val compared = Mismatches.compare( generated_Left, generated_Right, pl )
    val expected = AdjacentGraph
      .single( Diff.same( f ) )
      .connect( NonEmptyList.one( Diff.same( f ) ), Diff.same( j ) )
      .connect( NonEmptyList.one( Diff.same( f ) ), Diff.same( a ) )
      .connect( NonEmptyList.of( Diff.same( j ), Diff.same( f ) ), Diff.same( z ) )
      .connect( NonEmptyList.of( Diff.same( z ), Diff.same( j ), Diff.same( f ) ), Diff.same( i ) )
      .connect(
        NonEmptyList.of( Diff.same( i ), Diff.same( z ), Diff.same( j ), Diff.same( f ) ),
        Diff.same( Leaf( "1" ) )
      )
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
    assert( GraphOps.nodes( compared ).toSet == GraphOps.nodes( expected ).toSet )
  }
  test( "generate option/None" ) {
    case class A(i: Int )
    case class B(i: Int )
    case class Foo(a: Option[A], b: Option[B] )
    val foo = Foo( Some( A( 1 ) ), None )
    val generated = AdjacentGraph.create[Foo]( "Foo", foo )
    val expected = AdjacentGraph
      .single( f )
      .connect( NonEmptyList.one( f ), a )
      .connect( NonEmptyList.one( f ), b )
      .connect( NonEmptyList.of( a, f ), i )
      .connect( NonEmptyList.of( i, a, f ), Leaf( "1" ) )
    assert( generated === expected )
  }
  test( "generate option/Some" ) {
    case class A(i: Int )
    case class B(i: Int )
    case class Foo(a: Option[A], b: Option[B] )
    val foo = Foo( Some( A( 1 ) ), Some( B( 1 ) ) )
    val generated = AdjacentGraph.create[Foo]( "Foo", foo )
    val expected = AdjacentGraph
      .single( f )
      .connect( NonEmptyList.one( f ), a )
      .connect( NonEmptyList.one( f ), b )
      .connect( NonEmptyList.of( a, f ), i )
      .connect( NonEmptyList.of( i, a, f ), Leaf( "1" ) )
      .connect( NonEmptyList.of( b, f ), i )
      .connect( NonEmptyList.of( i, b, f ), Leaf( "1" ) )
    assert( generated === expected )
  }
  test( "generate sealed trait" ) {
    sealed trait A
    case class A1(i: Int ) extends A
    case class A2(i: Int ) extends A
    case class Foo(a: A, b: A )
    val foo = Foo( a = A2( 1 ), b = A1( 1 ) )
    val generated = AdjacentGraph.create[Foo]( "Foo", foo )
    val expected = AdjacentGraph
      .single( f )
      .connect( NonEmptyList.one( f ), a )
      .connect( NonEmptyList.one( f ), b )
      .connect( NonEmptyList.of( a, f ), i )
      .connect( NonEmptyList.of( i, a, f ), Leaf( "1" ) )
      .connect( NonEmptyList.of( b, f ), i )
      .connect( NonEmptyList.of( i, b, f ), Leaf( "1" ) )
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
    val generated = AdjacentGraph.create[Foo]( "Foo", foo )
    val expected = AdjacentGraph
      .single( f )
      .connect( NonEmptyList.one( f ), a )
      .connect( NonEmptyList.one( f ), b )
      .connect( NonEmptyList.of( a, f ), b )
      .connect( NonEmptyList.of( b, a, f ), i )
      .connect( NonEmptyList.of( i, b, a, f ), Leaf( "1" ) )
      .connect( NonEmptyList.of( b, f ), b )
      .connect( NonEmptyList.of( b, b, f ), i )
      .connect( NonEmptyList.of( i, b, b, f ), Leaf( "2" ) )
    assert(
      generated === expected,
      s"""
      result: ${generated.print}
      expected: ${expected.print}
    """
    )
  }
  test( "a list" ) {
    case class Foo(a: Int, ls: List[Int] )
    val foo = Foo( 1, List( 1, 2 ) )
    val generated = AdjacentGraph.create[Foo]( "Foo", foo )
    val expected = AdjacentGraph
      .single( f )
      .connect( NonEmptyList.one( f ), a )
      .connect( NonEmptyList.of( a, f ), Leaf( "1" ) )
      .connect( NonEmptyList.one( f ), ls )
      .connect( NonEmptyList.of( ls, f ), Index( 0 ) )
      .connect( NonEmptyList.of( Index( 0 ), ls, f ), Leaf( "1" ) )
      .connect( NonEmptyList.of( ls, f ), Index( 1 ) )
      .connect( NonEmptyList.of( Index( 1 ), ls, f ), Leaf( "2" ) )
    assert(
      generated === expected,
      s"""
      generated:
      ${generated.print}
      --------------------------------------
      expected: 
      ${expected.print}
      """
    )
  }
  test( "a list with case classes" ) {
    case class B(i: Int, e: String )
    case class Z(b: B )
    case class Foo(a: Int, ls: List[Z] )
    val foo = Foo( 1, List( Z( B( 1, "a" ) ), Z( B( 2, "a" ) ) ) )
    val generated = AdjacentGraph.create[Foo]( "Foo", foo )
    val expected = AdjacentGraph
      .single( f )
      .connect( NonEmptyList.one( f ), a )
      .connect( NonEmptyList.of( a, f ), Leaf( "1" ) )
      .connect( NonEmptyList.one( f ), ls )
      .connect( NonEmptyList.of( ls, f ), index( 0 ) )
      .connect( NonEmptyList.of( index( 0 ), ls, f ), b )
      .connect( NonEmptyList.of( b, index( 0 ), ls, f ), e )
      .connect( NonEmptyList.of( e, b, index( 0 ), ls, f ), Leaf( "a" ) )
      .connect( NonEmptyList.of( b, index( 0 ), ls, f ), i )
      .connect( NonEmptyList.of( i, b, index( 0 ), ls, f ), Leaf( "1" ) )
      .connect( NonEmptyList.of( ls, f ), index( 1 ) )
      .connect( NonEmptyList.of( index( 1 ), ls, f ), b )
      .connect( NonEmptyList.of( b, index( 1 ), ls, f ), e )
      .connect( NonEmptyList.of( e, b, index( 1 ), ls, f ), Leaf( "a" ) )
      .connect( NonEmptyList.of( b, index( 1 ), ls, f ), i )
      .connect( NonEmptyList.of( i, b, index( 1 ), ls, f ), Leaf( "2" ) )
    assert( generated === expected )
  }
  test( "a list with case classes and sealed traits" ) {
    sealed trait B
    case class B1(i: Int ) extends B
    case class B2(s: String ) extends B
    case class Z(b: B )
    case class Foo(a: Int, ls: List[Z] )
    val foo = Foo( 1, List( Z( B1( 1 ) ), Z( B2( "a" ) ) ) )
    val generated = AdjacentGraph.create[Foo]( "Foo", foo )
    val expected = AdjacentGraph
      .single( f )
      .connect( NonEmptyList.one( f ), a )
      .connect( NonEmptyList.of( a, f ), Leaf( "1" ) )
      .connect( NonEmptyList.one( f ), ls )
      .connect( NonEmptyList.of( ls, f ), index( 0 ) )
      .connect( NonEmptyList.of( index( 0 ), ls, f ), b )
      .connect( NonEmptyList.of( b, index( 0 ), ls, f ), i )
      .connect( NonEmptyList.of( i, b, index( 0 ), ls, f ), Leaf( "1" ) )
      .connect( NonEmptyList.of( ls, f ), index( 1 ) )
      .connect( NonEmptyList.of( index( 1 ), ls, f ), b )
      .connect( NonEmptyList.of( b, index( 1 ), ls, f ), s )
      .connect( NonEmptyList.of( s, b, index( 1 ), ls, f ), Leaf( "a" ) )
    assert( generated === expected )
  }
  test( "a map" ) {
    case class Foo(a: Int, ls: Map[Int, List[String]] )
    val foo = Foo( 1, Map( 1 -> List( "a", "b" ), 2 -> List( "b", "d" ) ) )
    val generated = AdjacentGraph.create[Foo]( "Foo", foo )
    val expected = AdjacentGraph
      .single( f )
      .connect( NonEmptyList.one( f ), a )
      .connect( NonEmptyList.of( a, f ), Leaf( "1" ) )
      .connect( NonEmptyList.one( f ), ls )
      .connect( NonEmptyList.of( ls, f ), Leaf( "1" ) )
      .connect( NonEmptyList.of( ls, f ), Leaf( "2" ) )
      .connect( NonEmptyList.of( Leaf( "1" ), ls, f ), Index( 0 ) )
      .connect( NonEmptyList.of( Index( 0 ), Leaf( "1" ), ls, f ), Leaf( "a" ) )
      .connect( NonEmptyList.of( Leaf( "1" ), ls, f ), Index( 1 ) )
      .connect( NonEmptyList.of( Index( 1 ), Leaf( "1" ), ls, f ), Leaf( "b" ) )
      .connect( NonEmptyList.of( Leaf( "2" ), ls, f ), Index( 0 ) )
      .connect( NonEmptyList.of( Index( 0 ), Leaf( "2" ), ls, f ), Leaf( "b" ) )
      .connect( NonEmptyList.of( Leaf( "2" ), ls, f ), Index( 1 ) )
      .connect( NonEmptyList.of( Index( 1 ), Leaf( "2" ), ls, f ), Leaf( "d" ) )
    assert(
      generated === expected,
      s"""
      generated:
      ${generated.print}
      --------------------------------------
      expected: 
      ${expected.print}
      """
    )
  }
  test( "a map with case classes and sealed traits" ) {
    sealed trait B
    case class B1(i: Int ) extends B
    case class B2(s: String ) extends B
    case class Z(b: B )
    case class Foo(a: Int, ls: Map[Int, Z] )
    val foo = Foo( 1, Map( 1 -> Z( B1( 1 ) ), 2 -> Z( B2( "a" ) ) ) )
    val generated = AdjacentGraph.create[Foo]( "Foo", foo )
    val expected = AdjacentGraph
      .single( f )
      .connect( NonEmptyList.one( f ), a )
      .connect( NonEmptyList.of( a, f ), Leaf( "1" ) )
      .connect( NonEmptyList.one( f ), ls )
      .connect( NonEmptyList.of( ls, f ), Leaf( "1" ) )
      .connect( NonEmptyList.of( Leaf( "1" ), ls, f ), b )
      .connect( NonEmptyList.of( b, Leaf( "1" ), ls, f ), i )
      .connect( NonEmptyList.of( i, b, Leaf( "1" ), ls, f ), Leaf( "1" ) )
      .connect( NonEmptyList.of( ls, f ), Leaf( "2" ) )
      .connect( NonEmptyList.of( Leaf( "2" ), ls, f ), b )
      .connect( NonEmptyList.of( b, Leaf( "2" ), ls, f ), s )
      .connect( NonEmptyList.of( s, b, Leaf( "2" ), ls, f ), Leaf( "a" ) )
    assert(
      generated === expected,
      s"""
      generated:
      ${generated.print}
      --------------------------------------
      expected: 
      ${expected.print}
      --------------------------------------
      diff:
      ${Mismatches.compare( generated, expected, pl ).print}
      """
    )
  }
}
