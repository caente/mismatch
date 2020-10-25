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
    val compared = Mismatches.compare( generated_Left, generated_Right )
    val expected = AdjacentGraph
      .single( Diff.same( 'Foo ) )
      .addEdge( Diff.same( 'Foo ), Diff.same( 'a ) )
      .addEdge( Diff.same( 'Foo ), Diff.added( 'l ) )
      .addEdge( Diff.added( 'l ), Diff.added( 'g ) )
      .addEdge( Diff.added( 'g ), Diff.added( 'k ) )
      .addEdge( Diff.added( 'l ), Diff.added( 'h ) )
      .addEdge( Diff.same( 'a ), Diff.same( 'd ) )
      .addEdge( Diff.same( 'a ), Diff.same( 'c ) )
      .addEdge( Diff.same( 'c ), Diff.removed( 'e ) )
      .addEdge( Diff.same( 'c ), Diff.added( 'j ) )
      .addEdge( Diff.same( 'Foo ), Diff.removed( 'b ) )
      .addEdge( Diff.removed( 'b ), Diff.removed( 'g ) )
      .addEdge( Diff.removed( 'b ), Diff.removed( 'h ) )
      .addEdge( Diff.removed( 'g ), Diff.removed( 'k ) )
    assert( compared.data.toSet == expected.data.toSet )
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
}
