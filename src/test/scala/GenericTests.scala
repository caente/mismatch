package generic
package tests

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalactic.TypeCheckedTripleEquals
import cats.instances.SymbolInstances
import graph._

class GenericTests extends AnyFunSuite with TypeCheckedTripleEquals with Matchers with SymbolInstances {
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
    import shapeless._
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
    val generated = generic.ToGraph[Foo, AdjacentGraph]( 'Foo, foo )
    assert( generated === manual )
  }
}
