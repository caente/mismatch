package mismatch

import graph._
import tograph._
import cats.implicits._
import algorithm._

object Compare {
  def apply[A, B](
      a: A,
      b: B
    )(implicit
      A: ToGraph[A, AdjacentGraph, Labelled[String]],
      B: ToGraph[B, AdjacentGraph, Labelled[String]]
    ): AdjacentGraph[Diff[Labelled.AsString]] = {
    val generatedA = ToGraph.create( 'ROOT, a )
    val generatedB = ToGraph.create( 'ROOT, b )
    Mismatches.compare( generatedA, generatedB, Node[String]( '- ) )
  }

}
