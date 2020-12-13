package utils
import tograph._
import cats.data.NonEmptyList

object NodeNames {
  val f: Labelled.AsString = Node( "Foo" )
  val a: Labelled.AsString = Node( "a")
  val b: Labelled.AsString = Node( "b")
  val c: Labelled.AsString = Node( "c")
  val d: Labelled.AsString = Node( "d")
  val h: Labelled.AsString = Node( "h")
  val g: Labelled.AsString = Node( "g")
  val k: Labelled.AsString = Node( "k")
  val e: Labelled.AsString = Node( "e")
  val i: Labelled.AsString = Node( "i")
  val l: Labelled.AsString = Node( "l")
  val j: Labelled.AsString = Node( "j")
  val x: Labelled.AsString = Node( "x")
  val y: Labelled.AsString = Node( "y")
  val z: Labelled.AsString = Node( "z")
  val s: Labelled.AsString = Node( "s")
  val r: Labelled.AsString = Node( "r")
  val ls: Labelled.AsString = Node( "ls" )
  val pl: Labelled.AsString = Node( "-" )
  def index(i: Int ) = Index[String]( i )
}
