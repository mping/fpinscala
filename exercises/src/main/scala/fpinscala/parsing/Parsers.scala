package fpinscala.parsing

import ReferenceTypes._

import language.higherKinds

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  case class ParserOps[A](p: Parser[A]) {

  }

  object Laws {
  }
}

object ReferenceTypes {
  type Parser[+A] = ParseState => Result[A]

  case class ParseState(loc: Location) {}

  /* Likewise, we define a few helper functions on `Result`. */
  sealed trait Result[+A] {}

  def firstNonMatchingIndex(s1: String, s2: String, offset: Int): Int = ???
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.linesIterator.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}

object Reference extends Parsers[Parser] {

  def run[A](p: Parser[A])(s: String): Either[ParseError, A] = ???
}