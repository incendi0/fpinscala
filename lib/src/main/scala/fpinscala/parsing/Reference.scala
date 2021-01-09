package fpinscala.parsing

import fpinscala.parsing.ReferenceTypes._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

object ReferenceTypes {

  type Parser[+A] = ParserState => Result[A]

  case class ParserState(loc: Location) {
    def advanceBy(numChars: Int): ParserState =
      copy(loc = loc.copy(offset = loc.offset + numChars))

    def input: String = loc.input.substring(loc.offset)

    def slice(n: Int): String = loc.input.substring(loc.offset, loc.offset + n)
  }

  sealed trait Result[+A] {
    def extract: Either[ParseError, A] = this match {
      case Failure(e, _) => Left(e)
      case Success(a, _) => Right(a)
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) =>Failure(e, false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _ => this
    }

    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _ => this
    }
  }

  case class Success[+A](get: A, length: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  def firstNonMatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i + offset < s1.length && i < s2.length) {
      if (s1.charAt(i + offset) != s2.charAt(i)) {
        return i
      }
      i += 1
    }
    if (s1.length - offset >= s2.length) -1
    else s1.length - offset
  }
}

object Reference extends Parsers[Parser] {
  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
    val s0 = ParserState(Location(input))
    p(s0).extract
  }

  override implicit def string(str: String): Parser[String] = {
    val msg = "'" + str + "'"
    s => {
      val i = firstNonMatchingIndex(s.loc.input, str, s.loc.offset)
      if (i == -1) {
        Success(str, str.length)
      } else {
        Failure(s.loc.advanceBy(1).toError(msg), i != 0)
      }
    }
  }

  override def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

  override def slice[A](p: Parser[A]): Parser[String] = s => p(s) match {
    case Success(_, n) => Success(s.slice(n), n)
    case f@Failure(_, _) => f
  }

  override def or[A](p: Parser[A], p2: Parser[A]): Parser[A] = s => p(s) match {
    case Failure(_, false) => p2(s)
    case r => r
  }

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = s => p(s) match {
    case Success(a, n) => f(a)(s.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
    case failure@Failure(_, _) => failure
  }

  override implicit def regex(r: Regex): Parser[String] = {
    val msg = "regex " + r
    s => r.findPrefixOf(s.input) match {
      case None => Failure(s.loc.toError(msg), false)
      case Some(m) => Success(m, m.length)
    }
  }

  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.push(s.loc, msg))

  override def attempt[A](p: Parser[A]): Parser[A] =
    s => p(s).uncommit

  override def many[A](p: Parser[A]): Parser[List[A]] =
    s => {
      val buf = new ListBuffer[A]
      @tailrec
      def go(p: Parser[A], offset: Int): Result[List[A]] = {
        p(s.advanceBy(offset)) match {
          case Success(a, n) => buf += a; go(p, offset + n)
          case f@Failure(_, true) => f
          case Failure(_, _) =>Success(buf.toList, offset)
        }
      }
      go(p, 0)
    }
}
