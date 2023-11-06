//> using scala "3.3.1"
//> using test.dependency "org.scalameta::munit::0.7.29"

package sail.parser

import fastparse.*
import munit.*

class ParserTest extends FunSuite:

  def passing[T](prefix: String, code: String, parser: P[?] => P[T]): Unit =
    test(s"$prefix - $code"):
      parse(code, parser, verboseFailures = true) match
        case Parsed.Success(value, index) => ()
        case failure: Parsed.Failure =>
          fail(failure.longMsg)

  def passingT[T](prefix: String, code: String, parser: P[?] => P[T])(check: T => Unit): Unit =
    test(s"$prefix - $code"):
      parse(code, parser, verboseFailures = true) match
        case Parsed.Success(value, index) => check(value)
        case failure: Parsed.Failure =>
          fail(failure.longMsg)

  def failing[T](prefix: String, code: String, parser: P[?] => P[T]): Unit =
    test(s"$prefix - $code"):
      parse(code, parser, verboseFailures = true) match
        case Parsed.Success(value, index) =>
          fail(s"Expected parsing failure, got $value at $index")
        case failure: Parsed.Failure => ()

  passing("Expr.Symbol", "foo", sym(_))
  passingT("Expr.Symbol", "foo:bar", sym(_)): t =>
    assertEquals(t.module, Some("foo"))
    assertEquals(t.name, "bar")

  passing("Expr.String", "''", str(_))
  passing("Expr.String", "'foobar fkl nadfn'", str(_))

  passing("Expr.Num", "0", num(_))
  passing("Expr.Num", "1234", num(_))
  passing("Expr.Num", "1234.12345", num(_))

  passing("Part.Capture", "{ foo }", part(_))
  passing("Part.Capture", "{foo}", part(_))
  passing("Part.Content", " foo }", part(_))
  passing("Part.Content", " foo '", part(_))
  passing("Part.Content", " foo ", part(_))

  passing("Template", "'{ foo }'", template(_))

  passingT("Template", "'foobar fkl nadfn {foo}'", template(_)): t =>
    assertEquals(t.parts, Seq(Part.Content("foobar fkl nadfn "), Part.Capture(Expr.Sym("foo"))))

  passingT("Template", "'{foo} foobar fkl nadfn'", template(_)): t =>
    assertEquals(t.parts, Seq(Part.Capture(Expr.Sym("foo")), Part.Content(" foobar fkl nadfn")))

  passingT("Template", "'foobar fkl {foo} nadfn'", template(_)): t =>
    assertEquals(t.parts, Seq(
      Part.Content("foobar fkl "),
      Part.Capture(Expr.Sym("foo")),
      Part.Content(" nadfn")
    ))

  passingT("Template", "'{foo} foobar {fkl} nadfn'", template(_)): t =>
    assertEquals(t.parts, Seq(
      Part.Capture(Expr.Sym("foo")),
      Part.Content(" foobar "),
      Part.Capture(Expr.Sym("fkl")),
      Part.Content(" nadfn")
    ))

  passing("FuncDef", "f = body", funcDef(_))
  passing("FuncDef", "f(a) = body", funcDef(_))
  passing("FuncDef", "f(a,b,c,d) = body", funcDef(_))

  failing("FuncCall", "f()", funcCall(_))
  passing("FuncCall", "f('str')", funcCall(_))
  passing("FuncCall", "f(g(x)) = body", funcCall(_))
  passing("FuncCall", "f(a,b,c)", funcCall(_))

  passing("Scope", "let a = b in body", scope(_))
  passing("Scope", "let a = b c = d in body", scope(_))
  passing("Scope", "let a(x) = b in body", scope(_))

  passing("Containter", "container a from 'scratch' with run ''", container(_))

