//> using scala 3.3.1
//> using test.dep org.scalameta::munit::0.7.29
//> using dep "com.lihaoyi::fastparse:3.0.2"

package sail

import fastparse.*
import munit.*

class ParserTest extends FunSuite:

  def passing[T](prefix: String, code: String, parser: P[?] => P[T]): Unit =
    test(s"$prefix - $code"):
      parse(code, parser, verboseFailures = true) match
        case Parsed.Success(value, index) => ()
        case failure: Parsed.Failure =>
          fail(failure.longMsg)

  def failing[T](prefix: String, code: String, parser: P[?] => P[T]): Unit =
    test(s"$prefix - $code"):
      parse(code, parser, verboseFailures = true) match
        case Parsed.Success(value, index) =>
          fail(s"Expected parsing failure, got $value at $index")
        case failure: Parsed.Failure => ()

  passing("Symbol", "foo", sym(_))

  passing("String", "''", str(_))
  passing("String", "'foobar fkl nadfn'", str(_))

  passing("Template", "'{ foo }'", template(_))
  passing("Template", "'foobar fkl nadfn {foo}'", template(_))
  passing("Template", "'foobar fkl {foo} nadfn'", template(_))
  passing("Template", "'{foo} foobar fkl nadfn'", template(_))
  passing("Template", "'{foo} foobar {fkl} nadfn'", template(_))

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

