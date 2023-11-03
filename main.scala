//> using dep "com.lihaoyi::fastparse:3.0.2"

import Expr.{FuncDef, Template}
import fastparse.*
import NoWhitespace.*

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

def symbol[$: P] = P(CharPred(_.isLetter).rep.!).filter(_.nonEmpty)

def ws[$: P] = P(CharsWhile(_.isWhitespace).rep)


final case class Part(capture: Option[Expr], content: String, next: Option[Part]):
  def capturing:Boolean =
    capture.nonEmpty || next.exists(_.capturing)

def part[$: P]:P[Part] =
  def capture[$: P]: P[Expr] = P("{" ~ ws ~ expr ~ ws ~ "}")

  def content[$:P] = P(CharPred(c => c != '\'' && c != '{').rep.!)

  ((capture.? ~ content).filter((cap, ctn) => cap.nonEmpty || ctn.nonEmpty) ~ part.?)
    .map(Part.apply)
    .filter(p => p.capturing)

sealed trait Expr

object Expr:
  case object Unit extends Expr
  case class Template(part: Part) extends Expr
  case class Str(content: String) extends Expr
  case class Sym(content: String) extends Expr
  case class FuncDef(name: Sym, args: Seq[Sym], body: Expr) extends Expr
  case class FuncCall(name: Sym, args: Seq[Expr]) extends Expr
  case class Container(name: Sym, build: Instr) extends Expr
  case class Scope(bindings: Seq[FuncDef], body: Expr) extends Expr

sealed trait Instr extends Expr

object Instr:
  case class Run(value: Expr) extends Instr
  case class Call(value: Expr.FuncCall) extends Instr
  case class Block(values: Seq[Instr]) extends Instr
  case class Defer(value: Instr) extends Instr

def expr[$: P]: P[Expr] = scope | container | funcDef | instr | template | str | funcCall | sym

def instr[$: P]: P[Instr] =
  def run[$:P]: P[Instr.Run] =
    P("run" ~ ws ~ expr).map(Instr.Run.apply)

  def call[$:P]: P[Instr.Call] =
    P(ws ~ funcCall).map(Instr.Call.apply)

  def block[$:P]: P[Instr.Block] =
    P("[" ~ ws ~ (instr ~ ws).rep ~ ws ~ "]").map(Instr.Block.apply)

  def defer[$:P]: P[Instr.Defer] =
    P("defer" ~ ws ~ instr).map(Instr.Defer.apply)

  block | defer | run | call

def scope[$:P]:P[Expr.Scope] =
  P("let" ~ ws ~ (funcDef ~ ws).rep ~ "in" ~ ws ~ expr).map(Expr.Scope.apply)

def str[$:P]: P[Expr.Str] = P( "'" ~ CharPred(c => c != '\'').rep.! ~ "'").map(Expr.Str.apply)

def sym[$:P]:P[Expr.Sym] = symbol.map(Expr.Sym.apply)

def template[$: P]: P[Expr.Template] = P("'" ~ part ~ "'").map(Expr.Template.apply)


def funcDef[$: P]:P[Expr.FuncDef] =
  def funcDefN[$: P]:P[Expr.FuncDef] =
    P(sym ~ ws ~ "(" ~ ws ~ sym ~ (ws ~ "," ~ ws ~ sym).rep ~ ws ~ ")" ~ ws ~ "=" ~ ws ~ expr)
      .map: (name, head, tail, body) =>
        Expr.FuncDef(name, head +: tail, body)

  def funcDef0[$: P]:P[Expr.FuncDef] =
    P(sym ~ ws ~ "=" ~ ws ~ expr)
      .map: (name, body) =>
        Expr.FuncDef(name, Seq.empty, body)

  funcDefN | funcDef0

def funcCall[$:P]:P[Expr.FuncCall] =
  P(sym ~ ws ~ "(" ~ ws ~ expr ~ (ws ~ "," ~ ws ~ expr).rep ~ ws ~ ")")
    .map: (name, head, tail) =>
      Expr.FuncCall(name, head +: tail)

def container[$:P]:P[Expr.Container] =
  P("container" ~ ws ~ sym ~ ws ~ "with" ~ ws ~ instr)
    .map(Expr.Container.apply)

def file[$:P]:P[Seq[Expr]] =
  P(ws ~ (expr ~ ws).rep ~ ws)

type Env = Map[Expr.Sym, Expr]

def renderPart(part: Part): String =
  val capture = part.capture.map(render).getOrElse("")
  capture + part.content + part.next.map(renderPart).getOrElse("")

def render(expr: Expr):String =
  expr match
    case instr: Instr => s"<instr $instr>"
    case Expr.Unit => "<>"
    case Expr.Scope(bindings, body) =>
      s"<scope ${bindings.map(render).mkString("[", ",", "]")} ${render(body)}"
    case Expr.Template(part) => renderPart(part)
    case Expr.Str(content) => content
    case Expr.Sym(content) =>
      s"<symbol $content>"
    case Expr.FuncDef(name, args, body) =>
      s"<func ${name.content}(${args.map(_.content).mkString(",")}) ${render(body)}>"
    case Expr.FuncCall(name, args) =>
      s"<call $name(${args.map(render).mkString(",")})>"
    case Expr.Container(name, build) =>
      s"<container ${name.content} - $build>"

def reduceInstr(env: Env, instr: Instr): Instr =
  instr match
    case Instr.Run(value) => Instr.Run(reduce(env, value)._2)
    case Instr.Call(value) => reduce(env, value)._2.asInstanceOf[Instr]
    case Instr.Block(values) => Instr.Block(values.map(reduceInstr(env, _)))
    case Instr.Defer(value) => Instr.Defer(reduceInstr(env, value))

def reducePart(env: Env, part: Part): Part =
  val capture = part.capture.map(reduce(env, _)._2)
  Part(capture, part.content, part.next.map(reducePart(env, _)))

def simplifyPart(part: Part): Part =
  part.capture match
    case Some(Expr.Str(text)) => Part(None, text ++ part.content, part.next.map(simplifyPart))
    case _ => Part(part.capture, part.content, part.next.map(simplifyPart))

def reduceTemplate(env: Env, template: Expr.Template): Expr.Template =
  Expr.Template(simplifyPart(reducePart(env, template.part)))

def reduce(env: Env, expr: Expr): (Env, Expr) =
  expr match
    case Expr.Unit => (env, Expr.Unit)
    case it: Expr.Template  => (env, reduceTemplate(env, it))
    case it: Expr.Str => env -> it
    case it: Expr.Sym =>
      env.get(it).fold(env -> it)(v => env -> v)
    case it: Expr.FuncDef =>
      val reduced = it.copy(body = reduce(env, it.body)._2)
      reduced match
        case Expr.FuncDef(_, Seq(), body) =>
          (env + (it.name -> body), Expr.Unit)
        case _ =>
          (env + (it.name -> reduced), Expr.Unit)

    case Expr.FuncCall(name, args) =>
      val func = env(name) match
        case it: Expr.FuncDef => it
        case it => throw new Exception(s"$it is not callable")

      val locals = func.args zip args.map(reduce(env, _)._2)

      reduce(env ++ locals, func.body)

    case Expr.Container(name, build) =>
      val reduced = Expr.Container(name, reduceInstr(env, build))
      (env + (name -> reduced), Expr.Unit)

    case Expr.Scope(bindings, body) =>
      val scope = bindings.foldLeft(env)(reduce(_, _)._1)
      reduce(scope, body)

    case instr: Instr => (env, reduceInstr(env, instr))

def reduceFile(exprs: Seq[Expr]): Env =
  exprs.foldLeft(Map.empty[Expr.Sym, Expr])(reduce(_, _)._1)

def resolveContainer(container: Expr.Container): Unit =
  val instructions = ListBuffer.empty[String]
  val deferred = ListBuffer.empty[String]

  def go(add: String => Unit, instr: Instr): Unit =
    instr match
      case Instr.Run(value) =>
        add(s"RUN ${render(value)}")
      case Instr.Call(_) => add("unresolved call")
      case Instr.Block(values) =>
        values.foreach(go(add, _))
      case Instr.Defer(value) =>
        go(deferred.addOne, value)

  go(instructions.addOne, container.build)
  instructions.foreach(println)
  deferred.foreach(println)

def resolveTargets(env: Env):Unit =
  env.values.collect:
    case it: Expr.Container => resolveContainer(it)

@main
def run: Unit =
  val code =
    """
      |foo(name, value) =
      |  let
      |     from = '$HOME/{ name }'
      |     to   = '/some/backup/{ from }'
      |  in [
      |    run       'cp { from } { to }'
      |    defer run 'rm { from }'
      |  ]
      |
      |container baz with foo('from', 'to')
      |""".stripMargin

  val parsed = parse(code, file(_)).get.value
  println(">> parsed")
  parsed.foreach(println)

  val reduced = reduceFile(parsed)
  println(">> reduced")
  reduced.foreach: (key, value) =>
    println(s"${render(key)} = ${render(value)}")

  resolveTargets(reduced)

  //println(parse("foo", expr(_)))
  //println(parse("''", expr(_)))

  //println(parse("'foobar fkl nadfn'", expr(_)))
  //println(parse("'{ foo }'", expr(_)))
  //println(parse("'foobar fkl nadfn {foo}'", expr(_)))
  //println(parse("'foobar fkl {foo} nadfn'", expr(_)))
  //println(parse("'{foo} foobar fkl nadfn'", expr(_)))
  //println(parse("'{foo} foobar {fkl} nadfn'", expr(_)))

  //println(parse("name(a) = '{ foo }'", expr(_)))
  //println(parse("name(a, b, c, d) = ''", expr(_)))
  //println(parse("name = 'woot'", expr(_)))

  //println(parse("f('foo')", expr(_)))
  //println(parse("f(g(h))", expr(_)))

  //println(parse("run 'some {template}' ", instr(_), verboseFailures = true))
  //println(parse("call f(x)' ", instr(_), verboseFailures = true))
  //println(parse("defer run 'some {template}' ", instr(_), verboseFailures = true))
  //println(parse(
  //  """[ run 'foo'
  //     | defer run 'bar' ]""".stripMargin, instr(_), verboseFailures = true))
