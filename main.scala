//> using scala 3.3.1
//> using dep "com.lihaoyi::fastparse:3.0.2"
package sail

import fastparse.*
import NoWhitespace.*

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

def symbol[$: P] = P(CharPred(_.isLetter).rep.!).filter(_.nonEmpty)

def ws[$: P] = P(CharsWhile(_.isWhitespace).rep)

enum Part:
  case Capture(expr: Expr)
  case Content(text: String)

def part[$: P]: P[Part] =
  def capture[$: P]: P[Part.Capture] =
    P("{" ~ ws ~ expr ~ ws ~ "}").map(Part.Capture.apply)

  def content[$: P]: P[Part.Content] =
    P(CharsWhile(c => c != '\'' && c != '{').!).map(Part.Content.apply)

  capture | content

sealed trait Expr

object Expr:
  case object Unit                      extends Expr
  case class Template(parts: Seq[Part]) extends Expr
  case class Str(content: String)       extends Expr

  case class Num(value: BigDecimal)                         extends Expr
  case class Sym(content: String)                           extends Expr
  case class FuncDef(name: Sym, args: Seq[Sym], body: Expr) extends Expr
  case class FuncCall(name: Sym, args: Seq[Expr])           extends Expr
  case class Container(name: Sym, from: Str, build: Instr)  extends Expr
  case class Scope(bindings: Seq[FuncDef], body: Expr)      extends Expr

sealed trait Instr extends Expr

object Instr:
  case class Run(value: Expr)            extends Instr
  case class Expose(value: Expr)         extends Instr
  case class Copy(src: Expr, dest: Expr) extends Instr
  case class Call(value: Expr.FuncCall)  extends Instr
  case class Block(values: Seq[Instr])   extends Instr
  case class Defer(value: Instr)         extends Instr

def expr[$: P]: P[Expr] =
  scope | container | funcDef | instr | template | str | num | funcCall | sym

def instr[$: P]: P[Instr] =
  def run[$: P]: P[Instr.Run] =
    P("run" ~ ws ~ expr).map(Instr.Run.apply)

  def expose[$: P]: P[Instr.Expose] =
    P("expose" ~ ws ~ expr).map(Instr.Expose.apply)

  def copy[$: P]: P[Instr.Copy] =
    P("copy" ~ ws ~ expr ~ ws ~ "to" ~ ws ~ expr).map(Instr.Copy.apply)

  def call[$: P]: P[Instr.Call] =
    P(ws ~ funcCall).map(Instr.Call.apply)

  def block[$: P]: P[Instr.Block] =
    P("[" ~ ws ~ (instr ~ ws).rep ~ ws ~ "]").map(Instr.Block.apply)

  def defer[$: P]: P[Instr.Defer] =
    P("defer" ~ ws ~ instr).map(Instr.Defer.apply)

  block | defer | run | expose | copy | call

def scope[$: P]: P[Expr.Scope] =
  P("let" ~ ws ~ (funcDef ~ ws).rep ~ "in" ~ ws ~ expr).map(Expr.Scope.apply)

def str[$: P]: P[Expr.Str] =
  P("'" ~ CharPred(c => c != '\'').rep.! ~ "'").map(Expr.Str.apply)

def num[$: P]: P[Expr.Num] =
  P(CharsWhile(_.isDigit).! ~ ("." ~ CharsWhile(_.isDigit)).!.?)
    .map: (principal, decimals) =>
      Expr.Num(BigDecimal(principal + decimals.getOrElse("")))

def sym[$: P]: P[Expr.Sym] = symbol.map(Expr.Sym.apply)

def template[$: P]: P[Expr.Template] =
  P("'" ~ part ~ part.rep ~ "'")
    .map((head, tail) => Expr.Template(head +: tail))
    .filter(_.parts.exists(_.isInstanceOf[Part.Capture]))

def funcDef[$: P]: P[Expr.FuncDef] =
  def funcDefN[$: P]: P[Expr.FuncDef] =
    P(
      sym ~ ws ~ "(" ~ ws ~ sym ~ (ws ~ "," ~ ws ~ sym).rep ~ ws ~ ")" ~ ws ~ "=" ~ ws ~ expr
    )
      .map: (name, head, tail, body) =>
        Expr.FuncDef(name, head +: tail, body)

  def funcDef0[$: P]: P[Expr.FuncDef] =
    P(sym ~ ws ~ "=" ~ ws ~ expr)
      .map: (name, body) =>
        Expr.FuncDef(name, Seq.empty, body)

  funcDefN | funcDef0

def funcCall[$: P]: P[Expr.FuncCall] =
  P(sym ~ ws ~ "(" ~ ws ~ expr ~ (ws ~ "," ~ ws ~ expr).rep ~ ws ~ ")")
    .map: (name, head, tail) =>
      Expr.FuncCall(name, head +: tail)

def container[$: P]: P[Expr.Container] =
  P(
    "container" ~ ws ~ sym ~ ws ~ "from" ~ ws ~ str ~ ws ~ "with" ~ ws ~ instr
  )
    .map(Expr.Container.apply)

def file[$: P]: P[Seq[Expr]] =
  P(ws ~ (expr ~ ws).rep ~ ws)

type Env = Map[Expr.Sym, Expr]

def renderPart(part: Part): String = part match
  case Part.Capture(expr) => render(expr)
  case Part.Content(text) => text

def render(expr: Expr): String =
  expr match
    case instr: Instr                      => s"<instr $instr>"
    case Expr.Unit                         => "<>"
    case Expr.Scope(bindings, body)        =>
      s"<scope ${bindings.map(render).mkString("[", ",", "]")} ${render(body)}"
    case Expr.Template(parts)              => parts.map(renderPart).mkString
    case Expr.Str(content)                 => content
    case Expr.Num(value)                   => value.toString()
    case Expr.Sym(content)                 =>
      s"<symbol $content>"
    case Expr.FuncDef(name, args, body)    =>
      s"<func ${name.content}(${args.map(_.content).mkString(",")}) ${render(body)}>"
    case Expr.FuncCall(name, args)         =>
      s"<call $name(${args.map(render).mkString(",")})>"
    case Expr.Container(name, from, build) =>
      s"<container ${name.content} from $from - $build>"

def reduceInstr(env: Env, instr: Instr): Instr =
  instr match
    case Instr.Run(value) =>
      Instr.Run(reduce(env, value)._2)

    case Instr.Expose(value) =>
      Instr.Expose(reduce(env, value)._2)

    case Instr.Copy(src, dst) =>
      Instr.Copy(reduce(env, src)._2, reduce(env, dst)._2)

    case Instr.Call(value) =>
      reduce(env, value)._2.asInstanceOf[Instr]

    case Instr.Block(values) =>
      Instr.Block(values.map(reduceInstr(env, _)))

    case Instr.Defer(value) =>
      Instr.Defer(reduceInstr(env, value))

def reducePart(env: Env, part: Part): Part = part match
  case p: Part.Capture =>
    reduce(env, p.expr)._2 match
      case Expr.Str(content) => Part.Content(content)
      case expr              => Part.Capture(expr)
  case p: Part.Content => p

def simplify(template: Expr.Template): Expr.Template =
  @tailrec
  def go(
      parts: List[Part],
      keep: Option[Part.Content],
      acc: List[Part]
  ): List[Part] =
    parts match
      case (head: Part.Content) :: tail =>
        keep match
          case Some(part) =>
            go(tail, Some(Part.Content(part.text + head.text)), acc)
          case None       =>
            go(tail, Some(head), acc)

      case (head: Part.Capture) :: tail =>
        keep match
          case Some(part) =>
            go(tail, None, head :: part :: acc)
          case None       =>
            go(tail, None, head :: acc)
      case Nil                          =>
        keep match
          case Some(part) =>
            part :: acc
          case None       =>
            acc
  Expr.Template(go(template.parts.toList, None, Nil).reverse)

def reduceTemplate(env: Env, template: Expr.Template): Expr.Template =
  simplify(Expr.Template(template.parts.map(reducePart(env, _))))

def reduce(env: Env, expr: Expr): (Env, Expr) =
  expr match
    case Expr.Unit         => (env, Expr.Unit)
    case it: Expr.Template => (env, reduceTemplate(env, it))
    case it: Expr.Str      => env -> it
    case it: Expr.Num      => env -> it
    case it: Expr.Sym      =>
      env.get(it).fold(env -> it)(v => env -> reduce(env, v)._2)
    case it: Expr.FuncDef  =>
      val reduced = it.copy(body = reduce(env, it.body)._2)
      reduced match
        case Expr.FuncDef(_, Seq(), body) =>
          (env + (it.name -> body), Expr.Unit)
        case _                            =>
          (env + (it.name -> reduced), Expr.Unit)

    case Expr.FuncCall(name, args) =>
      val func = env(name) match
        case it: Expr.FuncDef => it
        case it               => throw new Exception(s"$it is not callable")

      val locals = func.args zip args.map(reduce(env, _)._2)

      reduce(env ++ locals, func.body)

    case Expr.Container(name, from, build) =>
      val reduced = Expr.Container(name, from, reduceInstr(env, build))
      (env + (name -> reduced), Expr.Unit)

    case Expr.Scope(bindings, body) =>
      val scope = bindings.foldLeft(env)(reduce(_, _)._1)
      reduce(scope, body)

    case instr: Instr => (env, reduceInstr(env, instr))

def reduceFile(exprs: Seq[Expr]): Env =
  exprs.foldLeft(Map.empty[Expr.Sym, Expr])(reduce(_, _)._1)

def resolveContainer(container: Expr.Container): Unit =
  val instructions = ListBuffer.empty[String]
  val deferred     = ListBuffer.empty[String]

  println(s"FROM ${render(container.from)}")

  def go(add: String => Unit, instr: Instr): Unit =
    instr match
      case Instr.Run(value)     =>
        add(s"RUN ${render(value)}")
      case Instr.Expose(value)  =>
        add(s"EXPOSE ${render(value)}")
      case Instr.Copy(src, dst) =>
        add(s"COPY ${render(src)} ${render(dst)}")
      case Instr.Call(call)     =>
        throw new Exception(s"Unresolved call: $call")
      case Instr.Block(values)  =>
        values.foreach(go(add, _))
      case Instr.Defer(value)   =>
        go(deferred.addOne, value)

  go(instructions.addOne, container.build)
  instructions.foreach(println)
  deferred.reverse.foreach(println)

def resolveTargets(env: Env): Unit =
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
      |    expose    12345
      |    copy  'foo' to to
      |    defer run 'rm { from }'
      |  ]
      |
      |container baz from 'debian:latest' with [
      |  foo('a', 'b')
      |]
      |""".stripMargin

  val parsed = parse(code, file(_)).get.value
  println(">> parsed")
  parsed.foreach(println)

  val reduced = reduceFile(parsed)
  println(">> reduced")
  reduced.foreach: (key, value) =>
    println(s"${render(key)} = ${render(value)}")

  resolveTargets(reduced)
