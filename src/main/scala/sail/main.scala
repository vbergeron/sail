package sail

import com.monovore.decline.{CommandApp, Opts}
import sail.parser.*
import sail.model.*

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source

type Env = Map[Expr.Sym, Expr]

def showPart(part: Part): String = part match
  case Part.Capture(expr) => s"{ ${show(expr)} }"
  case Part.Content(text) => text

def showInstr(instr: Instr): String =
  instr match
    case Instr.Run(value)     => s"<run - ${show(value)}>"
    case Instr.Expose(value)  => s"<expose - ${show(value)}>"
    case Instr.Copy(src, dst) => s"<copy - ${show(src)} to ${show(dst)}>"
    case Instr.Call(value)    => s"<call - ${show(value)}>"
    case Instr.Block(values)  =>
      s"<block ${values.map(show).mkString("[", ",", "]")}>"
    case Instr.Defer(value)   => s"<defer - ${show(value)}>"

def showBoolean(expr: BooleanExpr): String =
  expr match
    case BooleanExpr.True      => "true"
    case BooleanExpr.False     => "false"
    case BooleanExpr.Not(e)    => s"not(${show(e)})"
    case BooleanExpr.Eq(l, r)  => s"${show(l)} == ${show(r)}"
    case BooleanExpr.And(l, r) => s"${show(l)} and ${show(r)}"
    case BooleanExpr.Or(l, r)  => s"${show(l)} or ${show(r)}"

def show(expr: Expr): String =
  expr match
    case instr: Instr                    => showInstr(instr)
    case expr: BooleanExpr               => showBoolean(expr)
    case Expr.Unit                       => "<>"
    case Expr.Scope(bindings, body)      =>
      s"<scope ${bindings.map(show).mkString("[", ",", "]")} ${show(body)}"
    case Expr.Template(parts)            => parts.map(showPart).mkString("'", "", "'")
    case Expr.Str(content)               => s"'$content'"
    case Expr.Num(value)                 => value.toString()
    case Expr.Sym(None, content)         => content
    case Expr.Sym(Some(module), content) => s"$module:$content"
    case Expr.FuncDef(name, args, body)  =>
      s"<func ${show(name)}(${args.map(show).mkString(",")}) - ${show(body)}>"
    case Expr.FuncCall(name, args)       =>
      s"<call $name(${args.map(show).mkString(",")})>"
    case Expr.Container(from, build)     =>
      s"<container from ${show(from)} - ${showInstr(build)}>"
    case Expr.Module(name, source)       =>
      s"<module ${show(name)} - ${show(source)}"

def render(expr: Expr): String =
  expr match
    case expr: Expr.Template =>
      throw Exception(s"Template is not reduced ${show(expr)}")
    case Expr.Str(content)   => content
    case Expr.Num(value)     => value.toString()
    case expr                => throw Exception(s"Not renderable: ${show(expr)}")

def reduceInstr(cmd: Args, env: Env, instr: Instr): Instr =
  instr match
    case Instr.Run(value) =>
      Instr.Run(reduce(cmd, env, value)._2)

    case Instr.Expose(value) =>
      Instr.Expose(reduce(cmd, env, value)._2)

    case Instr.Copy(src, dst) =>
      Instr.Copy(reduce(cmd, env, src)._2, reduce(cmd, env, dst)._2)

    case Instr.Call(value) =>
      reduce(cmd, env, value)._2.asInstanceOf[Instr]

    case Instr.Block(values) =>
      Instr.Block(values.map(reduceInstr(cmd, env, _)))

    case Instr.Defer(value) =>
      Instr.Defer(reduceInstr(cmd, env, value))

def reduceBool(cmd: Args, env: Env, expr: BooleanExpr): BooleanExpr =
  import BooleanExpr.*

  def subReduce(expr: Expr): True.type | False.type =
    reduce(cmd, env, expr)._2 match
      case x: (True.type | False.type) =>
        x
      case e                           =>
        throw Exception(s"Expression did not reduce to boolean : $e")

  expr match
    case True      => True
    case False     => False
    case Not(e)    => if subReduce(e) == True then False else True
    case And(l, r) =>
      if subReduce(l) == True && subReduce(r) == True then True else False
    case Or(l, r)  =>
      if subReduce(l) == False && subReduce(r) == False then False else True
    case Eq(l, r)  =>
      if subReduce(l) == subReduce(r) then True else False

def reducePart(cmd: Args, env: Env, part: Part): Part = part match
  case p: Part.Capture =>
    reduce(cmd, env, p.expr)._2 match
      case Expr.Str(text)  => Part.Content(text)
      case Expr.Num(value) => Part.Content(value.toString)
      case expr            => Part.Capture(expr)
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

def normalizeParts(parts: Seq[Part]): Seq[Part] =
  parts.flatMap:
    case p: Part.Content                   => Seq(p)
    case Part.Capture(expr: Expr.Template) => normalizeParts(expr.parts)
    case p: Part.Capture                   => Seq(p)

def reduceTemplate(cmd: Args, env: Env, template: Expr.Template): Expr =
  val reduced    = template.parts.map(reducePart(cmd, env, _))
  val normalized = Expr.Template(normalizeParts(reduced))
  simplify(normalized) match
    case Expr.Template(Seq(Part.Content(text))) => Expr.Str(text)
    case expr                                   => expr

def reduce(cmd: Args, env: Env, expr: Expr): (Env, Expr) =
  expr match
    case Expr.Unit         => (env, Expr.Unit)
    case it: Expr.Template => (env, reduceTemplate(cmd, env, it))
    case it: Expr.Str      => env -> it
    case it: Expr.Num      => env -> it
    case it: Expr.Sym      =>
      env.get(it).fold(env -> it)(v => env -> reduce(cmd, env, v)._2)
    case it: Expr.FuncDef  =>
      val reduced = it.copy(body = reduce(cmd, env, it.body)._2)
      reduced match
        case Expr.FuncDef(_, Seq(), body) =>
          (env + (it.name -> body), Expr.Unit)
        case _                            =>
          (env + (it.name -> reduced), Expr.Unit)

    case Expr.FuncCall(name, args) =>
      val func = env(name) match
        case it: Expr.FuncDef => it
        case it               => throw new Exception(s"$it is not callable")

      val locals = func.args zip args.map(reduce(cmd, env, _)._2)

      reduce(cmd, env ++ locals, func.body)

    case Expr.Container(from, build) =>
      val reduced = Expr.Container(from, reduceInstr(cmd, env, build))
      (env, reduced)

    case Expr.Scope(bindings, body) =>
      val scope = bindings.foldLeft(env)(reduce(cmd, _, _)._1)
      reduce(cmd, scope, body)

    case instr: Instr => (env, reduceInstr(cmd, env, instr))

    case bool: BooleanExpr => (env, reduceBool(cmd, env, bool))

    case mod: Expr.Module =>
      val env = loadFile(cmd.copy(source = mod.sourcePath.content)).map:
        (sym, expr) => sym.copy(module = Some(mod.name.value)) -> expr
      (env, Expr.Unit)

def reduceFile(cmd: Args, exprs: Seq[Expr]): Env =
  exprs.foldLeft(Map.empty[Expr.Sym, Expr])(reduce(cmd, _, _)._1)

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
        throw new Exception(s"Unresolved call: ${show(call)}")
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

def loadFile(args: Args): Env =
  val source = Source.fromFile(args.source)
  try
    if args.showFileLoad then //
      println(s"loaded - ${args.source}")

    val parsed = parser.parse(source.getLines())

    if args.showParsing then
      println(">> parsed")
      parsed.foreach: expr =>
        println(show(expr))

    reduceFile(args, parsed)
  finally source.close()

def run(args: Args): Unit =
  val reduced = loadFile(args)

  if args.showReduction then
    println(">> reduced")
    reduced.foreach: (key, value) =>
      println(s"${show(key)} = ${show(value)}")

  resolveTargets(reduced)

final case class Args(
    showParsing: Boolean,
    showReduction: Boolean,
    showFileLoad: Boolean,
    source: String
)

class Sail
    extends CommandApp(
      name = "sail",
      header = "A Functional, Declarative Dockerfile generator",
      main = {
        val showParsing =
          Opts
            .flag("show-parsing", "Show all files parsing output")
            .orFalse

        val showReduction =
          Opts
            .flag("show-reduction", "Show all files reduction output")
            .orFalse

        val showFileLoad =
          Opts
            .flag("show-file-load", "Show all files loads")
            .orTrue

        val source = Opts.argument[String]("source").withDefault("build.sail")

        import cats.syntax.apply.*
        (showParsing, showReduction, showFileLoad, source)
          .mapN(Args.apply)
          .map(run)
      }
    )

object SailJVM extends Sail
