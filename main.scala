package sail

import sail.parser.*

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

def show(expr: Expr): String =
  expr match
    case instr: Instr                      => showInstr(instr)
    case Expr.Unit                         => "<>"
    case Expr.Scope(bindings, body)        =>
      s"<scope ${bindings.map(show).mkString("[", ",", "]")} ${show(body)}"
    case Expr.Template(parts)              => parts.map(showPart).mkString("'", "", "'")
    case Expr.Str(content)                 => s"'$content'"
    case Expr.Num(value)                   => value.toString()
    case Expr.Sym(None, content)           => content
    case Expr.Sym(Some(module), content)   => s"$module:$content"
    case Expr.FuncDef(name, args, body)    =>
      s"<func ${show(name)}(${args.map(show).mkString(",")}) - ${show(body)}>"
    case Expr.FuncCall(name, args)         =>
      s"<call $name(${args.map(show).mkString(",")})>"
    case Expr.Container(name, from, build) =>
      s"<container ${show(name)} from ${show(from)} - ${showInstr(build)}>"
    case Expr.Module(name, source)         =>
      s"<module ${show(name)} - ${show(source)}"

def renderPart(part: Part): String = part match
  case Part.Capture(expr) => render(expr)
  case Part.Content(text) => text

def render(expr: Expr): String =
  expr match
    case expr: Expr.Template =>
      throw Exception(
        s"Template is not reduced ${show(expr)}"
      ) // parts.map(renderPart).mkString
    case Expr.Str(content)   => content
    case Expr.Num(value)     => value.toString()
    case expr                => throw Exception(s"Not renderable: ${show(expr)}")

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

def normalizePart(part: Part): Seq[Part] =
  part match
    case p: Part.Content                   => Seq(p)
    case Part.Capture(expr: Expr.Template) => normalizeParts(expr.parts)
    case p: Part.Capture                   => Seq(p)

def normalizeParts(parts: Seq[Part]): Seq[Part] =
  parts.flatMap(normalizePart)

def reduceTemplate(env: Env, template: Expr.Template): Expr =
  val reduced    = template.parts.map(reducePart(env, _))
  val normalized = Expr.Template(normalizeParts(reduced))
  simplify(normalized) match
    case Expr.Template(Seq(Part.Content(text))) => Expr.Str(text)
    case expr                                   => expr

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
      println(build)
      val reduced = Expr.Container(name, from, reduceInstr(env, build))
      (env + (name -> reduced), Expr.Unit)

    case Expr.Scope(bindings, body) =>
      val scope = bindings.foldLeft(env)(reduce(_, _)._1)
      reduce(scope, body)

    case instr: Instr => (env, reduceInstr(env, instr))

    case module: Expr.Module =>
      val env = loadFile(module.sourcePath.content).map: (sym, expr) =>
        sym.copy(module = Some(module.name.value)) -> expr
      (env, Expr.Unit)

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

def loadFile(path: String): Env =
  val source = Source.fromFile(path)
  try
    println(s"loaded - $path")
    reduceFile(parser.parse(source.getLines()))
  finally source.close()

@main
def run(args: String*): Unit =

  val reduced = loadFile(args(0))
  println(">> reduced")
  reduced.foreach: (key, value) =>
    println(s"${show(key)} = ${show(value)}")

  resolveTargets(reduced)
