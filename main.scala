package sail

import sail.parser.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

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
    case Expr.Sym(None, content)           =>
      s"<symbol $content>"
    case Expr.Sym(Some(module), content)   =>
      s"<symbol $module:$content>"
    case Expr.FuncDef(name, args, body)    =>
      s"<func ${render(name)}(${args.map(render).mkString(",")}) ${render(body)}>"
    case Expr.FuncCall(name, args)         =>
      s"<call $name(${args.map(render).mkString(",")})>"
    case Expr.Container(name, from, build) =>
      s"<container ${render(name)} from $from - $build>"

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

  val parsed = parser.parse(code)
  println(">> parsed")
  parsed.foreach(println)

  val reduced = reduceFile(parsed)
  println(">> reduced")
  reduced.foreach: (key, value) =>
    println(s"${render(key)} = ${render(value)}")

  resolveTargets(reduced)
