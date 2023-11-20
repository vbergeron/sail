package sail.model

sealed trait Expr

object Expr:

  case object Unit                                          extends Expr
  case class Template(parts: Seq[Part])                     extends Expr
  case class Str(content: String)                           extends Expr
  case class Num(value: BigDecimal)                         extends Expr
  case class Sym(module: Option[String], value: String)     extends Expr
  case class FuncDef(name: Sym, args: Seq[Sym], body: Expr) extends Expr
  case class FuncCall(name: Sym, args: Seq[Expr])           extends Expr
  case class Container(from: Str, build: Instr)             extends Expr
  case class Scope(bindings: Seq[FuncDef], body: Expr)      extends Expr

  case class Module(name: Expr.Sym, sourcePath: Expr.Str) extends Expr

sealed trait BooleanExpr extends Expr

object BooleanExpr:
  case object True                 extends BooleanExpr
  case object False                extends BooleanExpr
  case class And(l: Expr, r: Expr) extends BooleanExpr
  case class Or(l: Expr, r: Expr)  extends BooleanExpr
  case class Not(expr: Expr)       extends BooleanExpr
  case class Eq(l: Expr, r: Expr)  extends BooleanExpr

sealed trait Instr extends Expr

object Instr:
  case class Run(value: Expr)            extends Instr
  case class Expose(value: Expr)         extends Instr
  case class Copy(src: Expr, dest: Expr) extends Instr
  case class Call(value: Expr.FuncCall)  extends Instr
  case class Block(values: Seq[Instr])   extends Instr
  case class Defer(value: Instr)         extends Instr

enum Part:
  case Capture(expr: Expr)
  case Content(text: String)
