package sail.parser

import sail.model.*
import fastparse.*
import fastparse.NoWhitespace.*

def symbol[$: P] = P(CharPred(_.isLetter).rep.!).filter(_.nonEmpty)

def ws[$: P] = P(CharsWhile(_.isWhitespace).rep)

def part[$: P]: P[Part] =
  def capture[$: P]: P[Part.Capture] =
    P("{" ~ ws ~ expr ~ ws ~ "}").map(Part.Capture.apply)

  def content[$: P]: P[Part.Content] =
    P(CharsWhile(c => c != '\'' && c != '{').!).map(Part.Content.apply)

  capture | content

def expr[$: P]: P[Expr] =
  scope | container | module | funcDef | instr | template | str | num | funcCall | sym

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
    P("[" ~ ws ~ (rec ~ ws).rep ~ ws ~ "]").map(Instr.Block.apply)

  def defer[$: P]: P[Instr.Defer] =
    P("defer" ~ ws ~ rec).map(Instr.Defer.apply)

  def rec[$: P]: P[Instr] =
    block | defer | run | expose | copy | call

  block | defer | run | expose | copy

def scope[$: P]: P[Expr.Scope] =
  P("let" ~ ws ~ (funcDef ~ ws).rep ~ "in" ~ ws ~ expr).map(Expr.Scope.apply)

def str[$: P]: P[Expr.Str] =
  P("'" ~ CharPred(c => c != '\'').rep.! ~ "'").map(Expr.Str.apply)

def num[$: P]: P[Expr.Num] =
  P(CharsWhile(_.isDigit).! ~ ("." ~ CharsWhile(_.isDigit)).!.?)
    .map: (principal, decimals) =>
      Expr.Num(BigDecimal(principal + decimals.getOrElse("")))

def sym[$: P]: P[Expr.Sym] =
  def qualified[$: P]: P[Expr.Sym] =
    (symbol ~ ":" ~ symbol).map: (module, name) =>
      Expr.Sym(Some(module), name)

  def unqualified[$: P]: P[Expr.Sym] =
    symbol.map(Expr.Sym(None, _))

  qualified | unqualified

def template[$: P]: P[Expr.Template] =
  P("'" ~ part ~ part.rep ~ "'")
    .map((head, tail) => Expr.Template(head +: tail))
    .filter(_.parts.exists(_.isInstanceOf[Part.Capture]))

def funcDef[$: P]: P[Expr.FuncDef] =
  def funcDefN[$: P]: P[Expr.FuncDef] =
    P(
      sym ~ ws ~ "(" ~ ws ~ sym ~ (ws ~ "," ~ ws ~ sym).rep ~ ws ~ ")" ~ ws ~ ":" ~ ws ~ expr
    )
      .map: (name, head, tail, body) =>
        Expr.FuncDef(name, head +: tail, body)

  def funcDef0[$: P]: P[Expr.FuncDef] =
    P(sym ~ ws ~ ":" ~ ws ~ expr)
      .map: (name, body) =>
        Expr.FuncDef(name, Seq.empty, body)

  funcDefN | funcDef0

def funcCall[$: P]: P[Expr.FuncCall] =
  P(sym ~ ws ~ "(" ~ ws ~ expr ~ (ws ~ "," ~ ws ~ expr).rep ~ ws ~ ")")
    .map: (name, head, tail) =>
      Expr.FuncCall(name, head +: tail)

def container[$: P]: P[Expr.Container] =
  P(
    "from" ~ ws ~ str ~ ws ~ ":" ~ ws ~ instr
  )
    .map(Expr.Container.apply)

def module[$: P]: P[Expr.Module] =
  P("module" ~ ws ~ sym ~ ws ~ "<-" ~ ws ~ str).map(Expr.Module.apply)

def file[$: P]: P[Seq[Expr]] =
  P(ws ~ (expr ~ ws).rep ~ ws)

def parse(content: fastparse.ParserInputSource): Seq[Expr] =
  fastparse.parse(content, file(_)).get.value
