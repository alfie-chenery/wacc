import parsley.Parsley
import Parsley._
import parsley.character.anyChar

import scala.language.implicitConversions

object lexer {
  import parsley.token.{LanguageDef, Lexer}
  import parsley.implicits.character.{charLift, stringLift}
  import parsley.combinator.{many, eof}
  import parsley.character.{digit, isWhitespace}
  import parsley.token.Predicate

  private val lang = LanguageDef.plain.copy(
    commentLine = "#",
    nestedComments = false,
    keywords = Set("begin", "end", "is", "skip", "read", "return", "if", "then", "else",
      "if", "then", "else", "fi", "while", "do", "end"),
    identStart = Predicate(c => c.isLetter || c == '_'),
    identLetter = Predicate(c => c.isLetterOrDigit || c == '_'),
    space = Predicate(isWhitespace),
  )

  // Dont use number and string literal parsers!
  val lexer = new Lexer(lang)

  val INT_LITER: Parsley[Int] = token(digit.foldLeft1(0)((x, d) => x * 10 + d.asDigit))
  val BOOL_LITER: Parsley[String] = token("true") <|> token("false")
  val CHAR_LITER: Parsley[Char] = token('\'') ~> anyChar <~ token('\'')
  val STR_LITER: Parsley[List[Char]] = token(many(CHAR_LITER))
  val PAIR_LITER: Parsley[String] = token("null")
  val ESC_CHAR: Parsley[Char] = token('0') <|> token('b') <|> token('t') <|> token('n') <|> token('f') <|> token('r') <|> token('"') <|> token('\'') <|> token('\\')

  private def token[A](p: =>Parsley[A]): Parsley[A] = lexer.lexeme(attempt(p))
  def fully[A](p: =>Parsley[A]): Parsley[A] = lexer.whiteSpace ~> p <~ eof

  object implicits {
    implicit def tokenLift(c: Char): Parsley[Unit]
    = void(lexer.symbol(c))
    implicit def tokenLift(s: String): Parsley[Unit] = {
      if (lang.keywords(s)) lexer.keyword(s)
      else void(lexer.symbol(s))
    }
  }


}

object parser {
  import parsley.combinator.{sepBy, sepBy1}

  import lexer._
  import implicits.tokenLift
  import ast._

}

object ast {
  case class Program(funcs: List[Func], stat: Stat)
  case class Func(_type: Type, ident: Ident, params: ParamList, stat: Stat)
  case class ParamList(params: List[Param])
  case class Param(_type: Type, ident: Ident)

  sealed trait Stat
  case object Skip extends Stat
  case class Decl(_type: Type, ident: Ident, rhs: AssignRHS) extends Stat
  case class Assign(lhs: AssignLHS, rhs: AssignRHS) extends Stat
  case class Read(lhs: AssignLHS) extends Stat
  case class Free(expr: Expr) extends Stat
  case class Return(expr: Expr) extends Stat
  case class Exit(expr: Expr) extends Stat
  case class Print(expr: Expr) extends Stat
  case class Println(expr: Expr) extends Stat
  case class IfElse(cond: Expr, then_stat: Stat, then_else: Stat) extends Stat
  case class While(cond: Expr, body: Stat) extends Stat
  case class Scope(stat: Stat) extends Stat
  case class Combine(first: Stat, second: Stat) extends Stat

  sealed trait AssignLHS

  sealed trait AssignRHS
  case class NewPair(fst: Expr, snd: Expr) extends AssignRHS
  case class Call(ident: Ident, argList: ArgList)

  case class ArgList(args: List[Expr])

  sealed trait PairElem extends AssignLHS with AssignRHS
  case class FstPair(fst: Expr) extends PairElem
  case class SndPair(snd: Expr) extends PairElem

  sealed trait Type
  sealed trait BaseType extends Type with PairElemType
  case object WInt extends BaseType
  case object WBool extends BaseType
  case object WChar extends BaseType
  case class ArrayType(_type: Type) extends Type with PairElemType
  case class PairType(fst_type: PairElemType, snd_type: PairElemType) extends Type
  sealed trait PairElemType
  case object Pair extends PairElemType

  sealed trait Expr extends AssignRHS
  case class IntLiter(x: Int) extends Expr
  case class BoolLiter(b: Boolean) extends Expr
  case class CharLiter(c: Char) extends Expr
  case class StrLiter(s: String) extends Expr
  case object PairLiter extends Expr
  case class Ident(ident: String) extends Expr with AssignLHS
  case class ArrayElem(ident: Ident, expr: Expr) extends Expr with AssignLHS
  case class UnaryApp(op: UnaryOp, expr: Expr)
  case class BinaryApp(lhs: Expr, op: BinaryOp, rhs: Expr)
  case class ParensExpr(expr: Expr)

  sealed trait UnaryOp
  case object Not extends UnaryOp
  case object Negate extends UnaryOp
  case object Len extends UnaryOp
  case object Ord extends UnaryOp
  case object Chr extends UnaryOp

  sealed trait BinaryOp
  case object Mult extends BinaryOp
  case object Div extends BinaryOp
  case object Mod extends BinaryOp
  case object Plus extends BinaryOp
  case object Minus extends BinaryOp
  case object Greater extends BinaryOp
  case object GreaterEq extends BinaryOp
  case object Less extends BinaryOp
  case object LessEq extends BinaryOp
  case object Eq extends BinaryOp
  case object NotEq extends BinaryOp
  case object And extends BinaryOp
  case object Or extends BinaryOp

  case class ArrayElem(ident: Ident, exprs: List[Expr])

  case class ArrayLiter(exprs: List[Expr]) extends AssignRHS

  case class Comment(com: String)


}
