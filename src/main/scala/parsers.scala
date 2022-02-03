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
  import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}

  case class Program(funcs: List[Func], stat: Stat)
  case class Func(_type: Type, ident: Ident, params: ParamList, stat: Stat)
  case class ParamList(params: List[Param])
  case class Param(_type: Type, ident: Ident)

  sealed trait Stat
  case object Skip extends Stat with ParserBuilder[Stat] {val parser = pure(Skip)}
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
  case object WInt extends BaseType with ParserBuilder[BaseType]{val parser = pure(WInt)}
  case object WBool extends BaseType with ParserBuilder[BaseType]{val parser = pure(WBool)}
  case object WChar extends BaseType with ParserBuilder[BaseType]{val parser = pure(WChar)}
  case class ArrayType(_type: Type) extends Type with PairElemType
  case class PairType(fst_type: PairElemType, snd_type: PairElemType) extends Type
  sealed trait PairElemType
  case object Pair extends PairElemType with ParserBuilder[PairElemType]{val parser = pure(Pair)}

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
  case object Not extends UnaryOp with ParserBuilder[UnaryOp]{val parser = pure(Not)}
  case object Negate extends UnaryOp with ParserBuilder[UnaryOp]{val parser = pure(Negate)}
  case object Len extends UnaryOp with ParserBuilder[UnaryOp]{val parser = pure(Len)}
  case object Ord extends UnaryOp with ParserBuilder[UnaryOp]{val parser = pure(Ord)}
  case object Chr extends UnaryOp with ParserBuilder[UnaryOp]{val parser = pure(Chr)}

  sealed trait BinaryOp
  case object Mult extends BinaryOp with ParserBuilder[BinaryOp]{val parser = pure(Mult)}
  case object Div extends BinaryOp with ParserBuilder[BinaryOp]{val parser = pure(Div)}
  case object Mod extends BinaryOp with ParserBuilder[BinaryOp]{val parser = pure(Mod)}
  case object Plus extends BinaryOp with ParserBuilder[BinaryOp]{val parser = pure(Plus)}
  case object Minus extends BinaryOp with ParserBuilder[BinaryOp]{val parser = pure(Minus)}
  case object Greater extends BinaryOp with ParserBuilder[BinaryOp]{val parser = pure(Greater)}
  case object GreaterEq extends BinaryOp with ParserBuilder[BinaryOp]{val parser = pure(GreaterEq)}
  case object Less extends BinaryOp with ParserBuilder[BinaryOp]{val parser = pure(Less)}
  case object LessEq extends BinaryOp with ParserBuilder[BinaryOp]{val parser = pure(LessEq)}
  case object Eq extends BinaryOp with ParserBuilder[BinaryOp]{val parser = pure(Eq)}
  case object NotEq extends BinaryOp with ParserBuilder[BinaryOp]{val parser = pure(NotEq)}
  case object And extends BinaryOp with ParserBuilder[BinaryOp]{val parser = pure(And)}
  case object Or extends BinaryOp with ParserBuilder[BinaryOp]{val parser = pure(Or)}

  case class ArrayLiter(exprs: List[Expr]) extends AssignRHS

  trait ParserBuilder[T] {
    val parser: Parsley[T]
    final def <#(p: Parsley[_]): Parsley[T] = parser <* p
  }
  object Program {
    def apply(funcs: Parsley[List[Func]], stat: Parsley[Stat]): Parsley[Program] = (funcs, stat).zipped(Program(_, _))
  }
  object Func{
    def apply(_type: Parsley[Type], ident: Parsley[Ident], params: Parsley[ParamList], stat: Parsley[Stat]) : Parsley[Func] = (_type, ident, params, stat).zipped(Func(_,_,_,_))
  }
  object ParamList {
    def apply(params: Parsley[List[Param]]): Parsley[ParamList] = params.map(ParamList(_))
  }
  object Param{
    def apply(_type: Parsley[Type], ident: Parsley[Ident]) : Parsley[Param] = (_type, ident).zipped(Param(_,_))
  }

  object Decl {
    def apply(_type: Parsley[Type], ident: Parsley[Ident], rhs: Parsley[AssignRHS]): Parsley[Decl]
      = (_type, ident, rhs).zipped(Decl(_, _, _))
  }
  object Assign {
    def apply(lhs: Parsley[AssignLHS], rhs: Parsley[AssignRHS]): Parsley[Assign]
      = (lhs, rhs).zipped(Assign(_, _))
  }
  object Read {
    def apply(lhs: Parsley[AssignLHS]): Parsley[Read] = lhs.map(Read(_))
  }
  object Free {
    def apply(expr: Parsley[Expr]): Parsley[Free] = expr.map(Free(_))
  }
  object Return {
    def apply(expr: Parsley[Expr]): Parsley[Return] = expr.map(Return(_))
  }
  object Exit {
    def apply(expr: Parsley[Expr]): Parsley[Exit] = expr.map(Exit(_))
  }
  object Print {
    def apply(expr: Parsley[Expr]): Parsley[Print] = expr.map(Print(_))
  }
  object Println {
    def apply(expr: Parsley[Expr]): Parsley[Println] = expr.map(Println(_))
  }
  object IfElse {
    def apply(cond: Parsley[Expr], then_stat: Parsley[Stat], else_stat: Parsley[Stat]): Parsley[IfElse]
    = (cond, then_stat, else_stat).zipped(IfElse(_, _, _))
  }
  object While {
    def apply(cond: Parsley[Expr], body: Parsley[Stat]): Parsley[While]
    = (cond, body).zipped(While(_, _))
  }
  object Scope {
    def apply(stat: Parsley[Stat]): Parsley[Scope] = stat.map(Scope(_))
  }
  object Combine {
    def apply(fst: Parsley[Stat], snd: Parsley[Stat]): Parsley[Combine] = (fst, snd).zipped(Combine(_, _))
  }

  object NewPair {
    def apply(fst: Parsley[Expr], snd: Parsley[Expr]): Parsley[NewPair] = (fst, snd).zipped(NewPair(_,_))
  }
  object Call {
    def apply(ident: Parsley[Ident], argList: Parsley[ArgList]): Parsley[Call] = (ident, argList).zipped(Call(_,_))
  }

  object ArgList{
    def apply(args: Parsley[List[Expr]]): Parsley[ArgList] = args.map(ArgList(_))
  }

  object FstPair {
    def apply(fst: Parsley[Expr]): Parsley[FstPair] = fst.map(FstPair(_))
  }
  object SndPair {
    def apply(snd: Parsley[Expr]): Parsley[SndPair] = snd.map(SndPair(_))
  }

  object ArrayType{
    def apply(_type: Parsley[Type]) : Parsley[ArrayType] = _type.map(ArrayType(_))
  }
  object PairType {
    def apply(fst_type: Parsley[PairElemType], snd_type: Parsley[PairElemType]): Parsley[PairType]
    = (fst_type, snd_type).zipped(PairType(_,_))
  }

  object IntLiter {
    def apply(x: Parsley[Int]): Parsley[IntLiter] = x.map(IntLiter(_))
  }
  object BoolLiter {
    def apply(b: Parsley[Boolean]): Parsley[BoolLiter] = b.map(BoolLiter(_))
  }
  object CharLiter {
    def apply(c: Parsley[Char]): Parsley[CharLiter] = c.map(CharLiter(_))
  }
  object StrLiter {
    def apply(s: Parsley[String]): Parsley[StrLiter] = s.map(StrLiter(_))
  }
  object Ident {
    def apply(ident: Parsley[String]): Parsley[Ident] = ident.map(Ident(_))
  }

}