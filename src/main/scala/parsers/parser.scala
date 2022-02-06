package parsers

import parsley.Parsley._
import parsley.combinator.sepBy1
import parsley.{Failure, Parsley, Result, Success}

import java.io.File
import scala.language.implicitConversions

object lexer {
  import parsley.character.{anyChar, digit, isWhitespace}
  import parsley.combinator.{eof, many}
  import parsley.implicits.character.{charLift, stringLift}
  import parsley.token.{LanguageDef, Lexer, Predicate}

  val binaryOperators = Set("!", "-", "len", "ord", "chr")
  val unaryOperators = Set("*", "/", "%", "+", ">", ">=", "<", "<", "<=", "==", "!=", "&&", "||")

  private val lang = LanguageDef.plain.copy(
    commentLine = "#",
    nestedComments = false,
    keywords = Set("begin", "end", "is", "skip", "read", "return", "if", "then", "else",
      "if", "then", "else", "fi", "while", "do", "end"),
    operators = binaryOperators ++ unaryOperators,
    identStart = Predicate(c => c.isLetter || c == '_'),
    identLetter = Predicate(c => c.isLetterOrDigit || c == '_'),
    space = Predicate(isWhitespace),
  )

  // Dont use number and string literal parsers!
  val lex = new Lexer(lang)

  // TODO implement negatives
  private [parsers] val INT_LITER: Parsley[Int] = token(digit.foldLeft1(0)((x, d) => x * 10 + d.asDigit))
  private [parsers] val BOOL_LITER: Parsley[Boolean] =
    (token("true") #> true) <|> (token("false") #> false)
  private [parsers] val CHAR_LITER: Parsley[Char] = token('\'') ~> anyChar <~ token('\'')
  private [parsers] val STR_LITER: Parsley[String] = token('\"') ~> token(many(anyChar).map(_.mkString)) <~ token('\"')
  private [parsers] val PAIR_LITER: Parsley[String] = token("null")
  // TODO make this its own case class holding the char
  private [parsers] val ESC_CHAR: Parsley[Char] = token('0') <|> token('b') <|> token('t') <|> token('n') <|> token('f') <|> token('r') <|> token('"') <|> token('\'') <|> token('\\')

  private def token[A](p: =>Parsley[A]): Parsley[A] = lex.lexeme(attempt(p))
  def fully[A](p: =>Parsley[A]): Parsley[A] = lex.whiteSpace ~> p <~ eof

  object implicits {
    implicit def tokenLift(c: Char): Parsley[Unit]
    = void(lex.symbol(c))
    implicit def tokenLift(s: String): Parsley[Unit] = {
      if (lang.keywords(s)) lex.keyword(s)
      else void(lex.symbol(s))
    }
  }
}

object parser {
  import ast._
  import lexer._
  import parsley.combinator.{many, some, option}
  import parsley.errors.ErrorBuilder
  import parsley.expr._
  import parsley.io.ParseFromIO
  import implicits.tokenLift


  // TODO Make sure this means identifiers can't be keywords
  private [parsers] lazy val `<ident>`: Parsley[Ident] = attempt(Ident(lex.identifier))
  private [parsers] lazy val `<array-liter>`: Parsley[ArrayLiter] = ArrayLiter('[' ~> sepBy1(`<expr>`, ',') <~ ']')
  private [parsers] lazy val `<binary-oper>`: Parsley[Expr] =
    precedence(`<expr>`)(SOps(InfixR)(Or <# "||"),
                         SOps(InfixR)(And <# "&&"),
                         SOps(NonAssoc)(NotEq <# "!=", Eq <# "=="),
                         SOps(NonAssoc)(LessEq <# "<=", Less <# "<",
                             Greater <# ">", GreaterEq <# ">="),
                         SOps(InfixL)(Minus <# "-", Plus <# "+"),
                         SOps(InfixL)(Mod <# "%", Div <# "/", Mult <# "*"))
  private [parsers] lazy val `<unary-oper>`: Parsley[Expr] =
    precedence(`<expr>`)(SOps(Prefix)(Not <# "!" , Negate <# "-",
                             Len <# "len", Ord <# "ord", Chr <# "chr"))
  private [parsers] lazy val `<array-elem>`: Parsley[ArrayElem] = ArrayElem(`<ident>`, some('[' ~> `<expr>` <~ ']'))
  private [parsers] lazy val `<expr>`: Parsley[Expr] =
    attempt(IntLiter(INT_LITER)) <|>
      BoolLiter(BOOL_LITER) <|> CharLiter(CHAR_LITER) <|> attempt(StrLiter(STR_LITER)) <|>
      (PAIR_LITER #> PairLiter) <|> attempt(`<ident>`) <|> `<array-elem>` <|>
      attempt(`<unary-oper>`) <|> attempt(`<binary-oper>`) <|>('(' ~> `<expr>` <~ ')')
  private [parsers] lazy val `<pair-elem-type>`: Parsley[PairElemType] =
    attempt(Pair <# "pair") <|> attempt(`<array-type>`) <|> attempt(`<base-type>`)
  private [parsers] lazy val `<pair-type>`: Parsley[PairType] =
    PairType("pair" ~> '(' ~> `<pair-elem-type>` , ',' ~> `<pair-elem-type>` <~ ')')
  private [parsers] lazy val `<array-type>`: Parsley[ArrayType] = ArrayType(`<type>` <~ '[' <~ ']')
  // TODO figure out why or if we need parser.parser builder for W types
  private [parsers] lazy val `<base-type>`: Parsley[BaseType] =
    (WInt <# "int") <|> (WBool <# "bool") <|> (WChar <# "char") <|> (WString <# "string")
  private [parsers] lazy val `<type>`: Parsley[Type] = `<base-type>` <|>  attempt(`<array-type>`) <|> `<pair-type>`
  private [parsers] lazy val `<pair-elem>`: Parsley[PairElem] =
    FstPair("fst" ~> `<expr>`) <|> SndPair("snd" ~> `<expr>`)
  private [parsers] lazy val `<arg-list>`: Parsley[ArgList] = ArgList(sepBy1(`<expr>`, ','))
  private [parsers] lazy val `<assign-rhs>`: Parsley[AssignRHS] =
    `<expr>` <|> `<array-liter>` <|>
      NewPair("newpair" ~> '(' ~> `<expr>`, ',' ~> `<expr>` <~ ')') <|>
      `<pair-elem>` <|> Call("call" ~> `<ident>`, '(' ~> `<arg-list>` <~ ')')
  private [parsers] lazy val `<assign-lhs>` = attempt(`<ident>`) <|> attempt(`<array-elem>`) <|> `<pair-elem>`
  private [parsers] lazy val `<stat>`: Parsley[Stat] =
    attempt(Skip <# "skip") <|> Decl(`<type>`, `<ident>`, '=' ~> `<assign-rhs>`) <|>
      attempt(Assign(`<assign-lhs>`, '=' ~> `<assign-rhs>`)) <|>
      Read("read" ~> `<assign-lhs>`) <|> Free("free" ~> `<expr>`) <|>
      Return("return" ~> `<expr>`) <|> Exit("exit" ~> `<expr>`) <|>
      attempt(Print("print" ~> `<expr>`)) <|> Println("println" ~> `<expr>`) <|>
      IfElse("if" ~> `<expr>`, "then" ~> `<stat>`, "else" ~> `<stat>` <~ "fi") <|>
      While("while" ~> `<expr>`, "do" ~> `<stat>` <~ "done") <|>
      Scope("begin" ~> `<stat>` <~ "end") //<|> Combine(`<stat>`, ';' ~> `<stat>`) //This causes infinite recursion
  private [parsers] lazy val `<param>` = Param(`<type>`, `<ident>`)
  private [parsers] lazy val `<param-list>` = ParamList(sepBy1(`<param>`, ','))
  private [parsers] lazy val `<func>` =
    Func(`<type>`, `<ident>`, '(' ~> option(`<param-list>`) <~ ')', "is" ~> `<stat>` <~ "end")
  private [parsers] lazy val `<program>` = fully(Program("begin" ~> many(attempt(`<func>`)), `<stat>` <~ "end"))

  def parse[Err: ErrorBuilder](input: String): Result[Err, Program] =
    `<program>`.parse(input)
  def parse[Err: ErrorBuilder](input: File): Result[Err, Program] =
    `<program>`.parseFromFile(input).get

}

object ast {
  import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}

  case class Program(funcs: List[Func], stat: Stat)
  case class Func(_type: Type, ident: Ident, params: Option[ParamList], stat: Stat)
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
  case class Call(ident: Ident, argList: ArgList) extends AssignRHS

  case class ArgList(args: List[Expr])

  sealed trait PairElem extends AssignLHS with AssignRHS
  case class FstPair(fst: Expr) extends PairElem
  case class SndPair(snd: Expr) extends PairElem

  sealed trait Type
  sealed trait BaseType extends Type with PairElemType
  case object WInt extends BaseType with ParserBuilder[BaseType]{val parser = pure(WInt)}
  case object WBool extends BaseType with ParserBuilder[BaseType]{val parser = pure(WBool)}
  case object WChar extends BaseType with ParserBuilder[BaseType]{val parser = pure(WChar)}
  case object WString extends BaseType with ParserBuilder[BaseType]{val parser = pure(WChar)}
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
  case class ArrayElem(ident: Ident, expr: List[Expr]) extends Expr with AssignLHS
  case class UnaryApp(op: UnaryOp, expr: Expr)
  case class BinaryApp(lhs: Expr, op: BinaryOp, rhs: Expr)
  case class ParensExpr(expr: Expr)

  sealed trait UnaryOp extends Expr
  case class Not(expr: Expr) extends UnaryOp
  case class Negate(expr: Expr) extends UnaryOp
  case class Len(expr: Expr) extends UnaryOp
  case class Ord(expr: Expr) extends UnaryOp
  case class Chr(expr: Expr) extends UnaryOp

  sealed trait BinaryOp extends Expr
  case class Mult(l_expr: Expr, r_expr: Expr) extends BinaryOp
  case class Div(l_expr: Expr, r_expr: Expr) extends BinaryOp
  case class Mod(l_expr: Expr, r_expr: Expr) extends BinaryOp
  case class Plus(l_expr: Expr, r_expr: Expr) extends BinaryOp
  case class Minus(l_expr: Expr, r_expr: Expr) extends BinaryOp
  case class Greater(l_expr: Expr, r_expr: Expr) extends BinaryOp
  case class GreaterEq(l_expr: Expr, r_expr: Expr) extends BinaryOp
  case class Less(l_expr: Expr, r_expr: Expr) extends BinaryOp
  case class LessEq(l_expr: Expr, r_expr: Expr) extends BinaryOp
  case class Eq(l_expr: Expr, r_expr: Expr) extends BinaryOp
  case class NotEq(l_expr: Expr, r_expr: Expr) extends BinaryOp
  case class And(l_expr: Expr, r_expr: Expr) extends BinaryOp
  case class Or(l_expr: Expr, r_expr: Expr) extends BinaryOp

  case class ArrayLiter(exprs: List[Expr]) extends AssignRHS

  trait ParserBuilder[T] {
    val parser: Parsley[T]
    final def <#(p: Parsley[_]): Parsley[T] = parser <* p
  }
  trait ParserBuilder1[T1, R] extends ParserBuilder[T1 => R] {
    def apply(x: T1): R
    val parser = pure(apply(_))
  }
  trait ParserBuilder2[T1, T2, R] extends ParserBuilder[(T1, T2) => R] {
    def apply(x: T1, y: T2): R
    val parser = pure(apply(_, _))
  }

  object Program {
    def apply(funcs: Parsley[List[Func]], stat: Parsley[Stat]): Parsley[Program] = (funcs, stat).zipped(Program(_, _))
  }
  object Func{
    def apply(_type: Parsley[Type], ident: Parsley[Ident], params: Parsley[Option[ParamList]], stat: Parsley[Stat]) : Parsley[Func] = (_type, ident, params, stat).zipped(Func(_,_,_,_))
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
    def apply(fst_type: Parsley[PairElemType], snd_type: Parsley[PairElemType]): Parsley[PairType] =
      (fst_type, snd_type).zipped(PairType(_,_))
  }

  object ArrayElem {
    def apply(ident: Parsley[Ident], expr: Parsley[List[Expr]]): Parsley[ArrayElem] =
      (ident, expr).zipped(ArrayElem(_,_))
  }
  object ArrayLiter {
    def apply(exprs: Parsley[List[Expr]]): Parsley[ArrayLiter] =
      exprs.map(ArrayLiter(_))
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

  object Not extends ParserBuilder1[Expr, Not]
  object Negate extends ParserBuilder1[Expr, Negate]
  object Len extends ParserBuilder1[Expr, Len]
  object Ord extends ParserBuilder1[Expr, Ord]
  object Chr extends ParserBuilder1[Expr, Chr]

  object Mult extends ParserBuilder2[Expr, Expr, Mult]
  object Div extends ParserBuilder2[Expr, Expr, Div]
  object Mod extends ParserBuilder2[Expr, Expr, Mod]
  object Plus extends ParserBuilder2[Expr, Expr, Plus]
  object Minus extends ParserBuilder2[Expr, Expr, Minus]
  object Greater extends ParserBuilder2[Expr, Expr, Greater]
  object GreaterEq extends ParserBuilder2[Expr, Expr, GreaterEq]
  object Less extends ParserBuilder2[Expr, Expr, Less]
  object LessEq extends ParserBuilder2[Expr, Expr, LessEq]
  object Eq extends ParserBuilder2[Expr, Expr, Eq]
  object NotEq extends ParserBuilder2[Expr, Expr, NotEq]
  object And extends ParserBuilder2[Expr, Expr, And]
  object Or extends ParserBuilder2[Expr, Expr, Or]
}