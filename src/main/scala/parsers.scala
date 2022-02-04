import parsley.Parsley
import Parsley._
import parsley.character.anyChar
import parsley.combinator.sepBy1

import scala.language.implicitConversions

object lexer {
  import parsley.token.{LanguageDef, Lexer}
  import parsley.implicits.character.charLift
  import parsley.combinator.{eof, many}
  import parsley.character.{digit, isWhitespace}
  import parsley.token.Predicate

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

  val INT_LITER: Parsley[Int] = token(digit.foldLeft1(0)((x, d) => x * 10 + d.asDigit))
  val BOOL_LITER: Parsley[Boolean] = token(pure(true)) <|> token(pure(false))
  val CHAR_LITER: Parsley[Char] = token('\'') ~> anyChar <~ token('\'')
  val STR_LITER: Parsley[String] = token(many(CHAR_LITER).map(_.mkString))
  // val PAIR_LITER: Parsley[Null] = token("null")
  val ESC_CHAR: Parsley[Char] = token('0') <|> token('b') <|> token('t') <|> token('n') <|> token('f') <|> token('r') <|> token('"') <|> token('\'') <|> token('\\')

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
  import parsley.combinator.{some, many}
  import parsley.expr.{precedence, Ops, InfixR, InfixL, NonAssoc, Prefix}
  import lexer._
  import implicits.tokenLift
  import ast._


  private val `<ident>`: Parsley[Ident] = Ident(lex.identifier)
  private val `<array-liter>`: Parsley[ArrayLiter] = '[' ~> ArrayLiter(sepBy1(`<expr>`, ',')) <~ ']'
  /* TODO these shouldn't be returning BinaryOp they should be building application
      see <expr> in the spec e.g <expr> <binar-oper> <expr>
   */
  private lazy val `<binary-oper>`: Parsley[BinaryApp] =
    precedence(Ops[BinaryOp](InfixR)(Or <# "||" ),
               Ops[BinaryOp](InfixR)(And <# "&&"),
               Ops[BinaryOp](NonAssoc)(NotEq <# "!=", Eq <# "=="),
               Ops[BinaryOp](NonAssoc)(LessEq <# "<", Less <# "<",
                             Greater <# ">", GreaterEq <# ">="),
               Ops[BinaryOp](InfixL)(Minus <# "-", Plus <# "+"),
               Ops[BinaryOp](InfixL)(Mod <# "%", Div <# "/", Mult <# "*"))
  private lazy val `<unary-oper>`: Parsley[UnaryApp] =
    precedence(Ops(Prefix)(Not <# "!", Negate <# "-",
                             Len <# "len", Ord <# "ord", Chr <# "chr"))
  private val `<array-elem>`: Parsley[ArrayElem] = ArrayElem(`<ident>`, some('[' ~> `<expr>` <~ ']'))
  private lazy val `<expr>`: Parsley[Expr] =
    IntLiter(INT_LITER) <|> BoolLiter(BOOL_LITER) <|> CharLiter(CHAR_LITER) <|>
      StrLiter(STR_LITER) <|> Ident(lex.identifier) <|>
      `<array-elem>` <|> `<binary-oper>` <~> `<expr>` <|>
      `<expr>` <~ `<binary-oper>` <~> `<expr>`  <|>
      '(' ~> `<expr>` <~ ')'
  private val `<pair-elem-type>`: Parsley[PairElemType] = `<base-type>` <|> `<array-type>` <|> pure(Pair)
  private val `<pair-type>`: Parsley[PairType] =
    "pair" ~> '(' ~> PairType(`<pair-elem-type>` , ',' ~> `<pair-elem-type>`) <~ ')'
  private val `<array-type>`: Parsley[ArrayType] = ArrayType(`<type>`) <~ '[' <~ ']'
  // TODO figure out why or if we need parser builder for W types
  private val `<base-type>`: Parsley[BaseType] =
    (WInt <# "int") <|> (WBool <# "bool") <|> (WChar <# "char") <|> (WString <# "string")
  private val `<type>`: Parsley[Type] = `<base-type>` <|> `<array-type>` <|> `<pair-type>`
  private val `<pair-elem>`: Parsley[PairElem] =
    "fst" ~> FstPair(`<expr>`) <|> "snd" ~> SndPair(`<expr>`)
  private val `<arg-list>`: Parsley[ArgList] = ArgList(sepBy1(`<expr>`, ','))
  private val `<assign-rhs>`: Parsley[AssignRHS] =
    `<expr>` <|> `<array-liter>` <|>
      "newpair" ~> '(' ~> NewPair(`<expr>`, ',' ~> `<expr>`) <~ ')' <|>
      `<pair-elem>` <|> "call" ~> Call(`<ident>`, '(' ~> `<arg-list>`) <~ ')'
  private val `<assign-lhs>` = `<ident>` <|> `<array-elem>` <|> `<pair-elem>`
  private val `<stat>` =
    Skip <# "skip" <|> Decl(`<type>`, `<ident>`, '=' ~> `<assign-rhs>`) <|>
      Assign(`<assign-lhs>`, '=' ~> `<assign-rhs>`) <|>
      "read" ~> Read(`<assign-lhs>`) <|> "free" ~> Free(`<expr>`) <|>
      "return" ~> Return(`<expr>`) <|> "exit" ~> Exit(`<expr>`) <|>
      "print" ~> Print(`<expr>`) <|> "println" ~> Println(`<expr>`) <|>
      "if" ~> IfElse(`<expr>`, "then" ~> `<stat>`, "else" ~> `<stat>`) <~ "fi" <|>
      "while" ~> While(`<expr>`, "do" ~> `<stat>`) <~ "done" <|>
      "begin" ~> Scope(`<stat>`) <~ "end" <|> Combine(`<stat>`, ';' ~> `<stat>`)
  private val `<param>` = Param(`<type>`, `<ident>`)
  private val `<param-list>` = ParamList(sepBy1(`<param>`, ','))
  private val `<func>` =
    Func(`<type>`, `<ident>`, '(' ~> `<param-list>`, ')' ~> "is" ~> `<stat>`) <~ "end"
  private val `<program>` = fully("begin" ~> Program(many(`<func>`), `<stat>`) <~ "end")
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
    def apply(fst_type: Parsley[PairElemType], snd_type: Parsley[PairElemType]): Parsley[PairType] =
      (fst_type, snd_type).zipped(PairType(_,_))
  }

  object UnaryApp {
    def apply(op: Parsley[UnaryOp], expr: Parsley[Expr]): Parsley[UnaryApp] =
      (op, expr).zipped(UnaryApp(_, _))
  }
  object BinaryApp {
    def apply(lhs: Parsley[Expr], op: Parsley[BinaryOp], rhs: Parsley[Expr]): Parsley[BinaryApp] =
      (lhs, op, rhs).zipped(BinaryApp(_, _, _))
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

}