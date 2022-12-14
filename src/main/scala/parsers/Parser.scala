package parsers

import parsley.Parsley._
import parsley.character.{digit, noneOf, satisfy}
import parsley.combinator.{many, sepBy, sepBy1}
import parsley.{Parsley, Result}

import java.io.File
import scala.:+
import scala.language.implicitConversions

object lexer {
  import parsley.character.{anyChar, digit, isWhitespace}
  import parsley.combinator.{eof, manyUntil,optional}
  import parsley.implicits.character.{charLift, stringLift}
  import parsley.token.{LanguageDef, Lexer, Predicate}

  val unaryOperators = Set("!", "-", "len", "ord", "chr")
  val binaryOperators = Set("*", "/", "%", "+", "-", ">", ">=", "<", "<", "<=", "==", "!=", "&&", "||")

  private val lang = LanguageDef.plain.copy(
    commentLine = "#",
    nestedComments = false,
    keywords = Set("begin", "end", "is", "skip", "read", "return", "if", "then", "else",
      "if", "then", "else", "fi", "while", "do", "end", "len", "ord", "chr", "print", "println"),
    operators = binaryOperators ++ unaryOperators,
    identStart = Predicate(c => c.isLetter || c == '_'),
    identLetter = Predicate(c => c.isLetterOrDigit || c == '_'),
    space = Predicate(isWhitespace),
  )

  val lex = new Lexer(lang)

  private [parsers] val NEG_INT_LITER : Parsley[Int] = token(("-") ~> digit
    .foldLeft1(0: Long)((x, d) => x * 10 - d.asDigit))
    .filter(l => l >= Int.MinValue).map(_.toInt)
  private [parsers] val INT_LITER: Parsley[Int] = NEG_INT_LITER <|> token(optional("+") ~> digit
    .foldLeft1(0: Long)((x, d) => x * 10 + d.asDigit))
    .filter(l => l <= Int.MaxValue).map(_.toInt)
  private [parsers] val FLOAT_START: Parsley[Int] = INT_LITER <~ token('.')
  private [parsers] val FLOAT_END: Parsley[Int] = (token('.') ~> INT_LITER)
  private [parsers] val FLOAT_LITER: Parsley[(Int, Int)] = FLOAT_START <~> FLOAT_END
  private [parsers] val BOOL_LITER: Parsley[Boolean] =
    (token("true") #> true) <|> (token("false") #> false)
  private [parsers] val ESC_CHAR: Parsley[String] = {
    '\\' ~> (('0' #> "\\u0000") <|> ('b' #> "\\b") <|> ('t' #> "\\t") <|> ('n' #> "\\n") <|>
      ('f' #> "\\f") <|> ('r' #> "\\r") <|> ('\"' #> "\\\"") <|> ('\'' #> "\\\'") <|> ('\\' #> "\\\\"))
  }

  private [parsers] val CHAR: Parsley[String] = ESC_CHAR <|> noneOf('\\', '\'', '\"').map(_.toString)
  private [parsers] val CHAR_LITER: Parsley[String] = token('\'' ~> CHAR <~ '\'')
  private [parsers] val STR_LITER: Parsley[String] = token('\"' ~> manyUntil(CHAR, '\"').map(_.mkString))
  private [parsers] val PAIR_LITER: Parsley[String] = token("null")

  private def token[A](p: =>Parsley[A]): Parsley[A] = lex.lexeme(attempt(p))
  def fully[A](p: =>Parsley[A]): Parsley[A] = lex.whiteSpace ~> p <~ eof

  object implicits {
    implicit def tokenLift(c: Char): Parsley[Unit]
    = void(lex.symbol(c))
    implicit def tokenLift(s: String): Parsley[Unit] = {
      if (lang.keywords(s)) lex.keyword(s)
      else if (lang.operators(s)) lex.maxOp(s)
      else void(lex.symbol(s))
    }
  }
}

object Parser {
  import Ast._
  import lexer._
  import implicits.tokenLift
  import parsley.combinator.{many, some}
  import parsley.errors.ErrorBuilder
  import parsley.expr._
  import parsley.io.ParseFromIO


  private [parsers] lazy val `<ident>`: Parsley[Ident] = attempt(Ident(lex.identifier))

  private [parsers] lazy val `<array-liter>`: Parsley[ArrayLiter] = ArrayLiter('[' ~> sepBy(`<expr>`, ',') <~ ']')

  private [parsers] lazy val `<expr>`: Parsley[Expr] =
    precedence(SOps(InfixR)  (Or <# "||") +:
               SOps(InfixR)  (And <# "&&") +:
               SOps(NonAssoc)(NotEq <# attempt("!="), Eq <# attempt("==")) +:
               SOps(NonAssoc)(LessEq <# attempt("<="), Less <# "<",
                              GreaterEq <# attempt(">="), Greater <# ">") +:
               SOps(InfixL)  (Minus <# "-", Plus <# "+") +:
               SOps(InfixL)  (Mod <# "%", Div <# "/", Mult <# "*") +:
               SOps(Prefix)  (Chr <# attempt("chr "), Len <# attempt("len "), Ord <# attempt("ord "), Not <# "!" , Negate <# attempt("-" <~ notFollowedBy(digit))) +:
               Atoms(`<expr-atoms>`))

  private [parsers] lazy val `<expr-atoms>`: Parsley[Term] =
    attempt(IntLiter(INT_LITER))       <|> attempt(BoolLiter(BOOL_LITER)) <|>
      attempt(CharLiter(CHAR_LITER))   <|> StrLiter(STR_LITER)  <|>
      attempt(PAIR_LITER #> PairLiter) <|> attempt(`<array-elem>`)        <|>
      `<ident>`                        <|> ParensExpr('(' ~> `<expr>` <~ ')')

  private [parsers] lazy val `<array-elem>`: Parsley[ArrayElem] = ArrayElem(`<ident>`, some('[' ~> `<expr>` <~ ']'))

  private [parsers] lazy val `<pair-elem-type>`: Parsley[PairElemType] =
    attempt(Pair <# "pair") <|> attempt(`<array-type-pair-elem>`) <|> `<base-type>`

  private [parsers] lazy val `<pair-type>`: Parsley[PairType] =
    PairType("pair" ~> '(' ~> `<pair-elem-type>` , ',' ~> `<pair-elem-type>` <~ ')')

  private [parsers] lazy val `<array-type>`: Parsley[Type] =
    chain.postfix(`<type-atoms>`, '[' ~> ']' #> ((_type: Type) => ArrayType(_type)))

  private [parsers] lazy val `<array-type-pair-elem>`: Parsley[PairElemType] =
    chain.postfix1(`<type-atoms>`, '[' ~> ']' #> ((_type: Type) => ArrayType(_type)))

  private [parsers] lazy val `<base-type>`: Parsley[BaseType] =
    (WInt <# "int") <|> (WBool <# "bool") <|> (WChar <# "char") <|> (WString <# "string") <|> (WFloat <# "float")

  private [parsers] lazy val `<type>`: Parsley[Type] = `<array-type>`
  private [parsers] lazy val `<type-atoms>`: Parsley[TypeAtom] = `<base-type>` <|> `<pair-type>`

  private [parsers] lazy val `<pair-elem>`: Parsley[PairElem] =
    FstPair("fst" ~> `<expr>`) <|> SndPair("snd" ~> `<expr>`)

  private [parsers] lazy val `<arg-list>`: Parsley[ArgList] = ArgList(sepBy(`<expr>`, ','))

  private [parsers] lazy val `<assign-rhs>`: Parsley[AssignRHS] =
    `<array-liter>` <|> attempt(NewPair("newpair" ~> '(' ~> `<expr>`, ',' ~> `<expr>` <~ ')')) <|>
      attempt(`<pair-elem>`) <|> attempt(Call("call" ~> `<ident>`, '(' ~> `<arg-list>`) <~ ')') <|>
      `<expr>`

  private [parsers] lazy val `<assign-lhs>` =
    attempt(`<pair-elem>`) <|> attempt(`<array-elem>`) <|> `<ident>`

  private [parsers] lazy val `<stat>`: Parsley[Stat] = Combine(sepBy1(`<stat-atoms>`, ';'))

  private [parsers] lazy val `<terminate-atom>`: Parsley[StatAtom] =
    Return("return" ~> `<expr>`) <|> Exit("exit" ~> `<expr>`) <|>
      IfElse("if" ~> `<expr>`, "then" ~> `<terminate-stat>`, "else" ~> `<terminate-stat>` <~ "fi") <|>
      While("while" ~> `<expr>`, "do" ~> `<terminate-stat>` <~ "done") <|>
      Scope("begin" ~> `<terminate-stat>` <~ "end")

  private [parsers] lazy val `<terminate-stat>` =
    Combine(many(attempt(`<stat-atoms>` <~ ";")) <**> `<terminate-atom>`.map[List[StatAtom] => List[StatAtom]](s => _ :+ s))

  private [parsers] lazy val `<stat-atoms>`: Parsley[StatAtom] =
    attempt(Assign(`<assign-lhs>`, '=' ~> `<assign-rhs>`))                                         <|>
      attempt(Decl(`<type>`, `<ident>`, '=' ~> `<assign-rhs>`))                                    <|>
      attempt(Skip <# "skip")                                                                      <|>
      attempt(Read("read" ~> `<assign-lhs>`))      <|> Free("free" ~> `<expr>`)                    <|>
      Return("return" ~> `<expr>`)        <|> Exit("exit" ~> `<expr>`)                             <|>
      attempt(Print("print" ~> `<expr>`)) <|>Println("println" ~> `<expr>`)                        <|>
      IfElse("if" ~> `<expr>`, "then" ~> `<stat>`, "else" ~> `<stat>` <~ "fi") <|>
      While("while" ~> `<expr>`, "do" ~> `<stat>` <~ "done")                             <|>
      Scope("begin" ~> `<stat>` <~ "end")

  private [parsers] lazy val `<param>` = Param(`<type>`, `<ident>`)

  private [parsers] lazy val `<param-list>` = ParamList(sepBy(`<param>`, ','))

  private [parsers] lazy val `<func>` =
    Func(attempt(`<type>` <~> `<ident>` <~ '('), `<param-list>`  <~ ')', "is" ~> `<terminate-stat>` <~ "end")

  private [parsers] lazy val `<program>` = fully(Program("begin" ~> many(`<func>`), `<stat>` <~ "end"))

  def parse[Err: ErrorBuilder](input: String): Result[Err, Program] =
    `<program>`.parse(input)
  def parse[Err: ErrorBuilder](input: File): Result[Err, Program] =
    `<program>`.parseFromFile(input).get
}
