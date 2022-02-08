package parsers

import parsers.ast.Ident
import parsers.parser.`<expr>`
import parsley.Parsley._
import parsley.combinator.{sepBy, sepBy1}
import parsley.{Parsley, Result}

import java.io.File
import scala.annotation.tailrec
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

  // Don't use number and string literal parsers!
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

  private [parsers] lazy val `<expr>`: Parsley[Expr] =
    precedence(SOps(InfixR)  (Or <# "||") +:
               SOps(InfixR)  (And <# "&&") +:
               SOps(NonAssoc)(NotEq <# "!=", Eq <# "==") +:
               SOps(NonAssoc)(LessEq <# "<=", Less <# "<",
                              Greater <# ">", GreaterEq <# ">=") +:
               SOps(InfixL)  (Minus <# "-", Plus <# "+") +:
               SOps(InfixL)  (Mod <# "%", Div <# "/", Mult <# "*") +:
               SOps(Prefix)  (Not <# "!" , Negate <# "-",
                              Len <# "len", Ord <# "ord", Chr <# "chr") +:
               Atoms(`<expr-atoms>`))

  private [parsers] lazy val `<expr-atoms>`: Parsley[Term] =
    attempt(IntLiter(INT_LITER))   <|> BoolLiter(BOOL_LITER)     <|> CharLiter(CHAR_LITER) <|>
      attempt(StrLiter(STR_LITER)) <|> (PAIR_LITER #> PairLiter) <|> attempt(`<ident>`)    <|>
      `<array-elem>`               <|> ParensExpr('(' ~> `<expr>` <~ ')')

  private [parsers] lazy val `<array-elem>`: Parsley[ArrayElem] = ArrayElem(`<ident>`, some('[' ~> `<expr>` <~ ']'))

  private [parsers] lazy val `<pair-elem-type>`: Parsley[PairElemType] =
    attempt(Pair <# "pair") <|> `<base-type>` <|> `<array-type-pair-elem>`

  private [parsers] lazy val `<pair-type>`: Parsley[PairType] =
    PairType("pair" ~> '(' ~> `<pair-elem-type>` , ',' ~> `<pair-elem-type>` <~ ')')

  private [parsers] lazy val `<array-type>`: Parsley[Type] =
    chain.postfix(`<type-atoms>`, '[' ~> ']' #> ((_type: Type) => ArrayType(_type)))

  private [parsers] lazy val `<array-type-pair-elem>`: Parsley[PairElemType] =
    chain.postfix1(`<type-atoms>`, '[' ~> ']' #> ((_type: Type) => ArrayType(_type)))

  // TODO figure out why or if we need parser.parser builder for W types
  private [parsers] lazy val `<base-type>`: Parsley[BaseType] =
    (WInt <# "int") <|> (WBool <# "bool") <|> (WChar <# "char") <|> (WString <# "string")

  private [parsers] lazy val `<type>`: Parsley[Type] = `<array-type>`
  private [parsers] lazy val `<type-atoms>`: Parsley[TypeAtom] = `<base-type>` <|> `<pair-type>`

  private [parsers] lazy val `<pair-elem>`: Parsley[PairElem] =
    FstPair("fst" ~> `<expr>`) <|> SndPair("snd" ~> `<expr>`)

  private [parsers] lazy val `<arg-list>`: Parsley[ArgList] = ArgList(sepBy1(`<expr>`, ','))

  private [parsers] lazy val `<assign-rhs>`: Parsley[AssignRHS] =
    `<array-liter>` <|> NewPair("newpair" ~> '(' ~> `<expr>`, ',' ~> `<expr>` <~ ')') <|>
      `<pair-elem>` <|> Call("call" ~> `<ident>`, '(' ~> `<arg-list>` <~ ')') <|> `<expr>`

  private [parsers] lazy val `<assign-lhs>` =
    `<pair-elem>` <|> attempt(`<array-elem>`) <|> attempt(`<ident>`)

  private [parsers] lazy val `<stat>`: Parsley[Stat] = Combine(sepBy1(`<stat-atoms>`, ';'))

  private [parsers] lazy val `<stat-atoms>`: Parsley[StatAtom] =
    attempt(Skip <# "skip")                                                    <|>
      attempt(Assign(`<assign-lhs>`, '=' ~> `<assign-rhs>`))                   <|>
      Decl(`<type>`, `<ident>`, '=' ~> `<assign-rhs>`)                         <|>
      Read("read" ~> `<assign-lhs>`)      <|> Free("free" ~> `<expr>`)         <|>
      Return("return" ~> `<expr>`)        <|> Exit("exit" ~> `<expr>`)         <|>
      attempt(Print("print" ~> `<expr>`)) <|> Println("println" ~> `<expr>`)   <|>
      IfElse("if" ~> `<expr>`, "then" ~> `<stat>`, "else" ~> `<stat>` <~ "fi") <|>
      While("while" ~> `<expr>`, "do" ~> `<stat>` <~ "done")                   <|>
      Scope("begin" ~> `<stat>` <~ "end")

  private [parsers] lazy val `<param>` = Param(`<type>`, `<ident>`)

  private [parsers] lazy val `<param-list>` = ParamList(sepBy1(`<param>`, ','))

  //TODO return or exit needed before end
  private [parsers] lazy val `<func>` =
    Func(`<type>`, `<ident>`, '(' ~> option(`<param-list>`) <~ ')', "is" ~> `<stat>` <~ "end")

  private [parsers] lazy val `<program>` = fully(Program("begin" ~> many(attempt(`<func>`)), `<stat>` <~ "end"))





  def parse[Err: ErrorBuilder](input: String): Result[Err, Program] =
    `<program>`.parse(input)
  def parse[Err: ErrorBuilder](input: File): Result[Err, Program] =
    `<program>`.parseFromFile(input).get

  def main(args: Array[String]): Unit = {
    println(parse("begin int i = 5; int[5][6] = 9; pair(int, bool) pairTest = newpair(5, true) end"))
    //println(parse(new File("../wacc_examples/valid/expressions/intCalc.wacc")))
  }

}

object ast {
  import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}

  sealed trait AstNode

  case class Program(funcs: List[Func], stat: Stat) extends AstNode
  case class Func(_type: Type, ident: Ident, params: Option[ParamList], stat: Stat) extends AstNode
  case class ParamList(params: List[Param]) extends AstNode
  case class Param(_type: Type, ident: Ident) extends AstNode

  sealed trait Stat extends AstNode
  sealed trait StatAtom extends Stat
  case object Skip extends StatAtom with ParserBuilder[StatAtom] {val parser: Parsley[Skip.type] = pure(Skip)}
  case class Decl(_type: Type, ident: Ident, rhs: AssignRHS) extends StatAtom
  case class Assign(lhs: AssignLHS, rhs: AssignRHS) extends StatAtom
  case class Read(lhs: AssignLHS) extends StatAtom
  case class Free(expr: Expr) extends StatAtom
  case class Return(expr: Expr) extends StatAtom
  case class Exit(expr: Expr) extends StatAtom
  case class Print(expr: Expr) extends StatAtom
  case class Println(expr: Expr) extends StatAtom
  case class IfElse(cond: Expr, then_stat: Stat, then_else: Stat) extends StatAtom
  case class While(cond: Expr, body: Stat) extends StatAtom
  case class Scope(stat: Stat) extends StatAtom
  case class Combine(stats: List[StatAtom]) extends Stat

  sealed trait AssignLHS extends AstNode

  sealed trait AssignRHS extends AstNode
  case class NewPair(fst: Expr, snd: Expr) extends AssignRHS
  case class Call(ident: Ident, argList: ArgList) extends AssignRHS

  case class ArgList(args: List[Expr]) extends AstNode

  sealed trait PairElem extends AssignLHS with AssignRHS
  case class FstPair(fst: Expr) extends PairElem
  case class SndPair(snd: Expr) extends PairElem

  sealed trait Type extends AstNode
  sealed trait TypeAtom extends Type
  sealed trait BaseType extends TypeAtom with PairElemType
  case object WInt extends BaseType with ParserBuilder[BaseType]{val parser: Parsley[WInt.type] = pure(WInt)}
  case object WBool extends BaseType with ParserBuilder[BaseType]{val parser: Parsley[WBool.type] = pure(WBool)}
  case object WChar extends BaseType with ParserBuilder[BaseType]{val parser: Parsley[WChar.type] = pure(WChar)}
  case object WString extends BaseType with ParserBuilder[BaseType]{val parser: Parsley[WString.type] = pure(WString)}
  case class ArrayType(_type: Type) extends Type with PairElemType
  case class PairType(fst_type: PairElemType, snd_type: PairElemType) extends TypeAtom
  sealed trait PairElemType
  case object Pair extends PairElemType with ParserBuilder[PairElemType]{val parser: Parsley[Pair.type] = pure(Pair)}

  sealed trait Expr extends AssignRHS
  case class IntLiter(x: Int) extends Term
  case class BoolLiter(b: Boolean) extends Term
  case class CharLiter(c: Char) extends Term
  case class StrLiter(s: String) extends Term
  case object PairLiter extends Term
  case class Ident(ident: String) extends Term with AssignLHS
  case class ArrayElem(ident: Ident, expr: List[Expr]) extends Term with AssignLHS
  case class ParensExpr(expr: Expr) extends Term

  sealed trait Expr1 extends Expr
  case class Or(l_expr: Expr2, r_expr: Expr1) extends Expr1
  sealed trait Expr2 extends Expr1
  case class And(l_expr: Expr3, r_expr: Expr2) extends Expr2
  sealed trait Expr3 extends Expr2
  case class Eq(l_expr: Expr4, r_expr: Expr3) extends Expr3
  case class NotEq(l_expr: Expr4, r_expr: Expr3) extends Expr3
  sealed trait Expr4 extends Expr3
  case class Greater(l_expr: Expr5, r_expr: Expr4) extends Expr4
  case class GreaterEq(l_expr: Expr5, r_expr: Expr4) extends Expr4
  case class Less(l_expr: Expr5, r_expr: Expr4) extends Expr4
  case class LessEq(l_expr: Expr5, r_expr: Expr4) extends Expr4
  sealed trait Expr5 extends Expr4
  case class Plus(l_expr: Expr5, r_expr: Expr6) extends Expr5
  case class Minus(l_expr: Expr5, r_expr: Expr6) extends Expr5
  sealed trait Expr6 extends Expr5
  case class Mult(l_expr: Expr6, r_expr: Expr7) extends Expr6
  case class Div(l_expr: Expr6, r_expr: Expr7) extends Expr6
  case class Mod(l_expr: Expr6, r_expr: Expr7) extends Expr6
  sealed trait Expr7 extends Expr6
  case class Not(expr: Expr7) extends Expr7
  case class Negate(expr: Expr7) extends Expr7
  case class Len(expr: Expr7) extends Expr7
  case class Ord(expr: Expr7) extends Expr7
  case class Chr(expr: Expr7) extends Expr7
  sealed trait Term extends Expr7

  case class ArrayLiter(exprs: List[Expr]) extends AssignRHS

  trait ParserBuilder[T] {
    val parser: Parsley[T]
    final def <#(p: Parsley[_]): Parsley[T] = parser <* p
  }
  trait ParserBuilder1[T1, R] extends ParserBuilder[T1 => R] {
    def apply(x: T1): R
    val parser: Parsley[T1 => R] = pure(apply)
  }
  trait ParserBuilder2[T1, T2, R] extends ParserBuilder[(T1, T2) => R] {
    def apply(x: T1, y: T2): R
    val parser: Parsley[(T1, T2) => R] = pure(apply)
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
    def apply(stats: Parsley[List[StatAtom]]): Parsley[Combine] = stats.map(Combine(_))
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
  object ParensExpr {
    def apply(expr: Parsley[Expr]): Parsley[ParensExpr] = expr.map(ParensExpr(_))
  }

  object Not extends ParserBuilder1[Expr7, Expr7]
  object Negate extends ParserBuilder1[Expr7, Expr7]
  object Len extends ParserBuilder1[Expr7, Expr7]
  object Ord extends ParserBuilder1[Expr7, Expr7]
  object Chr extends ParserBuilder1[Expr7, Expr7]

  object Mult extends ParserBuilder2[Expr6, Expr7, Expr6]
  object Div extends ParserBuilder2[Expr6, Expr7, Expr6]
  object Mod extends ParserBuilder2[Expr6, Expr7, Expr6]
  object Plus extends ParserBuilder2[Expr5, Expr6, Expr5]
  object Minus extends ParserBuilder2[Expr5, Expr6, Expr5]
  object Greater extends ParserBuilder2[Expr5, Expr4, Expr4]
  object GreaterEq extends ParserBuilder2[Expr5, Expr4, Expr4]
  object Less extends ParserBuilder2[Expr5, Expr4, Expr4]
  object LessEq extends ParserBuilder2[Expr5, Expr4, Expr4]
  object Eq extends ParserBuilder2[Expr4, Expr3, Expr3]
  object NotEq extends ParserBuilder2[Expr4, Expr3, Expr3]
  object And extends ParserBuilder2[Expr3, Expr2, Expr2]
  object Or extends ParserBuilder2[Expr2, Expr1, Expr1]
}

class SymbolTable(val encSymTab: SymbolTable){

  import parsers.ast.AstNode

  val dictionary= scala.collection.mutable.Map[Ident, AstNode]()

  //types as before
  def add(name: Ident, obj: AstNode): Any = dictionary += (name -> obj)

  def lookup(name: Ident): AstNode = dictionary(name)

  def lookupAll(name: Ident): AstNode ={
    var s: SymbolTable = this
    while (s != null){
      val obj: AstNode = s.lookup(name)
      s = s.encSymTab
      if (obj != null){
        return obj
      }
    }
    null
  }

}

object semanticAnalysis {

  import parsers.ast._

  def traverse(node: AstNode, st: SymbolTable): Unit = {
    node match {
      case Decl(_type, ident, rhs) =>
        if (st.lookup(ident) != null) {
          println("scope error")
        } else {
          if (checkType(rhs) != _type) {
            println("type error")
          } else {
            st.add(ident, node)
          }
        }
      case Assign(_, rhs) =>
        //val t: Type = checkType(rhs)
        //need to find ident for lookup then also pull type
        //val n: AstNode = st.lookup(lhs)
        //compare types
        //if types match, add to st
    }

    def checkType(rhs: AssignRHS): Type = {
      rhs match {
        case ArrayLiter(exprs) =>
          var types: List[Type] = List()
          for (expr <- exprs) {
            types = checkType(expr) +: types
          }
          val same: Boolean = types.forall(_ == types.head)
          if (same) {
            types.head
          } else {
            null
          }
        //case NewPair(fst, snd) => PairType(checkExprType(fst), checkExprType(snd))
        case FstPair(expr) => checkExprType(expr)
        case SndPair(expr) => checkExprType(expr)
        //case Call(ident, argList) => st.lookupAll(ident)
        //case _ => checkExprType(rhs)
      }
    }

    @tailrec
    def checkExprType(expr: Expr): Type = {
      expr match {
        case IntLiter(_) => WInt
        case BoolLiter(_) => WBool
        case CharLiter(_) => WChar
        case StrLiter(_) => WString
        //case PairLiter => PairType()
        case Ident(_) => null
        case ParensExpr(expr) => checkExprType(expr)
        //case ArrayElem(_, exprs) => checkType(exprs.head)
        case Len(expr) =>
          if (unaryOperatorCheck(Len(expr))) {
            WInt
          } else {
            null
          }
        case Ord(expr) =>
          if (unaryOperatorCheck(Ord(expr))) {
            WInt
          } else {
            null
          }
        case Chr(expr) =>
          if (unaryOperatorCheck(Chr(expr))) {
            WChar
          } else {
            null
          }
        case Negate(expr) =>
          if (unaryOperatorCheck(Negate(expr))) {
            WInt
          } else {
            null
          }
        case Mult(expr1, expr2) =>
          if (binaryOperatorCheck(expr1, expr2, Mult(expr1, expr2))) {
            WInt
          } else {
            null
          }
        case Div(expr1, expr2) =>
          if (binaryOperatorCheck(expr1, expr2, Div(expr1, expr2))) {
            WInt
          } else {
            null
          }
        case Mod(expr1, expr2) =>
          if (binaryOperatorCheck(expr1, expr2, Mod(expr1, expr2))) {
            WInt
          } else {
            null
          }
        case Plus(expr1, expr2) =>
          if (binaryOperatorCheck(expr1, expr2, Plus(expr1, expr2))) {
            WInt
          } else {
            null
          }
        case Minus(expr1, expr2) =>
          if (binaryOperatorCheck(expr1, expr2, Minus(expr1, expr2))) {
            WInt
          } else {
            null
          }
        case Greater(expr1, expr2) =>
          if (binaryOperatorCheck(expr1, expr2, Greater(expr1, expr2))) {
            WInt
          } else {
            null
          }
        case GreaterEq(expr1, expr2) =>
          if (binaryOperatorCheck(expr1, expr2, GreaterEq(expr1, expr2))) {
            WInt
          } else {
            null
          }
        case Less(expr1, expr2) =>
          if (binaryOperatorCheck(expr1, expr2, Less(expr1, expr2))) {
            WInt
          } else {
            null
          }
        case LessEq(expr1, expr2) =>
          if (binaryOperatorCheck(expr1, expr2, LessEq(expr1, expr2))) {
            WInt
          } else {
            null
          }
        case Eq(expr1, expr2) =>
          if (binaryOperatorCheck(expr1, expr2, Eq(expr1, expr2))) {
            WInt
          } else {
            null
          }
        case NotEq(expr1, expr2) =>
          if (binaryOperatorCheck(expr1, expr2, NotEq(expr1, expr2))) {
            WInt
          } else {
            null
          }
        case And(expr1, expr2) =>
          if (binaryOperatorCheck(expr1, expr2, And(expr1, expr2))) {
            WBool
          } else {
            null
          }
        case Or(expr1, expr2) =>
          if (binaryOperatorCheck(expr1, expr2, Or(expr1, expr2))) {
            WBool
          } else {
            null
          }
        case Not(expr) =>
          if (unaryOperatorCheck(Not(expr))) {
            WBool
          } else {
            null
          }
      }
    }

    def binaryOperatorCheck(expr1: Expr, expr2: Expr, op: Expr): Boolean = {
      op match{
        case And(_,_) | Or(_,_) => checkExprType(expr1) == WBool && checkExprType(expr2) == WBool
        case _                  => checkExprType(expr1) == WInt  && checkExprType(expr2) == WInt
      }
    }

    def unaryOperatorCheck(op: Expr): Boolean ={
      op match{
        case Not(expr)    => checkExprType(expr) == WBool
        case Negate(expr) => checkExprType(expr) == WInt
        case Len(expr)    => checkExprType(expr) == WString
        case Ord(expr)    => checkExprType(expr) == WChar
        case Chr(expr)    => checkExprType(expr) == WInt
      }
    }

    def nodeType(node: AstNode): Type ={
      node match{
        //case NewPair(fst, snd) => PairType(checkType(fst), checkType(snd))
        case FstPair(expr) => checkType(expr)
        case SndPair(expr) => checkType(expr)

      }
    }

  }
}