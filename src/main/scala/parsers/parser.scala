package parsers

import parsers.ast.Ident
import parsley.Parsley._
import parsley.combinator.{sepBy, sepBy1}
import parsley.{Parsley, Result}

import java.io.File
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

object lexer {
  import parsley.character.{anyChar, digit, isWhitespace}
  import parsley.combinator.{eof, manyUntil,optional}
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

  val lex = new Lexer(lang)

  // TODO implement negatives values
  private [parsers] val INT_LITER: Parsley[Int] = token(optional("+") ~> digit.foldLeft1(0)((x, d) => x * 10 + d.asDigit))
  private [parsers] val BOOL_LITER: Parsley[Boolean] =
    (token("true") #> true) <|> (token("false") #> false)
  private [parsers] val ESC_CHAR: Parsley[Char] =
    token('\\' ~> (('0' #> '\u0000') <|> ('b' #> '\b') <|> ('t' #> '\t') <|> ('n' #> '\n') <|>
      ('f' #> '\f') <|> ('r' #> '\r') <|> '\"' <|> '\'' <|> '\\'))
  private [parsers] val CHAR: Parsley[Char] = ESC_CHAR <|> anyChar
  private [parsers] val CHAR_LITER: Parsley[Char] = token('\'' ~> CHAR <~ '\'')
  private [parsers] val STR_LITER: Parsley[String] = token('\"') ~> token(manyUntil(CHAR, '\"').map(_.mkString))
  private [parsers] val PAIR_LITER: Parsley[String] = token("null")
  // TODO make this its own case class holding the char

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
  import implicits.tokenLift
  import parsley.combinator.{many, some}
  import parsley.errors.ErrorBuilder
  import parsley.expr._
  import parsley.io.ParseFromIO


  // TODO Make sure this means identifiers can't be keywords
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
               SOps(Prefix)  (Chr <# attempt("chr "), Len <# "len ", Ord <# "ord ", Not <# "!" , Negate <# "-") +:
               Atoms(`<expr-atoms>`))

  // TODO refactor this to put ident at the top and reduce backtracking
  private [parsers] lazy val `<expr-atoms>`: Parsley[Term] =
    attempt(IntLiter(INT_LITER))       <|> attempt(BoolLiter(BOOL_LITER)) <|>
      attempt(CharLiter(CHAR_LITER))   <|> attempt(StrLiter(STR_LITER))   <|>
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

  // TODO figure out why or if we need parser.parser builder for W types
  private [parsers] lazy val `<base-type>`: Parsley[BaseType] =
    (WInt <# "int") <|> (WBool <# "bool") <|> (WChar <# "char") <|> (WString <# "string")

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

  private [parsers] lazy val `<stat-atoms>`: Parsley[StatAtom] =
    attempt(Assign(`<assign-lhs>`, '=' ~> `<assign-rhs>`))                            <|>
      attempt(Decl(`<type>`, `<ident>`, '=' ~> `<assign-rhs>`))                       <|>
      attempt(Skip <# "skip")                                                         <|>
      attempt(Read("read" ~> `<assign-lhs>`))      <|> Free("free" ~> `<expr>`)         <|>
      Return("return" ~> `<expr>`)        <|> Exit("exit" ~> `<expr>`)                  <|>
      attempt(Print("print" ~> `<expr>`)) <|>Println("println" ~> `<expr>`)   <|>
      IfElse("if" ~> `<expr>`, "then" ~> `<stat>`, "else" ~> `<stat>` <~ "fi") <|>
      While("while" ~> `<expr>`, "do" ~> `<stat>` <~ "done")                            <|>
      Scope("begin" ~> `<stat>` <~ "end")

  private [parsers] lazy val `<param>` = Param(`<type>`, `<ident>`)

  private [parsers] lazy val `<param-list>` = ParamList(sepBy(`<param>`, ','))

  //TODO return or exit needed before end
  private [parsers] lazy val `<func>` =
    Func(attempt(`<type>` <~> `<ident>` <~ '('), `<param-list>`  <~ ')', "is" ~> `<stat>` <~ "end")

  private [parsers] lazy val `<program>` = fully(Program("begin" ~> many(`<func>`), `<stat>` <~ "end"))

  def parse[Err: ErrorBuilder](input: String): Result[Err, Program] =
    `<program>`.parse(input)
  def parse[Err: ErrorBuilder](input: File): Result[Err, Program] =
    `<program>`.parseFromFile(input).get

  def main(args: Array[String]): Unit = {
    val validPrograms = new File("../wacc_examples/valid/")
    def findPrograms(file: File): Unit = {
      val files: List[File] = file.listFiles().toList
      for (currFile: File <- files) {
        if (currFile.isFile) {
          val program = parse(currFile)
          if (program.isSuccess) {
              println(currFile.getName + ": " + program.get)
              val renamedProgram = renamingPass.rename(program.get)
              println(currFile.getName + " (transform): " + renamedProgram)
            try {
              println(currFile.getName + " (type checked):" + semanticAnalysis.traverse(renamedProgram) +"\n")
            } catch {
              case e: Exception => println(e.printStackTrace())
            }
          } else {
            println(currFile.getName + ": " + program)
          }
        }
        else findPrograms(currFile)
      }
    }
    findPrograms(validPrograms)
  }
}

object ast {
  import parsley.implicits.zipped.{Zipped2, Zipped3 }

  sealed trait AstNode

  case class Program(funcs: List[Func], stat: Stat) extends AstNode
  case class Func(ident: (Type, Ident), params: ParamList, stat: Stat) extends AstNode
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
  sealed trait PairElemType extends AstNode
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
    def apply(ident: Parsley[(Type, Ident)], params: Parsley[ParamList], stat: Parsley[Stat]) : Parsley[Func] = (ident, params, stat).zipped(Func(_,_,_))
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

  val dictionary: mutable.Map[Ident, AstNode] = scala.collection.mutable.Map[Ident, AstNode]()

  //types as before
  def add(name: Ident, obj: AstNode): Any = dictionary += (name -> obj)

  def replace(name: Ident, obj: AstNode): Unit ={
    dictionary(name) = obj
  }

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

object Unary{
  import parsers.ast._
  def unapply(expr: Expr): Option[Expr] = expr match{
    case Not(x)    => Some(x)
    case Negate(x) => Some(x)
    case Len(x)    => Some(x)
    case Ord(x)    => Some(x)
    case Chr(x)    => Some(x)
    case _         => None
  }
}

object intComparisonBinary{
  import parsers.ast._
  def unapply(expr: Expr): Option[(Expr, Expr)] = expr match{
    case Greater(x, y)   => Some(x, y)
    case GreaterEq(x, y) => Some(x, y)
    case Less(x, y)      => Some(x, y)
    case LessEq(x, y)    => Some(x, y)
    case Eq(x, y)        => Some(x, y)
    case NotEq(x, y)     => Some(x, y)
    case _               => None
  }
}

object renamingPass {

  import parsers.ast._

  def renameIdent(ident: String, localScope: mutable.Map[String,String], varsInScope: mutable.Map[String, String]): Ident = {
    if (localScope.contains(ident)) {
      println(s"identifier $ident already declared in the current scope")
    }
    var renamedIdent = ""
    if(varsInScope.contains(ident)) {
      val nextIdentInt = varsInScope(ident).split('$')(1).toInt+1
      renamedIdent = ident+"$"+s"$nextIdentInt"
    } else {
      renamedIdent = ident + "$0"
    }
    varsInScope += (ident -> renamedIdent)
    localScope += (ident -> renamedIdent)
    Ident(localScope(ident))
  }

  private def rename(program: AstNode, localScope: mutable.Map[String, String], varsInScope: mutable.Map[String, String]): AstNode = {
    program match {
      case Program(funcs, stat) =>
        val renamedFuncs: ListBuffer[Func] = ListBuffer[Func]()
        for (func <- funcs) {
          func match {
            case Func((_, Ident(ident)), _, _) => renameIdent(ident+"$func", localScope, varsInScope)
          }
        }
        funcs.foreach(renamedFuncs += rename(_, localScope, varsInScope).asInstanceOf[Func])
        Program(renamedFuncs.toList, rename(stat, localScope, varsInScope).asInstanceOf[Stat])

      case Func((_type, Ident(ident)), ParamList(params), stat) =>
        val newScope = scala.collection.mutable.Map[String, String]()
        val renamedParams: ListBuffer[Param] = ListBuffer()
        for (param <- params) {
          param match {
            case Param(_type, Ident(ident)) =>
              renamedParams += Param(_type, renameIdent(ident, newScope, varsInScope))
            case _ =>
          }
        }
        Func((_type, Ident(varsInScope(ident+"$func"))), ParamList(renamedParams.toList),
          rename(stat, newScope, varsInScope).asInstanceOf[Stat])

      case Decl(_type, Ident(ident), rhs) =>
        // TODO lookup variable to see if it's declared in current scope, if not, add it
        val renamedRhs = rename(rhs, localScope, varsInScope).asInstanceOf[AssignRHS]
        Decl(_type, renameIdent(ident, localScope, varsInScope), renamedRhs)

      case Assign(lhs, rhs) =>
        Assign(rename(lhs, localScope, varsInScope).asInstanceOf[AssignLHS],
               rename(rhs, localScope, varsInScope).asInstanceOf[AssignRHS])
        // TODO check that lhs is declared in the current or higher scope

      case Ident(ident) =>
        if (!varsInScope.contains(ident)) {
          println(s"variable $ident not declared")
        }
        Ident(varsInScope(ident))

      case ParensExpr(expr) => ParensExpr(rename(expr, localScope, varsInScope).asInstanceOf[Expr])

      case Or(lhs, rhs) =>
        Or(rename(lhs, localScope, varsInScope).asInstanceOf[Expr2],
           rename(rhs, localScope, varsInScope).asInstanceOf[Expr1])
      case And(lhs, rhs) =>
        And(rename(lhs, localScope, varsInScope).asInstanceOf[Expr3],
            rename(rhs, localScope, varsInScope).asInstanceOf[Expr2])
      case Eq(lhs, rhs) =>
        Eq(rename(lhs, localScope, varsInScope).asInstanceOf[Expr4],
           rename(rhs, localScope, varsInScope).asInstanceOf[Expr3])
      case NotEq(lhs, rhs) =>
        NotEq(rename(lhs, localScope, varsInScope).asInstanceOf[Expr4],
              rename(rhs, localScope, varsInScope).asInstanceOf[Expr3])
      case Greater(lhs, rhs) =>
        Greater(rename(lhs, localScope, varsInScope).asInstanceOf[Expr5],
                rename(rhs, localScope, varsInScope).asInstanceOf[Expr4])
      case GreaterEq(lhs, rhs) =>
        GreaterEq(rename(lhs, localScope, varsInScope).asInstanceOf[Expr5],
                  rename(rhs, localScope, varsInScope).asInstanceOf[Expr4])
      case Less(lhs, rhs) =>
        Less(rename(lhs, localScope, varsInScope).asInstanceOf[Expr5],
             rename(rhs, localScope, varsInScope).asInstanceOf[Expr4])
      case LessEq(lhs, rhs) =>
        LessEq(rename(lhs, localScope, varsInScope).asInstanceOf[Expr5],
               rename(rhs, localScope, varsInScope).asInstanceOf[Expr4])
      case Plus(lhs, rhs) =>
        Plus(rename(lhs, localScope, varsInScope).asInstanceOf[Expr5],
             rename(rhs, localScope, varsInScope).asInstanceOf[Expr6])
      case Minus(lhs, rhs) =>
        Plus(rename(lhs, localScope, varsInScope).asInstanceOf[Expr5],
             rename(rhs, localScope, varsInScope).asInstanceOf[Expr6])
      case Mult(lhs, rhs) =>
        Mult(rename(lhs, localScope, varsInScope).asInstanceOf[Expr6],
             rename(rhs, localScope, varsInScope).asInstanceOf[Expr7])
      case Div(lhs, rhs) =>
        Mult(rename(lhs, localScope, varsInScope).asInstanceOf[Expr6],
             rename(rhs, localScope, varsInScope).asInstanceOf[Expr7])
      case Mod(lhs, rhs) =>
        Mult(rename(lhs, localScope, varsInScope).asInstanceOf[Expr6],
             rename(rhs, localScope, varsInScope).asInstanceOf[Expr7])
      case Not(expr) => Not(rename(expr, localScope, varsInScope).asInstanceOf[Expr7])
      case Negate(expr) => Negate(rename(expr, localScope, varsInScope).asInstanceOf[Expr7])
      case Len(expr) => Len(rename(expr, localScope, varsInScope).asInstanceOf[Expr7])
      case Ord(expr) => Ord(rename(expr, localScope, varsInScope).asInstanceOf[Expr7])
      case Chr(expr) => Chr(rename(expr, localScope, varsInScope).asInstanceOf[Expr7])

      case ArrayElem(ident, exprs) =>
        val renamedExprs: ListBuffer[Expr] = ListBuffer[Expr]()
        exprs.foreach(renamedExprs += rename(_, localScope, varsInScope).asInstanceOf[Expr])
        ArrayElem(rename(ident, localScope, varsInScope).asInstanceOf[Ident], renamedExprs.toList)

      case FstPair(expr) => FstPair(rename(expr, localScope, varsInScope).asInstanceOf[Expr])

      case SndPair(expr) => SndPair(rename(expr, localScope, varsInScope).asInstanceOf[Expr])

      case Read(lhs) => Read(rename(lhs, localScope, varsInScope).asInstanceOf[AssignLHS])
        // TODO check that lhs is declared in the current or higher scope

      case Free(expr) => Free(rename(expr, localScope, varsInScope).asInstanceOf[Expr])

      case Return(expr) => Return(rename(expr, localScope, varsInScope).asInstanceOf[Expr])

      case Exit(expr) =>  Exit(rename(expr, localScope, varsInScope).asInstanceOf[Expr])

      case Print(expr) => Print(rename(expr, localScope, varsInScope).asInstanceOf[Expr])

      case Println(expr) => Println(rename(expr, localScope, varsInScope).asInstanceOf[Expr])

      case IfElse(cond, then_stat, else_stat) =>
        // TODO check if any variables in cond are defined in the current scope
        // TODO enter a new scope for both bodys
        val newScope = scala.collection.mutable.Map[String, String]()
        IfElse(rename(cond, localScope, varsInScope).asInstanceOf[Expr],
               rename(then_stat, newScope, varsInScope.clone()).asInstanceOf[Stat],
               rename(else_stat, newScope,varsInScope.clone()).asInstanceOf[Stat])

      case While(cond, body) =>
        // TODO check if any variables in cond are defined in the current scope
        // TODO enter a new scope for the body
        val newScope = scala.collection.mutable.Map[String, String]()
        While(rename(cond, localScope, varsInScope).asInstanceOf[Expr],
              rename(body, newScope, varsInScope.clone()).asInstanceOf[Stat])

      case Scope(stat) =>
        // TODO enter a new scope
        val newScope = scala.collection.mutable.Map[String, String]()
        Scope(rename(stat, newScope, varsInScope.clone()).asInstanceOf[Stat])

      case Combine(stats) =>
        val renamedStats: ListBuffer[StatAtom] = ListBuffer()
        stats.foreach(renamedStats += rename(_, localScope, varsInScope).asInstanceOf[StatAtom])
        Combine(renamedStats.toList)

      case NewPair(fst, snd) =>
        NewPair(rename(fst, localScope, varsInScope).asInstanceOf[Expr],
                rename(snd, localScope, varsInScope).asInstanceOf[Expr])

      case Call(Ident(ident), ArgList(args)) =>
        // TODO check that ident is declared in this scope
        if (!varsInScope.contains(ident+"$func")) {
          println(s"function $ident not defined")
        }
        val renamedArgs: ListBuffer[Expr] = ListBuffer()
        args.foreach(renamedArgs += rename(_, localScope, varsInScope).asInstanceOf[Expr])
        Call(Ident(varsInScope(ident+"$func")), ArgList(renamedArgs.toList))

      case node: AstNode => node
    }
  }
  def rename(program: Program): Program = {
    val variableRenaming: mutable.Map[String, String] = scala.collection.mutable.Map[String, String]()
    val localScope: mutable.Map[String, String] = scala.collection.mutable.Map[String, String]()
    rename(program, localScope, variableRenaming).asInstanceOf[Program]
  }

}

object semanticAnalysis {
  import parsers.ast._

  val st: mutable.Map[Ident, (AstNode, Type)] = scala.collection.mutable.Map[Ident, (AstNode, Type)]()

  def traverse(node: AstNode): Unit = {
    node match {
      case Program(funcs, stat) =>
        for (func <- funcs){
          func match {
            case Func((_type, ident), _, _) => st += (ident -> (func, _type))
          }
        }
        funcs.foreach(traverse(_))
        traverse(stat)

      case Func((_type, ident), ParamList(params), stat) =>
        for (param <- params){
          traverse(param)
        }
        traverse(stat)
      case Param(_type, ident) => st += (ident -> (node, _type))

      // <Stat>
      // TODO type check expr
      case Skip =>
      case Decl(_type, ident, rhs) =>
        if (_type != checkType(rhs)) {
          println(s"The right hand side does not match type ${_type}")
        } else {
          st += (ident -> (rhs, _type))
        }
      case Assign(lhs, rhs) =>
        if (checkType(lhs) != checkType(rhs)) {
          println(s"The type of the left hand side does not match the right hand side")
        }
      case Read(lhs) => //TODO check this
        /*
        val _type = checkType(lhs)
        if(_type != WChar || _type != WInt) {
          println("type error")
        }
         */
      case Free(expr) => // TODO come back to this
        val _type = checkExprType(expr)
        if (_type.isInstanceOf[PairType] || _type.isInstanceOf[ArrayType]) {
          println("Free must be give a Pair or array expression")
        }
      case Print(expr) => checkExprType(expr)
      case Println(expr) => checkExprType(expr)
      case Return(expr) => // TODO come back to this
      case Exit(expr) => // TODO come back to this
      case IfElse(cond, stat_true, stat_false) =>
        if (checkExprType(cond) != WBool){
          println("Condition must be a boolean expression")
        }
        traverse(stat_true)
        traverse(stat_false)
      case While(cond, stat) =>
        if (checkExprType(cond) != WBool){
          println("Condition must be a boolean expression")
        }
        traverse(stat)
      case Scope(stat) => traverse(stat)
      case Combine(stats) => stats.foreach(traverse(_))
      case _ =>
    }

    def checkType(node: AstNode): Type = {
      node match {
        //AssignLHS
        case ident: Ident => st(ident)._2
        case ArrayElem(ident, exprs) => st(ident)._2 //TODO
        case FstPair(ident:Ident) => st(ident)._2
        case SndPair(ident:Ident) => st(ident)._2

        //AssignRHS
        case expr:Expr => checkExprType(expr)
        case ArrayLiter(exprs) =>
          var types: List[Type] = List()
          for (expr <- exprs) {
            types = checkExprType(expr) +: types
          }
          val same: Boolean = types.forall(_ == types.head)
          if (same) {
            ArrayType(types.head)
          } else {
            println("All elements of an array must have the same type")
            null
          }
        case PairLiter => PairType(Pair, Pair)
        case NewPair(PairLiter, PairLiter) => PairType(Pair, Pair)
        case NewPair(PairLiter, e2) => PairType(Pair, checkExprType(e2).asInstanceOf[PairElemType])
        case NewPair(e1, PairLiter) => PairType(checkExprType(e1).asInstanceOf[PairElemType], Pair)
        case NewPair(e1, e2) =>
          PairType(checkExprType(e1).asInstanceOf[PairElemType], checkExprType(e2).asInstanceOf[PairElemType])
        // TODO check the param types match
        case Call(ident, _) => st(ident)._2
      }
    }

    def checkExprType(expr: AstNode): Type = {
      expr match {
        case IntLiter(_) => WInt
        case BoolLiter(_) => WBool
        case CharLiter(_) => WChar
        case StrLiter(_) => WString
        // TODO check this
        case PairLiter => null
        case ident:Ident => st(ident)._2
        case ParensExpr(expr) => checkExprType(expr)
        case ArrayElem(ident, _) => checkType(st(ident)._1)
        case Unary(x) => checkExprType(x)
        case And(x, y) =>
          if (!(checkExprType(x) == WBool && checkExprType(y) == WBool)) {
            println(s"Both sides of the expression && must be Integer")
          }
          WBool
        case Or(x, y) =>
          if (!(checkExprType(x) == WBool && checkExprType(y) == WBool)) {
            println(s"Both sides of the expression || must be Integer")
          }
          WBool
        case Mult(x, y) =>
          if (!(checkExprType(x) == WInt && checkExprType(y) == WInt)) {
            println(s"Both sides of the expression * must be Integer")
          }
          WInt
        case Div(x, y) =>
          if (!(checkExprType(x) == WInt && checkExprType(y) == WInt)) {
            println(s"Both sides of the expression / must be Integer")
          }
          WInt
        case Mod(x, y) =>
          if (!(checkExprType(x) == WInt && checkExprType(y) == WInt)) {
            println(s"Both sides of the expression % must be Integer")
          }
          WInt
        case Plus(x, y) =>
          if (!(checkExprType(x) == WInt && checkExprType(y) == WInt)) {
            println(s"Both sides of the expression + must be Integer")
          }
          WInt
        case intComparisonBinary(x, y) =>
          val e1_type = checkExprType(x)
          val e2_type = checkExprType(y)
          if(!((e1_type == WInt &&  e2_type== WInt) || (e1_type == WBool &&  e2_type== WBool))){
            println("Both sides of the expression must be Integers or Characters")
          }
          WBool
      }
    }
  }
}