package parsers

import parsley.Parsley
import parsley.Parsley.pure

object Ast {
  import parsley.implicits.zipped.{Zipped2, Zipped3}

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
  sealed trait PairElemType extends AstNode with Type
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