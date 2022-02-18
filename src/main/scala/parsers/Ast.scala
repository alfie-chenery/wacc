package parsers

import parsley.Parsley
import parsley.Parsley.pos

object Ast {
  import parsley.implicits.zipped.{LazyZipped2, LazyZipped3}

  trait AstNode{
    val pos: (Int, Int)
  }

  case class Program(funcs: List[Func], stat: Stat)(val pos: (Int, Int)) extends AstNode
  case class Func(ident: (Type, Ident), params: ParamList, stat: Stat)(val pos: (Int, Int)) extends AstNode
  case class ParamList(params: List[Param])(val pos: (Int, Int)) extends AstNode
  case class Param(_type: Type, ident: Ident)(val pos: (Int, Int)) extends AstNode

  sealed trait Stat extends AstNode
  sealed trait StatAtom extends Stat
  case class Skip()(val pos: (Int, Int)) extends StatAtom
  case class Decl(_type: Type, ident: Ident, rhs: AssignRHS)(val pos: (Int, Int)) extends StatAtom
  case class Assign(lhs: AssignLHS, rhs: AssignRHS)(val pos: (Int, Int)) extends StatAtom
  case class Read(lhs: AssignLHS)(val pos: (Int, Int)) extends StatAtom
  case class Free(expr: Expr)(val pos: (Int, Int)) extends StatAtom
  case class Return(expr: Expr)(val pos: (Int, Int)) extends StatAtom
  case class Exit(expr: Expr)(val pos: (Int, Int)) extends StatAtom
  case class Print(expr: Expr)(val pos: (Int, Int)) extends StatAtom
  case class Println(expr: Expr)(val pos: (Int, Int)) extends StatAtom
  case class IfElse(cond: Expr, then_stat: Stat, then_else: Stat)(val pos: (Int, Int)) extends StatAtom
  case class While(cond: Expr, body: Stat)(val pos: (Int, Int)) extends StatAtom
  case class Scope(stat: Stat)(val pos: (Int, Int)) extends StatAtom
  case class Combine(stats: List[StatAtom])(val pos: (Int, Int)) extends Stat

  sealed trait AssignLHS extends AstNode

  sealed trait AssignRHS extends AstNode
  case class NewPair(fst: Expr, snd: Expr)(val pos: (Int, Int)) extends AssignRHS
  case class Call(ident: Ident, argList: ArgList)(val pos: (Int, Int)) extends AssignRHS

  case class ArgList(args: List[Expr])(val pos: (Int, Int)) extends AstNode

  sealed trait PairElem extends AssignLHS with AssignRHS
  case class FstPair(fst: Expr)(val pos: (Int, Int)) extends PairElem
  case class SndPair(snd: Expr)(val pos: (Int, Int)) extends PairElem

  sealed trait Type extends AstNode
  sealed trait TypeAtom extends Type
  sealed trait BaseType extends TypeAtom with PairElemType
  case class WInt()(val pos: (Int, Int)) extends BaseType
  case class WBool()(val pos: (Int, Int)) extends BaseType
  case class WChar()(val pos: (Int, Int)) extends BaseType
  case class WString()(val pos: (Int, Int)) extends BaseType
  case class ArrayType(_type: Type)(val pos: (Int, Int)) extends Type with PairElemType
  case class PairType(fst_type: PairElemType, snd_type: PairElemType)(val pos: (Int, Int)) extends TypeAtom
  sealed trait PairElemType extends AstNode with Type
  case class Pair()(val pos: (Int, Int)) extends PairElemType

  sealed trait Expr extends AssignRHS
  case class IntLiter(x: Int)(val pos: (Int, Int)) extends Term
  case class BoolLiter(b: Boolean)(val pos: (Int, Int)) extends Term
  case class CharLiter(c: Char)(val pos: (Int, Int)) extends Term
  case class StrLiter(s: String)(val pos: (Int, Int)) extends Term
  case class PairLiter()(val pos: (Int, Int)) extends Term
  case class Ident(ident: String)(val pos: (Int, Int)) extends Term with AssignLHS
  case class ArrayElem(ident: Ident, expr: List[Expr])(val pos: (Int, Int))extends Term with AssignLHS
  case class ParensExpr(expr: Expr)(val pos: (Int, Int)) extends Term

  sealed trait Expr1 extends Expr
  case class Or(l_expr: Expr2, r_expr: Expr1)(val pos: (Int, Int)) extends Expr1
  sealed trait Expr2 extends Expr1
  case class And(l_expr: Expr3, r_expr: Expr2)(val pos: (Int, Int)) extends Expr2
  sealed trait Expr3 extends Expr2
  case class Eq(l_expr: Expr4, r_expr: Expr3)(val pos: (Int, Int)) extends Expr3
  case class NotEq(l_expr: Expr4, r_expr: Expr3)(val pos: (Int, Int)) extends Expr3
  sealed trait Expr4 extends Expr3
  case class Greater(l_expr: Expr5, r_expr: Expr4)(val pos: (Int, Int)) extends Expr4
  case class GreaterEq(l_expr: Expr5, r_expr: Expr4)(val pos: (Int, Int)) extends Expr4
  case class Less(l_expr: Expr5, r_expr: Expr4)(val pos: (Int, Int)) extends Expr4
  case class LessEq(l_expr: Expr5, r_expr: Expr4)(val pos: (Int, Int)) extends Expr4
  sealed trait Expr5 extends Expr4
  case class Plus(l_expr: Expr5, r_expr: Expr6)(val pos: (Int, Int)) extends Expr5
  case class Minus(l_expr: Expr5, r_expr: Expr6)(val pos: (Int, Int)) extends Expr5
  sealed trait Expr6 extends Expr5
  case class Mult(l_expr: Expr6, r_expr: Expr7)(val pos: (Int, Int)) extends Expr6
  case class Div(l_expr: Expr6, r_expr: Expr7)(val pos: (Int, Int)) extends Expr6
  case class Mod(l_expr: Expr6, r_expr: Expr7)(val pos: (Int, Int)) extends Expr6
  sealed trait Expr7 extends Expr6
  case class Not(expr: Expr7)(val pos: (Int, Int)) extends Expr7
  case class Negate(expr: Expr7)(val pos: (Int, Int)) extends Expr7
  case class Len(expr: Expr7)(val pos: (Int, Int)) extends Expr7
  case class Ord(expr: Expr7)(val pos: (Int, Int)) extends Expr7
  case class Chr(expr: Expr7)(val pos: (Int, Int)) extends Expr7
  sealed trait Term extends Expr7

  case class ArrayLiter(exprs: List[Expr])(val pos: (Int, Int)) extends AssignRHS

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

  trait ParserBuilder[T] {
    val parser: Parsley[T]
    final def <#(p: Parsley[_]): Parsley[T] = parser <* p
  }
  trait ParserBuilder1[T1, R] extends ParserBuilder[T1 => R] {
    def apply(x: T1)(pos: (Int,Int)): R
    val parser: Parsley[T1 => R] = pos.map(p => apply(_)(p))
  }
  trait ParserBuilder2[T1, T2, R] extends ParserBuilder[(T1, T2) => R] {
    def apply(x: T1, y: T2)(pos: (Int,Int)): R
    val parser: Parsley[(T1, T2) => R] = pos.map(p => apply(_,_)(p))
  }

  object Program {
    def apply(funcs: =>Parsley[List[Func]], stat: =>Parsley[Stat]): Parsley[Program]
      = pos <**> (funcs, stat).lazyZipped(Program(_, _)_)
  }
  object Func{
    def apply(ident: Parsley[(Type, Ident)], params: Parsley[ParamList], stat: Parsley[Stat]) : Parsley[Func]
      = pos <**> (ident, params, stat).zipped(Func(_,_,_))
  }
  //TODO
  object ParamList {
    def apply(params: Parsley[List[Param]]): Parsley[ParamList]
      = pos <**> params.map(ParamList(_))
  }
  object Param{
    def apply(_type: Parsley[Type], ident: Parsley[Ident]) : Parsley[Param]
      = pos <**> (_type, ident).zipped(Param(_,_) _)
  }

  object Decl {
    def apply(_type: Parsley[Type], ident: Parsley[Ident], rhs: Parsley[AssignRHS]): Parsley[Decl]
    = pos <**> (_type, ident, rhs).zipped(Decl(_, _, _))
  }
  object Assign {
    def apply(lhs: Parsley[AssignLHS], rhs: Parsley[AssignRHS]): Parsley[Assign]
    = pos <**> (lhs, rhs).zipped(Assign(_, _))
  }
  object Read {
    def apply(lhs: Parsley[AssignLHS]): Parsley[Read]
      = pos <**> lhs.map(Read(_))
  }
  object Free {
    def apply(expr: Parsley[Expr]): Parsley[Free]
      = pos <**> expr.map(Free(_))
  }
  object Return {
    def apply(expr: Parsley[Expr]): Parsley[Return]
      = pos <**> expr.map(Return(_))
  }
  object Exit {
    def apply(expr: Parsley[Expr]): Parsley[Exit]
      = pos <**> expr.map(Exit(_))
  }
  object Print {
    def apply(expr: Parsley[Expr]): Parsley[Print]
      = pos <**> expr.map(Print(_))
  }
  object Println {
    def apply(expr: Parsley[Expr]): Parsley[Println]
      = pos <**> expr.map(Println(_))
  }
  object IfElse {
    def apply(cond: Parsley[Expr], then_stat: Parsley[Stat], else_stat: Parsley[Stat]): Parsley[IfElse]
      = pos<**> (cond, then_stat, else_stat).zipped(IfElse(_, _, _))
  }
  object While {
    def apply(cond: Parsley[Expr], body: Parsley[Stat]): Parsley[While]
      = pos <**> (cond, body).zipped(While(_, _))
  }
  object Scope {
    def apply(stat: Parsley[Stat]): Parsley[Scope]
      = pos <**> stat.map(Scope(_))
  }
  object Combine {
    def apply(stats: Parsley[List[StatAtom]]): Parsley[Combine]
      = pos <**> stats.map(Combine(_))
  }

  object NewPair {
    def apply(fst: Parsley[Expr], snd: Parsley[Expr]): Parsley[NewPair]
      = pos <**> (fst, snd).zipped(NewPair(_,_))
  }
  object Call {
    def apply(ident: Parsley[Ident], argList: Parsley[ArgList]): Parsley[Call]
      = pos <**> (ident, argList).zipped(Call(_,_))
  }

  object ArgList{
    def apply(args: Parsley[List[Expr]]): Parsley[ArgList]
      = pos <**> args.map(ArgList(_))
  }

  object FstPair {
    def apply(fst: Parsley[Expr]): Parsley[FstPair]
      = pos <**> fst.map(FstPair(_))
  }
  object SndPair {
    def apply(snd: Parsley[Expr]): Parsley[SndPair]
      = pos <**> snd.map(SndPair(_))
  }

  object ArrayType{
    def apply(_type: Parsley[Type]) : Parsley[ArrayType]
      = pos <**> _type.map(ArrayType(_))
  }
  object PairType {
    def apply(fst_type: Parsley[PairElemType], snd_type: Parsley[PairElemType]): Parsley[PairType]
      = pos <**> (fst_type, snd_type).zipped(PairType(_,_))
  }

  object ArrayElem {
    def apply(ident: Parsley[Ident], expr: Parsley[List[Expr]]): Parsley[ArrayElem]
      = pos <**> (ident, expr).zipped(ArrayElem(_,_))
  }
  object ArrayLiter {
    def apply(exprs: Parsley[List[Expr]]): Parsley[ArrayLiter]
      = pos <**> exprs.map(ArrayLiter(_))
  }
  object IntLiter {
    def apply(x: Parsley[Int]): Parsley[IntLiter]
      = pos <**> x.map(IntLiter(_))
  }
  object BoolLiter {
    def apply(b: Parsley[Boolean]): Parsley[BoolLiter]
      = pos <**> b.map(BoolLiter(_))
  }
  object CharLiter {
    def apply(c: Parsley[Char]): Parsley[CharLiter]
      = pos <**> c.map(CharLiter(_))
  }
  object StrLiter {
    def apply(s: Parsley[String]): Parsley[StrLiter]
      = pos <**> s.map(StrLiter(_))
  }
  object Ident {
    def apply(ident: Parsley[String]): Parsley[Ident]
      = pos <**> ident.map(Ident(_))
  }
  object ParensExpr {
    def apply(expr: Parsley[Expr]): Parsley[ParensExpr]
      = pos <**> expr.map(ParensExpr(_))
  }

}