package parsers

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object RenamingPass {

  import parsers.Ast._

  def renameIdent(ident: String, localScope: mutable.Map[String,String], varsInScope: mutable.Map[String, String], errors: ListBuffer[String]): Ident = {
    if (localScope.contains(ident)) {
      errors += "identifier $ident already declared in the current scope"
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

  private def rename(program: AstNode, localScope: mutable.Map[String, String], varsInScope: mutable.Map[String, String], errors: ListBuffer[String]): AstNode = {
    program match {
      case Program(funcs, stat) =>
        val renamedFuncs: ListBuffer[Func] = ListBuffer[Func]()
        for (func <- funcs) {
          func match {
            case Func((_, Ident(ident)), _, _) => renameIdent(ident+"$func", localScope, varsInScope, errors)
          }
        }
        funcs.foreach(renamedFuncs += rename(_, localScope, varsInScope, errors).asInstanceOf[Func])
        Program(renamedFuncs.toList, rename(stat, localScope, varsInScope, errors).asInstanceOf[Stat])

      case Func((_type, Ident(ident)), ParamList(params), stat) =>
        val newScope = scala.collection.mutable.Map[String, String]()
        val renamedParams: ListBuffer[Param] = ListBuffer()
        for (param <- params) {
          param match {
            case Param(_type, Ident(ident)) =>
              renamedParams += Param(_type, renameIdent(ident, newScope, varsInScope, errors))
            case _ =>
          }
        }
        Func((_type, Ident(varsInScope(ident+"$func"))), ParamList(renamedParams.toList),
          rename(stat, newScope, varsInScope, errors).asInstanceOf[Stat])

      case Decl(_type, Ident(ident), rhs) =>
        // TODO lookup variable to see if it's declared in current scope, if not, add it
        val renamedRhs = rename(rhs, localScope, varsInScope, errors).asInstanceOf[AssignRHS]
        Decl(_type, renameIdent(ident, localScope, varsInScope, errors), renamedRhs)

      case Assign(lhs, rhs) =>
        Assign(rename(lhs, localScope, varsInScope, errors).asInstanceOf[AssignLHS],
          rename(rhs, localScope, varsInScope, errors).asInstanceOf[AssignRHS])
      // TODO check that lhs is declared in the current or higher scope

      case Ident(ident) =>
        if (!varsInScope.contains(ident)) {
          errors += s"variable $ident not declared"
        }
        Ident(varsInScope(ident))

      case ParensExpr(expr) => ParensExpr(rename(expr, localScope, varsInScope, errors).asInstanceOf[Expr])

      case Or(lhs, rhs) =>
        Or(rename(lhs, localScope, varsInScope, errors).asInstanceOf[Expr2],
          rename(rhs, localScope, varsInScope, errors).asInstanceOf[Expr1])
      case And(lhs, rhs) =>
        And(rename(lhs, localScope, varsInScope, errors).asInstanceOf[Expr3],
          rename(rhs, localScope, varsInScope, errors).asInstanceOf[Expr2])
      case Eq(lhs, rhs) =>
        Eq(rename(lhs, localScope, varsInScope, errors).asInstanceOf[Expr4],
          rename(rhs, localScope, varsInScope, errors).asInstanceOf[Expr3])
      case NotEq(lhs, rhs) =>
        NotEq(rename(lhs, localScope, varsInScope, errors).asInstanceOf[Expr4],
          rename(rhs, localScope, varsInScope, errors).asInstanceOf[Expr3])
      case Greater(lhs, rhs) =>
        Greater(rename(lhs, localScope, varsInScope, errors).asInstanceOf[Expr5],
          rename(rhs, localScope, varsInScope, errors).asInstanceOf[Expr4])
      case GreaterEq(lhs, rhs) =>
        GreaterEq(rename(lhs, localScope, varsInScope, errors).asInstanceOf[Expr5],
          rename(rhs, localScope, varsInScope, errors).asInstanceOf[Expr4])
      case Less(lhs, rhs) =>
        Less(rename(lhs, localScope, varsInScope, errors).asInstanceOf[Expr5],
          rename(rhs, localScope, varsInScope, errors).asInstanceOf[Expr4])
      case LessEq(lhs, rhs) =>
        LessEq(rename(lhs, localScope, varsInScope, errors).asInstanceOf[Expr5],
          rename(rhs, localScope, varsInScope, errors).asInstanceOf[Expr4])
      case Plus(lhs, rhs) =>
        Plus(rename(lhs, localScope, varsInScope, errors).asInstanceOf[Expr5],
          rename(rhs, localScope, varsInScope, errors).asInstanceOf[Expr6])
      case Minus(lhs, rhs) =>
        Plus(rename(lhs, localScope, varsInScope, errors).asInstanceOf[Expr5],
          rename(rhs, localScope, varsInScope, errors).asInstanceOf[Expr6])
      case Mult(lhs, rhs) =>
        Mult(rename(lhs, localScope, varsInScope, errors).asInstanceOf[Expr6],
          rename(rhs, localScope, varsInScope, errors).asInstanceOf[Expr7])
      case Div(lhs, rhs) =>
        Mult(rename(lhs, localScope, varsInScope, errors).asInstanceOf[Expr6],
          rename(rhs, localScope, varsInScope, errors).asInstanceOf[Expr7])
      case Mod(lhs, rhs) =>
        Mult(rename(lhs, localScope, varsInScope, errors).asInstanceOf[Expr6],
          rename(rhs, localScope, varsInScope, errors).asInstanceOf[Expr7])
      case Not(expr) => Not(rename(expr, localScope, varsInScope, errors).asInstanceOf[Expr7])
      case Negate(expr) => Negate(rename(expr, localScope, varsInScope, errors).asInstanceOf[Expr7])
      case Len(expr) => Len(rename(expr, localScope, varsInScope, errors).asInstanceOf[Expr7])
      case Ord(expr) => Ord(rename(expr, localScope, varsInScope, errors).asInstanceOf[Expr7])
      case Chr(expr) => Chr(rename(expr, localScope, varsInScope, errors).asInstanceOf[Expr7])

      case ArrayElem(ident, exprs) =>
        val renamedExprs: ListBuffer[Expr] = ListBuffer[Expr]()
        exprs.foreach(renamedExprs += rename(_, localScope, varsInScope, errors).asInstanceOf[Expr])
        ArrayElem(rename(ident, localScope, varsInScope, errors).asInstanceOf[Ident], renamedExprs.toList)

      case FstPair(expr) => FstPair(rename(expr, localScope, varsInScope, errors).asInstanceOf[Expr])

      case SndPair(expr) => SndPair(rename(expr, localScope, varsInScope, errors).asInstanceOf[Expr])

      case Read(lhs) => Read(rename(lhs, localScope, varsInScope, errors).asInstanceOf[AssignLHS])
      // TODO check that lhs is declared in the current or higher scope

      case Free(expr) => Free(rename(expr, localScope, varsInScope, errors).asInstanceOf[Expr])

      case Return(expr) => Return(rename(expr, localScope, varsInScope, errors).asInstanceOf[Expr])

      case Exit(expr) =>  Exit(rename(expr, localScope, varsInScope, errors).asInstanceOf[Expr])

      case Print(expr) => Print(rename(expr, localScope, varsInScope, errors).asInstanceOf[Expr])

      case Println(expr) => Println(rename(expr, localScope, varsInScope, errors).asInstanceOf[Expr])

      case IfElse(cond, then_stat, else_stat) =>
        // TODO check if any variables in cond are defined in the current scope
        // TODO enter a new scope for both bodys
        val newScopeThen = scala.collection.mutable.Map[String, String]()
        val newScopeElse = scala.collection.mutable.Map[String, String]()
        IfElse(rename(cond, localScope, varsInScope, errors).asInstanceOf[Expr],
          rename(then_stat, newScopeThen, varsInScope.clone(), errors).asInstanceOf[Stat],
          rename(else_stat, newScopeElse,varsInScope.clone(), errors).asInstanceOf[Stat])

      case While(cond, body) =>
        // TODO check if any variables in cond are defined in the current scope
        // TODO enter a new scope for the body
        val newScope = scala.collection.mutable.Map[String, String]()
        While(rename(cond, localScope, varsInScope, errors).asInstanceOf[Expr],
          rename(body, newScope, varsInScope.clone(), errors).asInstanceOf[Stat])

      case Scope(stat) =>
        // TODO enter a new scope
        val newScope = scala.collection.mutable.Map[String, String]()
        Scope(rename(stat, newScope, varsInScope.clone(), errors).asInstanceOf[Stat])

      case Combine(stats) =>
        val renamedStats: ListBuffer[StatAtom] = ListBuffer()
        stats.foreach(renamedStats += rename(_, localScope, varsInScope, errors).asInstanceOf[StatAtom])
        Combine(renamedStats.toList)

      case NewPair(fst, snd) =>
        NewPair(rename(fst, localScope, varsInScope, errors).asInstanceOf[Expr],
          rename(snd, localScope, varsInScope, errors).asInstanceOf[Expr])

      case Call(Ident(ident), ArgList(args)) =>
        // TODO check that ident is declared in this scope
        if (!varsInScope.contains(ident+"$func")) {
          errors += s"function $ident not defined"
        }
        val renamedArgs: ListBuffer[Expr] = ListBuffer()
        args.foreach(renamedArgs += rename(_, localScope, varsInScope, errors).asInstanceOf[Expr])
        Call(Ident(varsInScope(ident+"$func")), ArgList(renamedArgs.toList))

      case node: AstNode => node
    }
  }
  def rename(program: Program, errors: ListBuffer[String]): Program = {
    val variableRenaming: mutable.Map[String, String] = scala.collection.mutable.Map[String, String]()
    val localScope: mutable.Map[String, String] = scala.collection.mutable.Map[String, String]()
    rename(program, localScope, variableRenaming, errors).asInstanceOf[Program]
  }

}
