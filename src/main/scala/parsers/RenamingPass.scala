package parsers

import parsers.Ast.Ident
import parsers.SemanticPass.checkExprType

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object RenamingPass {
  import parsers.Ast._

  def renameIdent(ident: String, _type: Type, localScope: mutable.Map[String,String], varsInScope: mutable.Map[String, (String,Type)], errors: ListBuffer[String]): Ident = {
    if (localScope.contains(ident)) {
      errors += "identifier $ident already declared in the current scope"
    }
    var renamedIdent = ""
    if(varsInScope.contains(ident)) {
      val splitString = varsInScope(ident)._1.split('$')
      val nextIdentInt = splitString(splitString.length-1).toInt+1
      renamedIdent = ident+"$"+s"$nextIdentInt"
    } else {
      renamedIdent = ident + "$0"
    }
    varsInScope += (ident -> (renamedIdent, _type))
    localScope += (ident -> renamedIdent)
    Ident(localScope(ident))
  }

  private def rename(program: AstNode, localScope: mutable.Map[String, String], varsInScope: mutable.Map[String, (String,Type)], errors: ListBuffer[String]): AstNode = {
    program match {
      case Program(funcs, stat) =>
        val renamedFuncs: ListBuffer[Func] = ListBuffer[Func]()
        for (func <- funcs) {
          func match {
             case Func((_type,ident),params,stat) =>
               val renamedFunc = Func((_type, renameIdent(renameFunc(func), _type, localScope, varsInScope, errors)),params,stat) //this just renames the ident of the func
               renamedFuncs.append(rename(renamedFunc, localScope, varsInScope, errors).asInstanceOf[Func]) //this recursively renames the contents of the function
          }
        }
        //funcs.foreach(renamedFuncs += rename(_, localScope, varsInScope, errors).asInstanceOf[Func])
        Program(renamedFuncs.toList, rename(stat, localScope, varsInScope, errors).asInstanceOf[Stat])

      case Func((_type, Ident(ident)), ParamList(params), stat) =>
        val newScope = scala.collection.mutable.Map[String, String]()
        val renamedParams: ListBuffer[Param] = ListBuffer()
        for (param <- params) {
          param match {
            case Param(_type, Ident(ident)) =>
              renamedParams += Param(_type, renameIdent(ident, _type, newScope, varsInScope, errors))
            case _ =>
          }
        }
        Func((_type, Ident(ident)), ParamList(renamedParams.toList),
          rename(stat, newScope, varsInScope, errors).asInstanceOf[Stat])

      case Decl(_type, Ident(ident), rhs) =>
        // TODO lookup variable to see if it's declared in current scope, if not, add it
        val renamedRhs = rename(rhs, localScope, varsInScope, errors).asInstanceOf[AssignRHS]
        Decl(_type, renameIdent(ident, _type, localScope, varsInScope, errors), renamedRhs)

      case Assign(lhs, rhs) =>
        Assign(rename(lhs, localScope, varsInScope, errors).asInstanceOf[AssignLHS],
          rename(rhs, localScope, varsInScope, errors).asInstanceOf[AssignRHS])
      // TODO check that lhs is declared in the current or higher scope

      case Ident(ident) =>
        if (!varsInScope.contains(ident)) {
          errors += s"variable $ident not declared"
          null
        }else {
          Ident(varsInScope(ident)._1)
        }

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
        Minus(rename(lhs, localScope, varsInScope, errors).asInstanceOf[Expr5],
          rename(rhs, localScope, varsInScope, errors).asInstanceOf[Expr6])
      case Mult(lhs, rhs) =>
        Mult(rename(lhs, localScope, varsInScope, errors).asInstanceOf[Expr6],
          rename(rhs, localScope, varsInScope, errors).asInstanceOf[Expr7])
      case Div(lhs, rhs) =>
        Div(rename(lhs, localScope, varsInScope, errors).asInstanceOf[Expr6],
          rename(rhs, localScope, varsInScope, errors).asInstanceOf[Expr7])
      case Mod(lhs, rhs) =>
        Mod(rename(lhs, localScope, varsInScope, errors).asInstanceOf[Expr6],
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
        val _type = WInt //placeholder not actually used - calculate from call type
        val renamedFunc: String = renameFuncCall(_type, ident, args, varsInScope, errors)

        if (!varsInScope.contains(renamedFunc)) {
          errors += s"function $ident ($renamedFunc) not defined"
          null
        } else {
          val renamedArgs: ListBuffer[Expr] = ListBuffer()
          args.foreach(renamedArgs += rename(_, localScope, varsInScope, errors).asInstanceOf[Expr])
          Call(Ident(varsInScope(renamedFunc)._1), ArgList(renamedArgs.toList))
        }

      case node: AstNode => node
    }
  }
  def rename(program: Program, errors: ListBuffer[String]): Program = {
    val variableRenaming: mutable.Map[String, (String,Type)] = scala.collection.mutable.Map[String, (String,Type)]()
    val localScope: mutable.Map[String, String] = scala.collection.mutable.Map[String, String]()
    rename(program, localScope, variableRenaming, errors).asInstanceOf[Program]
  }


  private def renameFunc(function: AstNode) : String = {
    function match {
      case Func((_type, Ident(ident)), ParamList(params), _) => renameFuncDecl(_type, ident, params)
      case _ => null //should not be reachable
    }
  }

  private def renameFuncDecl(_type: Type, ident: String, params: List[Param]): String = {
    val sb = new StringBuilder()
    sb.append(ident + "$" +
      //getType(_type) + //add in once call type is known
      "Func")
    for (param <- params){
      param match {
        case Param(_type, _) =>
          sb.append("_" + getType(_type))
        case _ =>
      }
    }
    sb.toString()
  }

  private def renameFuncCall(_type: Type, ident: String, args: List[Expr], varsInScope: mutable.Map[String, (String,Type)], errors: ListBuffer[String]): String = {
    val sb = new StringBuilder()
    sb.append(ident + "$" +
      //getType(_type) + //add in once call type is known
      "Func")
    for (arg <- args){
      val t: Type = arg match {
        case Ident(ident) =>
          if (!varsInScope.contains(ident)) {
            errors += s"variable $ident not declared"
            return null
          }else {
            varsInScope(ident)._2
          }
        case _ => checkExprType(arg, arg, new ListBuffer[String])
        //errors with type checking are caught in semantic pass. If we track errors found here it can incorrectly
        // find semantic errors as a result of not everything being renamed yet
      }

      sb.append("_" + getType(t))
    }
    sb.toString()
  }

  def getType(t: Type): String = {
    t match {
      case WInt => "int"
      case WBool => "bool"
      case WChar => "char"
      case WString => "str"
      case ArrayType(_type) => getType(_type) + "Array"
      case PairType(fst_type: PairElemType, snd_type: PairElemType) =>
        "p@" + getType(fst_type) + "-" + getType(snd_type) + "@"
      //case PairElemType => getType()
      case Pair => "pair?"
      case _ => "!ERROR!" //should not be reachable
    }
  }

}
