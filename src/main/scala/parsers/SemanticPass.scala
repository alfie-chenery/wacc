package parsers

import scala.collection.mutable

object SemanticPass {
  import parsers.Ast._

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

      case Func((_, _), ParamList(params), stat) =>
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
          println("The condition of a if statement must be a boolean expression")
        }
        traverse(stat_true)
        traverse(stat_false)
      case While(cond, stat) =>
        if (checkExprType(cond) != WBool){
          println("The condition of a while statement must be a boolean expression")
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
        case ArrayElem(ident, exprs) => st(ident)._2 //TODO make sure this works for multi-dimensional arrays
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

object Unary{
  import parsers.Ast._
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
  import parsers.Ast._
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
