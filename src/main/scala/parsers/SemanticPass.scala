package parsers

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object SemanticPass {
  import parsers.Ast._

  val st: mutable.Map[Ident, (AstNode, Type)] = scala.collection.mutable.Map[Ident, (AstNode, Type)]()

  def traverse(node: AstNode, errors: ListBuffer[String]): Unit = {
    node match {
      case Program(funcs, stat) =>
        for (func <- funcs){
          func match {
            case Func((_type, ident), _, _) => st += (ident -> (func, _type))
          }
        }
        funcs.foreach(traverse(_, errors))
        traverse(stat, errors)

      case Func((_, _), ParamList(params), stat) =>
        for (param <- params){
          traverse(param, errors)
        }
        traverse(stat, errors)
      case Param(_type, ident) => st += (ident -> (node, _type))

      // <Stat>
      // TODO type check expr
      case Skip =>
      case Decl(PairType(t1, t2), ident, PairLiter) =>  st += (ident -> (PairLiter, PairType(t1, t2)))
      case Decl(_type, ident, rhs) =>
        if (_type != checkType(rhs, errors)) {
          errors += s"The right hand side does not match type ${_type}"
        } else {
          st += (ident -> (rhs, _type))
        }
      case Assign(lhs, rhs) =>
        if (checkType(lhs, errors) != checkType(rhs, errors)) {
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
        val _type = checkExprType(expr, errors)
        if (!(_type.isInstanceOf[PairType] || _type.isInstanceOf[ArrayType])) {
          errors += "Free must be give a Pair or array expression"
        }
      case Print(expr) => checkExprType(expr, errors)
      case Println(expr) => checkExprType(expr, errors)
      case Return(expr) => // TODO come back to this
      case Exit(expr) => // TODO come back to this
      case IfElse(cond, stat_true, stat_false) =>
        if (checkExprType(cond, errors) != WBool){
          errors += "The condition of a if statement must be a boolean expression"
        }
        traverse(stat_true, errors)
        traverse(stat_false, errors)
      case While(cond, stat) =>
        if (checkExprType(cond, errors) != WBool){
          errors += "The condition of a while statement must be a boolean expression"
        }
        traverse(stat, errors)
      case Scope(stat) => traverse(stat, errors)
      case Combine(stats) => stats.foreach(traverse(_, errors))
      case _ =>
    }

    def checkType(node: AstNode, errors: ListBuffer[String]): Type = {
      node match {
        //AssignLHS
        case ident: Ident => st(ident)._2
        case ArrayElem(ident, exprs) => st(ident)._2 //TODO make sure this works for multi-dimensional arrays
        case FstPair(pair) =>
          val rhs_type = checkType(pair, errors)
          rhs_type match {
            case PairType(t1, _) => t1
            case _ =>
              errors += "The expression passed to fst must be of type Pair and must not be null"
              rhs_type
          }
        case SndPair(pair) =>
          val rhs_type = checkType(pair, errors)
          rhs_type match {
            case PairType(_, t2) => t2
            case _ =>
              errors += "The expression passed to snd must be of type Pair and must not be null"
              rhs_type
          }

        //AssignRHS
        case expr:Expr => checkExprType(expr, errors)
        case ArrayLiter(exprs) =>
          var types: List[Type] = List()
          for (expr <- exprs) {
            types = checkExprType(expr, errors) +: types
          }
          val same: Boolean = types.forall(_ == types.head)
          if (same) {
            val t : Option[Type] = types.headOption
            if (t.isDefined){
              ArrayType(t.get)
            }else{
              null
            }
          } else {
            errors += "All elements of an array must have the same type"
            null
          }
        case NewPair(PairLiter, PairLiter) => PairType(Pair, Pair)
        case NewPair(PairLiter, e2) => PairType(Pair, checkExprType(e2, errors).asInstanceOf[PairElemType])
        case NewPair(e1, PairLiter) => PairType(checkExprType(e1, errors).asInstanceOf[PairElemType], Pair)
        case NewPair(e1, e2) =>
          PairType(checkExprType(e1, errors).asInstanceOf[PairElemType], checkExprType(e2, errors).asInstanceOf[PairElemType])
        case Call(ident, ArgList(al)) =>
          val returnType : Type = st(ident)._2
          val n : AstNode = st(ident)._1
          n match{
            case Func(_, ParamList(pl), _) =>
              if (al.length != pl.length){
                errors += s"incorrect number of args when calling functions $ident"
              } else {
                for (i <- al.indices) {
                  if (checkExprType(al(i), errors) != pl(i)._type) {
                    errors += "the argument types do not match those expected"
                  }
                }
              }
            case _ =>
              errors += "cannot call a non function"
          }
          returnType
        case _ => null
      }
    }

    def checkExprType(expr: AstNode, errors: ListBuffer[String]): Type = {
      expr match {
        case IntLiter(_) => WInt
        case BoolLiter(_) => WBool
        case CharLiter(_) => WChar
        case StrLiter(_) => WString
        // TODO check this
        case PairLiter => null
        case ident:Ident =>
          if (st.contains(ident)) {
            st(ident)._2
          } else {
            null
          }
        case ParensExpr(expr) => checkExprType(expr, errors)
        case ArrayElem(ident, _) => checkType(st(ident)._1, errors)
        case Unary(x) => checkExprType(x, errors)
        case And(x, y) =>
          if (!(checkExprType(x, errors) == WBool && checkExprType(y, errors) == WBool)) {
            errors += s"Both sides of the expression && must be Integer"
          }
          WBool
        case Or(x, y) =>
          if (!(checkExprType(x, errors) == WBool && checkExprType(y, errors) == WBool)) {
            errors += s"Both sides of the expression || must be Integer"
          }
          WBool
        case Greater(x, y) =>
          val x_type = checkExprType(x, errors)
          val y_type = checkExprType(y, errors)
          if (!((x_type == WInt && y_type == WInt) || (x_type == WChar && y_type == WChar))) {
            errors += s"Both sides of the expression > must be Integer"
          }
          WBool
        case GreaterEq(x, y) =>
          val x_type = checkExprType(x, errors)
          val y_type = checkExprType(y, errors)
          if (!((x_type == WInt && y_type == WInt) || (x_type == WChar && y_type == WChar))) {
            errors += s"Both sides of the expression >= must be Integer"
          }
          WBool
        case Less(x, y) =>
          val x_type = checkExprType(x, errors)
          val y_type = checkExprType(y, errors)
          if (!((x_type == WInt && y_type == WInt) || (x_type == WChar && y_type == WChar))) {
            errors += s"Both sides of the expression < must be Integer"
          }
          WBool
        case LessEq(x, y) =>
          val x_type = checkExprType(x, errors)
          val y_type = checkExprType(y, errors)
          if (!((x_type == WInt && y_type == WInt) || (x_type == WChar && y_type == WChar))) {
            errors += s"Both sides of the expression <= must be Integer"
          }
          WBool
        case Eq(x, y) =>
          if (checkExprType(x, errors) != checkExprType(y, errors)) {
            errors += s"Both sides of the expression == must be the same"
          }
          WBool
        case NotEq(x, y) =>
          if (checkExprType(x, errors) != checkExprType(y, errors)) {
            errors += s"Both sides of the expression != must be the same"
          }
          WBool
        case intBinary(x, y) =>
          if(!(checkExprType(x, errors) == WInt && checkExprType(y, errors) == WInt)){
            errors += "Both sides of the expression must be Integers"
          }
          WInt
        case _ => null
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

object intBinary{
  import parsers.Ast._
  def unapply(expr: Expr): Option[(Expr, Expr)] = expr match{
    case Mult(x, y)  => Some(x, y)
    case Div(x, y)   => Some(x, y)
    case Mod(x, y)   => Some(x, y)
    case Plus(x, y)  => Some(x, y)
    case Minus(x, y) => Some(x, y)
    case _               => None
  }
}
