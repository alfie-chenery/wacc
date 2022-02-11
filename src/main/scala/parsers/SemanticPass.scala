package parsers

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import parsers.ExpressionPrinter
import parsers.ExpressionPrinter.prettyPrint

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
        val t : Type = checkType(rhs, errors)
        if (_type != t) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: " + prettyPrint(_type) + ", actual: " + prettyPrint(t))
        } else {
          st += (ident -> (rhs, _type))
        }
      case Assign(lhs, rhs) =>
        val t : Type = checkType(rhs, errors)
        val t1 : Type = checkType(lhs, errors)
        if (t1 != t) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: " + prettyPrint(t1) + ", actual: " + prettyPrint(t))
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
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: pair Pair or array expression, actual: " + prettyPrint(_type))
        }
      case Print(expr) => checkExprType(expr, errors)
      case Println(expr) => checkExprType(expr, errors)
      case Return(expr) => // TODO come back to this
      case Exit(expr) =>
        val _type = checkExprType(expr, errors)
        if (!(_type == WInt)) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: Int, actual: " + prettyPrint(_type))
        }
      case IfElse(cond, stat_true, stat_false) =>
        val t: Type = checkExprType(cond, errors)
        if (t != WBool){
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: bool, actual: " + prettyPrint(t))
        }
        traverse(stat_true, errors)
        traverse(stat_false, errors)
      case While(cond, stat) =>
        val t: Type = checkExprType(cond, errors)
        if (t != WBool){
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: bool, actual: " + prettyPrint(t))
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
              errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: Pair (not null)")
              rhs_type
          }
        case SndPair(pair) =>
          val rhs_type = checkType(pair, errors)
          rhs_type match {
            case PairType(_, t2) => t2
            case _ =>
              errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: Pair (not null)")
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
            errors += "Semantic error detected: Incompatible type at " + prettyPrint(node) + ". All types in an array must match."
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
                errors += "Semantic error detected: Too many arguments at " + prettyPrint(node)
              } else {
                var t : Type = null
                var t1 : Type = null
                for (i <- al.indices) {
                  t = checkExprType(al(i), errors)
                  t1 = pl(i)._type
                  if (t != t1) {
                    errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: " + prettyPrint(t) + ", actual: " + prettyPrint(t1))
                  }
                }
              }
            case _ =>
              errors += ("Semantic error detected at " + prettyPrint(node) + ". Cannot call anything not of type Function.")
          }
          returnType
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
        case ident:Ident => st(ident)._2
        case ParensExpr(expr) => checkExprType(expr, errors)
        case ArrayElem(ident, _) => checkType(st(ident)._1, errors)
        case Unary(x) => checkExprType(x, errors)
        case And(x, y) =>
          val t: Type = checkExprType(x, errors)
          val t1: Type = checkExprType(y, errors)
          if (!(t == WBool && t1 == WBool)) {
            errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: bool, actual: " + prettyPrint(t1) + " and " + prettyPrint(t))
          }
          WBool
        case Or(x, y) =>
          val t: Type = checkExprType(x, errors)
          val t1: Type = checkExprType(y, errors)
          if (!(t == WBool && t1 == WBool)) {
            errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: bool and bool, actual: " + prettyPrint(t1) + " and " + prettyPrint(t))
          }
          WBool
        case Greater(x, y) =>
          val x_type = checkExprType(x, errors)
          val y_type = checkExprType(y, errors)
          if (!((x_type == WInt && y_type == WInt) || (x_type == WChar && y_type == WChar))) {
            errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: int and int OR char and char, actual: " + prettyPrint(x_type) + " and " + prettyPrint(y_type))
          }
          WBool
        case GreaterEq(x, y) =>
          val x_type = checkExprType(x, errors)
          val y_type = checkExprType(y, errors)
          if (!((x_type == WInt && y_type == WInt) || (x_type == WChar && y_type == WChar))) {
            errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: int and int OR char and char, actual: " + prettyPrint(x_type) + " and " + prettyPrint(y_type))
          }
          WBool
        case Less(x, y) =>
          val x_type = checkExprType(x, errors)
          val y_type = checkExprType(y, errors)
          if (!((x_type == WInt && y_type == WInt) || (x_type == WChar && y_type == WChar))) {
            errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: int and int OR char and char, actual: " + prettyPrint(x_type) + " and " + prettyPrint(y_type))
          }
          WBool
        case LessEq(x, y) =>
          val x_type = checkExprType(x, errors)
          val y_type = checkExprType(y, errors)
          if (!((x_type == WInt && y_type == WInt) || (x_type == WChar && y_type == WChar))) {
            errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: int and int OR char and char, actual: " + prettyPrint(x_type) + " and " + prettyPrint(y_type))
          }
          WBool
        case Eq(x, y) =>
          val t: Type = checkExprType(x, errors)
          val t1: Type = checkExprType(y, errors)
          if (t != t1) {
            errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: matching types, actual: " + prettyPrint(t) + " and " + prettyPrint(t1))
          }
          WBool
        case NotEq(x, y) =>
          val t: Type = checkExprType(x, errors)
          val t1: Type = checkExprType(y, errors)
          if (t != t1) {
            errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: matching types, actual: " + prettyPrint(t) + " and " + prettyPrint(t1))
          }
          WBool
        case intBinary(x, y) =>
          val t: Type = checkExprType(x, errors)
          val t1: Type = checkExprType(y, errors)
          if(!(t == WInt && t1 == WInt)){
            errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: int and int, actual: " + prettyPrint(t) + " and " + prettyPrint(t1))
          }
          WInt
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
