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
      case Decl(PairType(t1, t2), ident, PairLiter) =>  st += (ident -> (PairLiter, PairType(t1, t2)))
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
        if (!(_type.isInstanceOf[PairType] || _type.isInstanceOf[ArrayType])) {
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
        case FstPair(pair) =>
          val rhs_type = checkType(pair)
          rhs_type match {
            case PairType(t1, _) => t1
            case _ =>
              println("The expression passed to fst must be of type Pair and must not be null")
              rhs_type
          }
        case SndPair(pair) =>
          val rhs_type = checkType(pair)
          rhs_type match {
            case PairType(_, t2) => t2
            case _ =>
              println("The expression passed to snd must be of type Pair and must not be null")
              rhs_type
          }

        //AssignRHS
        case expr:Expr => checkExprType(expr)
        case ArrayLiter(exprs) =>
          var types: List[Type] = List()
          for (expr <- exprs) {
            types = checkExprType(expr) +: types
          }
          val same: Boolean = types.forall(_ == types.head)
          if (same) {
            val t : Option[Type] = types.headOption
            if (t.isDefined){
              ArrayType(t.get)
            }else{
              println("empty array")
              null
            }
          } else {
            println("All elements of an array must have the same type")
            null
          }
        case NewPair(PairLiter, PairLiter) => PairType(Pair, Pair)
        case NewPair(PairLiter, e2) => PairType(Pair, checkExprType(e2).asInstanceOf[PairElemType])
        case NewPair(e1, PairLiter) => PairType(checkExprType(e1).asInstanceOf[PairElemType], Pair)
        case NewPair(e1, e2) =>
          PairType(checkExprType(e1).asInstanceOf[PairElemType], checkExprType(e2).asInstanceOf[PairElemType])
        case Call(ident, ArgList(al)) =>
          val returnType : Type = st(ident)._2
          val n : AstNode = st(ident)._1
          n match{
            case Func(_, ParamList(pl), _) =>
              if (al.length != pl.length){
                println(s"incorrect number of args when calling functions $ident")
              } else {
                for (i <- 0 to al.length-1) {
                  if (checkExprType(al(i)) != pl(i)._type) {
                    println("the argument types do not match those expected")
                  }
                }
              }
            case _ =>
              println("cannot call a non function")
          }
          returnType
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
        case Greater(x, y) =>
          val x_type = checkExprType(x)
          val y_type = checkExprType(y)
          if (!((x_type == WInt && y_type == WInt) || (x_type == WChar && y_type == WChar))) {
            println(s"Both sides of the expression > must be Integer")
          }
          WBool
        case GreaterEq(x, y) =>
          val x_type = checkExprType(x)
          val y_type = checkExprType(y)
          if (!((x_type == WInt && y_type == WInt) || (x_type == WChar && y_type == WChar))) {
            println(s"Both sides of the expression >= must be Integer")
          }
          WBool
        case Less(x, y) =>
          val x_type = checkExprType(x)
          val y_type = checkExprType(y)
          if (!((x_type == WInt && y_type == WInt) || (x_type == WChar && y_type == WChar))) {
            println(s"Both sides of the expression < must be Integer")
          }
          WBool
        case LessEq(x, y) =>
          val x_type = checkExprType(x)
          val y_type = checkExprType(y)
          if (!((x_type == WInt && y_type == WInt) || (x_type == WChar && y_type == WChar))) {
            println(s"Both sides of the expression <= must be Integer")
          }
          WBool
        case Eq(x, y) =>
          if (checkExprType(x) != checkExprType(y)) {
            println(s"Both sides of the expression == must be the same")
          }
          WBool
        case NotEq(x, y) =>
          if (checkExprType(x) != checkExprType(y)) {
            println(s"Both sides of the expression != must be the same")
          }
          WBool
        case intBinary(x, y) =>
          if(!(checkExprType(x) == WInt && checkExprType(y) == WInt)){
            println("Both sides of the expression must be Integers")
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
