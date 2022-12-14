package parsers

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import parsers.ExpressionPrinter.prettyPrint

object SemanticPass {
  import parsers.Ast._

  val st: mutable.Map[Ident, (AstNode, Type)] = scala.collection.mutable.Map[Ident, (AstNode, Type)]()

  def traverse(node: AstNode, errors: ListBuffer[String]): Unit = {
    node match {
      case Program(funcs, stat) =>
        for (func <- funcs) {
          func match {
            case Func((_type, ident), _, _) => st += (ident -> (func, _type))
          }
        }
        checkReturns(stat, errors, main = true)
        funcs.foreach(traverse(_, errors))
        traverse(stat, errors)

      case Func((_, _), ParamList(params), stat) =>
        for (param <- params) {
          traverse(param, errors)
        }
        traverse(stat, errors)
        //TODO check this implementation
        //~done: enforce that functions always return or exit
        //although found in the semantic pass, the spec defines this as a syntactic error
        checkReturns(stat, errors, main = false)

      case Param(_type, ident) => st += (ident -> (node, _type))

      // <Stat>
      // TODO type check expr
      case Skip =>
      case Decl(PairType(t1, t2), ident, PairLiter) => st += (ident -> (PairLiter, PairType(t1, t2)))
      case Decl(_type, ident, rhs) =>
        val t: Type = checkType(rhs, errors)
        if (_type != t) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: " + prettyPrint(_type) + ", actual: " + prettyPrint(t))
        } else {
          st += (ident -> (rhs, _type))
        }
      case Assign(lhs, PairLiter) =>
        val t = checkType(lhs, errors)
         t match {
          case PairType(_,_) =>
          case t =>
            errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: " + prettyPrint(t) + ", actual: " + prettyPrint(PairLiter))
        }
      case Assign(lhs, rhs) =>
        val t: Type = checkType(rhs, errors)
        val t1: Type = checkType(lhs, errors)
        if (t1 != t) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: " + prettyPrint(t1) + ", actual: " + prettyPrint(t))
        }
      case Read(lhs) =>
        val _type = checkType(lhs, errors)
        if (!(_type == WChar || _type == WInt)) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: Int, Char, actual: " + prettyPrint(_type))
        }
      case Free(expr) => // TODO come back to this
        val _type = checkExprType(expr, node, errors)
        if (!(_type.isInstanceOf[PairType] || _type.isInstanceOf[ArrayType])) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: pair Pair or array expression, actual: " + prettyPrint(_type))
        }
      case Print(expr) => checkExprType(expr, node, errors)
      case Println(expr) => checkExprType(expr, node, errors)
      case Return(expr) =>
        val _type = checkExprType(expr, node, errors)
        // TODO this should not always be checking for ints
        /*
        if (!(_type == WInt)) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: Int, actual: " + prettyPrint(_type))
        }

         */
      case Exit(expr) =>
        val _type = checkExprType(expr, node, errors)
        if (!(_type == WInt)) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: Int, actual: " + prettyPrint(_type))
        }
      case IfElse(cond, stat_true, stat_false) =>
        val t: Type = checkExprType(cond, node, errors)
        if (t != WBool) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: bool, actual: " + prettyPrint(t))
        }
        traverse(stat_true, errors)
        traverse(stat_false, errors)
      case While(cond, stat) =>
        val t: Type = checkExprType(cond, node, errors)
        if (t != WBool) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: bool, actual: " + prettyPrint(t))
        }
        traverse(stat, errors)
      case Scope(stat) => traverse(stat, errors)
      case Combine(stats) => stats.foreach(traverse(_, errors))
      case _ =>
    }
  }

  def checkReturns(node: AstNode, errors: ListBuffer[String], main: Boolean): Boolean = {
    //main flags whether we are checking the main function, in which case return shouldn't be accepted
    //TODO give more detailed error messages where the return should've been ie specific branch
    node match {
      case Return(expr) =>
        if(main) {
          errors += ("Syntax error detected: at \"" + prettyPrint(node) + "\". Main function should not contain a return statement.")
          false
        } else {
          true
        } //if main function return is invalid, should be exit
      case Exit(expr) => true //valid way to exit any function
      case IfElse(cond, stat_true, stat_false) => checkReturns(stat_true, errors, main) && checkReturns(stat_false, errors, main)
      case While(cond, stat) => checkReturns(stat, errors, main)
      case Scope(stat) => checkReturns(stat, errors, main)
      case Combine(stats) => checkReturns(stats.last, errors, main)

      //other stat types all return false as arent valid ways to end a function
      //other nodes which arent stat should not be passed into this function so all return false
      case _ =>
        if (!main) {
          errors += ("Syntax error detected: at \"" + prettyPrint(node) + "\". Function does not contain a return/exit in all conditional branches.")
        }
        false
    }
  }

  def checkType(node: AstNode, errors: ListBuffer[String]): Type = {
    node match {
      //AssignLHS
      case ident: Ident => st(ident)._2
      case ArrayElem(ident, exprs) =>
        var t = st(ident)._2 //TODO make sure this works for multi-dimensional arrays
        for (expr <- exprs) {
          t match {
            case ArrayType(arr) => t = arr
            case a => a
          }
        }
        t
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
      case expr:Expr => checkExprType(expr, node, errors)
      case ArrayLiter(exprs) =>
        var types: List[Type] = List()
        for (expr <- exprs) {
          types = checkExprType(expr, node, errors) +: types
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
      case NewPair(PairLiter, e2) => PairType(Pair, checkExprType(e2, node, errors).asInstanceOf[PairElemType])
      case NewPair(e1, PairLiter) => PairType(checkExprType(e1, node, errors).asInstanceOf[PairElemType], Pair)
      case NewPair(e1, e2) =>
        PairType(checkExprType(e1, node, errors).asInstanceOf[PairElemType], checkExprType(e2, node, errors).asInstanceOf[PairElemType])
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
                t = checkExprType(al(i), node, errors)
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
      case _ => null
    }
  }

  def toFloat(ints: (Int, Int)): Float ={
    val fst: Int = ints._1
    val snd: Int = ints._2
    val div = scala.math.pow(10, snd.toString.length)
    val fraction = snd / div
    (fst + fraction.toFloat)
  }

  def checkExprType(expr: AstNode, node:AstNode, errors: ListBuffer[String]): Type = {
    expr match {
      case IntLiter(_) => WInt
      case BoolLiter(_) => WBool
      case CharLiter(_) => WChar
      case StrLiter(_) => WString
      case FloatLiter(_) => WFloat
      case PairLiter => null
      case ident:Ident =>
        if(st.contains(ident)){
          st(ident)._2
        }else{
          null
        }
      case ParensExpr(expr) => checkExprType(expr, node, errors)
      case ArrayElem(ident, _) =>
        checkType(st(ident)._1, errors) match {
          case ArrayType(t) => t
          case t => t
        }
      case Ord(_) => WInt
      case Chr(_) => WChar
      case Len(_) => WInt
      case Unary(x) => checkExprType(x, node, errors)
      case And(x, y) =>
        val t: Type = checkExprType(x, node, errors)
        val t1: Type = checkExprType(y, node, errors)
        if (!(t == WBool && t1 == WBool)) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: bool, actual: " + prettyPrint(t1) + " and " + prettyPrint(t))
        }
        WBool
      case Or(x, y) =>
        val t: Type = checkExprType(x, node, errors)
        val t1: Type = checkExprType(y, node, errors)
        if (!(t == WBool && t1 == WBool)) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: bool and bool, actual: " + prettyPrint(t1) + " and " + prettyPrint(t))
        }
        WBool
      case Greater(x, y) =>
        val x_type = checkExprType(x, node, errors)
        val y_type = checkExprType(y, node, errors)
        if (!((x_type == WInt && y_type == WInt) || (x_type == WChar && y_type == WChar) || (x_type == WFloat && y_type == WFloat))) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: int and int OR char and char, actual: " + prettyPrint(x_type) + " and " + prettyPrint(y_type))
        }
        WBool
      case GreaterEq(x, y) =>
        val x_type = checkExprType(x, node, errors)
        val y_type = checkExprType(y, node, errors)
        if (!((x_type == WInt && y_type == WInt) || (x_type == WChar && y_type == WChar) || (x_type == WFloat && y_type == WFloat))) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: int and int OR char and char, actual: " + prettyPrint(x_type) + " and " + prettyPrint(y_type))
        }
        WBool
      case Less(x, y) =>
        val x_type = checkExprType(x, node, errors)
        val y_type = checkExprType(y, node, errors)
        if (!((x_type == WInt && y_type == WInt) || (x_type == WChar && y_type == WChar) || (x_type == WFloat && y_type == WFloat))) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: int and int OR char and char, actual: " + prettyPrint(x_type) + " and " + prettyPrint(y_type))
        }
        WBool
      case LessEq(x, y) =>
        val x_type = checkExprType(x, node, errors)
        val y_type = checkExprType(y, node, errors)
        if (!((x_type == WInt && y_type == WInt) || (x_type == WChar && y_type == WChar) || (x_type == WFloat && y_type == WFloat))) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: int and int OR char and char, actual: " + prettyPrint(x_type) + " and " + prettyPrint(y_type))
        }
        WBool
      case Eq(x, y) =>
        val t: Type = checkExprType(x, node, errors)
        val t1: Type = checkExprType(y, node, errors)
        if (t != t1) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: matching types, actual: " + prettyPrint(t) + " and " + prettyPrint(t1))
        }
        WBool
      case NotEq(x, y) =>
        val t: Type = checkExprType(x, node, errors)
        val t1: Type = checkExprType(y, node, errors)
        if (t != t1) {
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: matching types, actual: " + prettyPrint(t) + " and " + prettyPrint(t1))
        }
        WBool
      case intBinary(x, y) =>
        val t: Type = checkExprType(x, node, errors)
        val t1: Type = checkExprType(y, node, errors)
        if(!((t == WInt && t1 == WInt) || (t == WFloat && t1 == WFloat))){
          errors += ("Semantic error detected: Incompatible type at " + prettyPrint(node) + ". Expected: int and int, actual: " + prettyPrint(t) + " and " + prettyPrint(t1))
        }
        WInt
      case _ => null
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
    case _           => None
  }
}
