package parsers

import parsers.Ast._

object ExpressionPrinter {

  def prettyPrint(node: AstNode) : String = {
    node match{
      case Skip => "skip"
      case Decl(t, i, rhs) => prettyPrint(t) + " " +  prettyPrint(i) + " = " + prettyPrint(rhs)
      case Assign(lhs, rhs) => prettyPrint(lhs) + " = " + prettyPrint(rhs)
      case Read(lhs) => "read " + prettyPrint(lhs)
      case Free(expr) => "free " + prettyPrint(expr)
      case Return(expr) => "return " + prettyPrint(expr)
      case Exit(expr) => "exit " + prettyPrint(expr)
      case Print(expr) => "print " + prettyPrint(expr)
      case Println(expr) => "println " + prettyPrint(expr)
      case NewPair(fst, snd) => "newpair(" + prettyPrint(fst) + "," + prettyPrint(snd) + ")"
      case Call(Ident(i), ArgList(al)) =>
        val sb = new StringBuilder("call ")
        sb.append(i.split('$')(0))
        sb += '('
        if (al.nonEmpty) {
          if (al.length > 1) {
            for (j <- al.indices) {
              sb.append(prettyPrint(al(j)))
              sb += ','
            }
          }else{
            sb.append(prettyPrint(al.head))
          }
        }
        sb += ')'
        sb.toString()
      case FstPair(fst) => "fst " + prettyPrint(fst)
      case SndPair(snd) => "snd " + prettyPrint(snd)
      case WInt => "int"
      case WBool => "bool"
      case WChar => "char"
      case WString => "string"
      case ArrayType(t) => prettyPrint(t)
      case Pair => "pair"
      case IntLiter(i) => i.toString
      case BoolLiter(b) => b.toString
      case CharLiter(c) => c.toString
      case StrLiter(s) => s
      case PairLiter => "null"
      case Ident(i) => i.split('$')(0)
      case ArrayElem(Ident(i), exprs) =>
        val sb = new StringBuilder(i)
        sb.append(" = [")
        for (j <- exprs.indices){
          sb.append(prettyPrint(exprs(j)))
          sb.append(", ")
        }
        sb.setLength(sb.length - 2)
        sb += ']'
        sb.toString()
      case ParensExpr(expr) => "(" + prettyPrint(expr) + ")"
      case Or(l, r) => prettyPrint(l) + " || " + prettyPrint(r)
      case And(l, r) => prettyPrint(l) + " && " + prettyPrint(r)
      case Eq(l, r) => prettyPrint(l) + " == " + prettyPrint(r)
      case NotEq(l, r) => prettyPrint(l) + " != " + prettyPrint(r)
      case Greater(l, r) => prettyPrint(l) + " > " + prettyPrint(r)
      case GreaterEq(l, r) => prettyPrint(l) + " >= " + prettyPrint(r)
      case Less(l, r) => prettyPrint(l) + " < " + prettyPrint(r)
      case LessEq(l, r) => prettyPrint(l) + " <= " + prettyPrint(r)
      case Plus(l, r) => prettyPrint(l) + " + " + prettyPrint(r)
      case Minus(l, r) => prettyPrint(l) + " - " + prettyPrint(r)
      case Mult(l, r) => prettyPrint(l) + " * " + prettyPrint(r)
      case Div(l, r) => prettyPrint(l) + " / " + prettyPrint(r)
      case Mod(l, r) => prettyPrint(l) + " % " + prettyPrint(r)
      case Not(e) => "!" + prettyPrint(e)
      case Negate(e) => "-" + prettyPrint(e)
      case Len(e) => "len " + prettyPrint(e)
      case Ord(e) => "ord " + prettyPrint(e)
      case Chr(e) => "chr " + prettyPrint(e)
      case ArrayLiter(exprs) =>
        val sb = new StringBuilder("]")
        if (exprs.nonEmpty) {
          if (exprs.length > 1) {
            for (i <- exprs.indices) {
              sb.append(prettyPrint(exprs(i)))
              sb += ','
            }
          }else{
            sb.append(prettyPrint(exprs.head))
          }
        }
        sb +=']'
        sb.toString()
      case Param(_, Ident(i)) => i.split('$')(0)
      case Program(funcs, stat) =>
        val sb = new StringBuilder("begin ")
        if (funcs.nonEmpty) {
          for (i <- funcs.indices) {
            sb.append(prettyPrint(funcs(i)))
          }
        }
        sb.append(prettyPrint(stat))
        sb.append("end")
        sb.toString()
      case Func((t, Ident(i)), ParamList(pl), stat) =>
        val sb = new StringBuilder("")
        sb.append(prettyPrint(t))
        sb.append(i.split('$')(0))
        sb += '('
        if (pl.nonEmpty) {
          if (pl.length > 1) {
            for (j <- pl.indices) {
              sb.append(prettyPrint(pl(j)))
              sb += ','
            }
          }else{
            sb.append(prettyPrint(pl.head))
          }
        }
        sb.append(") is ")
        sb.append(prettyPrint(stat))
        sb.append(" end")
        sb.toString
      case ParamList(pl) =>
        val sb = new StringBuilder("")
        if (pl.nonEmpty) {
          if (pl.length > 1) {
            for (i <- pl.indices) {
              sb.append(prettyPrint(pl(i)))
              sb += ','
            }
          }else{
            sb.append(prettyPrint(pl.head))
          }
        }
        sb.toString
      case While(expr, stat) => "while " + prettyPrint(expr) + " do " + prettyPrint(stat) + " done"
      case IfElse(cond, stat, stat1) => "if " + prettyPrint(cond) + " then " + prettyPrint(stat) + " else " + prettyPrint(stat1) + "fi"
      case Scope(stat) => "begin " + prettyPrint(stat) + " end"
      case Combine(atoms) =>
        val sb = new StringBuilder("")
        for (i <- atoms.indices){
          sb.append(prettyPrint(atoms(i)))
          sb.append(";")
        }
        sb.setLength(sb.length() - 1).toString
    }
  }

}
