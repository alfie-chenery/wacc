package parsers

import org.scalatest.flatspec.AnyFlatSpec

class parserSpec extends AnyFlatSpec{
  import Parser._
  import lexer._
  import Ast._

  behavior of "<int-liter>"
  it should "parse positive numbers" in {
    assert(INT_LITER.parse("0").get == 0)
    assert(INT_LITER.parse("3248").get == 3248)
    assert(INT_LITER.parse("2342342").get == 2342342)
  }
  /*
  it should "parse negative numbers" in {
    assert(INT_LITER.parse("-1").get == -1)
    assert(INT_LITER.parse("-324").get == -324)
    assert(INT_LITER.parse("-5436").get == -5436)
  }
   */

  "<bool-liter>" should "parse boolean values" in {
    assert(BOOL_LITER.parse("true").get)
    assert(!BOOL_LITER.parse("false").get)
  }

  "<char-liter>" should "parse characters" in {
    assert(CHAR_LITER.parse("\'t\'").get == 't')
    assert(CHAR_LITER.parse("\'f\'").get == 'f')
    assert(CHAR_LITER.parse("\'a\'").get == 'a')
    assert(CHAR_LITER.parse("\'$\'").get == '$')
  }

  /*
  "<str-liter>" should "parse strings" in {
    assert(STR_LITER.parse("\"test\"").get == "test")
    assert(STR_LITER.parse("\"This is WACC!\"").get == "This is WACC!")
    assert(STR_LITER.parse("\"!£$%^&*()\"").get == "!£$%^&*()")
  }
   */

  // TODO implement this test
  "<pair-liter>" should "parse the pair literal" in {
  }

  "<escaped-char>" should "parse escaped characters" in {
  }

  "<ident>" should "parse only valid identity strings" in {
    assert(lex.identifier.parse("shouldWork").isSuccess)
    assert(lex.identifier.parse("24shouldFail").isFailure)
  }

  behavior of "<array-liter>"
  they should "parse integer array literals" in {
    assert(`<array-liter>`.parse("[123]").get == ArrayLiter(List(IntLiter(123))))
    assert(`<array-liter>`.parse("[123, 456]").get == ArrayLiter(List(IntLiter(123), IntLiter(456))))
  }
  they should "parse boolean array literals" in {
    assert(`<array-liter>`.parse("[true]").get == ArrayLiter(List(BoolLiter(true))))
    assert(`<array-liter>`.parse("[true, false]").get == ArrayLiter(List(BoolLiter(true), BoolLiter(false))))
  }

  "<array-elem>" should "parse array elems" in {
    assert(`<array-elem>`.parse("test[12]").get == ArrayElem(Ident("test"), List(IntLiter(12))))
    assert(`<array-elem>`.parse("array[12][34]").get == ArrayElem(Ident("array"), List(IntLiter(12), IntLiter(34))))
  }

  behavior of "<expr>"
  it should "parse int literals" in {
    assert(`<expr>`.parse("435").get == IntLiter(435))
    //assert(`<expr>`.parse("-352").get == IntLiter(-352))
  }
  it should "parse bool literals" in {
    assert(`<expr>`.parse("true").get == BoolLiter(true))
    assert(`<expr>`.parse("false").get == BoolLiter(false))
  }
  it should "parse char literals" in {
    assert(`<expr>`.parse("\'a\'").get == CharLiter('a'))
    assert(`<expr>`.parse("\'$\'").get == CharLiter('$'))
    assert(`<expr>`.parse("\'G\'").get == CharLiter('G'))
  }
  /*
  it should "parse string literals" in {
    assert(`<expr>`.parse("This is WACC!").get == StrLiter("This is WACC!"))
  }
   */
  it should "parse pair literals" in {
    assert(`<expr>`.parse("null").get == PairLiter)
  }
  /*
  it should "parse identifiers" in {
    assert(`<expr>`.parse("validIdentifier").isSuccess)
    assert(`<expr>`.parse("32invalidIdentifier").isFailure)
  }
  it should "parse array elems" in {
    assert(`<expr>`.parse("test[12]").get == ArrayElem(Ident("test"), List(IntLiter(12))))
    assert(`<expr>`.parse("array[12][34]").get == ArrayElem(Ident("array"), List(IntLiter(12), IntLiter(34))))
  }
  it should "parse unary application" in {
    assert(`<expr>`.parse("!true").get == Not(BoolLiter(true)))
    assert(`<expr>`.parse("-536").get == Negate(IntLiter(536)))
    assert(`<expr>`.parse("len \"test\"").get == Len(StrLiter("test")))
    assert(`<expr>`.parse("chr \'t\'").get == Chr(CharLiter('t')))
  }
   */
  it should "parse binary application" in {
    // TODO add more binary app tests
    assert(`<expr>`.parse("5 * 3").get == Mult(IntLiter(5), IntLiter(3)))
    assert(`<expr>`.parse("4 % 2").get == Mod(IntLiter(4), IntLiter(2)))
  }
  /*
  it should "parse bracketed expressions" in {
    assert(`<expr>`.parse("(5435)").get == ParensExpr(IntLiter(5435)))
    assert(`<expr>`.parse("(test[12])").get == ParensExpr(ArrayElem(Ident("test"), List(IntLiter(12)))))
  }
   */

  behavior of "<pair-elem-type>"
  it should "parse base type" in {
    assert(`<pair-elem-type>`.parse("int").get == WInt)
    assert(`<pair-elem-type>`.parse("bool").get == WBool)
    assert(`<pair-elem-type>`.parse("char").get == WChar)
    assert(`<pair-elem-type>`.parse("string").get == WString)
  }
  /*
  it should "parse array types" in {
    assert(`<pair-elem-type>`.parse("int[]").get == ArrayType(WInt))
    assert(`<pair-elem-type>`.parse("bool[]").get == ArrayType(WBool))
    assert(`<pair-elem-type>`.parse("bool[][]").get == ArrayType(WBool))
    assert(`<pair-elem-type>`.parse("pair(int, bool)[]").get == ArrayType(WBool))
  }
   */
  it should "parse pair" in {
    assert(`<pair-elem-type>`.parse("pair").get == Pair)
  }

  /*
  "<pair-type>" should "parse pairs" in {
    info("These may fail if <pair-elem-type> fails")
    assert(`<pair-type>`.parse("pair(int, bool)").get == PairType(WInt, WBool))
    assert(`<pair-type>`.parse("pair(string[], char[])").get == PairType(ArrayType(WString), ArrayType(WChar)))
    assert(`<pair-type>`.parse("pair(pair, pair)").get == PairType(ArrayType(WString), ArrayType(WChar)))
  }
   */

  "<array-type>" should "parse array types" in {
    info("These may fail if <type> fails")
    assert(`<array-type>`.parse("int[]").get == ArrayType(WInt))
    assert(`<array-type>`.parse("bool[]").get == ArrayType(WBool))
    // assert(`<array-type>`.parse("pair(int, bool)[]").get == ArrayType(PairType(WInt, WBool)))
    assert(`<array-type>`.parse("int[][]").get == ArrayType(ArrayType(WInt)))
  }

  "<base-type>" should "parse the 4 base type" in {
    assert(`<base-type>`.parse("int").get == WInt)
    assert(`<base-type>`.parse("bool").get == WBool)
    assert(`<base-type>`.parse("char").get == WChar)
    assert(`<base-type>`.parse("string").get == WString)
  }

  behavior of "<type>"
  it should "parse base type" in {
    assert(`<type>`.parse("string").get == WString)
    assert(`<type>`.parse("int").get == WInt)
  }
  it should "parse array types" in {
    info("will fail if <type> fails")
    assert(`<type>`.parse("int[]").get == ArrayType(WInt))
    assert(`<type>`.parse("bool[][]").get == ArrayType(ArrayType(WBool)))
    assert(`<type>`.parse("pair(int, char)[][]").get == ArrayType(ArrayType(PairType(WInt, WChar))))
  }
  it should "parse pair types" in {
    assert(`<type>`.parse("pair(int, char)").get == PairType(WInt, WChar))
  }

  behavior of "<pair-elem>"
  /*
  it should "parse fst elements" in {
    info("May fail if <expr> fails")
    assert(`<pair-elem>`.parse("fst 352").get == FstPair(IntLiter(352)))
    assert(`<pair-elem>`.parse("fst \"test\"").get == FstPair(StrLiter("test")))
  }
  it should "parse snd elements" in {
    info("May fail if <expr> fails")
    assert(`<pair-elem>`.parse("snd 352").get == SndPair(IntLiter(352)))
    assert(`<pair-elem>`.parse("snd \"test\"").get == SndPair(StrLiter("test")))
  }
   */

  "<arg-list>" should "parse multiple comma seperated expressions" in {
    // TODO add more arg list tests
    info("May fail if <expr> fails")
    assert(`<arg-list>`.parse("325,865,345").get == ArgList(List(IntLiter(325), IntLiter(865), IntLiter(345))))
  }

  behavior of "<assign-rhs>"
  it should "parse expressions" in {
    info("May fail if <expr> fails")
    assert(`<assign-rhs>`.parse("364634").get == IntLiter(364634))
  }
  it should "parse array literals" in {
    assert(`<assign-rhs>`.parse("[1,2,3]").get == ArrayLiter(List(IntLiter(1), IntLiter(2), IntLiter(3))))
  }
  it should "parse new pairs" in {
    assert(`<assign-rhs>`.parse("newpair(325,643)").get == NewPair(IntLiter(325), IntLiter(643)))
  }
  it should "parse pair elems" in {
    assert(`<assign-rhs>`.parse("fst 1").get == FstPair(IntLiter(1)))
  }
  it should "parse function calls" in {
    assert(`<assign-rhs>`.parse("call test(223,2)").get == Call(Ident("test"), ArgList(List(IntLiter(223), IntLiter(2)))))
  }

  behavior of "<assign-lhs>"
  it should "parse identifiers" in {
    assert(`<assign-lhs>`.parse("validIdentifier").isSuccess)
    assert(`<assign-lhs>`.parse("32invalidIdentifier").isFailure)
  }
  it should "parse array elems" in {
    assert(`<assign-lhs>`.parse("arr[34][35]").get == ArrayElem(Ident("arr"), List(IntLiter(34), IntLiter(35))))
  }
  it should "parse pair elems" in {
    assert(`<assign-lhs>`.parse("fst 1").get == FstPair(IntLiter(1)))
    assert(`<assign-lhs>`.parse("snd 65").get == SndPair(IntLiter(65)))
  }
}
