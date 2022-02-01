import parsley.Parsley
import Parsley._
import parsley.character.{noneOf, oneOf, string}
import parsley.combinator.{eof, many}
import parsley.implicits.character.charLift
import parsley.implicits.character.stringLift

/*
⟨program ⟩      ::=   ‘begin’  ⟨func⟩*  ⟨stat ⟩  ‘end’
⟨func⟩          ::=    ⟨type ⟩  ⟨ident ⟩  ‘(’  ⟨param-list ⟩?   ‘)’  ‘is’  ⟨stat ⟩  ‘end’
⟨param-list ⟩   ::=    ⟨param ⟩  (  ‘,’  ⟨param ⟩  )*
⟨param ⟩        ::=    ⟨type ⟩  ⟨ident ⟩ 
⟨stat ⟩         ::=   ‘skip’
    |        ⟨type ⟩  ⟨ident ⟩  ‘=’  ⟨assign-rhs ⟩
    |        ⟨assign-lhs ⟩  ‘=’  ⟨assign-rhs ⟩
    |        ‘read’  ⟨assign-lhs ⟩
    |        ‘free’  ⟨expr ⟩
    |        ‘return’  ⟨expr ⟩
    |        ‘exit’  ⟨expr ⟩
    |        ‘print’  ⟨expr ⟩
    |        ‘println’  ⟨expr ⟩
    |        ‘if’  ⟨expr ⟩  ‘then’  ⟨stat ⟩  ‘else’  ⟨stat ⟩  ‘fi’ 
    |        ‘while’  ⟨expr ⟩  ‘do’  ⟨stat ⟩  ‘done’
    |        ‘begin’  ⟨stat ⟩  ‘end’ 
    |        ⟨stat ⟩  ‘;’  ⟨stat ⟩
⟨assign-lhs ⟩    ::=    ⟨ident ⟩
    |        ⟨array-elem ⟩ 
    |        ⟨pair-elem ⟩
⟨assign-rhs ⟩     ::=    ⟨expr ⟩
    |        ⟨array-liter ⟩
    |        ‘newpair’  ‘(’  ⟨expr ⟩  ‘,’  ⟨expr ⟩  ‘)’ 
    |        ⟨pair-elem ⟩
    |        ‘call’  ⟨ident ⟩  ‘(’  ⟨arg-list ⟩?   ‘)’
⟨arg-list ⟩       ::=    ⟨expr ⟩  (‘,’  ⟨expr ⟩  )*
⟨pair-elem ⟩      ::= ‘fst’ ⟨expr ⟩
    |        ‘snd’  ⟨expr ⟩
⟨type ⟩           ::=    ⟨base-type ⟩ 
    |        ⟨array-type ⟩ 
    |        ⟨pair-type ⟩
⟨base-type ⟩      ::=   ‘int’
    |        ‘bool’ 
    |        ‘char’ 
    |        ‘string’
⟨array-type ⟩     ::=    ⟨type ⟩  ‘[’  ‘]’
⟨pair-type ⟩      ::=   ‘pair’  ‘(’  ⟨pair-elem-type ⟩  ‘,’  ⟨pair-elem-type ⟩  ‘)’ 
⟨pair-elem-type ⟩         ::=    ⟨base-type ⟩
    |        ⟨array-type ⟩ 
    |        ‘pair’
⟨expr ⟩           ::=    ⟨int-liter ⟩
    |        ⟨bool-liter ⟩
    |        ⟨char-liter ⟩
    |        ⟨str-liter ⟩
    |        ⟨pair-liter ⟩
    |        ⟨ident ⟩
    |        ⟨array-elem ⟩
    |        ⟨unary-oper ⟩  ⟨expr ⟩
    |        ⟨expr ⟩  ⟨binary-oper ⟩  ⟨expr ⟩
    |        ‘(’  ⟨expr ⟩  ‘)’
⟨unary-oper ⟩     ::=   ‘!’  |  ‘-’  |  ‘len’  |  ‘ord’  |  ‘chr’
⟨binary-oper ⟩    ::= ‘*’ | ‘/’ | ‘%’ | ‘+’ | ‘-’ | ‘>’ | ‘>=’ | ‘<’ | ‘<=’ | ‘==’ | ‘!=’ | ‘&&’ | ‘||’
⟨ident ⟩          ::=   (  ‘_’  |  ‘a’-‘z’  |  ‘A’-‘Z’  )  (  ‘      ’  |  ‘a’-‘z’  |  ‘A’-‘Z’  |  ‘0’-‘9’  )*
⟨array-elem ⟩     ::=    ⟨ident ⟩  (‘[’  ⟨expr ⟩  ‘]’)+
⟨int-liter ⟩      ::=    ⟨int-sign ⟩?   ⟨digit ⟩+ 
⟨digit ⟩          ::=   (‘0’-‘9’)
⟨int-sign ⟩       ::=   ‘+’  |  ‘-’
⟨bool-liter ⟩     ::=   ‘true’  |  ‘false’ 
⟨char-liter ⟩     ::=   ‘'’  ⟨character ⟩  ‘'’ 
⟨str-liter ⟩      ::=   ‘"’  ⟨character ⟩*  ‘"’
⟨character ⟩      ::=    any-ASCII-character-except-‘\’-‘'’-‘"’  |  ‘\’  ⟨escaped-char ⟩
⟨escaped-char ⟩   ::=   ‘0’  |  ‘b’  |  ‘t’  |  ‘n’  |  ‘f’  |  ‘r’  |  ‘"’  |  ‘'’  |  ‘\’ 
⟨array-liter ⟩    ::=   ‘[’  (  ⟨expr ⟩  (‘,’  ⟨expr ⟩)*  )?   ‘]’
⟨pair-liter ⟩     ::=   ‘null’
⟨comment ⟩        ::=   ‘#’  (any-character-except-EOL)*  ⟨EOL⟩
*/

lazy val program = "begin" ~> many(func) ~> stat <~ "end" <~ eof

lazy val func = _type ~> ident ~> '(' ~> (noneOf(param_list) <|> oneOf(param_list)) ~> ')' ~> "is" ~> stat <~ "end"

lazy val param_list = param <~> many(',' <~> param)

lazy val param = _type <-> ident

lazy val stat = attempt("skip") <|>
                _type ~> ident ~> '=' ~> assign_rhs <|>
                assign_lhs ~> '=' ~> assign_rhs <|>
                "read" = assign_lhs <|>
                "free" ~> expr <|>
                "return" ~> expr <|>
                "exit" ~> expr <|>
                "print" ~> expr <|>
                "println" ~> expr <|>
                "if" ~> expr ~> "then" ~> stat

lazy val assign_lhs = ???
lazy val assign_rhs = ???
lazy val arg_list = ???
lazy val pair_elem = ???
lazy val _type = ???
lazy val base_type = ???
lazy val array_type = ???
lazy val pair_type = ???
lazy val pair_elem_type = ???
lazy val expr = ???
lazy val unary_oper = ???
lazy val binary_oper = ???
lazy val ident = ???
lazy val array_elem = ???
lazy val int_liter = ???
lazy val digit = ???
lazy val int_sign = ???
lazy val bool_liter = ???
lazy val char_liter = ???
lazy val str_liter = ???
lazy val character = ???
lazy val escaped_char = ???
lazy val array_liter = ???
lazy val pair_liter = ???
lazy val comment = ???