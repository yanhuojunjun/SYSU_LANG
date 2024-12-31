lexer grammar SYsULexer;

Int : 'int';
Const : 'const';
Return : 'return';
Void : 'void';

LeftParen : '(';
RightParen : ')';
LeftBracket : '[';
RightBracket : ']';
LeftBrace : '{';
RightBrace : '}';

Plus : '+';
Minus: '-';
Star: '*';
Percent: '%';
Slash: '/';

Semi : ';';
Comma : ',';

Equalequal: '==';
Equal : '=';
Greater : '>';
Less : '<';
Lessequal : '<=';
Greaterequal : '>=';
Pipepipe : '||';
Ampamp : '&&';
Exclaimequal : '!=';
Exclaim : '!';

If : 'if';
Else : 'else';
While : 'while';
Break : 'break';
Continue : 'continue';

//变量----
Identifier 
    :   IdentifierNondigit
        (   IdentifierNondigit
        |   Digit
        )*
    ;

fragment
IdentifierNondigit
    :   Nondigit
    ;

fragment
Nondigit
    :   [a-zA-Z_]
    ;

fragment
Digit
    :   [0-9]
    ;

//数字----
Constant   
    :   IntegerConstant
    ;

fragment
IntegerConstant
    :   DecimalConstant
    |   OctalConstant
    |   HexConstant
    ;

fragment
DecimalConstant
    :   NonzeroDigit Digit*
    ;

fragment
OctalConstant
    :   '0' OctalDigit*
    ;

fragment
HexConstant
    :   '0x' HexDigit*
    ;

fragment
NonzeroDigit
    :   [1-9]
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
HexDigit
    :   [0-9a-f]
    ;

// 预处理信息处理，可以从预处理信息中获得文件名以及行号
// 预处理信息前面的数组即行号
LineAfterPreprocessing  //
    :   '#' Whitespace* ~[\r\n]*
    ;

Whitespace   //空格
    :   [ \t]+
    ;

Newline     //换行符号，可以利用这个信息来更新行号
    :   (   '\r' '\n'?  // 回车符后可能跟随的换行符
        |   '\n'
        )
    ;

