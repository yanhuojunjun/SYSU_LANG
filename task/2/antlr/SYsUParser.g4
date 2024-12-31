parser grammar SYsUParser;

options {
  tokenVocab=SYsULexer;
}

//表达式-------------------------------------------------------
//------------------------------------------------------------

primaryExpression       //基本表达式：常量或者变量(包括数组),函数调用
    :   Identifier
    |   Constant
    |   array
    |   functioncall
    ;

array  //数组
    :   Identifier(LeftBracket assignmentExpression? RightBracket)+
    ;

postfixExpression     //后缀表达式
    :   primaryExpression  //基本表达式：常量或者变量
    ;

unaryExpression        //加正负号的变量或者常量
    :   postfixExpression
    |   parenExpression
    |   unaryOperator unaryExpression
    ;

unaryOperator          //正负号，否定
    :   Plus | Minus | Exclaim
    ;

parenExpression //括号表达式：（加性）
    :   LeftParen additiveExpression RightParen
    ;

multiplicativeExpression //乘性表达式：一元表达式   */%  一元表达式
    :   (unaryExpression|parenExpression) ((Star|Percent|Slash) (unaryExpression|parenExpression))*
    ;

additiveExpression      //加性表达式：乘性表达式   +-  乘性表达式
    :   multiplicativeExpression ((Plus|Minus) multiplicativeExpression)*
    ;

condition_low   //优先级较高的条件表达式
    :   additiveExpression ((Greater|Less|Greaterequal|Lessequal|Equalequal|Exclaimequal) additiveExpression)*
    ;

condition_mid  //优先级较中的条件表达式  
    :   condition_low  (Ampamp condition_low )*
    ;

condition_high  //优先级较低的条件表达式  
    :   condition_mid  (Pipepipe condition_mid )*
    ;

assignmentExpression     //赋值表达式：一元表达式=一元表达式。。。==加性表达式
    :   additiveExpression
    |   unaryExpression Equal assignmentExpression
    ;

expression   // 表达式：多个赋值表达式
    :   assignmentExpression (Comma assignmentExpression)*
    ;

//声明-----------------------------------------------------------
//---------------------------------------------------------------

declaration  // 声明： 声明说明符  +  初始化列表
    :   declarationSpecifiers initDeclaratorList? Semi
    ;

declarationSpecifiers //一个或多个声明说明符
    :   declarationSpecifier+
    ;

declarationSpecifier  //一个声明说明符
    :   typeSpecifier
    ;

typeSpecifier
    :   Int
    |   Void
    |   Const

    ;

initDeclaratorList    // 初始化列表
    :   initDeclarator (Comma initDeclarator)*
    ;

initDeclarator       // 初始化：  声明变量 + 初始化式子
    :   declarator (Equal initializer)?
    ;

declarator            // 声明变量
    :   directDeclarator
    ;

directDeclarator      // 直接声明变量：变量或者数组
    :   Identifier
    |   directDeclarator LeftBracket assignmentExpression? RightBracket
    ;

identifierList       //变量列表
    :   Identifier (Comma Identifier)*
    ;

initializer   //初始化器：{赋值表达式，赋值表达式，赋值表达式......}
    :   assignmentExpression
    |   LeftBrace initializerList? Comma? RightBrace
    ;

initializerList
    // :   designation? initializer (Comma designation? initializer)*
    :   initializer (Comma initializer)*
    ;

//语句---------------------------------------------------------------------
//---------------------------------------------------------------------

statement            //语句：复合语句，表达式语句，跳转语句
    :   compoundStatement
    |   expressionStatement
    |   jumpStatement
    |   ifStatement
    |   whileStatement
    |   breakStatement
    |   continueStatement
    ;

ifStatement     // if语句
    :   If LeftParen condition_high RightParen (compoundStatement|blockItem) 
        (Else (compoundStatement|blockItem))?
    ;

whileStatement  //while语句
    :   While LeftParen condition_high RightParen (compoundStatement|blockItem)
    ; 

breakStatement   //break
    :   Break Semi
    ;

continueStatement   //continue
    :   Continue Semi
    ;

compoundStatement   //复合语句：{1;2;3;}
    :   LeftBrace blockItemList? RightBrace
    ;

blockItemList
    :   blockItem+
    ;

blockItem
    :   statement
    |   declaration
    ;

expressionStatement    //表达式语句：表达式
    :   expression? Semi
    ;



jumpStatement         //跳转语句
    :   (Return expression?)
    Semi
    ;

compilationUnit    //编译单元
    :   translationUnit? EOF
    ;

translationUnit    //翻译单元：一个或者多个外部声明
    :   externalDeclaration+
    ;

externalDeclaration  //外部声明：函数声明或者声明
    :   functionDefinition
    |   declaration
    ;

//函数---------------------------------------------------------------
//--------------------------------------------------------------------
declarations
    :   declaration (Comma declaration)*
    ;

functionDefinition   //函数声明：  声明说明符  直接声明变量（） 复合语句
    : declarationSpecifiers directDeclarator LeftParen declarations RightParen compoundStatement?
    ;

functioncall
    : directDeclarator LeftParen additiveExpression? (Comma additiveExpression)* RightParen
    ;