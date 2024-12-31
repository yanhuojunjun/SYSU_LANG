#include "SYsULexer.h" // 确保这里的头文件名与您生成的词法分析器匹配
#include <fstream>
#include <iostream>
#include <unordered_map>

// 映射定义，将ANTLR的tokenTypeName映射到clang的格式
std::unordered_map<std::string, std::string> tokenTypeMapping = {
  { "Int", "int" },
  { "Identifier", "identifier" },
  { "LeftParen", "l_paren" },
  { "RightParen", "r_paren" },
  { "RightBrace", "r_brace" },
  { "LeftBrace", "l_brace" },
  { "LeftBracket", "l_square" },
  { "RightBracket", "r_square" },
  { "Constant", "numeric_constant" },
  { "Return", "return" },
  { "Semi", "semi" },
  { "EOF", "eof" },
  { "Equal", "equal" },
  { "Plus", "plus" },
  { "Comma", "comma" },
  { "Const", "const" },
  { "Minus", "minus" },
  { "Star", "star" },
  { "Slash", "slash" },
  { "Percent", "percent" },
  { "Greater", "greater" },
  { "If", "if" },
  { "While", "while" },
  { "Else", "else" },
  { "Equalequal", "equalequal" },
  { "Void", "void" },
  { "Pipepipe", "pipepipe" },
  { "Ampamp", "ampamp" },
  { "Less", "less" },
  { "Break", "break" },
  { "Continue", "continue" },
  { "Lessequal", "lessequal" },
  { "Greaterequal", "greaterequal" },
  { "Exclaimequal", "exclaimequal" },
  { "Exclaim", "exclaim" },

// 在这里继续添加其他映射
};


void print_token(const antlr4::Token* token,
            const antlr4::CommonTokenStream& tokens,
            std::ofstream& outFile,
            const antlr4::Lexer& lexer)
{ 
  //将g4文件中的所有别名都取过来
  auto& vocabulary = lexer.getVocabulary();
  // Id --> 别名
  auto tokenTypeName =
    std::string(vocabulary.getSymbolicName(token->getType()));
  // 处理可能的空字符串情况
  if (tokenTypeName.empty())
    tokenTypeName = "<UNKNOWN>"; 
  // Antlr类别的别名到clang类别名称的映射
  if (tokenTypeMapping.find(tokenTypeName) != tokenTypeMapping.end()) {
    tokenTypeName = tokenTypeMapping[tokenTypeName];
  }
  static bool leadingSpace = 0;      //判断是否在空格后
  static bool startOfLine = 0;       //判断是否是新的一行
  static std::string location;       //地址
  static int line;                   //行号
  // 如果是preprocessing line，提取地址和行号，并跳过
  if (tokenTypeName == "LineAfterPreprocessing") {
    //提取地址
    std::string s = token->getText();
    size_t first_quote = s.find("\"");
    size_t second_quote = s.find("\"", first_quote + 1);
    location = s.substr(first_quote + 1, second_quote - first_quote - 1);
    //提取行号
    line = 0;
    int ptr = 2;
    while(s[ptr]==' ') ptr++;
    while(s[ptr]!=' '){
      line = line * 10 + (s[ptr]-'0');
      ptr++;
    }
    line = line - 1; //去掉注释行的换行符
    return;
  }
  // 如果是whitespace，记录并跳过
  if(tokenTypeName=="Whitespace"){
    leadingSpace = 1;
    return;
  }
  // 如果是newline，记录，更新行号，并跳过
  if(tokenTypeName=="Newline"){
    startOfLine = 1;
    line++;
    return;
  }
  // location信息
  std::string locInfo =
    "Loc=<" + location + ":" +
    std::to_string(line) + ":" +
    std::to_string(token->getCharPositionInLine() + 1) + ">";

  // 输出----------
  //输出token内容和名称
  if (token->getText() != "<EOF>")
    outFile << tokenTypeName << " '" << token->getText() << "'";
  else
    outFile << tokenTypeName << " '"<< "'";
  //输出空格和换行信息
  int cnt = 0;   //记录标志的个数，便于协调空格
  if (startOfLine) {
    outFile << "\t [StartOfLine]";
    startOfLine = 0; //清零，很重要
    cnt++;
  }
  if (leadingSpace){
    if(cnt==0)
      outFile << "\t [LeadingSpace]";
    else outFile << " [LeadingSpace]";
    leadingSpace = 0; //清零，很重要
    cnt++;
  }
  //输出位置信息
  if(cnt!=0)
    outFile << "\t" << locInfo << std::endl;
  else
    outFile << "\t\t" << locInfo << std::endl;

}

int
main(int argc, char* argv[])
{
  if (argc != 3) {
    std::cout << "Usage: " << argv[0] << " <input> <output>\n";
    return -1;
  }

  std::ifstream inFile(argv[1]);
  if (!inFile) {
    std::cout << "Error: unable to open input file: " << argv[1] << '\n';
    return -2;
  }

  std::ofstream outFile(argv[2]);
  if (!outFile) {
    std::cout << "Error: unable to open output file: " << argv[2] << '\n';
    return -3;
  }

  std::cout << "程序 '" << argv[0] << std::endl;
  std::cout << "输入 '" << argv[1] << std::endl;
  std::cout << "输出 '" << argv[2] << std::endl;

  antlr4::ANTLRInputStream input(inFile);
  SYsULexer lexer(&input);

  antlr4::CommonTokenStream tokens(&lexer);
  tokens.fill();

  for (auto&& token : tokens.getTokens()) {
    print_token(token, tokens, outFile, lexer);
  }
}
