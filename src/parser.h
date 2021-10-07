#pragma once
#include "ast.h"
#include "lexer.h"

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

class Parser {
    Lexer lexer;

    /// LogError* - These are little helper functions for error handling.
    std::unique_ptr<ExprAST> logError(const char *Str) {
        fprintf(stderr, "LogError, line %u: %s\n", lexer.line, Str);
        return nullptr;
    }

    std::unique_ptr<PrototypeAST> logErrorP(const char *Str) {
        logError(Str);
        return nullptr;
    }

    /// GetTokPrecedence - Get the precedence of the pending binary operator token.
    int getTokPrecedence() {
        switch(curTok) {
            case '=':
                return 2;
            case tok_equal:
            case tok_nonequal:
                return 9;
            case '<':
            case '>':
                return 10;
            case '+':
            case '-':
                return 20;
            case '*':
            case '/':
                return 40;
            default:
                return -1;
        }
    }

    /// doubleexpr ::= double
    std::unique_ptr<ExprAST> parseDoubleExpr(bool);
    /// intexpr ::= int
    std::unique_ptr<ExprAST> parseIntExpr(bool);
    /// charexpr ::= 'char'
    std::unique_ptr<ExprAST> parseCharExpr(bool);
    /// stringexpr ::= "string"
    std::unique_ptr<ExprAST> parseStringExpr(bool);
    /// expression
    ///   ::= var/return unary binoprhs
    std::unique_ptr<ExprAST> parseExpression();
    /// varexpr ::= var type identifier = value; / var type identifier;
    std::unique_ptr<ExprAST> parseVarExpr(bool);
    /// parenexpr ::= '(' expression ')'
    std::unique_ptr<ExprAST> parseParenExpr(bool);
    /// blockexpr ::= '{' expression* '}'
    std::unique_ptr<ExprAST> parseBlockExpr();
    /// identifierexpr
    ///   ::= identifier
    ///   ::= identifier '(' expression* ')'
    std::unique_ptr<ExprAST> parseIdentifierExpr(bool);
    /// ifexpr ::= 'if' expression 'then' expression 'else' expression
    std::unique_ptr<ExprAST> parseIfExpr();
    /// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
    std::unique_ptr<ExprAST> parseForExpr();
    /// primary
    ///   ::= identifierexpr
    ///   ::= doubleexpr
    ///   ::= intexpr
    ///   ::= charexpr
    ///   ::= stringexpr
    ///   ::= parenexpr
    ///   ::= ifexpr
    ///   ::= forexpr
    std::unique_ptr<ExprAST> parsePrimary(bool);
    /// unary
    ///   ::= primary
    ///   ::= '!*&' unary
    std::unique_ptr<ExprAST> parseUnary(bool);
    /// binoprhs
    ///   ::= ('+' unary)*
    std::unique_ptr<ExprAST> parseBinOpRHS(int, std::unique_ptr<ExprAST>);
    /// prototype
    ///   ::= type id '(' (type id)* ')'
    std::unique_ptr<PrototypeAST> parsePrototype();
public:
    int curTok;
    /// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
    /// token the parser is looking at.  getNextToken reads another token from the
    /// lexer and updates CurTok with its results.
    int getNextToken() {
        return curTok = lexer.gettok();
    }

    bool errorRecovery() {
      int previousLine = lexer.line;
      while(previousLine == lexer.line && curTok != EOF)
        getNextToken();
      if(curTok == EOF)
        return false;
      return true;
    }

    /// definition ::= 'def' prototype expression
    std::unique_ptr<FunctionAST> parseDefinition();
    /// external ::= 'extern' prototype
    std::unique_ptr<PrototypeAST> parseExtern();
    /// toplevelexpr ::= expression
    std::unique_ptr<FunctionAST> parseTopLevelExpr();
};

std::unique_ptr<ExprAST> Parser::parseDoubleExpr(bool isReturning) {
  auto Result = std::make_unique<DoubleExprAST>(lexer.numVal, isReturning, lexer.line);
  getNextToken(); // consume the number
  return std::move(Result);
}

std::unique_ptr<ExprAST> Parser::parseIntExpr(bool isReturning) {
  auto Result = std::make_unique<IntExprAST>(lexer.numValInt, isReturning, lexer.line);
  getNextToken(); // consume the number
  return std::move(Result);
}

std::unique_ptr<ExprAST> Parser::parseCharExpr(bool isReturning) {
  getNextToken();
  char c = lexer.identifierStr[0];
  if(c < 0 || c > 127)
    return logError("Char outside ASCII value.");
  auto Result = std::make_unique<CharExprAST>(c, isReturning, lexer.line);
  getNextToken(); // consume the char
  if(curTok != '\'')
    return logError("Expected '.");
  getNextToken();
  return std::move(Result);
}

std::unique_ptr<ExprAST> Parser::parseStringExpr(bool isReturning) {
  auto Result = std::make_unique<StringExprAST>(lexer.identifierStr, isReturning, lexer.line);
  getNextToken();
  return std::move(Result);
}

std::unique_ptr<ExprAST> Parser::parseVarExpr(bool isReturning) {
  int type = getNextToken();  //eat var
  if(type != tok_char_string && type != tok_double_string && type != tok_int_string && type != tok_string_string)
    return logError("Expected type after var keyword.");
  int count = 0;
  while(getNextToken() == '*') {
    count++;
  }
  if(curTok != tok_identifier)
    return logError("Expected variable name.");
  std::string name = lexer.identifierStr;
  if(getNextToken() == ';') {
    getNextToken();
    if(count > 0)
      return std::make_unique<VarExprAST>(name, isReturning, type, count, 
      std::make_unique<PointerExprAST>(isReturning, nullptr, lexer.line), lexer.line);
    if(type == tok_char_string)
      return std::make_unique<VarExprAST>(name, isReturning, type, count, std::make_unique<CharExprAST>(0, isReturning, lexer.line), lexer.line);
    if(type == tok_double_string) 
      return std::make_unique<VarExprAST>(name, isReturning, type, count, std::make_unique<DoubleExprAST>(0.0, isReturning, lexer.line), lexer.line);
    if(type == tok_int_string)
      return std::make_unique<VarExprAST>(name, isReturning, type, count, std::make_unique<IntExprAST>(0, isReturning, lexer.line), lexer.line);
    if(type == tok_string_string)
      return std::make_unique<VarExprAST>(name, isReturning, type, count, std::make_unique<StringExprAST>("", isReturning, lexer.line), lexer.line);
  }

  if(curTok != '=')
    return logError("Expected ';' or '=' after variable declaration.");
  getNextToken(); // consume =
  auto E = parseExpression();
  if(!E)
    return nullptr;
  getNextToken();
  return std::make_unique<VarExprAST>(name, isReturning, type, count, std::move(E), lexer.line);
}

std::unique_ptr<ExprAST> Parser::parseParenExpr(bool isReturning) {
  getNextToken(); // eat (.
  auto V = parseExpression();
  if (!V) {
    return nullptr;
  }

  if(V->isReturning_ && isReturning) 
    return logError("Double return found.");
  V->isReturning_ = V->isReturning_ || isReturning;

  if (curTok != ')') {
    return logError("Expected ')'.");
  }

  getNextToken(); // eat ).
  return V;
}

std::unique_ptr<ExprAST> Parser::parseBlockExpr() {
  if(curTok != '{') {
    return logError("Expected '{'.");
  }
  getNextToken(); // eat {.
  std::vector<std::unique_ptr<ExprAST>> expressions;
  std::unique_ptr<ExprAST> next;
  bool returnFound = false;

  while (1) {
    // Read next expression.
    next = parseExpression();

    if (!next) {
      return logError("Expected '}' at the end of the function.");
    }
    VarExprAST* LHSE = dynamic_cast<VarExprAST*>(next.get());
    if (!LHSE) {
      if(curTok != ';') {
        return logError("Expected ';' after expression.");
      }
      getNextToken(); // eat ;
    }

    if(next->isReturning_) {
      returnFound = true;
    }

    expressions.push_back(std::move(next));

    // End of expressions, exit loop.
    if (curTok == '}') break;
  }
  getNextToken(); // eat the '}'.

  return std::make_unique<BlockExprAST>(std::move(expressions), returnFound, lexer.line);
}

std::unique_ptr<ExprAST> Parser::parseIdentifierExpr(bool isReturning) {
  std::string idName = lexer.identifierStr;

  getNextToken();  // eat identifier.

  if (curTok != '(') { // Simple variable ref.
    return std::make_unique<VariableExprAST>(idName, isReturning, lexer.line);
  }

  // Call.
  getNextToken();  // eat (
  std::vector<std::unique_ptr<ExprAST>> args;
  if (curTok != ')') {
    while (1) {
      if (auto arg = parseExpression()) {
        args.push_back(std::move(arg));
      } else {
        return nullptr;
      }

      if (curTok == ')') {
        break;
      }

      if (curTok != ',') {
        return logError("Expected ')' or ',' in argument list.");
      }

      getNextToken();
    }
  }

  // Eat the ')'.
  getNextToken();

  return std::make_unique<CallExprAST>(idName, std::move(args), isReturning, lexer.line);
}

std::unique_ptr<ExprAST> Parser::parseIfExpr() {
  getNextToken();  // eat the if.

  // condition.
  auto cond = parseExpression();
  if (!cond)
    return nullptr;

  auto then = parseBlockExpr();
  if (!then)
    return nullptr;

  if (curTok != tok_else)
    return logError("Expected else.");

  getNextToken();

  auto else_ = parseBlockExpr();
  if (!else_)
    return nullptr;

  bool isReturning = then->isReturning_ && else_->isReturning_;

  return std::make_unique<IfExprAST>(std::move(cond), std::move(then),
                                      std::move(else_), isReturning, lexer.line);
}

std::unique_ptr<ExprAST> Parser::parseForExpr() {
  getNextToken();  // eat the for.

  if (curTok != tok_identifier)
    return logError("Expected identifier after for.");

  std::string idName = lexer.identifierStr;
  getNextToken();  // eat identifier.

  if (curTok != '=')
    return logError("Expected '=' after for.");
  getNextToken();  // eat '='.


  auto start = parseExpression();
  if (!start)
    return nullptr;
  if (curTok != ',')
    return logError("Expected ',' after for start value.");
  getNextToken();

  auto end = parseExpression();
  if (!end)
    return nullptr;

  // The step value is optional.
  std::unique_ptr<ExprAST> step;
  if (curTok == ',') {
    getNextToken();
    step = parseExpression();
    if (!step)
      return nullptr;
  }

  if (curTok != tok_in)
    return logError("Expected 'in' after for.");
  getNextToken();  // eat 'in'.

  auto body = parseExpression();
  if (!body)
    return nullptr;

  return std::make_unique<ForExprAST>(idName, std::move(start),
                                       std::move(end), std::move(step),
                                       std::move(body), body->isReturning_, lexer.line);
}

std::unique_ptr<ExprAST> Parser::parsePrimary(bool isReturning) {
  switch (curTok) {
  default:
    return logError("Unknown token when expecting an expression.");
  case tok_identifier:
    return parseIdentifierExpr(isReturning);
  case tok_double:
    return parseDoubleExpr(isReturning);
  case tok_int:
    return parseIntExpr(isReturning);
  case '(':
    return parseParenExpr(isReturning);
  case '\'':
    return parseCharExpr(isReturning);
  case tok_string:
    return parseStringExpr(isReturning);
  case tok_if:
    if(isReturning)
      return logError("Cannot return right before 'if' statement.");
    return parseIfExpr();
  case tok_for:
    if(isReturning)
      return logError("Cannot return right before 'for' statement.");
    return parseForExpr();
  }
}

std::unique_ptr<ExprAST> Parser::parseUnary(bool isReturning) {
  // If the current token is not negation operator, it must be a primary expr.
  if (curTok != tok_negation && curTok != '*' && curTok != '&')
    return parsePrimary(isReturning);

  // If this is a unary operator, read it.
  int opc = curTok;
  getNextToken();
  if (auto operand = parseUnary(isReturning))
    return std::make_unique<UnaryExprAST>(opc, std::move(operand), isReturning, lexer.line);
  return nullptr;
}

std::unique_ptr<ExprAST> Parser::parseBinOpRHS(int exprPrec, 
                                                std::unique_ptr<ExprAST> LHS) {
  // If this is a binop, find its precedence.
  while (1) {
    int tokPrec = getTokPrecedence();

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (tokPrec < exprPrec) {
      return LHS;
    }

    // Okay, we know this is a binop.
    int binOp = curTok;
    getNextToken();  // eat binop

    // Parse the primary expression after the binary operator.
    auto RHS = parseUnary(false);
    if (!RHS) {
      return nullptr;
    }

    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    int nextPrec = getTokPrecedence();
    if (tokPrec < nextPrec) {
      RHS = parseBinOpRHS(tokPrec+1, std::move(RHS));
      if (!RHS) {
        return nullptr;
      }
    }
    // Merge LHS/RHS.
    LHS = std::make_unique<BinaryExprAST>(binOp, std::move(LHS),
                                           std::move(RHS), LHS->isReturning_, lexer.line);
  }  // loop around to the top of the while loop.
}

std::unique_ptr<ExprAST> Parser::parseExpression() {
  bool isReturning = false;
  if(curTok == tok_return) {
    isReturning = true;
    getNextToken();
  } else if(curTok == tok_var) {
    return parseVarExpr(isReturning);
  }

  auto LHS = parseUnary(isReturning);
  if (!LHS) {
    return nullptr;
  }
  return parseBinOpRHS(0, std::move(LHS));
}

std::unique_ptr<PrototypeAST> Parser::parsePrototype() {
  if(curTok != tok_int_string && curTok != tok_double_string && curTok != tok_char_string && curTok != tok_string_string) 
    return logErrorP("Expected function type in prototype.");
  int fnType = curTok;
  getNextToken();

  if (curTok != tok_identifier) {
    return logErrorP("Expected function name in prototype.");
  }
  std::string fnName = lexer.identifierStr;
  getNextToken();

  if (curTok != '(') {
    return logErrorP("Expected '(' in prototype.");
  }

  // Read the list of argument names.
  std::vector<std::pair<int, std::string>> argNames;
  int type;
  while (curTok == '(' || curTok == ',') {
    type = getNextToken();
    if(type != tok_double_string && type != tok_int_string && type != tok_char_string && type != tok_string_string)
      return logErrorP("Expected type of parameter.");
    if(getNextToken() != tok_identifier)
      return logErrorP("Expected identifier after type of the parameter.");

    argNames.emplace_back(type, lexer.identifierStr);
    getNextToken();
  }
  if(curTok != ')')
    return logErrorP("Expected ',' or ')' after the argument.");

  // success.
  getNextToken();  // eat ')'.
  return std::make_unique<PrototypeAST>(fnName, std::move(argNames), fnType, lexer.line);
}

std::unique_ptr<FunctionAST> Parser::parseDefinition() {
  getNextToken();  // eat def.
  auto proto = parsePrototype();
  if (!proto) {
    return nullptr;
  }

  if (auto E = parseBlockExpr()) {
    if(!E->isReturning_) {
      logError("Expected return in the function body.");
      return nullptr;
    }
    return std::make_unique<FunctionAST>(std::move(proto), std::move(E), lexer.line);
  }
  return nullptr;
}

std::unique_ptr<PrototypeAST> Parser::parseExtern() {
  getNextToken();  // eat extern.
  return parsePrototype();
}

std::unique_ptr<FunctionAST> Parser::parseTopLevelExpr() {
  if(curTok != tok_main) {
    fprintf(stderr, "LogError, line %u: Expected main function\n", lexer.line);
    return nullptr;
  }
  getNextToken(); //eat main
  if (auto E = parseBlockExpr()) {
    // Make an anonymous proto.
    auto proto = std::make_unique<PrototypeAST>("main", std::vector<std::pair<int, std::string>>(), tok_int_string, lexer.line);
    return std::make_unique<FunctionAST>(std::move(proto), std::move(E), lexer.line);
  }
  return nullptr;
}