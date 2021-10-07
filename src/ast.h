#pragma once
#include <memory>
#include <vector>
#include <map>
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

using llvm::Value;
using llvm::Function;

/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
  unsigned int line_ = 1;
  bool isReturning_ = false;
  virtual ~ExprAST() = default;
  virtual Value *codegen() = 0;
};

/// DoubleExprAST - Expression class for double literals like "1.0".
class DoubleExprAST : public ExprAST {
  double val;

public:
  DoubleExprAST(double val, bool isReturning, unsigned int line) : val(val) {
    isReturning_ = isReturning;
    line_ = line;
  }
  Value *codegen() override;
};

/// IntExprAST - Expression class for int literals like "1".
class IntExprAST : public ExprAST {
  int val;

public:
  IntExprAST(int val, bool isReturning, unsigned int line) : val(val) {
    isReturning_ = isReturning;
    line_ = line;
  }
  Value *codegen() override;
};

/// CharExprAST - Expression class for char literals like 'c'.
class CharExprAST : public ExprAST {
  int val;

public:
  CharExprAST(int val, bool isReturning, unsigned int line) : val(val) {
    isReturning_ = isReturning;
    line_ = line;
  }
  Value *codegen() override;
};

/// StringExprAST - Expression class for string like "Hello world!".
class StringExprAST : public ExprAST {
  std::string val;

public:
  StringExprAST(const std::string& val, bool isReturning, unsigned int line) : val(val) {
    isReturning_ = isReturning;
    line_ = line;
  }
  Value *codegen() override;
};

/// ArrayExprAST - Expression class for char literals like '[0,1,2]'.
class ArrayExprAST : public ExprAST {
  std::vector<std::unique_ptr<ExprAST>> values;

public:
  ArrayExprAST(std::vector<std::unique_ptr<ExprAST>> values, bool isReturning, unsigned int line) : values(std::move(values)) {
    isReturning_ = isReturning;
    line_ = line;
  }
  Value *codegen() override;
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
  std::string name;

public:
  VariableExprAST(const std::string &name, bool isReturning, unsigned int line) : name(name) {
    isReturning_ = isReturning;
    line_ = line;
  }
  Value *codegen() override;
  const std::string &getName() const { return name; }
};

// VarExprAST - Expression class for introducing a new variable "var type id = ..."
class VarExprAST : public ExprAST {
  std::string name;
  std::unique_ptr<ExprAST> val;

public:
  int type;
  int count;
  VarExprAST(const std::string &name, bool isReturning, int type, int count, std::unique_ptr<ExprAST> val, unsigned int line) 
    : name(name), type(type), count(count), val(std::move(val)) {
    isReturning_ = isReturning;
    line_ = line;
  }
  Value *codegen() override;
  const std::string &getName() const { return name; }
};

// PointerExprAST - Expression class for pointers
class PointerExprAST : public ExprAST {
  std::unique_ptr<ExprAST> pointee;

public:
  PointerExprAST(bool isReturning, std::unique_ptr<ExprAST> pointee, unsigned int line) 
    : pointee(std::move(pointee)) {
    isReturning_ = isReturning;
    line_ = line;
  }
  Value *codegen() override;
};

/// UnaryExprAST - Expression class for a unary operator.
class UnaryExprAST : public ExprAST {
  std::unique_ptr<ExprAST> operand;
  char opCode;

public:
  UnaryExprAST(char opCode, std::unique_ptr<ExprAST> operand, bool isReturning, unsigned int line)
    : opCode(opCode), operand(std::move(operand)) {
    isReturning_ = isReturning;
    line_ = line;
  }

  Value *codegen() override;

  char getOpCode() const {
    return opCode;
  }
}; 


/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
  char op;
  std::unique_ptr<ExprAST> LHS, RHS;

public:
  BinaryExprAST(char op, std::unique_ptr<ExprAST> LHS,
                std::unique_ptr<ExprAST> RHS, bool isReturning, unsigned int line)
    : op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {
    isReturning_ = isReturning;
    line_ = line;
  }
  
  Value *codegen() override;
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
  std::string callee;
  std::vector<std::unique_ptr<ExprAST>> args;

public:
  CallExprAST(const std::string &callee,
              std::vector<std::unique_ptr<ExprAST>> args, bool isReturning, unsigned int line)
    : callee(callee), args(std::move(args)) {
    isReturning_ = isReturning;
    line_ = line;
  }

  Value *codegen() override;
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class PrototypeAST {
  std::string name;
  std::vector<std::pair<int, std::string>> args;
  unsigned int line_;

public:
  int type;
  PrototypeAST(const std::string &name, std::vector<std::pair<int, std::string>> args, int type, unsigned int line)
    : name(name), args(std::move(args)), type(type), line_(line) {}

  Function *codegen();
  const std::string &getName() const { return name; }
  int getType(int i) const { return args[i].first; }
};

/// BlockExprAST - Expression class for a vector of expressions inside one logic block
class BlockExprAST : public ExprAST {
  std::vector<std::unique_ptr<ExprAST>> expressions;

public:
  BlockExprAST(std::vector<std::unique_ptr<ExprAST>> expressions, bool isReturning, unsigned int line) 
  : expressions(std::move(expressions)) {
    isReturning_ = isReturning;
    line_ = line;
  }

  Value *codegen() override;
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
  std::unique_ptr<PrototypeAST> proto;
  std::unique_ptr<ExprAST> body;
  unsigned int line_;

public:
  FunctionAST(std::unique_ptr<PrototypeAST> proto,
              std::unique_ptr<ExprAST> body, unsigned int line)
    : proto(std::move(proto)), body(std::move(body)), line_(line) {}
  
  Function *codegen();
};

/// IfExprAST - Expression class for if/then/else.
class IfExprAST : public ExprAST {
  std::unique_ptr<ExprAST> cond, then, else_;

public:
  IfExprAST(std::unique_ptr<ExprAST> cond, std::unique_ptr<ExprAST> then,
            std::unique_ptr<ExprAST> else_, bool isReturning, unsigned int line)
    : cond(std::move(cond)), then(std::move(then)), else_(std::move(else_)) {
    isReturning_ = isReturning;
    line_ = line;
  }

  Value *codegen() override;
};

/// ForExprAST - Expression class for for/in.
class ForExprAST : public ExprAST {
  std::string varName;
  std::unique_ptr<ExprAST> start, end, step, body;

public:
  ForExprAST(const std::string &varName, std::unique_ptr<ExprAST> start,
            std::unique_ptr<ExprAST> end, std::unique_ptr<ExprAST> step,
            std::unique_ptr<ExprAST> body, bool isReturning, unsigned int line)
    : varName(varName), start(std::move(start)), end(std::move(end)),
      step(std::move(step)), body(std::move(body)) {
    isReturning_ = isReturning;
    line_ = line;
  }

  Value *codegen() override;
};