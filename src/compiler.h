#pragma once
#include "parser.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

using llvm::Type;
using llvm::AllocaInst;
using llvm::ConstantFP;
using llvm::ConstantInt;
using llvm::APFloat;
using llvm::APInt;
using llvm::PointerType;
using llvm::BasicBlock;

namespace Compiler {
    llvm::LLVMContext theContext;
    llvm::IRBuilder<> builder(theContext);
    std::unique_ptr<llvm::Module> theModule = std::make_unique<llvm::Module>("jit", theContext);
    std::map<std::string, AllocaInst*> namedValues;
    std::unique_ptr<llvm::legacy::FunctionPassManager> theFPM;
    std::map<std::string, std::unique_ptr<PrototypeAST>> functionProtos;

    void logError(unsigned int line, const std::string& Str) {
      std::string text = "LogError, line ";
      text += std::to_string(line);
      text += ": ";
      text += Str;
      throw std::logic_error(text);
    }

    Function *getFunction(unsigned int line, std::string name) {
        // First, see if the function has already been added to the current module.
        if (auto *F = theModule->getFunction(name))
            return F;

        // If not, check whether we can codegen the declaration from some existing
        // prototype.
        auto FI = functionProtos.find(name);
        if (FI != functionProtos.end())
            return FI->second->codegen();

        // If no existing prototype exists, throw.
        logError(line, "Unknown function name.\n");
        return nullptr;
    }

    /// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
    /// the function.  This is used for mutable variables etc.
    AllocaInst *createEntryBlockAlloca(Function *theFunction,
                                            const std::string &varName) {
        llvm::IRBuilder<> tmpB(&theFunction->getEntryBlock(),
                        theFunction->getEntryBlock().begin());
        return tmpB.CreateAlloca(Type::getDoubleTy(theContext), 0,
                                varName.c_str());
    }

    /// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
    /// the function.  This is used for mutable variables etc.
    AllocaInst *createEntryBlockAllocaInt(Function *theFunction,
                                            const std::string &varName) {
        llvm::IRBuilder<> tmpB(&theFunction->getEntryBlock(),
                        theFunction->getEntryBlock().begin());
        return tmpB.CreateAlloca(Type::getInt32Ty(theContext), 0,
                                varName.c_str());
    }

    /// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
    /// the function.  This is used for mutable variables etc.
    AllocaInst *createEntryBlockAllocaChar(Function *theFunction,
                                            const std::string &varName) {
        llvm::IRBuilder<> tmpB(&theFunction->getEntryBlock(),
                        theFunction->getEntryBlock().begin());
        return tmpB.CreateAlloca(Type::getInt8Ty(theContext), 0,
                                varName.c_str());
    }

    /// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
    /// the function.  This is used for mutable variables etc.
    AllocaInst *createEntryBlockAllocaString(Function *theFunction,
                                            const std::string &varName) {
        llvm::IRBuilder<> tmpB(&theFunction->getEntryBlock(),
                        theFunction->getEntryBlock().begin());
        return tmpB.CreateAlloca(PointerType::get(Type::getInt8Ty(Compiler::theContext), 0), 0,
                                varName.c_str());
    }

    class Generator {
        Parser parser;
        bool foundError_ = false;

    public:
        Generator() {
            parser.getNextToken();
        }

        void handleDefinition();
        void handleExtern();
        void handleMainFunction();

        void getNextToken() {
            parser.getNextToken();
        }

        int curTok() {
            return parser.curTok;
        }

        bool foundError() {
          return foundError_;
        }
    };

}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

Value *DoubleExprAST::codegen() {
  return ConstantFP::get(Compiler::theContext, APFloat(val));
}

Value *IntExprAST::codegen() {
  return ConstantInt::get(Compiler::theContext, APInt(32, val, true));
}

Value *CharExprAST::codegen() {
  return ConstantInt::get(Compiler::theContext, APInt(8, val, true));
}

Value *StringExprAST::codegen() {
  return Compiler::builder.CreateGlobalStringPtr(llvm::StringRef(val),"varName");
}

Value *VariableExprAST::codegen() {
  // Look this variable up in the function.
  Value *V = Compiler::namedValues[name];
  if (!V) {
    Compiler::logError(line_, "Unknown variable name.\n");
  }
  return Compiler::builder.CreateLoad(V, name.c_str());
}

Value *PointerExprAST::codegen() {
  if(pointee) {
    Value *V = pointee->codegen();
    if(!V) {
      Compiler::logError(line_, "Pointee codegen error.\n");
    }
    return V;
  }
  return nullptr;
}

Value *VarExprAST::codegen() {
  Value *varValue = val->codegen();

  AllocaInst *alloca;
  Type *finalType;

  if(type == tok_double_string)
    finalType = Type::getDoubleTy(Compiler::theContext);
  if(type == tok_int_string)
    finalType = Type::getInt32Ty(Compiler::theContext);
  if(type == tok_char_string)
    finalType = Type::getInt8Ty(Compiler::theContext);
  if(type == tok_string_string)
    finalType = PointerType::get(Type::getInt8Ty(Compiler::theContext), 0);
  for(int i = 0; i < count; i++) {
    finalType = PointerType::get(finalType, 0);
  }

  alloca = Compiler::builder.CreateAlloca(finalType, 0, getName());
  if(!varValue)
    varValue = llvm::Constant::getNullValue(finalType);
  else {
    UnaryExprAST* LHSE2 = dynamic_cast<UnaryExprAST*>(val.get());
    if(LHSE2 && LHSE2->getOpCode() == '*') 
      varValue = Compiler::builder.CreateLoad(varValue, "load");
  }
  
  if(varValue->getType()->getPointerTo() != alloca->getType())
    Compiler::logError(line_, "Different types of lvalue and rvalue.\n");
  Compiler::builder.CreateStore(varValue, alloca);
  Compiler::namedValues[getName()] = alloca;
  return varValue;
}

Value *UnaryExprAST::codegen() {
  switch(opCode) {
    case tok_negation: {
      Value *operandV = operand->codegen();
      operandV = Compiler::builder.CreateFCmpOEQ(operandV, ConstantFP::get(Compiler::theContext, APFloat(0.0)), "negtmp");
      return Compiler::builder.CreateUIToFP(operandV, Type::getDoubleTy(Compiler::theContext), "booltmp");
    }
    case '&': {
      VariableExprAST *LHSE = dynamic_cast<VariableExprAST*>(operand.get());
      if(!LHSE)
        Compiler::logError(line_, "Expected name of variable after '&'.\n");
      Value *V = Compiler::namedValues[LHSE->getName()];
      if(!V) {
        Compiler::logError(line_, "Unknown variable name.\n");
      }
      return V;
    }
    case '*': {
      VariableExprAST *LHSE = dynamic_cast<VariableExprAST*>(operand.get());
      if(!LHSE) {
        UnaryExprAST *LHSE = dynamic_cast<UnaryExprAST*>(operand.get());
        if(!LHSE)
          Compiler::logError(line_, "Expected name of variable after '*'.\n");
        Value *V = operand->codegen();
        if(!V->getType()->getPointerElementType()->isPointerTy())
          Compiler::logError(line_, "Variable is not a pointer type.\n");
        return Compiler::builder.CreateLoad(V, "load");
      }
      Value *V = Compiler::namedValues[LHSE->getName()];
      if(!V) {
        Compiler::logError(line_, "Unknown variable name.\n");
      }
      if(!V->getType()->getPointerElementType()->isPointerTy())
        Compiler::logError(line_, "Variable is not a pointer type.\n");
      return Compiler::builder.CreateLoad(V, "load");
    }
    default:
      Compiler::logError(line_, "Unknown unary operator.\n");
      return nullptr;
  }
}

Value *BinaryExprAST::codegen() {
  if (op == '=') {
    // Assignment requires the LHS to be an identifier.
    Value *variable = nullptr;
    VariableExprAST *LHSE = dynamic_cast<VariableExprAST*>(LHS.get());
    if(LHSE){
      variable = Compiler::namedValues[LHSE->getName()];
      if (!variable)
        Compiler::logError(line_, "Unknown variable name.\n");
    }
      
    UnaryExprAST *LHSE2 = dynamic_cast<UnaryExprAST*>(LHS.get());
    if(LHSE2 && LHSE2->getOpCode() == '*')
      variable = LHSE2->codegen();
    
    if(!variable)
      Compiler::logError(line_, "Destination of '=' must be lhs value.\n");

    // Codegen the RHS.
    Value *val = RHS->codegen();

    if(val->getType()->getPointerTo() != variable->getType())
      Compiler::logError(line_, "Types of Variable and Expression does not match.\n");
    Compiler::builder.CreateStore(val, variable);
    return val;
  }

  Value *L, *R;
  L = LHS->codegen();
  R = RHS->codegen();

  if(L->getType() != R->getType())
    Compiler::logError(line_, "Different types of binary operands.\n");

  switch (op) {
  case '+':
    if(L->getType() == Compiler::builder.getInt32Ty())
      return Compiler::builder.CreateAdd(L, R, "addtmp");
    if(L->getType() == Compiler::builder.getDoubleTy())
      return Compiler::builder.CreateFAdd(L, R, "addtmp");
    Compiler::logError(line_, "Cannot call + operator on this type.\n");
  case '-':
    if(L->getType() == Compiler::builder.getInt32Ty())
      return Compiler::builder.CreateSub(L, R, "subtmp");
    if(L->getType() == Compiler::builder.getDoubleTy())
      return Compiler::builder.CreateFSub(L, R, "subtmp");
    Compiler::logError(line_, "Cannot call - operator on this type.\n");
  case '*':
    if(L->getType() == Compiler::builder.getInt32Ty())
      return Compiler::builder.CreateMul(L, R, "multmp");
    if(L->getType() == Compiler::builder.getDoubleTy())
      return Compiler::builder.CreateFMul(L, R, "multmp");
    Compiler::logError(line_, "Cannot call * operator on this type.\n");
  case '/':
    if(L->getType() == Compiler::builder.getInt32Ty())
      return Compiler::builder.CreateSDiv(L, R, "divtmp");
    if(L->getType() == Compiler::builder.getDoubleTy())
      return Compiler::builder.CreateFDiv(L, R, "divtmp");
    Compiler::logError(line_, "Cannot call / operator on this type.\n");
  case '<':
    if(L->getType() == Compiler::builder.getInt32Ty()) 
      return Compiler::builder.CreateICmpULT(L, R, "cmptmp");
    if(L->getType() == Compiler::builder.getDoubleTy()) {
      L = Compiler::builder.CreateFCmpULT(L, R, "cmptmp");
      // Convert bool 0/1 to double 0.0 or 1.0
      return Compiler::builder.CreateUIToFP(L, Type::getDoubleTy(Compiler::theContext), "booltmp");
    }
    if(L->getType() == Compiler::builder.getInt8Ty())
      return Compiler::builder.CreateICmpULT(L, R, "cmptmp");
    Compiler::logError(line_, "Cannot call < operator on this type.\n");
  case '>':
    if(L->getType() == Compiler::builder.getInt32Ty())
      return Compiler::builder.CreateICmpULT(R, L, "cmptmp");
    if(L->getType() == Compiler::builder.getDoubleTy()) {
      L = Compiler::builder.CreateFCmpULT(R, L, "cmptmp");
      return Compiler::builder.CreateUIToFP(L, Type::getDoubleTy(Compiler::theContext), "booltmp");
    }
    if(L->getType() == Compiler::builder.getInt8Ty())
      return Compiler::builder.CreateICmpULT(R, L, "cmptmp");
    Compiler::logError(line_, "Cannot call > operator on this type.\n");
  case tok_equal:
    if(L->getType() == Compiler::builder.getInt32Ty())
        return Compiler::builder.CreateICmpEQ(L, R, "cmptmp");
    if(L->getType() == Compiler::builder.getDoubleTy()) {
      L = Compiler::builder.CreateFCmpOEQ(L, R, "cmptmp");
      return Compiler::builder.CreateUIToFP(L, Type::getDoubleTy(Compiler::theContext), "booltmp");
    }
    if(L->getType() == Compiler::builder.getInt8Ty())
        return Compiler::builder.CreateICmpEQ(L, R, "cmptmp");
  case tok_nonequal:
    if(L->getType() == Compiler::builder.getInt32Ty())
      return Compiler::builder.CreateICmpNE(L, R, "cmptmp");
    if(L->getType() == Compiler::builder.getDoubleTy()) {
      L = Compiler::builder.CreateFCmpONE(L, R, "cmptmp");
      return Compiler::builder.CreateUIToFP(L, Type::getDoubleTy(Compiler::theContext), "booltmp");
    }
    if(L->getType() == Compiler::builder.getInt8Ty())
      return Compiler::builder.CreateICmpNE(L, R, "cmptmp");
  default:
    Compiler::logError(line_, "Invalid binary operator.\n");
    return nullptr;
  }
}

Value *BlockExprAST::codegen() {
  std::vector<std::pair<std::string, AllocaInst*>> oldValues;
  Value *val = nullptr;
  bool noError = true;
  for (auto &exp : expressions) {
    VarExprAST* LHSE = dynamic_cast<VarExprAST*>(exp.get());
    if(LHSE) {
      oldValues.emplace_back(LHSE->getName(), Compiler::namedValues[LHSE->getName()]);
    }
    try {
      val = exp->codegen();
    } catch(const std::logic_error& e) {
      fprintf(stderr, "%s", e.what());
      noError = false;
    }
    UnaryExprAST* LHSE2 = dynamic_cast<UnaryExprAST*>(exp.get());
    if(LHSE2) 
      val = Compiler::builder.CreateLoad(val, "load");
    if(noError && exp->isReturning_ && val) {
      for(auto &val : oldValues) {
        if(val.second)
          Compiler::namedValues[val.first] = val.second;
        else {
          Compiler::namedValues.erase(val.first);
        }
      }
      return val;
    }
  }
  for(auto &valu : oldValues) {
    if(valu.second)
      Compiler::namedValues[valu.first] = valu.second;
    else {
      Compiler::namedValues.erase(valu.first);
    }
  }
  return nullptr;
}

Value *CallExprAST::codegen() {
  // Look up the name in the global module table.
  Function *calleeF = Compiler::getFunction(line_, callee);
  auto FI = Compiler::functionProtos.find(callee);

  // If argument mismatch error.
  if (calleeF->arg_size() != args.size() && !calleeF->isVarArg()) {
    Compiler::logError(line_, "Incorrect # arguments passed.\n");
  }

  std::vector<Value *> argsV;
  for (unsigned i = 0, e = args.size(); i != e; ++i) {
    Value *val = args[i]->codegen();
    Type *argType;
    if(FI->second->getType(i) == tok_double_string)
      argType = Type::getDoubleTy(Compiler::theContext);
    if(FI->second->getType(i) == tok_int_string)
      argType = Type::getInt32Ty(Compiler::theContext);
    if(FI->second->getType(i) == tok_char_string)
      argType = Type::getInt8Ty(Compiler::theContext);
    if(FI->second->getType(i) == tok_string_string)
      argType = PointerType::get(Type::getInt8Ty(Compiler::theContext), 0);
    if(val->getType() != argType && !calleeF->isVarArg()) { 
      std::string text = "Wrong type of ";
      text += std::to_string(i + 1);
      text += ". argument passed.\n";
      Compiler::logError(line_, text);
    }
    argsV.push_back(val);
  }

  return Compiler::builder.CreateCall(calleeF, argsV, "calltmp");
}

Function *PrototypeAST::codegen() {
  // Make the function type:  double(double,double) etc.
  std::vector<Type*> types;
  for(const auto &arg : args) {
    if(arg.first == tok_double_string)
      types.push_back(Type::getDoubleTy(Compiler::theContext));
    if(arg.first == tok_int_string)
      types.push_back(Type::getInt32Ty(Compiler::theContext));
    if(arg.first == tok_char_string)
      types.push_back(Type::getInt8Ty(Compiler::theContext));
    if(arg.first == tok_string_string)
      types.push_back(PointerType::get(Type::getInt8Ty(Compiler::theContext), 0));
  }
  Type* fnType;
  if(type == tok_double_string)
    fnType = Type::getDoubleTy(Compiler::theContext);
  if(type == tok_int_string)
    fnType = Type::getInt32Ty(Compiler::theContext);
  if(type == tok_char_string)
    fnType = Type::getInt8Ty(Compiler::theContext);
  if(type == tok_string_string)
    fnType = PointerType::get(Type::getInt8Ty(Compiler::theContext), 0);

  bool variadic = false;
  if(!name.compare("printf"))
    variadic = true;
  llvm::FunctionType *FT = llvm::FunctionType::get(fnType, types, variadic);

  Function *F =
    Function::Create(FT, Function::ExternalLinkage, name, Compiler::theModule.get());

  // Set names for all arguments.
  unsigned idx = 0;
  for (auto &arg : F->args()) {
    arg.setName(args[idx++].second);
  }

  return F;
}

Function *FunctionAST::codegen() {
  // Transfer ownership of the prototype to the functionProtos map, but keep a
  // reference to it for use below.
  auto &P = *proto;
  Compiler::functionProtos[proto->getName()] = std::move(proto);
  Function *theFunction = Compiler::getFunction(line_, P.getName());

  // Create a new basic block to start insertion into.
  BasicBlock *BB = BasicBlock::Create(Compiler::theContext, "entry", theFunction);
  Compiler::builder.SetInsertPoint(BB);

  // Record the function arguments in the Compiler::namedValues map.
  int i = 0;
  for (auto &arg : theFunction->args()) {
    // Create an alloca for this variable.
    AllocaInst *alloca;
    if(P.type == tok_double_string)
      alloca = Compiler::createEntryBlockAlloca(theFunction, arg.getName());
    if(P.type == tok_int_string)
      alloca = Compiler::createEntryBlockAllocaInt(theFunction, arg.getName());
    if(P.type == tok_char_string)
      alloca = Compiler::createEntryBlockAllocaChar(theFunction, arg.getName());
    if(P.type == tok_string_string)
      alloca = Compiler::createEntryBlockAllocaString(theFunction, arg.getName());

    // Store the initial value into the alloca.
    Compiler::builder.CreateStore(&arg, alloca);

    // Add arguments to variable symbol table.
    Compiler::namedValues[arg.getName()] = alloca;
  }
  Value *retVal;
  try {
    retVal = body->codegen();
  } catch(const std::logic_error& e) {
    theFunction->eraseFromParent();
    throw;
  }

  if (retVal) {
    Type* fnType;
    if(P.type == tok_double_string)
      fnType = Type::getDoubleTy(Compiler::theContext);
    if(P.type == tok_int_string)
      fnType = Type::getInt32Ty(Compiler::theContext);
    if(P.type == tok_char_string)
      fnType = Type::getInt8Ty(Compiler::theContext);
    if(P.type == tok_string_string)
      fnType = PointerType::get(Type::getInt8Ty(Compiler::theContext), 0);
    if(retVal->getType() != fnType) {
      theFunction->eraseFromParent();
      Compiler::logError(line_, "Return type does not match.\n");
    }

    // Finish off the function.
    Compiler::builder.CreateRet(retVal);

    // Validate the generated code, checking for consistency.
    verifyFunction(*theFunction);

    // Optimize the function.
    //theFPM->run(*TheFunction);

    return theFunction;
  }

  // Error reading body, remove function.
  theFunction->eraseFromParent();
  Compiler::logError(line_, "Missing return or error in function.\n");
  return nullptr;
}

Value *IfExprAST::codegen() {
  Value *condV = cond->codegen();

  if(condV->getType() == Compiler::builder.getInt32Ty())
    condV = Compiler::builder.CreateICmpNE(condV, ConstantInt::get(Compiler::theContext, APInt(32, 0, true)), "ifcond");
  else if(condV->getType() == Compiler::builder.getFloatTy())
    condV = Compiler::builder.CreateFCmpONE(condV, ConstantFP::get(Compiler::theContext, APFloat(0.0)), "ifcond");
  else if(condV->getType() == Compiler::builder.getInt8Ty())
    condV = Compiler::builder.CreateICmpNE(condV, ConstantInt::get(Compiler::theContext, APInt(8, 0, true)), "ifcond");
  // Convert condition to a bool by comparing non-equal to 0.0.

  Function *theFunction = Compiler::builder.GetInsertBlock()->getParent();

  // Create blocks for the then and else cases.  Insert the 'then' block at the
  // end of the function.
  BasicBlock *thenBB =
      BasicBlock::Create(Compiler::theContext, "then", theFunction);
  BasicBlock *elseBB = BasicBlock::Create(Compiler::theContext, "else");
  BasicBlock *mergeBB = BasicBlock::Create(Compiler::theContext, "ifcont");

  Compiler::builder.CreateCondBr(condV, thenBB, elseBB); //after Cond->codegen create jump into Then/Else depending on CondV

  // Emit then value.
  Compiler::builder.SetInsertPoint(thenBB);

  Value *thenV = then->codegen();

  if(then->isReturning_)
    Compiler::builder.CreateRet(thenV);
  else
    Compiler::builder.CreateBr(mergeBB); //after Then we jump into Merge basic block

  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  thenBB = Compiler::builder.GetInsertBlock();

  // Emit else block.
  theFunction->getBasicBlockList().push_back(elseBB);
  Compiler::builder.SetInsertPoint(elseBB);

  Value *elseV = else_->codegen();
  
  if(else_->isReturning_)
    Compiler::builder.CreateRet(elseV);
  else
    Compiler::builder.CreateBr(mergeBB); //after Else we jump into Merge basic block

  // codegen of 'Else' can change the current block, update ElseBB for the PHI.
  elseBB = Compiler::builder.GetInsertBlock();

  theFunction->getBasicBlockList().push_back(mergeBB);
  Compiler::builder.SetInsertPoint(mergeBB);
  if(!thenV || !elseV) 
    return nullptr;

  if(thenV->getType() != elseV->getType())
    Compiler::logError(line_, "Different types of then/else return values.\n");

  llvm::PHINode *PN = Compiler::builder.CreatePHI(thenV->getType(), 2, "iftmp");

  PN->addIncoming(thenV, thenBB);
  PN->addIncoming(elseV, elseBB);
  return PN;
}

Value *ForExprAST::codegen() {
  Function *theFunction = Compiler::builder.GetInsertBlock()->getParent();

  // Create an alloca for the variable in the entry block.
  AllocaInst *alloca = Compiler::createEntryBlockAllocaInt(theFunction, varName);

  // Emit the start code first, without 'variable' in scope.
  Value *startVal = start->codegen();

  // Store the value into the alloca.
  Compiler::builder.CreateStore(startVal, alloca);

  BasicBlock *loopBB =
      BasicBlock::Create(Compiler::theContext, "loop", theFunction);

  // Insert an explicit fall through from the current block to the LoopBB.
  Compiler::builder.CreateBr(loopBB);

  // Start insertion in LoopBB.
  Compiler::builder.SetInsertPoint(loopBB);

  // Within the loop, the variable is defined equal to the PHI node.  If it
  // shadows an existing variable, we have to restore it, so save it now.
  AllocaInst *oldVal = Compiler::namedValues[varName];
  Compiler::namedValues[varName] = alloca;

  // Emit the body of the loop.  This, like any other expr, can change the
  // current BB.  Note that we ignore the value computed by the body, but don't
  // allow an error.
  body->codegen();


  // Emit the step value.
  Value *stepVal = nullptr;
  if (step) {
    stepVal = step->codegen();
  } else {
    // If not specified, use 1.0.
    stepVal = ConstantInt::get(Compiler::theContext, APInt(32, 1, true));
  }

  // Reload, increment, and restore the alloca.  This handles the case where
  // the body of the loop mutates the variable.
  Value *curVar = Compiler::builder.CreateLoad(alloca, varName.c_str());
  Value *nextVar = Compiler::builder.CreateAdd(curVar, stepVal, "nextvar");
  Compiler::builder.CreateStore(nextVar, alloca);

  // Compute the end condition.
  Value *endCond = end->codegen();

  // Convert condition to a bool by comparing non-equal to 0.0.
  endCond = Compiler::builder.CreateICmpNE(
      endCond, ConstantInt::get(Compiler::theContext, APInt(1, 0, true)), "loopcond");

  // Create the "after loop" block and insert it.
  BasicBlock *afterBB =
      BasicBlock::Create(Compiler::theContext, "afterloop", theFunction);

  // Insert the conditional branch into the end of LoopEndBB.
  Compiler::builder.CreateCondBr(endCond, loopBB, afterBB);

  // Any new code will be inserted in AfterBB.
  Compiler::builder.SetInsertPoint(afterBB);

  // Restore the unshadowed variable.
  if (oldVal)
    Compiler::namedValues[varName] = oldVal;
  else
    Compiler::namedValues.erase(varName);

  return llvm::Constant::getNullValue(Type::getInt32Ty(Compiler::theContext));
}

void Compiler::Generator::handleDefinition() {
  if (auto fnAST = parser.parseDefinition()) {
    Function *fnIR;
    try {
      fnIR = fnAST->codegen();
    } catch(const std::logic_error& e) {
      foundError_ = true;
      fprintf(stderr, "%s", e.what());
      return;
    }
    fprintf(stderr, "Read function definition: \n");
    fnIR->print(llvm::errs());
    fprintf(stderr, "\n");
  } else {
    foundError_ = true;
    // Skip token for error recovery.
    parser.errorRecovery();
  }
}

void Compiler::Generator::handleExtern() {
  if (auto protoAST = parser.parseExtern()) {
    Function *fnIR;
    try {
      fnIR = protoAST->codegen();
    } catch(const std::logic_error& e) {
      foundError_ = true;
      fprintf(stderr, "%s", e.what());
      return;
    }
    fprintf(stderr, "Read extern: \n");
    fnIR->print(llvm::errs());
    fprintf(stderr, "\n");
    functionProtos[protoAST->getName()] = std::move(protoAST);
  } else {
    foundError_ = true;
    // Skip token for error recovery.
    parser.errorRecovery();
  }
}

void Compiler::Generator::handleMainFunction() {
  // Evaluate main function.
  if (auto fnAST = parser.parseTopLevelExpr()) {
    Function *fnIR;
    try {
      fnIR = fnAST->codegen();
    } catch(const std::logic_error& e) {
      foundError_ = true;
      fprintf(stderr, "%s", e.what());
      return;
    }
    fprintf(stderr, "Read main function: \n");
    fnIR->print(llvm::errs());
    fprintf(stderr, "\n");
  } else {
    foundError_ = true;
    parser.errorRecovery();
    return;
  }
}