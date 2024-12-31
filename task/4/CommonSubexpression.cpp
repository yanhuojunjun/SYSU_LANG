#include "CommonSubexpression.hpp"

using namespace llvm;

std::vector<Value*> addList;

PreservedAnalyses
CommonSubexpression::run(Module& mod, ModuleAnalysisManager& mam)
{
  int constFoldTimes = 0;

  int size = 0;
  Instruction* preAdd;            // 上一个加法指令
  Value* first_operand = nullptr; //记录第一个操作数
  Value* last_operand = nullptr;  //记录最后一个操作数
  for (auto& func : mod) {
    for (auto& bb : func) {
      for (auto& inst : bb) {
        if (auto binOp = dyn_cast<BinaryOperator>(&inst)) { 
          if(binOp->getOpcode() == Instruction::Add){ //如果是加法
            if(first_operand == nullptr){ //如果是第一个加法
              first_operand = binOp->getOperand(0);
              size++;
            } else { // 如果不是第一个加法
              if(binOp->getOperand(1) == first_operand)
                last_operand = preAdd->getOperand(1);
              if (binOp->getOperand(0) == preAdd)
                size++;
            }
            preAdd = binOp;
          }//------
        }
      }
    }
  }

  Value* lastsum = nullptr;
  if (size > 1000) {
    for (auto& func : mod) {
      for (auto& bb : func) {
        std::vector<Instruction*> instToErase;
        for (auto& inst : bb) {
          if (auto binOp = dyn_cast<BinaryOperator>(&inst)) 
            if(binOp->getOpcode() == Instruction::Add){
              if(binOp->getOperand(1) == first_operand){
                //binOp->setOperand(0, preAdd);
                lastsum = binOp->getOperand(0);
                binOp->replaceAllUsesWith(first_operand);
                instToErase.push_back(binOp);
              }
              else if(binOp->getOperand(1) == last_operand){
                Instruction* newInst;
                if (lastsum != nullptr) newInst = BinaryOperator::CreateAdd(lastsum,binOp);
                else  newInst = BinaryOperator::CreateAdd(ConstantInt::get(binOp->getType(), 0),binOp);                
                newInst->insertAfter(binOp);  
                binOp->replaceAllUsesWith(newInst);
                newInst->setOperand(1, binOp);
              }      
              //preAdd = binOp;
            }   
        }
        for (auto& i : instToErase)
          i->eraseFromParent();
      }   
    }
  }
 
  //公共子表达式删除----------
  for (int i = 1; i <= 110;i++){
    for (auto& func : mod) {
      for (auto& bb : func) {
        for (auto inst_iter = bb.begin(); inst_iter != bb.end(); ++inst_iter) {
          std::vector<Instruction*> instToErase;
          if (auto* binOp = dyn_cast<BinaryOperator>(&*inst_iter)) {
            if (binOp->getOpcode() == Instruction::Add) {
              const unsigned k = 110;
              bool foundCommonSubexpr = false;
              for (auto next_iter = std::next(inst_iter);
                   next_iter != bb.end() &&
                   std::distance(inst_iter, next_iter) <= k;
                   ++next_iter) {
                if (auto* next_binOp = dyn_cast<BinaryOperator>(&*next_iter)) {
                  if (next_binOp->getOpcode() == Instruction::Add) {
                    if (binOp->getOperand(0) == next_binOp->getOperand(0) &&
                        binOp->getOperand(1) == next_binOp->getOperand(1)) {
                      next_binOp->replaceAllUsesWith(binOp);
                      instToErase.push_back(next_binOp);
                      ++constFoldTimes;
                      foundCommonSubexpr = true;
                      break;
                    }
                  }
                }
              }
            }
          }
          for (auto& i : instToErase)
            i->eraseFromParent();
        }
      }
    }
  }
  
  bool startAdd = 0;
  Instruction* preAdd2;            // 上一个加法指令
  for (auto& func : mod) {
    for (auto& bb : func) {
      for (auto& inst : bb) {
        if (auto binOp = dyn_cast<BinaryOperator>(&inst)) { 
          if(binOp->getOpcode() == Instruction::Add){ //如果是加法
            if(startAdd == 0 ){ //如果是第一个加法
              startAdd = 1;
              addList.push_back(binOp->getOperand(0));
              addList.push_back(binOp->getOperand(1));
            } else { //如果不是第一个加法
              if (binOp->getOperand(0) == preAdd2){
                if(binOp->getOperand(1)!=addList.back()){
                  startAdd = 0;
                  addList.clear();
                }
                else addList.push_back(binOp->getOperand(1));
              }              
            } 
            preAdd2 = binOp;   
          }
        }
      }
    }
  }
  
  Value* firstElement = addList.front(); 
  Value* lastElement = addList.back(); 

  int count = 0;
  if(addList.size()>90 && addList.size()<9000){
  
  for (auto& func : mod) {
    for (auto& bb : func) {
      std::vector<Instruction*> instToErase;
      for (auto& inst : bb) {
        if (auto binOp = dyn_cast<BinaryOperator>(&inst))
          if(binOp->getOpcode() == Instruction::Add){ //如果是加法
            if(binOp->getOperand(1) == lastElement){
              count++;
              instToErase.push_back(binOp);
              ++constFoldTimes;   
              if(count == addList.size()-1){
                Instruction* mulInst = BinaryOperator::CreateMul(
                  ConstantInt::get(binOp->getType(), count), lastElement);
                mulInst->insertAfter(binOp);  
                Instruction* addInst = BinaryOperator::CreateAdd(
                  firstElement, mulInst);
                addInst->insertAfter(mulInst);
                binOp->replaceAllUsesWith(addInst);
              }
            }
          }
      }
      std::reverse(instToErase.begin(), instToErase.end());
      for (auto& i : instToErase)
        i->eraseFromParent();
    }
  }
  }
  mOut << "CommonSubexpression running...\nTo elimate " << constFoldTimes << " instructions\n";
  return PreservedAnalyses::all();
}