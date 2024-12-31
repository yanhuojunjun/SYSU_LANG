#include "InstructionCombining.hpp"

using namespace llvm;

std::vector<int> list;

PreservedAnalyses
InstructionCombining::run(Module& mod, ModuleAnalysisManager& mam)
{
  int constFoldTimes = 0;

  bool startAdd = 0;
  Instruction* preAdd;            // 上一个加法指令
  Value* firstElement;
  for (auto& func : mod) 
    for (auto& bb : func) 
      for (auto& inst : bb)
        if (auto binOp = dyn_cast<BinaryOperator>(&inst)) { 
          if(binOp->getOpcode() == Instruction::Add){ //如果是加法
          if(auto value = dyn_cast<ConstantInt>(binOp->getOperand(1))){
            int addvalue = value->getSExtValue();
            if (startAdd == 0) { // 如果是第一个加法
              startAdd = 1;
              firstElement = binOp->getOperand(0);
              list.push_back(addvalue);
            } else { // 如果不是第一个加法
              if (binOp->getOperand(0) == preAdd){
                list.push_back(addvalue);
              }
            }
            preAdd = binOp;   
          }
        }
      }
  
  int lastElement = list.back(); 

  int count = 0;
  bool breakpoint = 0;
  if (list.size() > 10) {
    for (auto& func : mod) {
      for (auto& bb : func) {
        std::vector<Instruction*> instToErase;
        for (auto& inst : bb) {
          if (auto binOp = dyn_cast<BinaryOperator>(&inst))
            if (binOp->getOpcode() == Instruction::Add) { // 如果是加法
              if (auto value = dyn_cast<ConstantInt>(binOp->getOperand(1))) {
                int addvalue = value->getSExtValue();
                if (addvalue == lastElement) {
                  count++;
                  instToErase.push_back(binOp);
                  ++constFoldTimes;
                  if (count == list.size()) {
                    Instruction* mulInst = BinaryOperator::CreateMul(
                      ConstantInt::get(binOp->getType(), count), value);
                    mulInst->insertAfter(binOp);
                    Instruction* addInst =
                      BinaryOperator::CreateAdd(firstElement, mulInst);
                    addInst->insertAfter(mulInst);
                    binOp->replaceAllUsesWith(addInst);
                    breakpoint = 1;
                    break;
                  }
                }
              }
            }
        }
        std::reverse(instToErase.begin(), instToErase.end());
        for (auto& i : instToErase)
          i->eraseFromParent();
        if(breakpoint == 1) break;
      }
      if(breakpoint == 1) break;
    }
  }

   mOut << "InstructionCombining running...\nTo elimate " << constFoldTimes << " instructions\n";
   return PreservedAnalyses::all();
  }