#include "AlgebraicIdentities.hpp"

using namespace llvm;

PreservedAnalyses
AlgebraicIdentities::run(Module& mod, ModuleAnalysisManager& mam)
{
  int constFoldTimes = 0;

  // 遍历所有指令，将无效的计算删除
  for (auto& func : mod) {
    for (auto& bb : func) {
      std::vector<Instruction*> instToErase;
      for (auto& inst : bb) {
        if (auto binOp = dyn_cast<BinaryOperator>(&inst)) { 
          Value* lhs = binOp->getOperand(0);
          Value* rhs = binOp->getOperand(1);
          auto constLhs = dyn_cast<ConstantInt>(lhs); 
          auto constRhs = dyn_cast<ConstantInt>(rhs);  
          switch (binOp->getOpcode()) {
            case Instruction::Mul: { //乘------- *1  *0  1*  0*
              if (constRhs){                           // 如果右边是常数
                int rhsVal = constRhs->getSExtValue(); // 获取右边的值
                if(rhsVal == 0){      
                  binOp->replaceAllUsesWith(ConstantInt::get(binOp->getType(), 0));
                  instToErase.push_back(binOp);
                  ++constFoldTimes;
                }
                else if(rhsVal == 1){
                  binOp->replaceAllUsesWith(lhs);
                  instToErase.push_back(binOp);
                  ++constFoldTimes;                  
                }
              }
              else if (constLhs){                      // 如果左边是常数
                int lhsVal = constLhs->getSExtValue(); // 获取左边的值
                if(lhsVal == 0){      
                  binOp->replaceAllUsesWith(ConstantInt::get(binOp->getType(), 0));
                  instToErase.push_back(binOp);
                  ++constFoldTimes;
                }
                else if(lhsVal == 1){
                  binOp->replaceAllUsesWith(rhs);
                  instToErase.push_back(binOp);
                  ++constFoldTimes;                  
                }
              }              
              break;
            }
            case Instruction::SDiv: { //除------- /1  0/
              if (constRhs){                           // 如果右边是常数
                int rhsVal = constRhs->getSExtValue(); // 获取右边的值
                if(rhsVal == 1){
                  binOp->replaceAllUsesWith(lhs);
                  instToErase.push_back(binOp);
                  ++constFoldTimes;                  
                }
              }
              else if (constLhs){                      // 如果左边是常数
                int lhsVal = constLhs->getSExtValue(); // 获取左边的值
                if(lhsVal == 0){      
                  binOp->replaceAllUsesWith(ConstantInt::get(binOp->getType(), 0));
                  instToErase.push_back(binOp);
                  ++constFoldTimes;
                }
              }              
              break;
            }
            case Instruction::Add: { //加------- +0  0+
              if (constRhs){                           // 如果右边是常数
                int rhsVal = constRhs->getSExtValue(); // 获取右边的值
                if(rhsVal == 0){
                  binOp->replaceAllUsesWith(lhs);
                  instToErase.push_back(binOp);
                  ++constFoldTimes;                  
                }
              }
              else if (constLhs){                      // 如果左边是常数
                int lhsVal = constLhs->getSExtValue(); // 获取左边的值
                if(lhsVal == 0){
                  binOp->replaceAllUsesWith(rhs);
                  instToErase.push_back(binOp);
                  ++constFoldTimes;                  
                }
              }               
              break;
            }
            case Instruction::Sub: { //减------- -0
              if (constRhs){                           // 如果右边是常数
                int rhsVal = constRhs->getSExtValue(); // 获取右边的值
                if(rhsVal == 0){
                  binOp->replaceAllUsesWith(lhs);
                  instToErase.push_back(binOp);
                  ++constFoldTimes;                  
                }
              }              
              break;
            }
            case Instruction::SRem: { //模-------%1 0%
              if (constRhs){                           // 如果右边是常数
                int rhsVal = constRhs->getSExtValue(); // 获取右边的值
                if(rhsVal == 1){
                  binOp->replaceAllUsesWith(ConstantInt::get(binOp->getType(), 0));
                  instToErase.push_back(binOp);
                  ++constFoldTimes;                  
                }
              }
              else if (constLhs){                      // 如果左边是常数
                int lhsVal = constLhs->getSExtValue(); // 获取左边的值
                if(lhsVal == 0){      
                  binOp->replaceAllUsesWith(ConstantInt::get(binOp->getType(), 0));
                  instToErase.push_back(binOp);
                  ++constFoldTimes;
                }
              }                            
              break;
            }
            default:
              break;
          }
        }
      }
      for (auto& i : instToErase)
        i->eraseFromParent();
    }
  }

  mOut << "AlgebraicIdentities running...\nTo elimate " << constFoldTimes << " instructions\n";
  return PreservedAnalyses::all();
}