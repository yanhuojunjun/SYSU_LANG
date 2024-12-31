#include "StrengthReduction.hpp"

using namespace llvm;

PreservedAnalyses
StrengthReduction::run(Module& mod, ModuleAnalysisManager& mam)
{
  int constFoldTimes = 0;

  // 遍历所有指令，将将乘法指令改为左移指令，将模指令改为与指令
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
            case Instruction::Mul: { //乘-------
              if (constRhs){                           // 如果右边是常数
                long long int rhsVal = constRhs->getSExtValue(); // 获取右边的值
                if((rhsVal & (rhsVal - 1)) == 0 && rhsVal!=0){      // 如果是2的整数幂
                  int shiftAmount = std::log2(rhsVal);
                  Instruction* newInst = BinaryOperator::CreateShl(lhs, ConstantInt::get(binOp->getType(), shiftAmount));
                  newInst->insertAfter(binOp);
                  binOp->replaceAllUsesWith(newInst);
                  instToErase.push_back(binOp);
                  ++constFoldTimes;
                }
              }
              else if (constLhs){                      // 如果左边是常数
                long long int lhsVal = constLhs->getSExtValue(); // 获取左边的值
                if((lhsVal & (lhsVal - 1)) == 0 && lhsVal!=0){      // 如果是2的整数幂
                  int shiftAmount = std::log2(lhsVal);
                  Instruction* newInst = BinaryOperator::CreateShl(rhs, ConstantInt::get(binOp->getType(), shiftAmount));
                  newInst->insertAfter(binOp);
                  binOp->replaceAllUsesWith(newInst);
                  instToErase.push_back(binOp);
                  ++constFoldTimes;
                }
              }
              break;
            }
            case Instruction::SDiv: { //除------
              if (constRhs){                           // 如果右边是常数
                long long int rhsVal = constRhs->getSExtValue(); // 获取右边的值
                if(rhsVal == 2 ){ 
                  Instruction* newInst = BinaryOperator::CreateAShr(lhs, ConstantInt::get(binOp->getType(), 1));
                  newInst->insertAfter(binOp);
                  binOp->replaceAllUsesWith(newInst);
                  instToErase.push_back(binOp);
                  ++constFoldTimes;
                }
              }
              break;
            }
            // case Instruction::SRem: { //模------
            //   if (constRhs){                           // 如果右边是常数
            //     long long int rhsVal = constRhs->getSExtValue(); // 获取右边的值
            //     if((rhsVal & (rhsVal - 1)) == 0 && rhsVal!=0){      // 如果是2的整数幂
            //       Value* mask = ConstantInt::get(binOp->getType(), rhsVal - 1);
            //       Instruction* newInst = BinaryOperator::CreateAnd(lhs, mask); 
            //       newInst->insertAfter(binOp);
            //       binOp->replaceAllUsesWith(newInst);
            //       instToErase.push_back(binOp);
            //       ++constFoldTimes;
            //     }
            //   }
            //   break;
            // }
            default:
              break;
          }
        }
      }
      for (auto& i : instToErase)
        i->eraseFromParent();
    }
  }

  mOut << "StrengthReduction running...\nTo replace " << constFoldTimes << " instructions\n";
  return PreservedAnalyses::all();
}