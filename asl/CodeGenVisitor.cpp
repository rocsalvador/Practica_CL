//////////////////////////////////////////////////////////////////////
//
//    CodeGenVisitor - Walk the parser tree to do
//                     the generation of code
//
//    Copyright (C) 2017-2022  Universitat Politecnica de Catalunya
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU General Public License
//    as published by the Free Software Foundation; either version 3
//    of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Affero General Public License for more details.
//
//    You should have received a copy of the GNU Affero General Public
//    License along with this library; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
//
//    contact: José Miguel Rivero (rivero@cs.upc.edu)
//             Computer Science Department
//             Universitat Politecnica de Catalunya
//             despatx Omega.110 - Campus Nord UPC
//             08034 Barcelona.  SPAIN
//
//////////////////////////////////////////////////////////////////////

#include "CodeGenVisitor.h"
#include "antlr4-runtime.h"

#include "../common/TypesMgr.h"
#include "../common/SymTable.h"
#include "../common/TreeDecoration.h"
#include "../common/code.h"

#include <iostream>
#include <string>
#include <cstddef>    // std::size_t

// uncomment the following line to enable debugging messages with DEBUG*
// #define DEBUG_BUILD
#include "../common/debug.h"
#include "support/Any.h"

// using namespace std;


// Constructor
CodeGenVisitor::CodeGenVisitor(TypesMgr       & Types,
                               SymTable       & Symbols,
                               TreeDecoration & Decorations) :
  Types{Types},
  Symbols{Symbols},
  Decorations{Decorations} {
}

// Accessor/Mutator to the attribute currFunctionType
TypesMgr::TypeId CodeGenVisitor::getCurrentFunctionTy() const {
  return currFunctionType;
}

void CodeGenVisitor::setCurrentFunctionTy(TypesMgr::TypeId type) {
  currFunctionType = type;
}

// Methods to visit each kind of node:
//
antlrcpp::Any CodeGenVisitor::visitProgram(AslParser::ProgramContext *ctx) {
  DEBUG_ENTER();
  code my_code;
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  for (auto ctxFunc : ctx->function()) { 
    subroutine subr = visit(ctxFunc);
    my_code.add_subroutine(subr);
  }
  Symbols.popScope();
  DEBUG_EXIT();
  return my_code;
}

antlrcpp::Any CodeGenVisitor::visitFunction(AslParser::FunctionContext *ctx) {
  DEBUG_ENTER();
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);

  subroutine subr(ctx->ID()->getText());
  codeCounters.reset();

  if (ctx->retType()) {
    TypesMgr::TypeId retTy = getTypeDecor(ctx->retType()->type());
    setCurrentFunctionTy(retTy);
    subr.add_param("_result");
  }

  if (ctx->paramsDef()) {
    for (auto param : ctx->paramsDef()->parameter()) {
      subr.add_param(param->ID()->getText());
    }
  }
  std::vector<var> && lvars = visit(ctx->declarations());
  for (auto & onevar : lvars) {
    subr.add_var(onevar);
  }

  instructionList && code = visit(ctx->statements());
  subr.add_instructions(code);

  if (not ctx->retType()) {
    instructionList && code = instruction::RETURN();
    subr.add_instructions(code);
  }
  Symbols.popScope();
  DEBUG_EXIT();
  return subr;
}

antlrcpp::Any CodeGenVisitor::visitDeclarations(AslParser::DeclarationsContext *ctx) {
  DEBUG_ENTER();
  std::vector<var> lvars;
  for (auto & varDeclCtx : ctx->variable_decl()) {
    std::vector<var> varsInOneLine = visit(varDeclCtx);
    for (auto & onevar : varsInOneLine) {
      lvars.push_back(onevar);
    }
  }
  DEBUG_EXIT();
  return lvars;
}

antlrcpp::Any CodeGenVisitor::visitVariable_decl(AslParser::Variable_declContext *ctx) {
  DEBUG_ENTER();
  std::vector<var> lvarsInOneLine;
  TypesMgr::TypeId   t1 = getTypeDecor(ctx->type());
  for (uint i = 0; i < ctx->multid()->ident().size(); ++i) {
    std::size_t size = Types.getSizeOfType(t1);
    lvarsInOneLine.push_back(var{ctx->multid()->ident(i)->getText(), size});
  }
  DEBUG_EXIT();
  return lvarsInOneLine;
}

antlrcpp::Any CodeGenVisitor::visitStatements(AslParser::StatementsContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  for (auto stCtx : ctx->statement()) {
    instructionList && codeStmt = visit(stCtx);
    code = code || codeStmt;
  }
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitAssignStmt(AslParser::AssignStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  CodeAttribs     && codAtsE1 = visit(ctx->left_expr());
  std::string           addr1 = codAtsE1.addr;
  std::string           offs1 = codAtsE1.offs;
  instructionList &     code1 = codAtsE1.code;
  TypesMgr::TypeId    tidLeft = getTypeDecor(ctx->left_expr());

  CodeAttribs     && codAtsE2 = visit(ctx->expr());
  std::string           addr2 = codAtsE2.addr;
  std::string           offs2 = codAtsE2.offs;
  instructionList &     code2 = codAtsE2.code;
  TypesMgr::TypeId   tidRight = getTypeDecor(ctx->expr());

  std::string temp = addr1;
  code = code1 || code2;
  if (offs1 != "" ) {
    std::string tempFloat = addr2;
    if (Types.isFloatTy(tidLeft) and Types.isIntegerTy(tidRight)) {
      tempFloat = "%" + codeCounters.newTEMP();
      code = code || instruction::FLOAT(tempFloat, addr2);
    }
    code = code || instruction::XLOAD(temp, offs1, tempFloat);
  } else {
    // Assignment coercion
    if (Types.isIntegerTy(tidRight) && not Types.isFloatTy(tidLeft)) {
      code = code || instruction::ILOAD(temp, addr2);
    } 
    else if (Types.isCharacterTy(tidRight)) {
      code = code || instruction::LOAD(temp, addr2);
    } 
    else if (Types.isFloatTy(tidRight)) {
      code = code || instruction::FLOAD(temp, addr2);
    } 
    else if (Types.isFloatTy(tidLeft)) {
      std::string tempFloat = "%" + codeCounters.newTEMP();
      code = code || instruction::FLOAT(tempFloat, addr2);
      code = code || instruction::FLOAD(temp, tempFloat);
    }
    else if (Types.isArrayTy(tidRight)) {
      // còpia de vector per valor
      std::string leftArrayTemp = addr1;
      if (Symbols.isParameterClass(addr1)) {
        leftArrayTemp = "%" + codeCounters.newTEMP();
        code = code || instruction::LOAD(leftArrayTemp, addr1);
      }

      std::string rightArrayTemp = addr2;
      if (Symbols.isParameterClass(addr2)) {
        rightArrayTemp = "%" + codeCounters.newTEMP();
        code = code || instruction::LOAD(rightArrayTemp, addr2);
      }

      std::string arraySize = std::to_string(Types.getArraySize(tidRight));
      std::string auxTemp = "%" + codeCounters.newTEMP();
      std::string labelNum = codeCounters.newLabelWHILE();
      std::string startWhileLabel = "startarraycopy" + labelNum;
      std::string endWhileLabel = "endarraycopy" + labelNum;
      std::string conditionTemp = "%" + codeCounters.newTEMP();
      std::string indexTemp = "%" + codeCounters.newTEMP();
      std::string arraySizeTemp = "%" + codeCounters.newTEMP();
      std::string oneTemp = "%" + codeCounters.newTEMP();

      code = code || instruction::ILOAD(arraySizeTemp, arraySize) || instruction::ILOAD(oneTemp, "1") || instruction::ILOAD(indexTemp, "0");
      code = code || instruction::LABEL(startWhileLabel) || instruction::LT(conditionTemp, indexTemp, arraySizeTemp);
      code = code || instruction::FJUMP(conditionTemp, endWhileLabel) || instruction::LOADX(auxTemp, rightArrayTemp, indexTemp);
      code = code || instruction::XLOAD(leftArrayTemp, indexTemp, auxTemp) || instruction::ADD(indexTemp, indexTemp, oneTemp);
      code = code || instruction::UJUMP(startWhileLabel) || instruction::LABEL(endWhileLabel);
    }
    else {
      code = code || instruction::LOAD(temp, addr2);
    }
  }
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitFuncCallStmt(AslParser::FuncCallStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  std::string funcName = ctx->funcCall()->ident()->ID()->getText();

  TypesMgr::TypeId functionTy = Symbols.getGlobalFunctionType(funcName);
  if (not Types.isVoidFunction(functionTy)) {
    code = code || instruction::PUSH();
  }

  auto actualParamsTypes = Types.getFuncParamsTypes(Symbols.getGlobalFunctionType(funcName));
  
  if (ctx->funcCall()->exprList()) {
    for (uint i = 0; i < ctx->funcCall()->exprList()->expr().size(); ++i) {
      auto passedParam = ctx->funcCall()->exprList()->expr(i);
      CodeAttribs     && paramAttr = visit(passedParam);
      std::string temp = paramAttr.addr;

      code = code || paramAttr.code;
      
      TypesMgr::TypeId passedParamTy = getTypeDecor(passedParam);

      // Check integer-float coercion
      if (Types.isIntegerTy(passedParamTy) && Types.isFloatTy(actualParamsTypes[i])) {
        temp = "%" + codeCounters.newTEMP();
        code = code || instruction::FLOAT(temp, paramAttr.addr);
      } 
      // Check array type
      else if (Types.isArrayTy(passedParamTy)) {
        temp = "%" + codeCounters.newTEMP();
        if (Symbols.isParameterClass(paramAttr.addr)) {
          code = code || instruction::LOAD(temp, paramAttr.addr);
        }
        else {
          code = code || instruction::ALOAD(temp, paramAttr.addr);
        }
      }

      // Push parameter to stack
      code = code || instruction::PUSH(temp);
    }
  }

  code = code || instruction::CALL(funcName);

  // Pop all parameters, ignoring its value
  if (ctx->funcCall()->exprList()) {
    for (uint i = 0; i < ctx->funcCall()->exprList()->expr().size(); ++i) {
      code = code || instruction::POP();
    }
  }
  if (not Types.isVoidFunction(functionTy)) {
    code = code || instruction::POP();
  }
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitFuncAccess(AslParser::FuncAccessContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  std::string funcName = ctx->funcCall()->ident()->ID()->getText();
  auto actualParamsTypes = Types.getFuncParamsTypes(Symbols.getGlobalFunctionType(funcName));
  code = code || instruction::PUSH();

  if (ctx->funcCall()->exprList()) {
    for (uint i = 0; i < ctx->funcCall()->exprList()->expr().size(); ++i) {
      auto passedParam = ctx->funcCall()->exprList()->expr(i);
      CodeAttribs     && paramAttr = visit(passedParam);
      code = code || paramAttr.code;

      std::string temp = paramAttr.addr;
      TypesMgr::TypeId passedParamTy = getTypeDecor(passedParam);
      if (Types.isIntegerTy(passedParamTy) && Types.isFloatTy(actualParamsTypes[i])) {
        temp = "%" + codeCounters.newTEMP();
        code = code || instruction::FLOAT(temp, paramAttr.addr);
      }
      // Check array type
      else if (Types.isArrayTy(passedParamTy)) {
        temp = "%" + codeCounters.newTEMP();
        if (Symbols.isParameterClass(paramAttr.addr)) {
          code = code || instruction::LOAD(temp, paramAttr.addr);
        }
        else {
          code = code || instruction::ALOAD(temp, paramAttr.addr);
        }
      }
      code = code || instruction::PUSH(temp);
    }
  }
  code = code || instruction::CALL(funcName);

  if (ctx->funcCall()->exprList()) {
    for (uint i = 0; i < ctx->funcCall()->exprList()->expr().size(); ++i) {
      code = code || instruction::POP();
    }
  }

  std::string newTmp = "%" + codeCounters.newTEMP();
  code = code || instruction::POP(newTmp);
  CodeAttribs codeAttr(newTmp, "", code);
  DEBUG_EXIT();
  return codeAttr;
}

antlrcpp::Any CodeGenVisitor::visitIfStmt(AslParser::IfStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  CodeAttribs &&        codAtsE = visit(ctx->expr());
  std::string          addrCond = codAtsE.addr;
  instructionList &    codeCond = codAtsE.code;
  instructionList &&   codeThen = visit(ctx->statements(0)); 
  
  std::string label = codeCounters.newLabelIF();
  std::string labelEndIf = "endif"+label;

  if (ctx->ELSE()) {
    std::string labelElse = "else"+label;
    instructionList &&   codeElse = visit(ctx->statements(1)); 

    code = codeCond || instruction::FJUMP(addrCond, labelElse) ||
          codeThen || instruction::UJUMP(labelEndIf) || 
          instruction::LABEL(labelElse) || codeElse ||
          instruction::LABEL(labelEndIf);
  } else {
    code = codeCond || instruction::FJUMP(addrCond, labelEndIf) ||
          codeThen || instruction::LABEL(labelEndIf);
  }
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitWhileStmt(AslParser::WhileStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  CodeAttribs     && codAtsE = visit(ctx->expr());
  std::string          addr1 = codAtsE.addr;
  instructionList &    code1 = codAtsE.code;
  instructionList &&   code2 = visit(ctx->statements()); 

  std::string label = codeCounters.newLabelWHILE();
  std::string labelStartWhile = "startwhile"+label;
  std::string labelEndWhile = "endwhile"+label;

  code = instruction::LABEL(labelStartWhile) || 
        code1 || instruction::FJUMP(addr1, labelEndWhile) ||
        code2 || instruction::UJUMP(labelStartWhile) ||
        instruction::LABEL(labelEndWhile);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitReturnStmt(AslParser::ReturnStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;

  if (ctx->expr()) {
    CodeAttribs && exprAttr = visit(ctx->expr());
    std::string retTemp = exprAttr.addr;

    TypesMgr::TypeId exprTy = getTypeDecor(ctx->expr());
    TypesMgr::TypeId functionTy = getCurrentFunctionTy();
    code = exprAttr.code;

    if (Types.isIntegerTy(exprTy) and Types.isFloatTy(functionTy)) {
      retTemp = "%" + codeCounters.newTEMP();
      code = code || instruction::FLOAT(retTemp, exprAttr.addr);
    }
    code = code || instruction::LOAD("_result", retTemp);
  }

  code = code || instruction::RETURN();
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitReadStmt(AslParser::ReadStmtContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAtsE = visit(ctx->left_expr());
  std::string          addr1 = codAtsE.addr;
  std::string          offs1 = codAtsE.offs;
  instructionList &    code1 = codAtsE.code;
  instructionList &     code = code1;
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->left_expr());

  if (offs1 != "") {
    std::string tmp = "%" + codeCounters.newTEMP();
    if (Types.isIntegerTy(tid1)) {
        code = code1 || instruction::READI(tmp);
      } else if (Types.isFloatTy(tid1)) {
        code = code1 || instruction::READF(tmp);
      } else if (Types.isCharacterTy(tid1)) {
        code = code1 || instruction::READC(tmp);
      } else {
        code = code1 || instruction::READI(tmp);
      }
    code = code || instruction::XLOAD(addr1, offs1, tmp);
  } else {
    if (Types.isIntegerTy(tid1)) {
        code = code1 || instruction::READI(addr1);
      } else if (Types.isFloatTy(tid1)) {
        code = code1 || instruction::READF(addr1);
      } else if (Types.isCharacterTy(tid1)) {
        code = code1 || instruction::READC(addr1);
      } else {
        code = code1 || instruction::READI(addr1);
      }
  }

  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitWriteExpr(AslParser::WriteExprContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->expr());
  std::string         addr1 = codAt1.addr;
  // std::string         offs1 = codAt1.offs;
  instructionList &   code1 = codAt1.code;
  instructionList &    code = code1;
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->expr());
  if (Types.isIntegerTy(tid1))          code = code1 || instruction::WRITEI(addr1);
  else if (Types.isFloatTy(tid1))       code = code1 || instruction::WRITEF(addr1);
  else if (Types.isCharacterTy(tid1))   code = code1 || instruction::WRITEC(addr1);
  else if (Types.isBooleanTy(tid1))     code = code1 || instruction::WRITEI(addr1);
  else                                  code = code1 || instruction::WRITES(addr1);

  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitWriteString(AslParser::WriteStringContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  std::string s = ctx->STRING()->getText();
  code = code || instruction::WRITES(s);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitLeftExprIdent(AslParser::LeftExprIdentContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = visit(ctx->ident());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitLeftArrayAccess(AslParser::LeftArrayAccessContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs &&      codAtsId = visit(ctx->ident());
  std::string           addrId = codAtsId.addr;
  CodeAttribs &&    codAtsExpr = visit(ctx->expr());
  std::string         addrExpr = codAtsExpr.addr;

  instructionList code;
  code = code || codAtsExpr.code;

  std::string addrTemp = addrId;
  if (Symbols.isParameterClass(addrId)) {
    addrTemp = "%" + codeCounters.newTEMP();
    TypesMgr::TypeId idTy = getTypeDecor(ctx->ident());
    TypesMgr::TypeId arrayTy = Types.getArrayElemType(idTy);
    if (Types.isFloatTy(arrayTy)) {
      code = code || instruction::FLOAD(addrTemp, addrId);
    }
    else {
      code = code || instruction::LOAD(addrTemp, addrId);
    }
  }
  CodeAttribs codAts(addrTemp, addrExpr, code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitArithmetic(AslParser::ArithmeticContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->expr(0));
  std::string         addr1 = codAt1.addr;
  instructionList &   code1 = codAt1.code;
  CodeAttribs     && codAt2 = visit(ctx->expr(1));
  std::string         addr2 = codAt2.addr;
  instructionList &   code2 = codAt2.code;
  instructionList &&   code = code1 || code2;

  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  // TypesMgr::TypeId  t = getTypeDecor(ctx);

  std::string temp = "%"+codeCounters.newTEMP();
  if (Types.isFloatTy(t1) || Types.isFloatTy(t2)) {
    std::string temp1 = addr1;
    std::string temp2 = addr2;
    if (not Types.isFloatTy(t1)) {
      temp1 = "%"+codeCounters.newTEMP();
      code = code || instruction::FLOAT(temp1, addr1);
    }
    else if (not Types.isFloatTy(t2)) {
      temp2 = "%"+codeCounters.newTEMP();
      code = code || instruction::FLOAT(temp2, addr2);
    }
    if (ctx->MUL())
      code = code || instruction::FMUL(temp, temp1, temp2);
    else if (ctx->PLUS())
      code = code || instruction::FADD(temp, temp1, temp2);
    else if (ctx->DIV())
      code = code || instruction::FDIV(temp, temp1, temp2);
    else if (ctx->MINUS()) 
      code = code || instruction::FSUB(temp, temp1, temp2);
    else if (ctx->MOD()) {
      // dividend = divisor *. quotient +. remainder
      std::string tempQuotient = "%"+codeCounters.newTEMP();
      code = code || instruction::FDIV(tempQuotient, addr1, addr2);

      std::string tempDxQ = "%"+codeCounters.newTEMP();
      code = code || instruction::FMUL(tempDxQ, tempQuotient, addr2);

      // temp contains the remainder
      code = code || instruction::FSUB(temp, addr1, tempDxQ);
    }
  }
  else {
    if (ctx->MUL())
      code = code || instruction::MUL(temp, addr1, addr2);
    else if (ctx->PLUS())
      code = code || instruction::ADD(temp, addr1, addr2);
    else if (ctx->DIV())
      code = code || instruction::DIV(temp, addr1, addr2);
    else if (ctx->MINUS()) 
      code = code || instruction::SUB(temp, addr1, addr2);
    else if (ctx->MOD()) {
      // dividend = divisor *. quotient +. remainder
      std::string tempQuotient = "%"+codeCounters.newTEMP();
      code = code || instruction::DIV(tempQuotient, addr1, addr2);

      std::string tempDxQ = "%"+codeCounters.newTEMP();
      code = code || instruction::MUL(tempDxQ, tempQuotient, addr2);

      // temp contains the remainder
      code = code || instruction::SUB(temp, addr1, tempDxQ);
    }
  }

  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitRelational(AslParser::RelationalContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->expr(0));
  std::string         addr1 = codAt1.addr;
  instructionList &   code1 = codAt1.code;
  CodeAttribs     && codAt2 = visit(ctx->expr(1));
  std::string         addr2 = codAt2.addr;
  instructionList &   code2 = codAt2.code;
  instructionList &&   code = code1 || code2;

  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  // TypesMgr::TypeId  t = getTypeDecor(ctx);

  std::string temp = "%"+codeCounters.newTEMP();
  if (Types.isFloatTy(t1) || Types.isFloatTy(t2)) {
    std::string temp1 = addr1;
    std::string temp2 = addr2;
    if (not Types.isFloatTy(t1)) {
      temp1 = "%"+codeCounters.newTEMP();
      code = code || instruction::FLOAT(temp1, addr1);
    }
    else if (not Types.isFloatTy(t2)) {
      temp2 = "%"+codeCounters.newTEMP();
      code = code || instruction::FLOAT(temp2, addr2);
    }
    if (ctx->EQUAL()) code = code || instruction::FEQ(temp, temp1, temp2);
    else if (ctx->GT()) code = code || instruction::FLT(temp, temp2, temp1);
    else if (ctx->GE()) code = code || instruction::FLE(temp, temp2, temp1);
    else if (ctx->LT()) code = code || instruction::FLT(temp, temp1, temp2);
    else if (ctx->LE()) code = code || instruction::FLE(temp, temp1, temp2);
    else if (ctx->NEQ()) code = code || instruction::FEQ(temp, temp1, temp2) || instruction::NOT(temp, temp);
  }
  else {
    if (ctx->EQUAL()) code = code || instruction::EQ(temp, addr1, addr2);
    else if (ctx->GT()) code = code || instruction::LT(temp, addr2, addr1);
    else if (ctx->GE()) code = code || instruction::LE(temp, addr2, addr1);
    else if (ctx->LT()) code = code || instruction::LT(temp, addr1, addr2);
    else if (ctx->LE()) code = code || instruction::LE(temp, addr1, addr2);
    else if (ctx->NEQ()) code = code || instruction::EQ(temp, addr1, addr2) || instruction::NOT(temp, temp);
  }
  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitUnary(AslParser::UnaryContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs &&   codeAt = visit(ctx->expr());
  std::string       addr1 = codeAt.addr;
  instructionList & code1 = codeAt.code;
  instructionList &  code = code1;

  std::string temp = "%"+codeCounters.newTEMP();
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());
  
  if (ctx->NOT()) {
    code = code1 || instruction::NOT(temp, addr1);
  }
  else if (ctx->MINUS())  {
    if (Types.isFloatTy(t1)) {
      code = code1 || instruction::FNEG(temp, addr1);
    } else {
      code = code1 || instruction::NEG(temp, addr1);
    }
  }
  else {  //ctx->PLUS()
    if (Types.isFloatTy(t1)) {
      code = code1 || instruction::FLOAD(temp, addr1);
    } else {
      code = code1 || instruction::LOAD(temp, addr1);
    }
  }

  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitBoolean(AslParser::BooleanContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->expr(0));
  std::string         addr1 = codAt1.addr;
  instructionList &   code1 = codAt1.code;
  CodeAttribs     && codAt2 = visit(ctx->expr(1));
  std::string         addr2 = codAt2.addr;
  instructionList &   code2 = codAt2.code;
  instructionList &&   code = code1 || code2;
  // TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  // TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  // TypesMgr::TypeId  t = getTypeDecor(ctx);

  std::string temp = "%"+codeCounters.newTEMP();
  if (ctx->AND()) {
    code = code || instruction::AND(temp, addr1, addr2);
  }
  else {
    code = code || instruction::OR(temp, addr1, addr2);
  }
  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitValue(AslParser::ValueContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  std::string temp = "%"+codeCounters.newTEMP();
  if (ctx->BOOLVAL()) {
    if (ctx->BOOLVAL()->getText() == "true") {
      code = instruction::ILOAD(temp, "1");
    } else {
      code = instruction::ILOAD(temp, "0");
    }
  } else {
    code = instruction::ILOAD(temp, ctx->getText());
  }
  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitArrayAccess(AslParser::ArrayAccessContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs &&   codAtsId = visit(ctx->ident());
  std::string        addrId = codAtsId.addr;
  CodeAttribs && codAtsExpr = visit(ctx->expr());
  std::string      addrExpr = codAtsExpr.addr;

  instructionList code;
  std::string temp = "%"+codeCounters.newTEMP();
  std::string tempCopyReference = addrId;

  // If it's a parameter, reference it through a temporal register
  if (Symbols.isParameterClass(addrId)) {
    tempCopyReference = "%"+codeCounters.newTEMP();
    code = code || instruction::LOAD(tempCopyReference, addrId);
  }

  code = code || codAtsExpr.code || instruction::LOADX(temp, tempCopyReference, addrExpr);
  CodeAttribs codAts(temp, "", code);

  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitExprIdent(AslParser::ExprIdentContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = visit(ctx->ident());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitIdent(AslParser::IdentContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs codAts(ctx->ID()->getText(), "", instructionList());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitParenthesis(AslParser::ParenthesisContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = visit(ctx->expr());
  DEBUG_EXIT();
  return codAts;
}

// Getters for the necessary tree node atributes:
//   Scope and Type
SymTable::ScopeId CodeGenVisitor::getScopeDecor(antlr4::ParserRuleContext *ctx) const {
  return Decorations.getScope(ctx);
}
TypesMgr::TypeId CodeGenVisitor::getTypeDecor(antlr4::ParserRuleContext *ctx) const {
  return Decorations.getType(ctx);
}


// Constructors of the class CodeAttribs:
//
CodeGenVisitor::CodeAttribs::CodeAttribs(const std::string & addr,
                                         const std::string & offs,
                                         instructionList & code) :
  addr{addr}, offs{offs}, code{code} {
}

CodeGenVisitor::CodeAttribs::CodeAttribs(const std::string & addr,
                                         const std::string & offs,
                                         instructionList && code) :
  addr{addr}, offs{offs}, code{code} {
}
