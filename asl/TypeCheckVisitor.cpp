//////////////////////////////////////////////////////////////////////
//
//    TypeCheckVisitor - Walk the parser tree to do the semantic
//                       typecheck for the Asl programming language
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

#include "TypeCheckVisitor.h"
#include "antlr4-runtime.h"

#include "../common/TypesMgr.h"
#include "../common/SymTable.h"
#include "../common/TreeDecoration.h"
#include "../common/SemErrors.h"

#include <iostream>
#include <string>

// uncomment the following line to enable debugging messages with DEBUG*
// #define DEBUG_BUILD
#include "../common/debug.h"

// using namespace std;


// Constructor
TypeCheckVisitor::TypeCheckVisitor(TypesMgr       & Types,
                                   SymTable       & Symbols,
                                   TreeDecoration & Decorations,
                                   SemErrors      & Errors) :
  Types{Types},
  Symbols{Symbols},
  Decorations{Decorations},
  Errors{Errors} {
}

// Accessor/Mutator to the attribute currFunctionType
TypesMgr::TypeId TypeCheckVisitor::getCurrentFunctionTy() const {
  return currFunctionType;
}

void TypeCheckVisitor::setCurrentFunctionTy(TypesMgr::TypeId type) {
  currFunctionType = type;
}

// Methods to visit each kind of node:
//
antlrcpp::Any TypeCheckVisitor::visitProgram(AslParser::ProgramContext *ctx) {
  DEBUG_ENTER();
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  for (auto ctxFunc : ctx->function()) {
    visit(ctxFunc);
  }
  if (Symbols.noMainProperlyDeclared())
    Errors.noMainProperlyDeclared(ctx);
  Symbols.popScope();
  Errors.print();
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitParenthesis(AslParser::ParenthesisContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t = getTypeDecor(ctx->expr());
  putTypeDecor(ctx, t);
  //std::cout << t << std::endl;
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitFunction(AslParser::FunctionContext *ctx) {
  DEBUG_ENTER();
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  // return value check
  if (ctx->retType()) {
    visit(ctx->retType()->type());
    TypesMgr::TypeId t1 = getTypeDecor(ctx->retType()->type());
    setCurrentFunctionTy(t1);
  } else {
    setCurrentFunctionTy(Types.createVoidTy());
  }
  // Symbols.print();
  visit(ctx->statements());
  Symbols.popScope();
  DEBUG_EXIT();
  return 0;
}

// antlrcpp::Any TypeCheckVisitor::visitDeclarations(AslParser::DeclarationsContext *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// antlrcpp::Any TypeCheckVisitor::visitVariable_decl(AslParser::Variable_declContext *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// antlrcpp::Any TypeCheckVisitor::visitType(AslParser::TypeContext *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

antlrcpp::Any TypeCheckVisitor::visitStatements(AslParser::StatementsContext *ctx) {
  DEBUG_ENTER();
  visitChildren(ctx);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitAssignStmt(AslParser::AssignStmtContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->left_expr());
  visit(ctx->expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->left_expr());
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr());

  if ((not Types.isErrorTy(t1)) and (not Types.isErrorTy(t2)) and
      (not Types.equalTypes(t1, t2)) and
      (not (Types.isFloatTy(t1) and Types.isIntegerTy(t2)))) {
    Errors.incompatibleAssignment(ctx->ASSIGN());
  }
  else if (Types.isFunctionTy(t2) and Types.isVoidFunction(t2)) {
    Errors.isNotProcedure(ctx->expr());
  }
  if ((not Types.isErrorTy(t1)) and (not getIsLValueDecor(ctx->left_expr())))
    Errors.nonReferenceableLeftExpr(ctx->left_expr());

  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitArrayMapStmt(AslParser::ArrayMapStmtContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->left_expr());    //left array (destination)
  TypesMgr::TypeId arrayType0 = getTypeDecor(ctx->left_expr());
  uint sizeArray0 = Types.getArraySize(arrayType0);
  putTypeDecor(ctx, arrayType0);

  visit(ctx->ident(1));       //second array (origin)
  TypesMgr::TypeId arrayType1 = getTypeDecor(ctx->ident(1));
  uint sizeArray1 = Types.getArraySize(arrayType1);

  // both arrays same size check--------
  if (sizeArray0 != sizeArray1) {
    Errors.mapWithNonArraysOrDifferentSizes(ctx);
    putTypeDecor(ctx, Types.createErrorTy());
  }

  TypesMgr::TypeId elemType1 = Types.getArrayElemType(arrayType1);

  visit(ctx->ident(0));      //for variable
  TypesMgr::TypeId forType = getTypeDecor(ctx->ident(0));

  // for variable typecheck--------
  if (not Types.isErrorTy(forType) and not Types.equalTypes(forType, elemType1) and not (Types.isIntegerTy(elemType1) and Types.isFloatTy(forType))) {
    Errors.mapWithIncompatibleControlVar(ctx);
    putTypeDecor(ctx, Types.createErrorTy());
  }

  visit(ctx->expr(0));      // bool cond var
  TypesMgr::TypeId condType = getTypeDecor(ctx->expr(0));

  // bool check--------
  if (not Types.isBooleanTy(condType) and not Types.isErrorTy(condType)) {
    Errors.mapWithNonBooleanCondition(ctx);
        putTypeDecor(ctx, Types.createErrorTy());

  }

  visit(ctx->expr(1));        // exprs vars
  TypesMgr::TypeId exprType1 = getTypeDecor(ctx->expr(1));
  visit(ctx->expr(2));
  TypesMgr::TypeId exprType2 = getTypeDecor(ctx->expr(2));

  TypesMgr::TypeId elemType0 = Types.getArrayElemType(arrayType0);

  // same type as dest array or int to float check-------
  if ((not Types.isErrorTy(exprType1)) and (not Types.isErrorTy(exprType2)) and
      (not Types.equalTypes(exprType1, elemType0) and not (Types.isIntegerTy(exprType1) and Types.isFloatTy(elemType0))) or 
      (not Types.equalTypes(exprType2, elemType0) and not (Types.isIntegerTy(exprType2) and Types.isFloatTy(elemType0)))
      ) {
    Errors.mapWithIncompatibleValues(ctx);
        putTypeDecor(ctx, Types.createErrorTy());

  }

  DEBUG_EXIT();
  return 0;
}


antlrcpp::Any TypeCheckVisitor::visitReturnStmt(AslParser::ReturnStmtContext *ctx) {
    DEBUG_ENTER();
    if (ctx->expr()) {
        visit(ctx->expr());
        TypesMgr::TypeId t = getTypeDecor(ctx->expr());
        if (getCurrentFunctionTy() != t and !(Types.isIntegerTy(t) and Types.isFloatTy(getCurrentFunctionTy()))) {
            Errors.incompatibleReturn(ctx->RETURN());
        }
    } else {    //function is void
        if (!Types.isVoidTy(getCurrentFunctionTy())) {
            Errors.incompatibleReturn(ctx->RETURN());
        }
    }
    DEBUG_EXIT();
    return 0;
}

antlrcpp::Any TypeCheckVisitor::visitIfStmt(AslParser::IfStmtContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());
  if ((not Types.isErrorTy(t1)) and (not Types.isBooleanTy(t1)))
    Errors.booleanRequired(ctx);
  visit(ctx->statements(0));
  if (ctx->ELSE()) {
    visit(ctx->statements(1));
  }
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitWhileStmt(AslParser::WhileStmtContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());
  if ((not Types.isErrorTy(t1)) and (not Types.isBooleanTy(t1)))
    Errors.booleanRequired(ctx);
  visit(ctx->statements());
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitReadStmt(AslParser::ReadStmtContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->left_expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->left_expr());
  if ((not Types.isErrorTy(t1)) and (not Types.isPrimitiveTy(t1)) and
      (not Types.isFunctionTy(t1)))
    Errors.readWriteRequireBasic(ctx);
  if ((not Types.isErrorTy(t1)) and (not getIsLValueDecor(ctx->left_expr())))
    Errors.nonReferenceableExpression(ctx);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitWriteExpr(AslParser::WriteExprContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());
  if ((not Types.isErrorTy(t1)) and (not Types.isPrimitiveTy(t1)))
    Errors.readWriteRequireBasic(ctx);
  DEBUG_EXIT();
  return 0;
}

// antlrcpp::Any TypeCheckVisitor::visitWriteString(AslParser::WriteStringContext *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

antlrcpp::Any TypeCheckVisitor::visitLeftExprIdent(AslParser::LeftExprIdentContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->ident());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->ident());
  putTypeDecor(ctx, t1);
  bool b = getIsLValueDecor(ctx->ident());
  putIsLValueDecor(ctx, b);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitLeftArrayAccess(AslParser::LeftArrayAccessContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->ident());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->ident());
  if (not Types.isErrorTy(t1)) {
    if (not Types.isArrayTy(t1)) {
        Errors.nonArrayInArrayAccess(ctx);
        putTypeDecor(ctx, Types.createErrorTy());
    }
    else putTypeDecor(ctx, Types.getArrayElemType(t1));
    visit(ctx->expr());
    TypesMgr::TypeId t2 = getTypeDecor(ctx->expr());
    if (not Types.isIntegerTy(t2))
      Errors.nonIntegerIndexInArrayAccess(ctx->expr());
    putIsLValueDecor(ctx, true);
  } else {
    putTypeDecor(ctx, t1);
  }
  
  DEBUG_EXIT();
  return 0;
}


antlrcpp::Any TypeCheckVisitor::visitUnary(AslParser::UnaryContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());

  if (ctx->PLUS() or ctx->MINUS()) {
    if (not Types.isErrorTy(t1) and (not Types.isNumericTy(t1))) {
      Errors.incompatibleOperator(ctx->op);
    }
    else putTypeDecor(ctx, t1);
  } else if (ctx->NOT()) {
    if (not Types.isErrorTy(t1) and (not Types.isBooleanTy(t1))) {
      Errors.incompatibleOperator(ctx->op);
    }
    else putTypeDecor(ctx, t1);
  }

  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitArithmetic(AslParser::ArithmeticContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr(0));
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  visit(ctx->expr(1));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  TypesMgr::TypeId t;
  if (ctx->MOD()) {
    if (((not Types.isErrorTy(t1)) and (not Types.isIntegerTy(t1))) or
        ((not Types.isErrorTy(t2)) and (not Types.isIntegerTy(t2))))
      Errors.incompatibleOperator(ctx->op);
    t = Types.createIntegerTy();
  } else {
    if (((not Types.isErrorTy(t1)) and (not Types.isNumericTy(t1))) or
        ((not Types.isErrorTy(t2)) and (not Types.isNumericTy(t2))))
      Errors.incompatibleOperator(ctx->op);
    if (Types.isFloatTy(t1) or Types.isFloatTy(t2)) t = Types.createFloatTy();
    else t = Types.createIntegerTy();
  }
  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, false);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitBoolean(AslParser::BooleanContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr(0));
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  visit(ctx->expr(1));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  if (((not Types.isErrorTy(t1)) and (not Types.isBooleanTy(t1))) or
      ((not Types.isErrorTy(t2)) and (not Types.isBooleanTy(t2))))
    Errors.incompatibleOperator(ctx->op);
  TypesMgr::TypeId t = Types.createBooleanTy();
  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, true);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitRelational(AslParser::RelationalContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr(0));
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  visit(ctx->expr(1));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  std::string oper = ctx->op->getText();
  if ((not Types.isErrorTy(t1)) and (not Types.isErrorTy(t2)) and
      (not Types.comparableTypes(t1, t2, oper)))
    Errors.incompatibleOperator(ctx->op);
  TypesMgr::TypeId t = Types.createBooleanTy();
  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, false);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitValue(AslParser::ValueContext *ctx) {
  DEBUG_ENTER();
  TypesMgr::TypeId t;
  if (ctx->CHARVAL()) t = Types.createCharacterTy();
  else if (ctx->INTVAL()) t = Types.createIntegerTy();
  else if (ctx->FLOATVAL()) t = Types.createFloatTy();
  else if (ctx->BOOLVAL()) t = Types.createBooleanTy();
  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, false);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitFuncCall(AslParser::FuncCallContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->ident());
  TypesMgr::TypeId t = getTypeDecor(ctx->ident());

  bool isFunctionTy = Types.isFunctionTy(t);

  if (not isFunctionTy and not Types.isErrorTy(t)) {
    Errors.isNotCallable(ctx->ident());
  }
  putTypeDecor(ctx, t);
  bool b = getIsLValueDecor(ctx->ident());
  putIsLValueDecor(ctx, b);
  if (ctx->exprList()) {
    uint nParams = ctx->exprList()->expr().size();
    if (Types.isFunctionTy(t) and Types.getNumOfParameters(t) != nParams) {
      Errors.numberOfParameters(ctx->ident());
    }
    for(uint i = 0; i < nParams; ++i) {
      // check type
      visit(ctx->exprList()->expr(i));
      TypesMgr::TypeId t1 = getTypeDecor(ctx->exprList()->expr(i));
      if (isFunctionTy and i < Types.getNumOfParameters(t)) {
        TypesMgr::TypeId paramTy = Types.getParameterType(t, i);
        if (not Types.equalTypes(t1, paramTy) and
            not (Types.isIntegerTy(t1) and Types.isFloatTy(paramTy)) and
            not Types.isErrorTy(t1))
        Errors.incompatibleParameter(ctx->exprList()->expr(i), i+1, ctx->ident());
      }
    }

  }
  else {
    if (isFunctionTy and Types.getNumOfParameters(t) != 0) {
      Errors.numberOfParameters(ctx->ident());
    }
  }
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitExprIdent(AslParser::ExprIdentContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->ident());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->ident());
  putTypeDecor(ctx, t1);
  bool b = getIsLValueDecor(ctx->ident());
  putIsLValueDecor(ctx, b);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitArrayAccess(AslParser::ArrayAccessContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->ident());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->ident());
  if (not Types.isErrorTy(t1)) {
    if (not Types.isArrayTy(t1)) {
      Errors.nonArrayInArrayAccess(ctx);
      putTypeDecor(ctx, Types.createErrorTy());
    } else {
      TypesMgr::TypeId tElem = Types.getArrayElemType(t1);
      putTypeDecor(ctx, tElem);
    }
    putIsLValueDecor(ctx, true);
    // Has expr because it is an array
    visit(ctx->expr());
    TypesMgr::TypeId t2 = getTypeDecor(ctx->expr());
    if (not Types.isIntegerTy(t2))
      Errors.nonIntegerIndexInArrayAccess(ctx->expr());
  } else {
    putTypeDecor(ctx, t1);
  }
  DEBUG_EXIT();
  return 0;
}


antlrcpp::Any TypeCheckVisitor::visitFuncAccess(AslParser::FuncAccessContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->funcCall());
  TypesMgr::TypeId funcType = getTypeDecor(ctx->funcCall());
  if (Types.isFunctionTy(funcType) and Types.isVoidFunction(funcType)) {
    Errors.isNotFunction(ctx);
  }
  else if (Types.isFunctionTy(funcType)) {
    TypesMgr::TypeId t = Types.getFuncReturnType(funcType);
    putTypeDecor(ctx, t);
    putIsLValueDecor(ctx, false);
    bool b = getIsLValueDecor(ctx->funcCall());
    putIsLValueDecor(ctx, b);
  }
  else {
    putTypeDecor(ctx, funcType);
    putIsLValueDecor(ctx, false);
    bool b = getIsLValueDecor(ctx->funcCall());
    putIsLValueDecor(ctx, b);
  }
  DEBUG_EXIT();
  return 0;
}


antlrcpp::Any TypeCheckVisitor::visitIdent(AslParser::IdentContext *ctx) {
  DEBUG_ENTER();
  std::string ident = ctx->getText();
  // TODO preguntar al profe sobre els errors de les funcions del jp 9
  if (Symbols.findInStack(ident) == -1) {
    Errors.undeclaredIdent(ctx->ID());
    TypesMgr::TypeId te = Types.createErrorTy();
    putTypeDecor(ctx, te);
    putIsLValueDecor(ctx, true);
  }
  else {
    TypesMgr::TypeId t1 = Symbols.getType(ident);
    putTypeDecor(ctx, t1);
    if (Symbols.isFunctionClass(ident))
      putIsLValueDecor(ctx, false);
    else
      putIsLValueDecor(ctx, true);
  }
  DEBUG_EXIT();
  return 0;
}


// Getters for the necessary tree node atributes:
//   Scope, Type ans IsLValue
SymTable::ScopeId TypeCheckVisitor::getScopeDecor(antlr4::ParserRuleContext *ctx) {
  return Decorations.getScope(ctx);
}
TypesMgr::TypeId TypeCheckVisitor::getTypeDecor(antlr4::ParserRuleContext *ctx) {
  return Decorations.getType(ctx);
}
bool TypeCheckVisitor::getIsLValueDecor(antlr4::ParserRuleContext *ctx) {
  return Decorations.getIsLValue(ctx);
}

// Setters for the necessary tree node attributes:
//   Scope, Type ans IsLValue
void TypeCheckVisitor::putScopeDecor(antlr4::ParserRuleContext *ctx, SymTable::ScopeId s) {
  Decorations.putScope(ctx, s);
}
void TypeCheckVisitor::putTypeDecor(antlr4::ParserRuleContext *ctx, TypesMgr::TypeId t) {
  Decorations.putType(ctx, t);
}
void TypeCheckVisitor::putIsLValueDecor(antlr4::ParserRuleContext *ctx, bool b) {
  Decorations.putIsLValue(ctx, b);
}
