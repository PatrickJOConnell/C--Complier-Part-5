/*
   Patrick O'Connell
   CS 473 Project: Part 5
   oconne16
 */

#include "util.h"
#include "typecheck.h"

extern AstNodePtr program;
AstNodePtr myAstNodePtr;
int typeInt1, typeInt2;

int typecheck() {//DONE
	int isTypeCheckCertified = 1;
	AstNodePtr tempProgram = program;
	while (tempProgram) {
		Type* currentType = tempProgram->nSymbolPtr->stype->function;
		AstNodePtr FORMALVARS = tempProgram->children[0];
		while (FORMALVARS) {
			currentType->function = FORMALVARS->nType;
			currentType = currentType->function;
			FORMALVARS = FORMALVARS->sibling;
		}
		tempProgram = tempProgram->sibling;
	}
	myAstNodePtr = program;
	while (myAstNodePtr && isTypeCheckCertified) {
		isTypeCheckCertified = typecheck_method(myAstNodePtr);
		myAstNodePtr = myAstNodePtr->sibling;
	}
	return isTypeCheckCertified;
};//DONE


Type* type_equiv(Type *firstTypeItem, Type *secondTypeItem) {//DONE
	if (firstTypeItem->kind == secondTypeItem->kind) {
		return firstTypeItem;
	}

	else {
		typeInt1 = 0;
		printf("Error: Type %d does not match type %d\n", firstTypeItem->kind, secondTypeItem->kind);
		yyerror("Error: Types do not match");
	}
}//DONE


int typecheck_method(AstNode *myAstNodePtr) {//DONE
	int methodCheckReturn = 1;
	AstNodePtr tempAstNode = myAstNodePtr->children[1];
	while (tempAstNode && methodCheckReturn) {
		if (tempAstNode->nKind != STMT) {
			yyerror("Ast error tree in function block");
		}
		methodCheckReturn = typecheck_stmt(tempAstNode);
		tempAstNode = tempAstNode->sibling;
	}
	return methodCheckReturn;
}//DONE


int typecheck_stmt(AstNode *AstNodePtrForStmt) {//DONE
	int statementCheckReturn = 1;
	AstNodePtr tempStatement;
	Type * tempType;
	switch (AstNodePtrForStmt->sKind) {
		case IF_THEN_ELSE_STMT:
			typeInt2 = 1;
			tempType = typecheck_expr(AstNodePtrForStmt->children[0]);
			if (tempType->kind != INT) {
				statementCheckReturn = 0;
				yyerror("ERROR: Improper IF_THEN_ELSE_STMT");
				break;
			}
			statementCheckReturn = typecheck_stmt(AstNodePtrForStmt->children[1]);
			if (AstNodePtrForStmt->children[2])
				statementCheckReturn = typecheck_stmt(AstNodePtrForStmt->children[2]);
			break;
		case COMPOUND_STMT:
			tempStatement = AstNodePtrForStmt->children[0];
			while (tempStatement != NULL && statementCheckReturn) {
				if (tempStatement->nKind != STMT) {
					yyerror("ERROR: Improper COMPOUND_STMT");
				}
				statementCheckReturn = typecheck_stmt(tempStatement);
				tempStatement = tempStatement->sibling;
			}
			break;
		case EXPRESSION_STMT:
			typeInt2 = 1;
			if (AstNodePtrForStmt->children[0])
				typecheck_expr(AstNodePtrForStmt->children[0]);
			statementCheckReturn = typeInt2;
			break;
		case WHILE_STMT:
			typeInt2 = 1;
			tempType = typecheck_expr(AstNodePtrForStmt->children[0]);
			if (tempType->kind != INT) {
				yyerror("ERROR: Improper WHILE_STMT");
				statementCheckReturn = 0;
				break;
			}
			statementCheckReturn = typecheck_stmt(AstNodePtrForStmt->children[1]);
			break;
		case RETURN_STMT:
			typeInt2 = 1;
			if (AstNodePtrForStmt->children[0] == NULL) {
				if (myAstNodePtr->nSymbolPtr->stype->function != NULL && myAstNodePtr->nSymbolPtr->stype->function->kind != VOID) {
					yyerror("ERROR: Improper RETURN_STMT");
				}
			}
			else {
				typeInt2 = 1;
				tempType = typecheck_expr(AstNodePtrForStmt->children[0]);
				statementCheckReturn = statementCheckReturn & typeInt2;
				if (myAstNodePtr->nSymbolPtr->stype->function->kind != tempType->kind) {

					yyerror("ERROR: Improper RETURN_STMT");
					break;
				}
			}
			break;
		default:
			yyerror("ERROR: Improper STMT");
	}
	return statementCheckReturn;
}//DONE


Type *typecheck_expr(AstNode *myAstNodePtr2) {//Done
	Type *type1, *type2;
	AstNodePtr ANPparmeters;
	ElementPtr CallExpElement;
	switch (myAstNodePtr2->eKind){
		case EQ_EXP:
			type1 = typecheck_expr(myAstNodePtr2->children[0]);
			type2 = typecheck_expr(myAstNodePtr2->children[1]);
			if (type1->kind != type2->kind) {
				yyerror("ERROR: Improper EQ_EXP");
				typeInt2 = 0;
			}
			return (new_type(INT));
			break;
		case NE_EXP:
			type1 = typecheck_expr(myAstNodePtr2->children[0]);
			type2 = typecheck_expr(myAstNodePtr2->children[1]);
			if (type1->kind != type2->kind) {
				yyerror("ERROR: Improper NE_EXP");
				typeInt2 = 0;
			}
			return (new_type(INT));
			break;
		case GT_EXP:
			type1 = typecheck_expr(myAstNodePtr2->children[0]);
			type2 = typecheck_expr(myAstNodePtr2->children[1]);
			if (type1->kind != INT || type2->kind != INT)
			{
				yyerror("ERROR: Improper GT_EXP");
				typeInt2 = 0;
			}
			return (new_type(INT));
			break;
		case GE_EXP:
			type1 = typecheck_expr(myAstNodePtr2->children[0]);
			type2 = typecheck_expr(myAstNodePtr2->children[1]);
			if (type1->kind != INT || type2->kind != INT) {
				yyerror("ERROR: Improper GE_EXP");
				typeInt2 = 0;
			}
			return (new_type(INT));
			break;
		case LT_EXP:
			type1 = typecheck_expr(myAstNodePtr2->children[0]);
			type2 = typecheck_expr(myAstNodePtr2->children[1]);
			if (type1->kind != INT || type2->kind != INT) {
				yyerror("ERROR: Improper LT_EXP");
				typeInt2 = 0;
			}
			return (new_type(INT));
			break;
		case LE_EXP:
			type1 = typecheck_expr(myAstNodePtr2->children[0]);
			type2 = typecheck_expr(myAstNodePtr2->children[1]);
			if (type1->kind != INT || type2->kind != INT) {
				yyerror("ERROR: Improper LE_EXP");
				typeInt2 = 0;
			}
			return (new_type(INT));
			break;
		case ADD_EXP:
			type1 = typecheck_expr(myAstNodePtr2->children[0]);
			type2 = typecheck_expr(myAstNodePtr2->children[1]);
			if (type1->kind != INT || type2->kind != INT) {
				yyerror("ERROR: Improper ADD_EXP");
				typeInt2 = 0;
			}
			return type1;
			break;
		case SUB_EXP:
			type1 = typecheck_expr(myAstNodePtr2->children[0]);
			type2 = typecheck_expr(myAstNodePtr2->children[1]);
			if (type1->kind != INT || type2->kind != INT) {
				yyerror("ERROR: Improper SUB_EXP");
				typeInt2 = 0;
			}
			return type1;
			break;
		case MULT_EXP:
			type1 = typecheck_expr(myAstNodePtr2->children[0]);
			type2 = typecheck_expr(myAstNodePtr2->children[1]);
			if (type1->kind != INT || type2->kind != INT) {
				yyerror("ERROR: Improper MULT_EXP");
				typeInt2 = 0;
			}
			return type1;
			break;
		case DIV_EXP:
			type1 = typecheck_expr(myAstNodePtr2->children[0]);
			type2 = typecheck_expr(myAstNodePtr2->children[1]);
			if (type1->kind != INT || type2->kind != INT) {
				yyerror("ERROR: Improper DIV_EXP");
				typeInt2 = 0;
			}
			return type1;
			break;
		case CONST_EXP:
			return new_type(INT);
			break;
		case VAR_EXP:
			if (myAstNodePtr2->nSymbolPtr == NULL) {
				yyerror("ERROR: Improper VAR_EXP");
			}
			else {
				return myAstNodePtr2->nSymbolPtr->stype;
			}
			break;

		case ASSI_EXP:
			if (!(myAstNodePtr2->children[0]->eKind == VAR_EXP && myAstNodePtr2->children[0]->nSymbolPtr->stype->kind == INT) && myAstNodePtr2->children[0]->eKind != ARRAY_EXP) {
				yyerror("ERROR: Improper ASSI_EXP");
				typeInt2 = 0;
			}
			type2 = typecheck_expr(myAstNodePtr2->children[1]);
			if (type2->kind != INT) {
				yyerror("ERROR: Improper ASSI_EXP");
				typeInt2 = 0;
			}
			return (type2);
			break;
		case CALL_EXP:
			ANPparmeters = myAstNodePtr2->children[0];
			if (myAstNodePtr2->nSymbolPtr == NULL) {
				CallExpElement = symLookup(myAstNodePtr2->fname);
				if (CallExpElement == NULL) {
					yyerror("ERROR: Improper CALL_EXP");
				}
			}
			else {
				CallExpElement = myAstNodePtr2->nSymbolPtr;
			}
			if (CallExpElement) {
				type1 = CallExpElement->stype->function;
				type2 = type1->function;
			}
			if (ANPparmeters == NULL && type2) {
				yyerror("ERROR: Improper CALL_EXP");
			}
			if (type2 == NULL && ANPparmeters) {
				yyerror("ERROR: Improper CALL_EXP");
				typeInt2 = 0;
			}
			while (ANPparmeters) {
				type1 = typecheck_expr(ANPparmeters);
				if (type2->kind != type1->kind) {
					yyerror("ERROR: Improper CALL_EXP");
					typeInt2 = 0;
				}
				ANPparmeters = ANPparmeters->sibling;
				type2 = type2->function;
				if (type2 == NULL && ANPparmeters) {
					yyerror("ERROR: Improper CALL_EXP");
					typeInt2 = 0;
					break;
				}
			}
			if (type2) {
				yyerror("ERROR: Improper CALL_EXP");
			}
			type1 = CallExpElement->stype->function;
			return type1;
			break;
		case ARRAY_EXP:
			type1 = typecheck_expr(myAstNodePtr2->children[0]);
			if (type1->kind != INT) {
				yyerror("ERROR: Improper ARRAY_EXP");
			}
			if (myAstNodePtr2->nSymbolPtr->stype->kind != ARRAY) {
				yyerror("ERROR: Improper ARRAY_EXP");
			}
			return new_type(INT);
			break;
	}
}//Done








