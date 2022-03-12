//////////////////////////////////////////////////////////////////////
//
//    Asl - Another simple language (grammar)
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

grammar Asl;

//////////////////////////////////////////////////
/// Parser Rules
//////////////////////////////////////////////////

// A program is a list of functions
program : function+ EOF
        ;

// A function has a name, a list of parameters and a list of statements
function
        : FUNC ID '(' paramsDef? ')' retType? declarations statements ENDFUNC 
        ;

retType 
        : (':' type)
        ;

paramsDef
        :  parameter (',' parameter)*
        ;

parameter
		: (ID ':' type)
		;

exprList
        : expr (',' expr)*
        ;

funcCall
        : ident '(' exprList? ')'
        ;

declarations
        : (variable_decl)*
        ;

variable_decl
        : VAR multid ':' type
        ;

multid
        : ID (',' ID)* ;

type    
        : INT
		| BOOL
		| FLOAT
		| CHAR
        | ARRAY '[' INTVAL ']' 'of' type  
        ;

statements
        : (statement)*
        ;

// The different types of instructions
statement
          // Assignment
        : funcCall ';'                              # funcCallStmt
        | left_expr ASSIGN expr ';'           		# assignStmt
          // if-then-else statement (else is optional)
        | IF expr THEN statements (ELSE statements)? ENDIF       		# ifStmt
        // A function/procedure call has a list of arguments in parenthesis (possibly empty)
        | WHILE expr DO statements ENDWHILE     	# whileStmt
        | ident '(' ')' ';'                   		# procCall
          // Read a variable
        | READ left_expr ';'                  		# readStmt
          // Write an expression
        | WRITE expr ';'                      		# writeExpr
          // Write a string
        | WRITE STRING ';'                    		# writeString
        | RETURN expr? ';'                     		# return
        ;

// Grammar for left expressions (l-values in C++)
left_expr
        : (ident|ident '[' expr ']')
        ;

// Grammar for expressions with boolean, relational and aritmetic operators
expr    : '(' expr ')'                        		# parenthesis
        | op=(PLUS|MINUS|NOT) expr                  # unary
        | expr op=(MUL|DIV|MOD) expr              	# arithmetic
        | expr op=(PLUS|MINUS) expr          		# arithmetic
        | expr op=(EQUAL|NEQ|GT|GE|LT|LE) expr      # relational
        | expr op=(AND|OR) expr               		# boolean
        | (INTVAL|FLOATVAL|CHARVAL|BOOLVAL)         # value
        | (ident|ident '[' expr ']'|funcCall)       # exprIdent
        ;

// Identifiers
ident   : ID
        ;

//////////////////////////////////////////////////
/// Lexer Rules
//////////////////////////////////////////////////

ASSIGN  	: '=' ;

EQUAL		: '==' ;
NEQ         : '!=' ;
GT          : '>' ;
GE          : '>=' ;
LT          : '<' ;
LE          : '<=' ;

PLUS        : '+' ;
MINUS       : '-' ;
MUL         : '*' ;
DIV         : '/' ;
MOD         : '%' ;

NOT         : 'not' ;
AND     	: 'and' ;
OR			: 'or' ;

VAR       	: 'var' ;
INT       	: 'int' ;
BOOL       	: 'bool' ;
FLOAT       : 'float' ;
CHAR       	: 'char' ;
ARRAY   	: 'array' ;

IF        	: 'if' ;
THEN      	: 'then' ;
ELSE      	: 'else' ;
ENDIF     	: 'endif' ;
WHILE   	: 'while' ;
DO      	: 'do' ;
ENDWHILE    : 'endwhile';
FUNC      	: 'func' ;
ENDFUNC   	: 'endfunc' ;
RETURN  	: 'return' ;

READ      	: 'read' ;
WRITE     	: 'write' ;

fragment
DIGIT   	: ('0'..'9') ;
INTVAL      : DIGIT+ ;
EXP         : 'e' ('+'|'-') DIGIT+ ;
FLOATVAL    : (DIGIT* '.' DIGIT+ EXP? | DIGIT+ '.' DIGIT* | DIGIT+ '.'? DIGIT* EXP) ;
CHARVAL     : '\'' ( ESC_SEQ | ~('\\'|'"') )? '\'' ;
BOOLVAL     : ('true' | 'false') ; 

// Strings (in quotes) with escape sequences
STRING    	: '"' ( ESC_SEQ | ~('\\'|'"') )* '"' ;

fragment
ESC_SEQ   	: '\\' ('b'|'t'|'n'|'f'|'r'|'"'|'\''|'\\') ;

ID      	: ('a'..'z'|'A'..'Z') ('a'..'z'|'A'..'Z'|'_'|'0'..'9')* ;

// Comments (inline C++-style)
COMMENT   	: '//' ~('\n'|'\r')* '\r'? '\n' -> skip ;

// White spaces
WS        	: (' '|'\t'|'\r'|'\n')+ -> skip ;
// Alternative description
// WS        : [ \t\r\n]+ -> skip ;
