%{
#include <stdio.h>

#define yyerror(_c_) \
    SyntaxError(_c_, yylineno);

extern FILE* yyout;
int yylineno;

%}

%start start
%token L_BRACKET R_BRACKET L_BRACE R_BRACE
 COMMENT NUM
 ERR TERM
 
%%	/* rules */

%{
%}

start	: rule
		{ /* m_rule = (CLgsRule*)$1;*/ }
	;

rule : COMMENT splist tagsetlist numlist TERM
	{
		$$ = CreateRule($1, $2, $3, $4);
	}
	;

sp	: L_BRACKET NUM NUM NUM R_BRACKET
	{
		$$ = CreateSp($2, $3, $4);
	}
	;

splist	: sp
	{
		$$ = CreateSpList($1);
	}
	| sp splist
	{
		$$ = AddSp($1, $2);
	}
	;

tagsetlist	:
	{
		$$ = CreateTagsetList();
	}
	| tagset tagsetlist
	{
		$$ = AddTagset($1, $2);
	}
	;

tagset	: L_BRACE NUM numlist R_BRACE
	{
		$$ = CreateTagset($2, $3);
	}
	;

numlist	:
	{
		$$ = CreateNumList();
	}
	| NUM numlist
	{
		$$ = AddNum($1, $2);
	}
	;
