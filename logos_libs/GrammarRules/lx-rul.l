%{
#include "y.tab.h"

extern int yylval;
%}

%%

\*.*$	{
	yylval = (int)yytext;
	return COMMENT;
	}

[ \t\n]+ ;

\(	return L_BRACKET;
\)	return R_BRACKET;
\{	return L_BRACE;
\}	return R_BRACE;
\$	return TERM;

\-[0-9]+ {
		int result = sscanf (yytext, "%d", &yylval);
		return NUM;
	}


[0-9]+ {
		int result = sscanf (yytext, "%d", &yylval);
		return NUM;
	}

.	{
		return ERR;
	}
