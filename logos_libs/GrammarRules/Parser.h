/*
This file is part of OpenLogos/LogOSMaTrans.  Copyright (C) 2005 Globalware AG

OpenLogos/LogOSMaTrans has two licensing options:

The Commercial License, which allows you to provide commercial software
licenses to your customers or distribute Logos MT based applications or to use
LogOSMaTran for commercial purposes. This is for organizations who do not want
to comply with the GNU General Public License (GPL) in releasing the source
code for their applications as open source / free software.

The Open Source License allows you to offer your software under an open source
/ free software license to all who wish to use, modify, and distribute it
freely. The Open Source License allows you to use the software at no charge
under the condition that if you use OpenLogos/LogOSMaTran in an application you
redistribute, the complete source code for your application must be available
and freely redistributable under reasonable conditions. GlobalWare AG bases its
interpretation of the GPL on the Free Software Foundation's Frequently Asked
Questions.

OpenLogos is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the License conditions along with this
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
// Parser.h: interface for the CParser class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_PARSER_H__47ED7A03_F950_11D1_86DB_00104B22CF15__INCLUDED_)
#define AFX_PARSER_H__47ED7A03_F950_11D1_86DB_00104B22CF15__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

struct CSp { int wc; int ty; int fc; };

#define YYTYPE char
	struct yysvf { 
		struct yywork *yystoff;
		struct yysvf *yyother;
		int *yystops; };
	struct yywork { YYTYPE verify, advance; };

#ifndef YYMAXDEPTH
	#define YYMAXDEPTH 150
#endif

#ifndef YYSTYPE
	#define YYSTYPE int
#endif

# define YYLMAX 2048 // was BUFSIZ

class AFX_EXT_CLASS CParser
{
public:
	CParser();
	virtual ~CParser();
	CLgsRule* ParseRule(LPCTSTR str, bool isResRule);
	LPCTSTR GetErrorDescription();

private:
	#define CTagset LgsList(int)

	CLgsRule* m_rule;
	bool m_isResRule;

	void SyntaxError(LPCTSTR errmsg, int lineNo);

	CLgsRule* CreateRule(LPCTSTR comment,
		LgsList(CSp*)* splist, LgsList(CTagset*)* tagsetList, LgsList(int)* vtrList);

	CSp* CreateSp(int wc, int ty, int fc);
	LgsList(CSp*)* CreateSpList(CSp* sp);
	LgsList(CSp*)* AddSp(CSp* sp, LgsList(CSp*)* spList);

	LgsList(CTagset*)* CreateTagsetList();
	LgsList(CTagset*)* AddTagset(CTagset* tagset, LgsList(CTagset*)* tagsetList);
	CTagset* CreateTagset(int num, LgsList(int)* numList);

	LgsList(int)* CreateNumList();
	LgsList(int)* AddNum(int num, LgsList(int)* numList);

	LgsString m_errStr;
	LgsString m_str;
	int m_ix;
	int m_len;

	// temporary data
	LgsStack(CSp*) m_tmpSpElems; // one or more sp elems
	LgsList(CSp*)* m_tmpSpList; // there is only one splist per rule
	LgsList(CTagset*)* m_tmpTagsetList; // there is only one tagset list per rule
	LgsStack(LgsList(int)*) m_tmpNumLists; // one or more numlists
	void Cleanup(bool bDeleteContents);

	// rule scanner stuff
	static struct yywork yycrank[];
	static int yyvstop[];
	static struct yysvf yysvec[];
	static char yymatch[];
	static char yyextra[];

	int yyleng;
	int yymorfg;
	int yytchar;
	int yylineno;
	char yytext[YYLMAX];
	struct yysvf* yylstate[YYLMAX], **yylsp, **yyolsp;
	char yysbuf[YYLMAX];
	char* yysptr;
	int* yyfnd;
	struct yysvf* yyestate;
	int yyprevious;
	int yylval;
	struct yywork *yytop;
	struct yysvf *yybgin;

	void output(char c);
	int gc();
	int yylex();
	int yylook();

	LgsString m_comment;

	// parser stuff
	int yychar;			// current input token number
	int yyerrflag;			// error recovery flag
	//YYSTYPE yylval, yyval;
	YYSTYPE yyval;
	int yynerrs;			// number of errors

	static int yyexca[];
	static int yyact[];
	static int yypact[];
	static int yypgo[];
	static int yyr1[];
	static int yyr2[];
	static int yychk[];
	static int yydef[];
	int yyparse();
};

#endif // !defined(AFX_PARSER_H__47ED7A03_F950_11D1_86DB_00104B22CF15__INCLUDED_)
