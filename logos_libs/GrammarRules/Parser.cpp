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
program. If not, write to Globalware AG, Hospitalstraﬂe 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
// Parser.cpp: implementation of the CParser class.
//
//////////////////////////////////////////////////////////////////////

#ifdef _MSC_VER
#include <logos_libs/GrammarRules/stdafx.h>
#endif
#include <logos_libs/GrammarRules/LgsLanguage.h>
#include <logos_libs/GrammarRules/LgsTypeStatement.h>
#include <logos_libs/GrammarRules/LgsSpElement.h>
#include <logos_libs/GrammarRules/LgsVtrStatement.h>
#include <logos_libs/GrammarRules/LgsSortKeyTemplate.h>
#include <logos_libs/GrammarRules/ExtRec.h>
#include <logos_libs/GrammarRules/LgsRule.h>
#include <logos_libs/GrammarRules/LgsTable.h>
#include <logos_libs/GrammarRules/Parser.h>

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

# define YYNEWLINE 10

CParser::CParser()
{
	m_tmpTagsetList = NULL;
	m_tmpSpList = NULL;
	m_isResRule = false;
	m_ix = 0;
	m_len = 0;
	m_rule = NULL;
	yylineno = 1;
}

CParser::~CParser()
{
}

CLgsRule* CParser::ParseRule(LPCTSTR str, bool isResRule)
{
	CLgsRule* result = NULL;

	m_str = str;
	m_len = m_str.length();
	m_ix = 0;
	m_isResRule = isResRule;

	// init scanner variables
	yyleng = 0;
	int yymorfg = 0;
	int yytchar = 0;
	yyfnd = NULL;
	yyestate = NULL;
	yylval = 0;

	yyprevious = YYNEWLINE;
	yylineno = 1;
	yysptr = yysbuf;
	yytop = yycrank+136;
	yybgin = yysvec+1;

	if ( 0 == yyparse() )
	{
		result = m_rule; // deleted by the caller
		m_rule = NULL;
	}

	return result;
}

# define MAX_NUMS_IN_BUFFER 20
//# define MAX_NUMS_IN_BUFFER 21

static inline bool IsTermResSwitch( int num )
{
	return 6010 == num || 6012 == num || 6013 == num || 6014 == num || 6050 == num;
}

static inline bool IsFixedResSwitch( int num )
{
	return 6025 == num || 6211 == num || 6212 == num || 6213 == num || 6214 == num;
}

static inline bool IsVarResSwitch( int num )
{
	return 1000 == num || 2000 == num;
}

static inline bool IsSpecResSwitch( int num )
{
	return 6055 == num;
}

static inline bool IsResSwitch( int num )
{
	return IsTermResSwitch(num) ||
			IsFixedResSwitch(num) ||
			IsVarResSwitch(num) ||
			IsSpecResSwitch(num);
}

static inline int ExtractResSwitch( short* nums, int start, int count)
{
	int i = 0;

	if ( 0 == count )
		return 0;

	if ( !IsResSwitch(nums[start]) )
		return 1; // regular tagset element

	if ( IsTermResSwitch(nums[start]) )
	{
		// 9000 terminates the switch
		for ( i = 1; i < count; i++ )
		{
			if ( 9000 == nums[start+i] )
			{
				i++;
				break;
			}
		}
	}
	else if ( IsFixedResSwitch(nums[start]) )
	{
		// fixed length switch
		i = 2;
	}
	else if ( IsVarResSwitch(nums[start]) )
	{
		// variable length switch
		for ( i = 3; i < count; i += 2 )
		{
			if ( IsResSwitch(nums[start+i]) )
				break;
		}
	}
	else if ( IsSpecResSwitch(nums[start]) )
	{
		// 6055 switch
		if ( count < 5 )
			i = 4;
		else
		{
			i = 3 + nums[start+2] % 10;
			if ( i > count )
			{
				TRACE("%s", "Invalidly formed 6055 switch\n");
				i = count;
			}
		}
	}

	return i;
}

static short* BuildResHints(short* nums, int numNums, char* hints, int* newNumTags)
{
	// save effort if all nums fit in one buffer
	if ( numNums <= MAX_NUMS_IN_BUFFER )
	{
		hints[0] = numNums;
		hints[1] = 0;
		return nums;
	}

	// array needs splitting
	short* newnums = new short[2*numNums];

	int numsInLine = 1;
	newnums[0] = 8888;
	int swLen = 0;
        int pos, i, j, hIx;
	for ( pos = 0, i = 0, j = 1, hIx = 0;
		   pos < numNums;
			pos += swLen )
	{
		swLen = ExtractResSwitch(nums, pos, numNums - pos);

		if ( numsInLine + swLen > MAX_NUMS_IN_BUFFER )
		{
			hints[hIx++] = numsInLine;
			numsInLine = 0;
			if ( numNums - pos > MAX_NUMS_IN_BUFFER )
			{
				// the remaining array needs splitting 
				newnums[j++] = 8888;
				numsInLine++;
			}
		}

		// add switch to the current line
		for ( int k = 0; k < swLen; k++ )
			newnums[j++] = nums[i++];
		numsInLine += swLen;
	}

	hints[hIx++] = numsInLine;
	hints[hIx] = 0;
	*newNumTags = j;

	delete nums;
	return newnums;
}


// tran hints stuff
static bool IsTranSwitch( int num, int* swOff, int* swInc )
{
	if ( 7777 == num ||
		 5555 == num ||
		 1111 == num ||
		 2222 == num )
	{
		if (swOff) *swOff = 1;
		if (swInc) *swInc = 0;
		return true;
	}
	else if ( 1000 == num || 4100 == num || (num > 1080 && num < 1090) ||
			  2000 == num || 4200 == num ||
			  4000 == num || 6050 == num || (num > 6080 && num < 6090) )
	{
		// swsw (xxxx xxxx) ...
		if (swOff) *swOff = 3;
		if (swInc) *swInc = 2;
		return true;
	}
	else if ( 6010 == num || 6012 == num || 6013 == num || 6014 == num )
	{
		// swsw relp xxxx ...
		if (swOff) *swOff = 3;
		if (swInc) *swInc = 1;
		return true;
	}
	else if ( 6055 == num )
	{
		// swsw relp abcd xxxx ...
		if (swOff) *swOff = 4;
		if (swInc) *swInc = 1;
		return true;
	}
	else if ( 8000 == num || 6070 == num || 6071 == num )
	{
		// swsw (xxxx xxxx xxxx) ...
		if (swOff) *swOff = 4;
		if (swInc) *swInc = 3;
		return true;
	}

	if (swOff) *swOff = 1;
	if (swInc) *swInc = 1;
	return false;
}

// details on tagsets format are in tagsets.txt file
static inline int ExtractTranSwitch(short* nums, int start, int count, int *splitArray = NULL)
{
	if ( 0 == count )
		return 0;

   if( splitArray != NULL )
      splitArray[start] = 2; // 2 means beginning of tagset

	int swOff, swInc;
	if ( !IsTranSwitch(nums[start], &swOff, &swInc) )
		return 1;

   if( swInc == 0 )
      return swOff;

	for ( int i = swOff; i < count; i += swInc )
	{
		if ( IsTranSwitch(nums[start+i], NULL, NULL) )
			return i;
      if( splitArray != NULL )
         splitArray[start+i] = 1; // 1 means possible split position
	}

	return count;
}

static short* BuildTranHints(short* nums, int numNums, char* hints, int* newNumTags)
{
	// save effort if all nums fit in one buffer
	if ( numNums <= MAX_NUMS_IN_BUFFER )
	{
		hints[0] = numNums;
		hints[1] = 0;
		return nums;
	}

	// array needs splitting
	short* newnums = new short[2*numNums];

   // Create split array to hold possible positions for splitting
   int *splitArray = new int[numNums];
   ZeroMemory(splitArray, numNums * sizeof(int));
   int swLen = 0;
   int pos;
   for( pos = 0; pos < numNums;	pos+= swLen )
   {
      swLen = ExtractTranSwitch(nums, pos, numNums - pos, splitArray);
   }

   // Do splitting
   pos = 0;
   int i = 0, j = 0, hIx = 0;
   while( pos < numNums )
   {
      // How many can be copied?
      int numsInLine = numNums - pos;
      if( numsInLine > MAX_NUMS_IN_BUFFER )
      {
         for( numsInLine = MAX_NUMS_IN_BUFFER - 1; numsInLine > 0; numsInLine-- )
         {
            if( splitArray[pos + numsInLine] > 0 )
            {
               break;
            }
         }
      }
      // Add to the current line
      for( int k = 0; k < numsInLine; k++ )
      {
         newnums[j++] = nums[i++];
      }
      pos+= numsInLine;
      if( pos < numNums )
      {
         newnums[j++] = 8888;
         numsInLine++;
      }
      hints[hIx++] = numsInLine;
   }

	hints[hIx] = 0;
	*newNumTags = j;

   delete[] splitArray;
   delete nums;
	return newnums;
}

LPCTSTR CParser::GetErrorDescription()
{
	return NULL;
}

void CParser::SyntaxError(LPCTSTR errmsg, int lineNo)
{
}

CLgsRule* CParser::CreateRule(LPCTSTR comment,
	LgsList(CSp*)* splist, LgsList(CTagset*)* tagsetList, LgsList(int)* vtrList)
{
	CLgsRule* result = new CLgsRule();
	bool errFlag = false;

	result -> Description(comment);

	// vtrs
	CLgsVtrStatement** vtrStatements = new CLgsVtrStatement* [1];
	int i = 0;
	short* nums = NULL;
	if ( vtrList->size() > 0 )
	{
		nums = new short[vtrList->size()];
		for ( i = 0; !vtrList->empty(); i++ )
		{
			nums[i] = vtrList -> front();
			vtrList -> pop_front();
		}
	}

	delete vtrList;
	vtrStatements[0] = new CLgsVtrStatement(nums, i);
	result -> VtrStatements(vtrStatements, 1);

	// sps
	int numSps = splist->size();
    CLgsSpElement** spElements = new CLgsSpElement* [numSps];

	for ( i = 0; !splist->empty(); i++ )
	{
		CSp* sp = splist -> front();
		short* tss = NULL;
		int numTags = 0;

		char hh[256];
		char* hints = NULL;

		if ( -2 == sp -> ty )
		{
			if ( !tagsetList->empty() )
			{
				CTagset* tagset = tagsetList -> front();

				numTags = tagset -> size();
				tss = new short[numTags];
				for ( int j = 0; !tagset -> empty(); j++ )
				{
					tss[j] = tagset -> front();
					tagset -> pop_front();
				}
				tagsetList -> pop_front();
				delete tagset;


				// build hints
				if ( m_isResRule )
					tss = BuildResHints(tss, numTags, hh, &numTags);
				else
					tss = BuildTranHints(tss, numTags, hh, &numTags);

				hints = hh;
                                int k;
				for (k = 0; hints[k]; k++ )
				{
					TRACE("%d ", hints[k]);
				}
				TRACE("%s","\n");
				for ( k = 0; k < numTags; k++ )
				{
					TRACE("%d ", tss[k]);
				}
				TRACE("%s","\n");
			}
			else
			{
				// there is not enough tagsets for this sp line
				//
				// use some arbitrary values,
				// the rule will be deleted anyway before this function returns
				errFlag = true;
				numTags = 1;
				tss = new short[1];
				tss[0] = 555;
			}
		}
		else
		{
			numTags = 1;
            tss = new short[1];
            tss[0] = sp->ty;
		}
		spElements[i] = new CLgsSpElement(sp->wc, sp->fc, tss, numTags, hints);

		splist -> pop_front();
		delete sp;
	}
	delete splist;
	result -> SpStatements(spElements, numSps);
	result -> Level(numSps);

	if ( !tagsetList->empty() )
		errFlag = true; // too many tagsets for this sp line

	while ( !tagsetList->empty() )
	{
		delete tagsetList->front();
		tagsetList->pop_front();
	}
	delete tagsetList;

	if ( errFlag )
	{
		delete result;
		result = NULL;
	}

	return result;
}

CSp* CParser::CreateSp(int wc, int ty, int fc)
{
	CSp* sp = new CSp();
	sp -> wc = wc;
	sp -> ty = ty;
	sp -> fc = fc;
	m_tmpSpElems.push(sp);
	return sp;
}

LgsList(CSp*)* CParser::CreateSpList(CSp* sp)
{
	LgsList(CSp*)* spList = new LgsList(CSp*)();
	spList -> push_front(sp);
	m_tmpSpList = spList;
	return spList;
}

LgsList(CSp*)* CParser::AddSp(CSp* sp, LgsList(CSp*)* spList)
{
	spList -> push_front(sp);
	return spList;
}

LgsList(CTagset*)* CParser::CreateTagsetList()
{
	LgsList(CTagset*)* result = new LgsList(CTagset*)();
	m_tmpTagsetList = result;
	return result;
}

LgsList(CTagset*)* CParser::AddTagset(CTagset* tagset, LgsList(CTagset*)* tagsetList)
{
	tagsetList -> push_front(tagset);
	return tagsetList;
}

CTagset* CParser::CreateTagset(int num, LgsList(int)* numList)
{
	numList -> push_front(num);
	return numList;
}

LgsList(int)* CParser::CreateNumList()
{
	LgsList(int)* result = new LgsList(int)();
	m_tmpNumLists.push(result);
	return result;
}

LgsList(int)* CParser::AddNum(int num, LgsList(int)* numList)
{
	numList -> push_front(num);
	return numList;
}

void CParser::Cleanup(bool bDeleteContents)
{
	if ( m_tmpSpList )
	{
		if ( bDeleteContents ) delete m_tmpSpList;
		m_tmpSpList = NULL;
	}

	while( !m_tmpSpElems.empty() )
	{
		if ( bDeleteContents ) delete m_tmpSpElems.top();
		m_tmpSpElems.pop();
	}

	if ( m_tmpTagsetList )
	{
		if ( bDeleteContents ) delete m_tmpTagsetList;
		m_tmpTagsetList = NULL;
	}

	while( !m_tmpNumLists.empty() )
	{
		if ( bDeleteContents ) delete m_tmpNumLists.top();
		m_tmpNumLists.pop();
	}
}

// scanner generated by unix lex ===============================================
#define EOS (-1)

int CParser::gc()
{
	int result = EOS;
	if ( m_ix < m_len )
		result = m_str[m_ix++];
	return result;
}

# define YYLERR yysvec

void CParser::output( char c )
{
	m_errStr += c;
}

# define input() (((yytchar=yysptr>yysbuf?(*--yysptr):gc())==10?(yylineno++,yytchar):yytchar)==EOS?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define yywrap() (1)

// stuff from y.tab.h
# define L_BRACKET 257
# define R_BRACKET 258
# define L_BRACE 259
# define R_BRACE 260
# define COMMENT 261
# define NUM 262
# define ERR 263
# define TERM 264


int CParser::yylex()
{
	int nstr;

	while((nstr = yylook()) >= 0)
	switch(nstr){
	case 0:
	if(yywrap()) return(0); break;
	case 1:
	{
		m_comment.erase();
		m_comment = yytext;
		yylval = (int)m_comment.c_str();
		return COMMENT;
		}
	break;
	case 2:
	;
	break;
	case 3:
	return L_BRACKET;
	break;
	case 4:
	return R_BRACKET;
	break;
	case 5:
	return L_BRACE;
	break;
	case 6:
	return R_BRACE;
	break;
	case 7:
	return TERM;
	break;
	case 8:
	{
			int result = sscanf (yytext, "%d", &yylval);
			return NUM;
		}
	break;
	case 9:
	{
			int result = sscanf (yytext, "%d", &yylval);
			return NUM;
		}
	break;
	case 10:
	{
			return ERR;
		}
	break;
	case -1:
	break;
	default:
		TRACE("bad switch yylook %d\n", nstr);
	}
	
	return(0);
}

int CParser::yyvstop[] = {
0,

10,
0,

2,
10,
0,

2,
0,

7,
10,
0,

3,
10,
0,

4,
10,
0,

10,
-1,
0,

10,
0,

9,
10,
0,

5,
10,
0,

6,
10,
0,

-1,
0,

1,
0,

8,
0,

9,
0,
0};

struct yywork CParser::yycrank[] = {
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	9,14,	
0,0,	0,0,	1,4,	1,5,	
4,5,	4,5,	0,0,	9,14,	
9,15,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	4,5,	
0,0,	1,6,	0,0,	0,0,	
0,0,	1,7,	1,8,	1,9,	
0,0,	0,0,	1,10,	2,6,	
0,0,	1,11,	0,0,	2,7,	
2,8,	2,9,	9,14,	0,0,	
2,10,	10,16,	10,16,	10,16,	
10,16,	10,16,	10,16,	10,16,	
10,16,	10,16,	10,16,	11,17,	
11,17,	11,17,	11,17,	11,17,	
11,17,	11,17,	11,17,	11,17,	
11,17,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
1,12,	0,0,	1,13,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	2,12,	0,0,	
2,13,	0,0,	0,0,	0,0,	
0,0};

struct yysvf CParser::yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-11,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+3,	0,		yyvstop+3,
yycrank+0,	yysvec+4,	yyvstop+6,
yycrank+0,	0,		yyvstop+8,
yycrank+0,	0,		yyvstop+11,
yycrank+0,	0,		yyvstop+14,
yycrank+-6,	0,		yyvstop+17,
yycrank+9,	0,		yyvstop+20,
yycrank+19,	0,		yyvstop+22,
yycrank+0,	0,		yyvstop+25,
yycrank+0,	0,		yyvstop+28,
yycrank+0,	yysvec+9,	yyvstop+31,
yycrank+0,	0,		yyvstop+33,
yycrank+0,	yysvec+10,	yyvstop+35,
yycrank+0,	yysvec+11,	yyvstop+37,
0,	0,	0};

char CParser::yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
0};

char CParser::yyextra[] = {
0,1,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};

static inline int yyback( int* p, int m)
{
	if ( p==0 ) return 0;
	while ( *p )
	{
		if ( *p++ == m )
			return 1;
	}
	return 0;
}

int CParser::yylook()
{
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
	char *yylastch;

	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  // may not be any transitions
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
			*yylastch++ = yych = input();
			yyfirst=0;
		tryagain:
			yyr = yyt;
			if ( (int)yyt > (int)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	// error transitions
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
			else if((int)yyt < (int)yycrank) {		// r < yycrank
				yyt = yyr = yycrank+(yycrank-yyt);
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	// error transitions
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				yyt = yyr + yymatch[yych];
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	// error transition
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
				goto tryagain;
				}
			else
				{unput(*--yylastch);break;}
		contin:
			;
			}
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		// must backup
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = *yylastch;
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0 )
			{
			yysptr=yysbuf;
			return(0);
			}
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		yylastch=yytext;
		}
}

// parser generated by unix yacc ===============================================
//extern char *malloc(), *realloc();

//#include <stdio.h>

#define yyerror(errmsg) \
    SyntaxError(errmsg, yylineno);

#define YYERRCODE 256

int CParser::yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
	};

# define YYNPROD 11
# define YYLAST 28
int CParser::yyact[]={

    17,    20,    13,    16,    15,    11,     3,    21,     9,    22,
     6,    12,     7,     4,     8,     5,     2,     1,     0,    10,
     0,    14,     0,     0,     0,    18,     0,    19 };

int CParser::yypact[]={

  -255, -1000, -1000,  -247,  -251,  -247,  -257,  -260,  -251,  -258,
 -1000,  -259,  -264,  -260, -1000,  -260,  -261, -1000, -1000,  -253,
  -249, -1000, -1000 };

int CParser::yypgo[]={

     0,    17,    16,    13,    12,    11,    15,    14 };
int CParser::yyr1[]={

     0,     1,     2,     6,     3,     3,     4,     4,     7,     5,
     5 };
int CParser::yyr2[]={

     0,     3,    11,    11,     3,     5,     1,     5,     9,     1,
     5 };
int CParser::yychk[]={

 -1000,    -1,    -2,   261,    -3,    -6,   257,    -4,    -7,   259,
    -3,   262,    -5,   262,    -4,   262,   262,   264,    -5,    -5,
   262,   260,   258 };
int CParser::yydef[]={

     0,    -2,     1,     0,     6,     4,     0,     9,     6,     0,
     5,     0,     0,     9,     7,     9,     0,     2,    10,     0,
     0,     8,     3 };

#define YYACCEPT	{ Cleanup(false); free(yys); free(yyv); return(0); }
#define YYABORT		{ Cleanup(true); free(yys); free(yyv); return(1); }

// driver internal defines
#define YYFLAG		(-1000)

// yyparse - return 0 if worked, 1 if syntax error not recovered from
int CParser::yyparse()
{
	// static variables used by the parser
	//   static storage specifier has been removed from the following six defs
	YYSTYPE *yyv;			// value stack
	int *yys;			// state stack
	YYSTYPE *yypv;			// top of value stack
	int *yyps;			// top of state stack
	int yystate;			// current state
	int yytmp;			// extra var (lasts between blocks)


	register YYSTYPE *yypvt;	// top of value stack for $vars
	unsigned yymaxdepth = YYMAXDEPTH;

	// Initialize externals - yyparse may be called more than once
	yyv = (YYSTYPE*)malloc(yymaxdepth*sizeof(YYSTYPE));
	yys = (int*)malloc(yymaxdepth*sizeof(int));
	if (!yyv || !yys)
	{
		yyerror( "out of memory" );
		return(1);
	}
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

	goto yystack;
	{
		register YYSTYPE *yy_pv;	// top of value stack
		register int *yy_ps;		// top of state stack
		register int yy_state;		// current state
		register int  yy_n;		// internal state number info

		// get globals into registers.
		// branch to here only if YYBACKUP was called.
	//yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		// get globals into registers.
		// either we just started, or we just finished a reduction
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		// top of for (;;) loop while no reductions done
	yy_stack:
		// put a state and value onto the stacks
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	// room on stack?
		{
			// reallocate and recover.  Note that pointers
			// have to be reset, or bad things will happen
			int yyps_index = (yy_ps - yys);
			int yypv_index = (yy_pv - yyv);
			int yypvt_index = (yypvt - yyv);
			yymaxdepth += YYMAXDEPTH;
			yyv = (YYSTYPE*)realloc((char*)yyv,
				yymaxdepth * sizeof(YYSTYPE));
			yys = (int*)realloc((char*)yys,
				yymaxdepth * sizeof(int));
			if (!yyv || !yys)
			{
				yyerror( "yacc stack overflow" );
				return(1);
			}
			yy_ps = yys + yyps_index;
			yy_pv = yyv + yypv_index;
			yypvt = yyv + yypvt_index;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		// we have a new state - find out what to do
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		// simple state
		if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
			yychar = 0;		// reached EOS
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	// valid shift
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
			if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
				yychar = 0;		// reached EOS/
			// look through exception table
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		// check for syntax error
		if ( yy_n == 0 )	// have an error
		{
			// no worry about speed here!
			switch ( yyerrflag )
			{
			case 0:		// new error
				yyerror( "syntax error" );
				goto skip_init;
			//yyerrlab:
				// get globals into registers.
				// we have a user generated syntax type error
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
				yynerrs++;
			skip_init:
			case 1:
			case 2:		// incompletely recovered error
					// try again...
				yyerrflag = 3;
				// find state where "error" is a legal
				// shift action
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						// simulate shift of "error"
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					// current state has no shift on
					// "error", pop stack
					yy_ps--;
					yy_pv--;
				}
				// there is no state on stack with "error" as
				// a valid shift.  give up.
				YYABORT;
			case 3:		// no shift yet; eat a token
				if ( yychar == 0 )	// reached EOS. quit
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}// end if ( yy_n == 0 )
		// reduction by production yy_n
		// put stack tops, etc. so things right after switch
		yytmp = yy_n;			// value to switch over
		yypvt = yy_pv;			// $vars top of value stack
		// Look in goto table for next state
		// Sorry about using yy_state here as temporary
		// register variable, but why not, if it works...
		// If yyr2[ yy_n ] doesn't have the low order bit
		// set, then there is no action to be done for
		// this reduction.  So, no saving & unsaving of
		// registers done.  The only difference between the
		// code just after the if and the body of the if is
		// the goto yy_stack in the body.  This way the test
		// can be made before the choice of what to do is needed.
		{
			// length of production doubled with extra bit
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	// $$ = $1
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	// $$ = $1
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}

		// save until reenter driver code
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}

	// code supplied by user is placed in this switch
	switch( yytmp )
	{
case 1:
{
	VERIFY( !m_rule );
	m_rule = (CLgsRule*)yypvt[-0];
	} break;
case 2:
{
		yyval = (int)CreateRule(
						(LPCTSTR) yypvt[-4],
						(LgsList(CSp*)*) yypvt[-3],
						(LgsList(CTagset*)*) yypvt[-2],
						(LgsList(int)*) yypvt[-1]);
	} break;
case 3:
{
		yyval = (int)CreateSp(yypvt[-3], yypvt[-2], yypvt[-1]);
	} break;
case 4:
{
		yyval = (int)CreateSpList( (CSp*) yypvt[-0] );
	} break;
case 5:
{
		yyval = (int)AddSp( (CSp*)yypvt[-1], (LgsList(CSp*)*)yypvt[-0] );
	} break;
case 6:
{
		yyval = (int)CreateTagsetList();
	} break;
case 7:
{
		yyval = (int)AddTagset( (CTagset*)yypvt[-1], (LgsList(CTagset*)*)yypvt[-0] );
	} break;
case 8:
{
		yyval = (int)CreateTagset( yypvt[-2], (LgsList(int)*)yypvt[-1] );
	} break;
case 9:
{
		yyval = (int)CreateNumList();
	} break;
case 10:
{
		yyval = (int)AddNum(yypvt[-1], (LgsList(int)*)yypvt[-0]);
	} break;
	}
	goto yystack;		// reset registers in driver code
}

