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
// file: RegularExpression.cpp
#if _MSC_VER >= 1100
#define __STDC__ 1
#endif
#include <assert.h>
#include "regularexpression.h"
#include "charutil.h"

#define REGID_UNKNOWN	0
#define REGID_FINISH_218	1
#define REGID_FINISH_220	2
#define REGID_START_1CATCH_ALL	3
#define REGID_START_2CATCH_ALL	4
#define REGID_START_1LOOKUP_NEEDED	5
#define REGID_START_2LOOKUP_NEEDED	6
#define REGID_START_DOTTED_ALPHANUM	7
#define REGID_FINISH_205	8
#define REGID_FINISH_206	9
#define REGID_START_HYPHEN_SEQUENCE	10
#define REGID_START_MULTI_HYPHEN_ALPHANUM	11
#define REGID_START_ORDINAL	12
#define REGID_START_NUMERIC	13
#define REGID_START_TIME	14

// german source related
#define REGID_CONJ0	15
#define REGID_CONJ1	16
#define REGID_FINISH_101	17

#undef REGEX_TIMING

#ifdef REGEX_TIMING

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
typedef struct {
	int ncalls;
	char expr[256];
	char *addr;
	long time;
} regex_time;

static regex_time rtimes[400];
static regex_time_index=0;

static regex_time *find_or_insert_rtime(char *e, char *a) {
	int i;
	for (i=0;i<regex_time_index;i++) {
		if(rtimes[i].addr==a) {
//printf("find_or_insert_rtime HIT: index=%d, addr = %d\n", i, a);
			return &rtimes[i];
		}
	}
	rtimes[regex_time_index].addr = a;
	rtimes[regex_time_index].time = 0;
	rtimes[regex_time_index].ncalls = 0;
	strncpy(rtimes[regex_time_index].expr, e, 256);
//printf("find_or_insert_rtime: index=%d, addr = %d\n", regex_time_index, a);
	regex_time_index++;
	return &rtimes[regex_time_index-1];
}
static regex_time *find_rtime(char *a) {
	int i;
	for (i=0;i<regex_time_index;i++) {
		if(rtimes[i].addr==a) {
//printf("find_rtime HIT: index=%d, addr = %d\n", i, a);
//fflush(stdout);
			return &rtimes[i];
		}
	}
	return NULL;
}
static int rtime_total() {
	int i, s=0;
	for (i=0;i<regex_time_index;i++) {
		s += rtimes[i].time;
	}
	return s;
}
#endif // REGEX_TIMING

// ----------------------------------------------------------
#ifdef DEBUG
void printchar(unsigned char c)
{
    cout << c;
}
#endif

// ----------------------------------------------------------
const LgsString RegularExpression::escapeSymbol = "\\";
const LgsString RegularExpression::leftParen = "(";
const LgsString RegularExpression::rightParen = ")";
//const LgsString RegularExpression::leftBoundary  = "\\b[[({'\"]*";
//const LgsString RegularExpression::rightBoundary =    "[])}'\".?!]*\\b";
const LgsString RegularExpression::leftBoundary = "( |^)[[({'\"]*";
const LgsString RegularExpression::leftElisionBoundary = "( |^|/|-)[[({'\"]*";
//const LgsString RegularExpression::leftContractBoundary = "( |^|--+)[[({'\"]*";
const LgsString RegularExpression::leftContractBoundary = "(\\<|--+)";
const LgsString RegularExpression::rightContractBoundary = "(\\>)";
const LgsString RegularExpression::rightBoundary = "[])}'\".?!,;:]*( |$)";
//const LgsString RegularExpression::rightElisionBoundary = "[])}'\".?!,;:]*([ $(])";
const LgsString RegularExpression::rightElisionBoundary = "[])}'\".?!,;:]*( |$|\\(|@)";

static int leftElisionBoundaryHash[256];
static void initLeftElisionBoundaryHash() {
	memset(leftElisionBoundaryHash, 0, 256);
	char *cp = "( /-)[({'\"";
	while(*cp) {
		int i = *cp++;
		leftElisionBoundaryHash[i] = 1;
	}
}
static int rightElisionBoundaryHash[256];
static void initRightElisionBoundaryHash() {
	memset(rightElisionBoundaryHash, 0, 256);
	char *cp = "])}'\".?!,;: \t\r\n\\(|@";
	while(*cp) {
		rightElisionBoundaryHash[*cp] = 1;
		cp++;
	}
	rightElisionBoundaryHash[0] = 1;
}
static int leftBoundaryHash[256];
static void initLeftBoundaryHash() {
	memset(leftBoundaryHash, 0, 256);
	char *cp = " [({'\"*";
	while(*cp) {
		leftBoundaryHash[*cp] = 1;
		cp++;
	}
}
static int rightBoundaryHash[256];
static void initRightBoundaryHash() {
	memset(rightBoundaryHash, 0, 256);
	char *cp = "])}'\".?!,;:";
	while(*cp) {
		rightBoundaryHash[*cp] = 1;
		cp++;
	}
}
static int frule205Hash[256];
static void initFrule205Hash() {
	memset(frule205Hash, 0, 256);
	unsigned char *cp = (unsigned char *)"aA‡¿‚¬eEÈ…Ë»Í ÎÀiIÓŒoOuU˚€håúHyY";
	while(*cp) {
		frule205Hash[*cp] = 1;
		cp++;
	}
}
static int frule101Hash[256];
static void initFrule101Hash() {
	memset(frule101Hash, 0, 256);
	unsigned char *cp = (unsigned char *)"aeiouhAEIOUH";
	while(*cp) {
		frule101Hash[*cp] = 1;
		cp++;
	}
}

RegularExpression::RegularExpression(const LgsString &expression, Borders borders)
                  :calls_(0),
                   borders_(borders),
                   expression_(expression)
{
	if(expression.find("(d|l|qu|j|m|t|s|n)")!=LgsString::npos) {
		regid = REGID_FINISH_205;
	} else 
		if(expression.find("l(a +)([aA‡¿‚¬eEÈ…Ë»Í ÎÀiIÓŒoOuU˚€håúHyY")!=LgsString::npos) {
		regid = REGID_FINISH_206;
	} else if(expression.compare("(.*')?(‡ +le)(quel)?")==0) {
		regid = REGID_FINISH_218;
	} else 
		if(expression.compare("(.*')?(‡ +les)(quels|quelles)?")==0) {
		regid = REGID_FINISH_220;
	} else 
		if(expression.find("[^[:blank:]]*([@#~$%^&*+_=|£¢:?.!")!=LgsString::npos) {
		regid = REGID_START_1CATCH_ALL;
	} else 
		if(expression.find("[^[:blank:]]*([@#~$%^&*+_=|£¢")!=LgsString::npos) {
		regid = REGID_START_2CATCH_ALL;
	} else
		if(expression.find("([[:alpha:]]*(\\.-?([[:alpha:]]*))+(\\.)?)")!=LgsString::npos) {
		regid = REGID_START_1LOOKUP_NEEDED;
	} else
		if(expression.compare("(([[:alpha:]]\\.)+('s))")==0) {
		regid = REGID_START_2LOOKUP_NEEDED;
	} else
		if(expression.compare("([[:alnum:]]*[[:digit:]]+[[:alnum:]]*)(\\.[[:alnum:]]*[[:digit:]]+[[:alnum:]]*)+(\\.)?")==0) {
		regid = REGID_START_DOTTED_ALPHANUM;
//	} else
//		if(expression.compare("( (--*) )|([^- ](---*)[^- ])|((---*) )|((---*)[^- ])")==0) {
//		regid = REGID_START_HYPHEN_SEQUENCE;
//	} else
//		if(expression.compare("([[:alnum:]]*[[:digit:]][[:alnum:]]*)(-[[:alnum:]]*[[:digit:]][[:alnum:]]*)+")==0) {
//		regid = REGID_START_MULTI_HYPHEN_ALPHANUM;
//	} else
//		if(expression.find("(([[:digit:]]{1,3}(`thou_sep`?[[:digit:]]{3})*)(`ord_suffix`))((-)(,|[[:alpha:]]+))?")!=LgsString::npos) {
// english source thou_sep
//		if(expression.find("(([[:digit:]]{1,3}(\\,?[[:digit:]]{3})*)")!=LgsString::npos) {
//printf("ORDINAL \"%s\"\n", expression.c_str()); fflush(stdout);
//		regid = REGID_START_ORDINAL;
//	} else
//		if(expression.find("([+±-]?)([$£]?)(((([[:digit:]]{1,3}((`thou_sep`?[[:digit:]]{3})+)?)(`dec_sep`([[:digit:]])+)?)|(`dec_sep`([[:digit:]])+))( [[:digit:]]{1,2}/[[:digit:]]+| ?[ºΩæ])?)[ ]?([¢'\"%∞]|∞C|C|∞F|F|mal)?")!=LgsString::npos) {
// english source thou_sep and dec_sep
//		if(expression.find("([+±-]?)([$£]?)(((([[:digit:]]{1,3}((\\,?[[:digit:]]{3})+)?)(\\.([[:digit:]])+)?)|(\\.([[:digit:]])+))( [[:digit:]]{1,2}/[[:digit:]]+| ?[ºΩæ])?)[ ]?([¢'\"%∞]|∞C|C|∞F|F|mal)?")!=LgsString::npos) {
//printf("NUMERIC\n"); fflush(stdout);
//		regid = REGID_START_NUMERIC;
//	} else
//		if(expression.find("([[:digit:]]{1,2})(`time_sep`([[:digit:]]{2}))((`time_sep`([[:digit:]]{2}))(`time_dec_sep`([[:digit:]]+))?)?(`time_suffix`)?")!=LgsString::npos) {
// english source time_sep and time_dec_sep
//		if(expression.find("([[:digit:]]{1,2})(\\:([[:digit:]]{2}))((\\:([[:digit:]]{2}))(\\.([[:digit:]]+))?)?")!=LgsString::npos) {
//printf("TIME\n"); fflush(stdout);
//		regid = REGID_START_TIME;
	} else
// german conjugation, see gerdem/hndepwrdlookup.cpp
		if(expression.find("([[:alpha:]]+(-[[:alpha:]]+)*) +(noch|prep|statt|und|oder|bis|bzw|als auch|ebenso wie|sondern auch|wie auch|und/oder|aber auch|jedoch auch|oder auch|und auch|so wie auch|ebenso wie auch|u.) +(-)([[:alpha:]]+)")!=LgsString::npos) {
			regid = REGID_CONJ0;
	} else 
		if(expression.find("([[:alpha:]]+)(-) +(noch|prep|statt|und|oder|bis|bzw|als auch|ebenso wie|sondern auch|wie auch|und/oder|aber auch|jedoch auch|oder auch|und auch|so wie auch|ebenso wie auch|u.) +([[:alpha:]]+(-[[:alpha:]]+)*)")!=LgsString::npos) {
			regid = REGID_CONJ1;
	} else 
		if(expression.find("aeiouhAEIOUH")!=LgsString::npos) {
			regid = REGID_FINISH_101;
	} else
		regid = REGID_UNKNOWN;

	LgsString fullExpression;

   if (borders_ == None)
   {
      fullExpression = expression;
   }
   else
   {
      fullExpression.reserve(128);

      if ((borders_ == ElideLeft) || (borders_ == ElideRight) || (borders_ == ElideBoth))
      {
         if (borders_ != ElideRight)
         {
            leftBorder_ = leftElisionBoundary;
            fullExpression += leftParen;
            fullExpression += leftBorder_;
            fullExpression += rightParen;
         }

         fullExpression += leftParen;
         fullExpression += expression;
         fullExpression += rightParen;

         if (borders_ != ElideLeft)
         {
            rightBorder_ = rightElisionBoundary;
            fullExpression += leftParen;
            fullExpression += rightBorder_;
            fullExpression += rightParen;
         }
      }
      else
      {
         if (borders_ != Right)
         {
            if (borders_ == Contract)
            {
               leftBorder_ = leftContractBoundary;
            }
            else
            {
               leftBorder_ = leftBoundary;
            }
            fullExpression += leftParen;
            fullExpression += leftBorder_;
            fullExpression += rightParen;
         }

         fullExpression += leftParen;
         fullExpression += expression;
         fullExpression += rightParen;

         if (borders_ != Left)
         {
			if(borders_ == Contract) {
				rightBorder_ = rightContractBoundary;
			} else
				rightBorder_ = rightBoundary;
            fullExpression += leftParen;
            fullExpression += rightBorder_;
            fullExpression += rightParen;
         }
      }
   }

   // class initialization
   static bool firstTime = true;
   if (firstTime)
   {
      firstTime = false;
      re_set_syntax(RE_SYNTAX_POSIX_EXTENDED);
//XXX
		initLeftElisionBoundaryHash();
		initRightElisionBoundaryHash();
		initLeftBoundaryHash();
		initRightBoundaryHash();
		initFrule205Hash();
		initFrule101Hash();
   }

   // use fixed registers to prevent memory leaks,
   // since gnu code does not tell us when registers are allocated
   registers.num_regs = RE_NREGS;
   registers.start = start_;
   registers.end = end_;

   // initialize regex_ buffer variable
   memset(&regex_, 0, sizeof(regex_));
   // regex_.translate can be set to a translate table - not done here
   regex_.regs_allocated = REGS_FIXED; // use fixed registers
   regex_.fastmap = fastmap;           // use a fast map to speed up matching

   // compile pattern
   const char *error = re_compile_pattern(fullExpression.c_str(), fullExpression.length(), &regex_);
   regex_.regs_allocated = REGS_FIXED; // use fixed registers

#ifdef REGEX_TIMING
	find_or_insert_rtime((char *)fullExpression.c_str(), (char *)&regex_);
#endif // REGEX_TIMING

   if (error)
      throw error;
}

RegularExpression::~RegularExpression()
{

   regex_.fastmap = 0; // to avoid a free
   regfree(&regex_);
}

void RegularExpression::printTimes() {
#ifdef REGEX_TIMING
	printf("=========== Total Time in regex matches = %d\n", rtime_total());
// sort on absolute time
	regex_time a;
	bool chg=true;
	while(chg) {
		chg = false;
		for(int i=0;i<regex_time_index-1;i++) {
			if(rtimes[i].time < rtimes[i+1].time) {
				a = rtimes[i];
				rtimes[i] = rtimes[i+1];
				rtimes[i+1] = a;
				chg = true;
			}
		}
	}
	int sum = 0;
	for(int i=0;i<10;i++) {
		sum += rtimes[i].time;
	}
	printf("========= Top 10 Summary Time = %d\n", sum);
	for(i=0;i<10;i++) {
		printf("\t%d calls and %d time for \"%s\"\n",
			rtimes[i].ncalls, rtimes[i].time, rtimes[i].expr);
	}
#endif
}

int RegularExpression::groups() const
{
   int groups_ = regex_.re_nsub + 1;
   assert(groups_ < MaxGroups);      // implementation defined limit

   switch (borders_)
   {
   case Left:
   case Right:
   case ElideLeft:
   case ElideRight:
      // expression is of form (expression)(rightBorder_()) or (()leftBorder_)(expression)
      groups_ -= 3;
      break;
   case Both:
   case ElideBoth:
   case Contract:
      // since expression is of form (()leftBorder_)(expression)(rightBorder_())
      groups_ -= 5;
      break;
   }
   return groups_;
}

void RegularExpression::printGroups(const char *buffer) {
	int ngrp = regex_.re_nsub + 1;
	printf("buffer=\"%s\"\n\tngrp=%d, nsub=%d\n", 
		buffer, ngrp, regex_.re_nsub);
	fflush(stdout);
	for(int i=0;i<ngrp;i++) {
		char grp[1024];
		int grstart = registers.start[i];
		int grend = registers.end[i];
		int len = grend - grstart;
		strncpy(grp, buffer+grstart, len);
		grp[len] = '\0';
		printf("\tgroup %d[%d,%d]: \"%s\"\n", i, registers.start[i],
			registers.end[i], grp);
		fflush(stdout);
	}
}

bool RegularExpression::matches(const LgsString &buffer, int startPosition)
{
   if (startPosition >= buffer.length())
      return false;

   return matches(buffer.c_str(), buffer.length(), startPosition);
}

bool RegularExpression::matches(const char *buffer, int bufferLength, int startPosition)
{

#ifdef REGEX_TIMING
	long t0 = GetTickCount();
#endif // REGEX_TIMING

	calls_++;

   assert(bufferLength > 0);
   assert(startPosition >= 0 && startPosition < bufferLength);

#ifdef UNICODE
#error This needs to be re-written for unicode
#endif


	if(regid==REGID_FINISH_205)
		return matchFinishRule205(buffer, startPosition);
	else 
		if(regid==REGID_FINISH_206)
		return matchFinishRule206(buffer, startPosition);
	else 
		if(regid==REGID_FINISH_218 || regid==REGID_FINISH_220)
		return matchFinishRule218or220(buffer, startPosition);
	else if(regid==REGID_START_1CATCH_ALL)
		return matchStartRuleCatchAllFirst(buffer, startPosition);
	else if(regid==REGID_START_2CATCH_ALL)
		return matchStartRuleCatchAllSecond(buffer, startPosition);
	else if(regid==REGID_START_1LOOKUP_NEEDED)
		return matchStartRuleLookupNeededFirst(buffer, startPosition);
	else if(regid==REGID_START_2LOOKUP_NEEDED)
		return matchStartRuleLookupNeededSecond(buffer, startPosition);
	else if(regid==REGID_START_DOTTED_ALPHANUM)
		return matchStartRuleDottedAlphanum(buffer, startPosition);
	else if(regid==REGID_START_HYPHEN_SEQUENCE)
		return matchStartRuleHyphensequence(buffer, startPosition);
	else if(regid==REGID_START_MULTI_HYPHEN_ALPHANUM)
		return matchStartRuleMultiHyphenAlphanum(buffer, startPosition);
	else if(regid==REGID_CONJ0)
		return matchGermanConjugation0(buffer, startPosition);
	else if(regid==REGID_CONJ1)
		return matchGermanConjugation1(buffer, startPosition);
	else if(regid==REGID_FINISH_101)
		return matchFinishRule101(buffer, startPosition);
// not implemented yet.
//	else if(regid==REGID_START_ORDINAL)
//		return matchStartRuleOrdinal(buffer, startPosition);
//	else if(regid==REGID_START_NUMERIC)
//		return matchStartRuleNumeric(buffer, startPosition);
//	else if(regid==REGID_START_TIME)
//		return matchStartRuleTime(buffer, startPosition);


   const char *start_char = (const char *) buffer;
   int rc = re_search(&regex_, start_char, bufferLength, startPosition,
                      bufferLength - startPosition, &registers);
   assert(rc != -2); // no internal error in gnu code
/*
if(regid==REGID_CONJ0 && (rc>-1)) {
printf("CONJ0 match\n"); 
printGroups(buffer);
fflush(stdout);
} else if(regid==REGID_CONJ1 && (rc>-1)) {
//printf("CONJ1 match\n"); 
//printGroups(buffer);
//fflush(stdout);
}
*/

#ifdef REGEX_TIMING
	long t1 = GetTickCount();
	regex_time *rt = find_rtime((char *)&regex_);
	if(rt) {
		rt->time += (t1-t0);
		rt->ncalls++;
	}
#endif // REGEX_TIMING

/*
	if(regid==REGID_FINISH_205) {
printf("buffer \"%s\", startPos = %d, rc=%d\n", 
buffer, startPosition, rc); fflush(stdout);
		if(rc>-1) {
			printGroups(buffer);
		}
	}
*/
   return rc > -1;
}

RegularExpression::MatchPos RegularExpression::match(int group) const
{
   assert(calls_ != 0);
   assert(0 <= group && group <= groups());

   switch (borders_)
   {
   case Right:
   case ElideRight:
      group += 1; // expression is of form (expression)(rightBorder_())
      break;
   case Left:
   case ElideLeft:
   case Both:
   case ElideBoth:
   case Contract:
      group += 3; // since expression is of form (()leftBorder_)(expression)
      break;
   }

   return MatchPos(registers.start[group], registers.end[group]);
}

LgsString RegularExpression::escapeForm(const LgsString& literals)
{
   LgsString ret;

   for (LgsString::const_iterator iter = literals.begin(); iter != literals.end(); iter++)
   {
      char c = char(*iter);
      if (!CharUtil::isAlphaNumeric(c) && !strchr("<>'\"", c))
      {
         // add escape char - it is ignored most of the time and
         // it is only really needed for a period, and other isolated
         ret += escapeSymbol;
      }
      ret += c;
   }

   return ret;
}

// ----------------------------------------------------------
RegularExpression::MatchPos::MatchPos(int start, int end)
                 :pos(start == invalid ? -1 : start),
                  length(start == invalid ? -1 : end - start)
{
}

bool 
RegularExpression::matchStartRuleCatchAllFirst(const char *buffer, 
												 int startPosition) {

	char *startpos = (char *)(buffer + startPosition);
//([^[:blank:]]*([@#~$%^&*+_=|£¢:?.!ø°][^[:blank:]]*[^[:punct:]']))
//--------------------------- group #3 ----------------------------
// cb0-----------------------------------------------------------cb1
	char *core = "@#~$%^&*+_=|£¢:?.!ø°";
	char *cp = startpos;
	char *lb0, *lb1, *rb0, *rb1, *cb0, *cb1, *start;
	while(*cp) {
		cp = strpbrk(cp, core);
		if(cp==NULL)
			return false;

		start =  cp;	// "start" is the position of a char from core
						// group we're centered on

//printf("start point: %d, \"%s\"\n", start - buffer, start); fflush(stdout);
// move to the left until blank or beginning of the buffer
			lb0 = cp;	// start of left border
//printf("start pos=%d, lb0=%d\n", startpos - buffer, lb0-buffer); fflush(stdout);
		while(lb0>startpos) {
			lb0--;
			if(CharUtil::isBlank(*lb0))
				break;
		}
//printf("start of left border: %d\n", lb0-buffer); fflush(stdout);
		lb1 = lb0;
		while((lb1 < start) && leftBoundaryHash[*lb1])
			lb1++;	// end of left border
//printf("end of left border: %d\n", lb1-buffer); fflush(stdout);

		cb0 = lb1;	// start of core (#3) group

// now moving to the right
//printf("beginning: \"%s\"\n", beg); fflush(stdout);
// [^[:blank]]* is greedy, thus we move right till the border
// then backtrack
		while(1) {
			cp++;

			if(CharUtil::isBlank(*cp) ||
					(*cp=='\0'))
				break;
		}

		rb1 = cp + 1;

// dot in the end of the sentence should be excluded
		if(rb1==start+1) {
			cp = start + 1;
			continue;
		}

//printf("end of right border: %d\n", rb1-buffer); fflush(stdout);

		rb0 = rb1 - 2;
		for(;rb0>start;rb0--)
			if(!rightBoundaryHash[*rb0])
				break;
		rb0++;
		cb1 = rb0;
//printf("real end of core group: %d\n", cb1-buffer); fflush(stdout);

// we need to make sure that between "start" and "end"
// there is at least one non :punct character
		if(*(cb1-1)!='/' && (CharUtil::isPunctuation(*(cb1-1)) || *(cb1-1)=='\'')) {
			cp = start + 1;
			continue;
		}

//printf("start of core group: %d\n", cb0-buffer); fflush(stdout);

//printf("match: \"%s\"\n", beg); fflush(stdout);

		regex_.re_nsub = 6;

		registers.start[0] = lb0 - buffer;
		registers.end[0] = rb1 - buffer;

		registers.start[1] = lb0 - buffer;
		registers.end[1] = lb1 - buffer;

		registers.start[2] = -1;
		registers.end[2] = -1;

		registers.start[3] = cb0 - buffer;
		registers.end[3] = cb1 - buffer;

		registers.start[4] = -1;
		registers.end[4] = -1;

		registers.start[5] = rb0 - buffer;
		registers.end[5] = rb1 - buffer;
		registers.start[6] = rb1 - buffer - 1;
		registers.end[6] = rb1 - buffer;

//printGroups(buffer);

		return true;
	}
	return false;
}

bool 
RegularExpression::matchStartRuleCatchAllSecond(const char *buffer, 
												 int startPosition) {
	char *startpos = (char *)(buffer + startPosition);
//([^[:blank:]]*([@#~$%^&*+_=|£¢))
//--------- group #3 -------------
// cb0-----------------------------------------------------------cb1
	char *core = "@#~$%^&*+_=|£¢";
	char *cp = startpos;
	char *lb0, *lb1, *rb0, *rb1, *cb0, *cb1, *start;
	while(*cp) {
		cp = strpbrk(cp, core);
		if(cp==NULL)
			return false;

		start =  cp;	// "start" is the position of a char from core
						// group we're centered on

//printf("start point: %d, \"%s\"\n", start - buffer, start); fflush(stdout);
// move to the left until blank or beginning of the buffer
		lb0 = cp;	// start of left border
//printf("start pos=%d, lb0=%d\n", startpos - buffer, lb0-buffer); fflush(stdout);
		while(lb0>startpos) {
			lb0--;
			if(CharUtil::isBlank(*lb0))
				break;
		}
//printf("start of left border: %d\n", lb0-buffer); fflush(stdout);
		lb1 = lb0;
		while((lb1 < start) && leftBoundaryHash[*lb1])
			lb1++;	// end of left border
//printf("end of left border: %d\n", lb1-buffer); fflush(stdout);

		cb0 = lb1;	// start of core (#3) group
		cb1 = start + 1;
		rb0 = cb1;

// now moving to the right
		bool isMatch = false;
		while(1) {
			cp++;

			if(CharUtil::isBlank(*cp) ||
				(*cp=='\0')) {
				isMatch = true;
				break;
			}
			if(!rightBoundaryHash[*cp]) {
				break;
			}
		}
		if(!isMatch) {
			cp = start + 1;
			continue;
		}

		rb1 = cp + 1;

//printf("end of right border: %d\n", rb1-buffer); fflush(stdout);

		regex_.re_nsub = 6;

		registers.start[0] = lb0 - buffer;
		registers.end[0] = rb1 - buffer;

		registers.start[1] = lb0 - buffer;
		registers.end[1] = lb1 - buffer;

		registers.start[2] = -1;
		registers.end[2] = -1;

		registers.start[3] = cb0 - buffer;
		registers.end[3] = cb1 - buffer;

		registers.start[4] = -1;
		registers.end[4] = -1;

		registers.start[5] = rb0 - buffer;
		registers.end[5] = rb1 - buffer;
		registers.start[6] = rb1 - buffer - 1;
		registers.end[6] = rb1 - buffer;

//printGroups(buffer);

		return true;
	}
	return false;
}

bool 
RegularExpression::matchFinishRule218or220(const char *buffer, 
											int startPosition) {
//printf("218/220 rule\n"); fflush(stdout);
	char *beg, *cp=(char *)buffer + startPosition;
	while(1) {
//printf("\tlooking for a in \"%s\"\n", cp); fflush(stdout);
		cp = strchr(cp, '‡');
		beg = cp;
		if(cp==NULL)
			return false;
//printf("\tfound \"%c\"/%d at %d\n", *cp, *cp, cp - buffer); fflush(stdout);
		if(cp>buffer)
			if(leftElisionBoundaryHash[*(cp-1)]==0) {
				cp++;
				continue;
			}
//printf("\tskipping blanks\n"); fflush(stdout);
		cp++;
//printf("\tnext is \"%c\"/%d at %d\n", *cp, *cp, cp - buffer); fflush(stdout);
		if(!CharUtil::isBlank(*cp))	{ // there should be at least 1 blank
			cp++;
			continue;
		}
		while(*cp)
			if(CharUtil::isBlank(*cp))
				cp++;
			else break;

//printf("\tblanks skipped\n"); fflush(stdout);
		if(*cp!='l')
			continue;
		cp++;

		if(*cp!='e')
			continue;
		cp++;

		if(regid==REGID_FINISH_220) {
			if(*cp=='s') {
				cp++;
			}
		}
//printf("\tle[s] found\n"); fflush(stdout);

		char *end = cp;
		if(regid==REGID_FINISH_220) {
			if(strncmp(cp, "quels", 5)==0) {
				end += 5;
			} else if(strncmp(cp, "quelles", 7)==0) {
				end += 7;
			}
		} else {
			if(strncmp(cp, "quel", 4)==0) {
				end += 4;
			}
		}

//printf("\tlooking for elision border\n"); fflush(stdout);

		if(rightElisionBoundaryHash[*end]==0)
				continue;
// we got match
//printf("\tmatch\n"); fflush(stdout);
		regex_.re_nsub = 8;

// out of 8 groups these rules are interested in groups 3-6 inclusive

		registers.start[3] = beg - buffer;
		registers.end[3] = cp - buffer;

		registers.start[4] = -1;
		registers.end[4] = -1;

		registers.start[5] = beg - buffer;
		registers.end[5] = cp - buffer;
		if(regid==REGID_FINISH_220) {
			registers.start[6] = cp - buffer;
			registers.end[6] = end - buffer;
		}
		return true;
	}	
	return false;
}

bool
RegularExpression::matchStartRuleLookupNeededFirst(const char *buffer, 
												   int startPosition) {
//printf("lookup needed 1\n");
	char *startpos = (char *)(buffer + startPosition);
//([[:alpha:]]*(\\.-?([[:alpha:]]*))+(\\.)?)
//--------- group #3 -----------------------

	char *cp = startpos;
	char *lb0, *lb1, *rb0, *rb1, *cb0, *cb1, *start;
	while(*cp) {
		cp = strpbrk(cp, ".");
		if(cp==NULL)
			return false;

		start =  cp;	// "start" is the position of a char from core
						// group we're centered on

//printf("start point: %d, \"%s\"\n", start - buffer, start); fflush(stdout);
// move to the left until blank or beginning of the buffer
		lb0 = cp;	// start of left border
		while(lb0>startpos) {
			lb0--;
			if(CharUtil::isBlank(*lb0))
				break;
		}
//printf("start of left border: %d\n", lb0-buffer); fflush(stdout);
		lb1 = lb0;
		while((lb1 < start) && leftBoundaryHash[*lb1])
			lb1++;	// end of left border
//printf("end of left border: %d\n", lb1-buffer); fflush(stdout);

// between the dot and left boundary could be 0 or more Alpha chars.
		bool isMatch = true;
		for(cp=lb1;cp<start;cp++) {
			if(!CharUtil::isAlphabetic(*cp)) {
				isMatch = false;
				break;
			}
		}
		if(!isMatch) {
			cp = start + 1;
			continue;
		}

		cb0 = lb1;	// start of the core (#3) group
		cp = cb1 = start;
//printf("start of core group: %d\n", lb1-buffer); fflush(stdout);

// now moving to the right
		bool spaceOrEnd = true;
		bool prevDot = true;
		while(1) {
			cp++;

			if(CharUtil::isBlank(*cp)) {
				break;
			}
			if(*cp=='\0')
				break;

// is it "\.-?" ?
			if(*cp=='-' && prevDot)
				continue;
			prevDot = (*cp=='.');
// or just yet another dot ?
			if(prevDot) {
//printf("continuing core group (prevDot): %d\n", cp-buffer); fflush(stdout);
				continue;
			}

			if(!CharUtil::isAlphabetic(*cp)) {
				spaceOrEnd = false;
				break;
			}
		}
		cb1 = cp;
//printf("end of core group: %d\n", cp-buffer); fflush(stdout);

		rb0 = rb1 = cb1;

		isMatch = true;

		if(!spaceOrEnd) {
			while(1) {
				if(*rb1=='\0') {
					break;
				}
				if(CharUtil::isBlank(*rb1)) {
					rb1++;
					break;
				}
				
				if(!rightBoundaryHash[*rb1]) {
//printf("failure to match, rb1: %d\n", rb1-buffer); fflush(stdout);
					isMatch = false;
					break;
				}
//printf("continuing right boundary: %d\n", rb1-buffer); fflush(stdout);
				rb1++;
			}
		}

		if(!isMatch) {
			cp = start + 1;
			continue;
		}

//printf("end of right border: %d\n", rb1-buffer); fflush(stdout);

		regex_.re_nsub = 9;

		registers.start[0] = lb0 - buffer;
		registers.end[0] = rb1 - buffer;

		registers.start[1] = lb0 - buffer;
		registers.end[1] = lb1 - buffer;

		registers.start[2] = -1;
		registers.end[2] = -1;

		registers.start[3] = cb0 - buffer;
		registers.end[3] = cb1 - buffer;

		registers.start[4] = -1;
		registers.end[4] = -1;
		registers.start[5] = -1;
		registers.end[5] = -1;
		registers.start[6] = -1;
		registers.end[6] = -1;
		registers.start[7] = -1;
		registers.end[7] = -1;

		registers.start[8] = rb0 - buffer;
		registers.end[8] = rb1 - buffer;

		registers.start[9] = rb1 - buffer;
		if(rb1-rb0>1)
			registers.start[9] = rb1 - buffer - 1;
		registers.end[9] = rb1 - buffer;

//printGroups(buffer);

		return true;
	}
	return false;
}

bool 
RegularExpression::matchStartRuleDottedAlphanum(const char *buffer, 
												int startPosition) {
	char *startpos = (char *)(buffer + startPosition);
//([[:alnum:]]*[[:digit:]]+[[:alnum:]]*)(\\.[[:alnum:]]*[[:digit:]]+[[:alnum:]]*)+(\\.)?
//--------- group #3 -----------------------
	char *cp = startpos;
	char *lb0, *lb1, *rb0, *rb1, *cb0, *cb1, *start;
	while(*cp) {
		cp = strpbrk(cp, ".");
		if(cp==NULL)
			return false;

		start =  cp;	// "start" is the position of a char from core
						// group we're centered on

//printf("start point: %d, \"%s\"\n", start - buffer, start); fflush(stdout);
// move to the left until blank or beginning of the buffer
		lb0 = cp;	// start of left border
		while(lb0>startpos) {
			lb0--;
			if(CharUtil::isBlank(*lb0))
				break;
		}
//printf("start of left border: %d\n", lb0-buffer); fflush(stdout);
		lb1 = lb0;
		while((lb1 < start) && leftBoundaryHash[*lb1])
			lb1++;	// end of left border
//printf("end of left border: %d\n", lb1-buffer); fflush(stdout);

		cb0 = lb1;	// start of the core (#3) group
		bool isMatch = true;
		bool thereWasDigit = false;
		for(cp=lb1;cp<start;cp++) {
			if(!CharUtil::isAlphaNumeric(*cp)) {
				isMatch = false;
				break;
			}
			thereWasDigit |= CharUtil::isNumeric(*cp);
		}
//printf("left thereWasDigit %d\n", thereWasDigit);
		isMatch &= thereWasDigit;
		if(!isMatch) {
			cp = start + 1;
			continue;
		}

		cp = cb1 = start;
//printf("start of core group: %d\n", lb1-buffer); fflush(stdout);

		thereWasDigit = false;
		bool thereWasDot = false;
		isMatch = true;
		while(1) {
			cp++;
//printf("\t\"%c\"\n", *cp);
			if(CharUtil::isBlank(*cp))
				break;
			if(rightBoundaryHash[*cp])
				break;
			if(*cp=='\0')
				break;

			if(*cp=='.') {
				if(thereWasDot) {
					isMatch = false;
					break;
				}
				if(!thereWasDigit) {
					isMatch = false;
					break;
				}
				thereWasDot = true;
			}
			if(!CharUtil::isAlphaNumeric(*cp)) {
				isMatch = false;
				break;
			}
			thereWasDigit |= CharUtil::isNumeric(*cp);
		}
//printf("right isMatch %d thereWasDigit %d\n", isMatch, thereWasDigit);
		isMatch &= thereWasDigit;
		if(!isMatch) {
			cp = start + 1;
			continue;
		}

		cb1 = cp;
		rb0 = rb1 = cp;
		isMatch = true;
		while(1) {
			if(CharUtil::isBlank(*rb1))
				break;
			if(*rb1=='\0')
				break;
			if(!rightBoundaryHash[*rb1]) {
				isMatch = false;
				break;
			}
			rb1++;
		}
//printf("right boundary isMatch %d\n", isMatch);

		if(!isMatch) {
			cp = start + 1;
			continue;
		}
		rb1++;

		registers.start[0] = lb0 - buffer;
		registers.end[0] = rb1 - buffer;

		registers.start[1] = lb0 - buffer;
		registers.end[1] = lb1 - buffer;

		registers.start[2] = -1;
		registers.end[2] = -1;

		registers.start[3] = cb0 - buffer;
		registers.end[3] = cb1 - buffer;

		registers.start[4] = -1;
		registers.end[4] = -1;
		registers.start[5] = -1;
		registers.end[5] = -1;
		registers.start[6] = -1;
		registers.end[6] = -1;

		registers.start[7] = rb0 - buffer;
		registers.end[7] = rb1 - buffer;

		registers.start[8] = rb1 - buffer;
		if(rb1-rb0>0)
			registers.start[8] = rb1 - buffer - 1;
		registers.end[8] = rb1 - buffer;

//printGroups(buffer);

		return true;
	}

	return false;
}

bool 
RegularExpression::matchStartRuleLookupNeededSecond(const char *buffer, 
													int startPosition) {
//printf("lookup needed 2\n");
	char *startpos = (char *)(buffer + startPosition);
//(([[:alpha:]]\\.)+('s))
//--------- group #3 -----------------------
	char *cp = startpos;
	char *lb0, *lb1, *rb0, *rb1, *cb0, *cb1, *start;
	while(*cp) {
		cp = strpbrk(cp, ".");
		if(cp==NULL)
			return false;

		start =  cp;	// "start" is the position of a char from core
						// group we're centered on

//printf("start point: %d, \"%s\"\n", start - buffer, start); fflush(stdout);
// move to the left until blank or beginning of the buffer
		lb0 = cp;	// start of left border
		while(lb0>startpos) {
			lb0--;
			if(CharUtil::isBlank(*lb0))
				break;
		}
//printf("start of left border: %d\n", lb0-buffer); fflush(stdout);
		lb1 = lb0;
		while((lb1 < start) && leftBoundaryHash[*lb1])
			lb1++;	// end of left border
//printf("end of left border: %d\n", lb1-buffer); fflush(stdout);

// cp points to the 1-st dot
		if(!CharUtil::isAlphabetic(*lb1)) {
			cp = start + 1;
			continue;
		}
		cb0 = lb1;

		bool wasDot = true;
		bool wasAlpha = false;
		bool wasQuote = false;
		bool isMatch = false;
		while(1) {
			cp++;
			if(CharUtil::isBlank(*cp))
				break;
			if(*cp=='\0')
				break;
			if(*cp=='.' && wasAlpha) {
				wasDot = true;
				wasAlpha = false;
				continue;
			}
			if(CharUtil::isAlphabetic(*cp) && wasDot) {
				wasDot = false;
				wasAlpha = true;
				continue;
			}
			if(*cp=='\'') {
				wasDot = wasAlpha = false;
				wasQuote = true;
				continue;
			}
			if(*cp=='s' && wasQuote) {
				isMatch = true;
				cp++;
				break;
			}
			
// no match
			break;
		}

		if(!isMatch) {
			cp = start + 1;
			continue;
		}

// cp points to the 1-st char after "'s"
		cb1 = cp;
		rb0 = rb1 = cb1;
		isMatch = true;
		while(1) {
			if(CharUtil::isBlank(*rb1))
				break;
			if(*rb1=='\0')
				break;
			if(!rightBoundaryHash[*rb1]) {
				isMatch = false;
				break;
			}
			rb1++;
		}

		if(!isMatch) {
			cp = start + 1;
			continue;
		}
		rb1++;

		registers.start[0] = lb0 - buffer;
		registers.end[0] = rb1 - buffer;

		registers.start[1] = lb0 - buffer;
		registers.end[1] = lb1 - buffer;

		registers.start[2] = -1;
		registers.end[2] = -1;

		registers.start[3] = cb0 - buffer;
		registers.end[3] = cb1 - buffer;

		registers.start[4] = -1;
		registers.end[4] = -1;
		registers.start[5] = -1;
		registers.end[5] = -1;
		registers.start[6] = -1;
		registers.end[6] = -1;

		registers.start[7] = rb0 - buffer;
		registers.end[7] = rb1 - buffer;

		registers.start[8] = rb1 - buffer;
		if(rb1-rb0>0)
			registers.start[8] = rb1 - buffer - 1;
		registers.end[8] = rb1 - buffer;

//printGroups(buffer);

		return true;
	}
	return false;
}

// Total cost of remaining (those below) start rules is 1.38s out of 
// total translation time == 43s (emike1) and total regex time == 2.9

//----  cost of this rule is 0.1s out of 43s (emike1)
bool RegularExpression::matchStartRuleNumericHyphenAlphanum(const char *buffer, 
															int startPos) {
	return false;
}

// Total cost of remaining (those below) french finish rules is 1.06s out of 
// total translation time == 43s (emike1) and total regex time == 2.9

//----  cost of this rule is 0.46s out of 43s (emike1)
bool 
RegularExpression::matchFinishRule205(const char *buffer, int startPos) {
	char *beg =(char *)buffer + startPos;
	char *cp, *anc;
	char *lb0, *rb0, *lb5, *rb5, *lb6, *rb6;

//cp = strstr(buffer, "le yen japo");
//bool prt = (cp!=NULL);
bool prt = false;
	while(1) {
		cp = beg;
if(prt) {
printf("--- looking for a blank (frule 205) in \"%s\"\n", cp); fflush(stdout);
}
		cp = strchr(cp, ' ');
		if(cp==NULL)
			return false;
		anc = cp;	// anchor set to the 1-st blank
		if(anc<beg+2) {
			beg = anc + 1;
			continue;
		}
if(prt) {
printf("\tfound blank at %d\n", cp - beg); fflush(stdout);
}

		while(1) {
			if(!CharUtil::isBlank(*cp))
				break;
			cp++;
			if(*cp=='\0')
				break;
		}
		if(*cp=='\0') {
			beg = anc + 1;
			continue;
		}
		unsigned char *ucp = (unsigned char *)cp;
		if(frule205Hash[*ucp]==0) {
			beg = anc + 1;
			continue;
		}
if(prt) {
printf("\tright side looks ok\n"); fflush(stdout);
}

		rb5 = cp;
		lb6 = rb5;

// check for cases like "de A.O."
		cp++;
		ucp = (unsigned char *)cp;
		bool notGood = false, breakOnAlphaNum = false;
		while(*ucp) {
			if(*ucp==' ' || *ucp=='\\' || *ucp=='(' || *ucp=='|' || *ucp=='@')
				break;
			else if(rightElisionBoundaryHash[*ucp]==1 && *ucp!='\'') {
				breakOnAlphaNum = true;
if(prt) {
printf("\tbreakOnAN true on %c/%d in position %d\n", *ucp,
*ucp, ucp-(unsigned char *)buffer); fflush(stdout);
}
			} else if(breakOnAlphaNum) {
				notGood = true;
				break;
			}
			ucp++;
		}
		if(notGood) {
if(prt) {
printf("\tnot good because of A.O.\n"); fflush(stdout);
}
			beg = anc + 1;
			continue;
		}

		rb0 = (char *)ucp + 1;
		rb6 = rb0 - 1;

// now moving to the left from blank
		cp = anc - 1;	// character before 1-st blank

		if(*cp != 'e') {
			beg = anc + 1;
			continue;
		}

		lb5 = cp;
		cp--;
if(prt) {
printf("\t\'e\' from the left ok\n"); fflush(stdout);
}

		if(*cp!='d' && *cp!='l' && *cp!='j' && *cp!='m' && *cp!='t'
				&& *cp!='s' && *cp!='n' && *cp!='u') {
			beg = anc + 1;
			continue;
		}
		if(*cp=='u' && ((cp == beg) || (*(cp-1)!='q'))) {
			beg = anc + 1;
			continue;
		}

		if(*cp=='u')
			cp--;
		cp--;
if(prt) {
printf("\tbefore \'e\' is ok\n"); fflush(stdout);
}

		if(leftElisionBoundaryHash[*cp]==0) {
			beg = anc + 1;
			continue;
		}

		lb0 = cp;

if(prt) {
printf("\tmatch\n"); fflush(stdout);
}

// we care only about groups 5 and 6. Adding group 0 just in case.
// 5 is getting replaced and 6 is checked against list of special cases.
		regex_.re_nsub = 8;

		registers.start[0] = lb0-buffer;
		registers.end[0] = rb0-buffer;

		registers.start[1] = lb5-buffer;
		registers.end[1] = rb5-buffer;

		registers.start[2] = lb5-buffer;
		registers.end[2] = rb5-buffer;

		registers.start[3] = lb5-buffer;
		registers.end[3] = rb5-buffer;

		registers.start[4] = lb5-buffer;
		registers.end[4] = rb5-buffer;

		registers.start[5] = lb5-buffer;
		registers.end[5] = rb5-buffer;

		registers.start[6] = lb6-buffer;
		registers.end[6] = rb6-buffer;

		registers.start[7] = lb5-buffer;
		registers.end[7] = rb5-buffer;

		registers.start[8] = lb5-buffer;
		registers.end[8] = rb5-buffer;

		return true;
	}	
	return false;
}

//----  cost of this rule is 0.18s out of 43s (emike1)
// it's very similar to rule 205.
bool RegularExpression::matchFinishRule206(const char *buffer, int startPos) {
	char *beg =(char *)buffer + startPos;
	char *cp, *anc;
	char *lb0, *rb0, *lb4, *rb4, *lb5, *rb5;

//cp = strstr(buffer, "le yen japo");
//bool prt = (cp!=NULL);
bool prt = false;
	while(1) {
		cp = beg;
if(prt) {
printf("--- looking for a blank (frule 206) in \"%s\"\n", cp); fflush(stdout);
}
		cp = strchr(cp, ' ');
		if(cp==NULL)
			return false;

		anc = cp;	// anchor set to the 1-st blank
		if(anc<beg+1) {
			beg = anc + 1;
			continue;
		}
if(prt) {
printf("\tfound blank at %d\n", cp - beg); fflush(stdout);
}

		while(1) {
			if(!CharUtil::isBlank(*cp))
				break;
			cp++;
			if(*cp=='\0')
				break;
		}
		if(*cp=='\0') {
			beg = anc + 1;
			continue;
		}
		unsigned char *ucp = (unsigned char *)cp;
		if(frule205Hash[*ucp]==0) {	// the same for 206 as for 205
			beg = anc + 1;
			continue;
		}
if(prt) {
printf("\tright side looks ok\n"); fflush(stdout);
}

		rb4 = cp;
		lb5 = rb4;

// check for cases like "de A.O."
		cp++;
		ucp = (unsigned char *)cp;
		bool notGood = false, breakOnAlphaNum = false;
		while(*ucp) {
			if(*ucp==' ' || *ucp=='\\' || *ucp=='(' || *ucp=='|' || *ucp=='@')
				break;
			else if(rightElisionBoundaryHash[*ucp]==1 && *ucp!='\'') {
				breakOnAlphaNum = true;
if(prt) {
printf("\tbreakOnAN true on %c/%d in position %d\n", *ucp,
*ucp, ucp-(unsigned char *)buffer); fflush(stdout);
}
			} else if(breakOnAlphaNum) {
				notGood = true;
				break;
			}
			ucp++;
		}
		if(notGood) {
if(prt) {
printf("\tnot good because of A.O.\n"); fflush(stdout);
}
			beg = anc + 1;
			continue;
		}

		rb0 = (char *)ucp + 1;
		rb5 = rb0 - 1;

// now moving to the left from blank
		cp = anc - 1;	// character before 1-st blank

		if(*cp != 'a') {
			beg = anc + 1;
			continue;
		}

		lb4 = cp;
		cp--;
if(prt) {
printf("\t\'e\' from the left ok\n"); fflush(stdout);
}

		if(*cp!='l') {
			beg = anc + 1;
			continue;
		}

		cp--;
if(prt) {
printf("\tbefore \'a\' is ok\n"); fflush(stdout);
}

		if(leftElisionBoundaryHash[*cp]==0) {
			beg = anc + 1;
			continue;
		}

		lb0 = cp;

if(prt) {
printf("\tmatch\n"); fflush(stdout);
}

// we care only about groups 4 and 5. Adding group 0 just in case.
// 4 is getting replaced and 5 is checked against list of special cases.
		regex_.re_nsub = 7;

		registers.start[0] = lb0-buffer;
		registers.end[0] = rb0-buffer;

		registers.start[1] = lb4-buffer;
		registers.end[1] = rb4-buffer;

		registers.start[2] = lb4-buffer;
		registers.end[2] = rb4-buffer;

		registers.start[3] = lb4-buffer;
		registers.end[3] = rb4-buffer;

		registers.start[4] = lb4-buffer;
		registers.end[4] = rb4-buffer;

		registers.start[5] = lb5-buffer;
		registers.end[5] = rb5-buffer;

		registers.start[6] = lb4-buffer;
		registers.end[6] = rb4-buffer;

		registers.start[7] = lb4-buffer;
		registers.end[7] = rb4-buffer;

		return true;
	}	
	return false;
}

//----  cost of this rule is 0.28s out of 43s (emike1)
bool RegularExpression::matchStartRuleHyphensequence(const char *buffer, 
													 int startPos) {
	return false;
}

//----  cost of this rule is 0.12s out of 43s (emike1)
bool RegularExpression::matchFinishRule207or208(const char *buffer, 
												int startPos) {
	return false;
}

//----  cost of this rule is 0.3s out of 43s (emike1)
bool RegularExpression::matchStartRuleMultiHyphenAlphanum(const char *buffer, 
														  int startPos) {
	return false;
}

//----  cost of this rule is 0.25s out of 43s (emike1)
bool 
RegularExpression::matchStartRuleOrdinal(const char *buffer, int startPos) {
	return false;
}

//----  cost of this rule is 0.25s out of 43s (emike1)
bool 
RegularExpression::matchStartRuleNumeric(const char *buffer, int startPos) {
	return false;
}

//----  cost of this rule is 0.2s out of 43s (emike1)
bool RegularExpression::matchStartRuleTime(const char *buffer, int startPos) {
	return false;
}

//----  This rule is for english target.
// Its cost - 0.2s out of 20s (de2en, emiele2)
// the rule looks for smthg like "a opener" and replaces it
// w/ "an opener". It means that we're interested only
// in one group.
bool RegularExpression::matchFinishRule101(const char *buffer, int startPos) {
	char *beg =(char *)buffer + startPos;
	char *cp, *anc;
	char *lb4, *rb4;

bool prt = false;
	while(1) {
// The idea is to find blank and then look to the right and to the left.
// On the blank's left there has to be [aA] followed by left boundary.
// To the right - we dont even have to check, it's either zero or more
// words or some kind of delimiter or an end, thus we dont care.

		cp = beg;

		cp = strchr(cp, ' ');
		if(cp==NULL)
			return false;

		anc = cp;	// anchor set to this blank's position
		if(anc<beg+1) {
			beg = anc + 1;
			continue;
		}

// let's find the 1-st non-blank character to the right
		while(1) {
			if(!CharUtil::isBlank(*cp))
				break;
			cp++;
			if(*cp=='\0')
				break;
		}
		if(*cp=='\0') {
			beg = anc + 1;
			continue;
		}

// is this non-blank character a good one ?
		unsigned char *ucp = (unsigned char *)cp;
		bool isMatch = false;
		if(*ucp==(unsigned char)'$' 
                   || *ucp==(unsigned char)'£') { // [$£]?(8|1[18])
			ucp++;
		} else {
			if(*ucp) {
				if(*ucp=='8') isMatch = true;
				else if(*ucp=='1') {
					ucp++;
					if(*ucp=='1') isMatch = true;
					else
						if(*ucp=='8') isMatch = true;
				}
			}
		}
		if(!isMatch) {	
			ucp = (unsigned char *)cp;
			if(frule101Hash[*ucp]==0) {
				beg = anc + 1;
				continue;
			}
		}
/*
// check for right boundary
		cp++;
		ucp = (unsigned char *)cp;
		while(*ucp) {
			if(*ucp==' ' || *ucp=='\\' || *ucp=='(' || *ucp=='|' || *ucp=='@')
				break;
			if(rightElisionBoundaryHash[*ucp])
				break;
			ucp++;
		}
		if(!*ucp) {
			beg = anc + 1;
			continue;
		}
*/

// now moving to the left from blank
		cp = anc - 1;	// character before 1-st blank

		if(*cp != 'a' && *cp !='A') {
			beg = anc + 1;
			continue;
		}

		lb4 = cp;
		rb4 = anc;

		cp--;

		if(leftElisionBoundaryHash[*cp]==0) {
			beg = anc + 1;
			continue;
		}

if(prt) {
printf("\tmatch finish_101 in \"%s\"\n", beg);
}

// we care only about group #4.
		regex_.re_nsub = 8; // like it was in the original, just in case.
		registers.start[4] = lb4-buffer;
		registers.end[4] = rb4-buffer;

		return true;
	}	
	return false;
}

//----  This rule is for german source.
// Its cost - 0.7s out of 19s (de2en, emiele2)
bool RegularExpression::matchGermanConjugation1(const char *buffer, int startPos) {
	char *beg =(char *)buffer + startPos;
	char *cp, *anc;
	char *lb0, *rb0, *lb1, *rb1, *lb2, *rb2, *lb3, *rb3, *lb4, *rb4, *lb5, *rb5;

bool prt = false;
	while(1) {
// The idea is to find dash and then look to the right and to the left.
// On the dash's left there has to be 1+ alpha.
// To the right - blanks, some keyword, blanks and then 1+ alpha.

		cp = beg;

		cp = strchr(cp, '-');
		if(cp==NULL)
			return false;
if(prt) {
	printf("\ndash found in \"%s\"\n", beg);
	fflush(stdout);
}

		anc = cp;	// anchor set to dash position
		if(anc<beg+1) {
			beg = anc + 1;
			continue;
		}

if(prt) {
	printf("\ndash good1 in \"%s\"\n", beg);
	fflush(stdout);
}

// check what's on the left
		if(!CharUtil::isAlphabetic(*(anc-1))) {
			beg = anc + 1;
			continue;
		}
if(prt) {
	printf("\ndash good2 in \"%s\"\n", beg);
	fflush(stdout);
}

// see if there is blank after the dash
		cp++;
		if(!CharUtil::isBlank(*cp)) {
			beg = anc + 1;
			continue;
		}
if(prt) {
	printf("\ndash good3 in \"%s\"\n", beg);
	fflush(stdout);
}

// let's find the 1-st non-blank character to the right
		cp++;
		while(1) {
			if(!CharUtil::isBlank(*cp))
				break;
			cp++;
			if(*cp=='\0')
				break;
		}
		if(*cp=='\0') {
			beg = anc + 1;
			continue;
		}

// "cp" points to first non-blank and we check against magic words list.
if(prt) {
	printf("\ncandidate in \"%s\"\n", beg);
	fflush(stdout);
}
		bool isMatch = false;
		lb3 = cp;
		if(*cp=='u') {
			if(*(cp+1)) {
				if(*(cp+2) && CharUtil::isBlank(*(cp+2))) {
					isMatch = true;
					rb3 = cp + 2;
				}
			}
		}
		if(!isMatch) {
			if(strncmp(cp, "noch ", 5)==0) {
				isMatch = true;
				rb3 = cp + 4;
			}
			else if(strncmp(cp, "prep ", 5)==0) {
				isMatch = true;
				rb3 = cp + 4;
			}
			else if(strncmp(cp, "statt ", 6)==0) {
				isMatch = true;
				rb3 = cp + 5;
			}
			else if(strncmp(cp, "und ", 4)==0) {
				isMatch = true;
				rb3 = cp + 3;
			}
			else if(strncmp(cp, "oder ", 5)==0) {
				isMatch = true;
				rb3 = cp + 4;
			}
			else if(strncmp(cp, "bis ", 4)==0) {
				isMatch = true;
				rb3 = cp + 3;
			}
			else if(strncmp(cp, "bzw ", 4)==0) {
				isMatch = true;
				rb3 = cp + 3;
			}
			else if(strncmp(cp, "als auch ", 9)==0) {
				isMatch = true;
				rb3 = cp + 8;
			}
			else if(strncmp(cp, "ebenso wie ", 10)==0) {
				isMatch = true;
				rb3 = cp + 9;
			}
			else if(strncmp(cp, "sondern auch ", 12)==0) {
				isMatch = true;
				rb3 = cp + 11;
			}
			else if(strncmp(cp, "wie auch ", 9)==0) {
				isMatch = true;
				rb3 = cp + 8;
			}
			else if(strncmp(cp, "und/oder ", 9)==0) {
				isMatch = true;
				rb3 = cp + 8;
			}
			else if(strncmp(cp, "aber auch ", 10)==0) {
				isMatch = true;
				rb3 = cp + 9;
			}
			else if(strncmp(cp, "jedoch auch ", 11)==0) {
				isMatch = true;
				rb3 = cp + 10;
			}
			else if(strncmp(cp, "oder auch ", 10)==0) {
				isMatch = true;
				rb3 = cp + 9;
			}
			else if(strncmp(cp, "und auch ", 9)==0) {
				isMatch = true;
				rb3 = cp + 8;
			}
			else if(strncmp(cp, "so wie auch ", 11)==0) {
				isMatch = true;
				rb3 = cp + 10;
			}
			else if(strncmp(cp, "ebenso wie auch ", 14)==0) {
				isMatch = true;
				rb3 = cp + 13;
			}
		}

		if(!isMatch) {
			beg = anc + 1;
			continue;
		}

// now moving to the left from blank
if(prt) {
printf("\tmatch conj1 in \"%s\"\n", beg);
fflush(stdout);
}

		cp = anc-2;
		bool breakOnAlpha = false;
		while(cp>=beg) {
			if(!CharUtil::isAlphabetic(*cp)) {
				breakOnAlpha = true;
				break;
			}
			cp--;
		}
		if(breakOnAlpha)
			lb0 = cp + 1;
		else
			lb0 = beg;

		lb1 = lb0;
		rb1 = anc;
		lb2 = anc;
		rb2 = anc + 1;

// skip blanks after keyword
		cp = rb3 + 1;
		while(*cp) {
			if(!CharUtil::isBlank(*cp))
				break;
			cp++;
		}
		if(!CharUtil::isAlphabetic(*cp)) {
			beg = anc + 1;
			continue;
		}

		lb4 = cp;
		lb5 = beg-1; rb5 = beg-1;
		bool dashInRHS = false;
		while(*cp) {
			if(*cp=='-' && CharUtil::isAlphabetic(*(cp+1))) {
				lb5 = cp;
				dashInRHS = true;
				cp++;
				continue;
			}
			if(!(CharUtil::isAlphabetic(*cp)))
				break;
			cp++;
		}
		rb4 = cp;
		rb0 = cp;
		if(dashInRHS)
			rb5 = rb4;

		regex_.re_nsub = 5;
		registers.start[0] = lb0-buffer;
		registers.end[0] = rb0-buffer;
		registers.start[1] = lb1-buffer;
		registers.end[1] = rb1-buffer;
		registers.start[2] = lb2-buffer;
		registers.end[2] = rb2-buffer;
		registers.start[3] = lb3-buffer;
		registers.end[3] = rb3-buffer;
		registers.start[4] = lb4-buffer;
		registers.end[4] = rb4-buffer;
		if(dashInRHS) {
			registers.start[5] = lb5-buffer;
			registers.end[5] = rb5-buffer;
		} else {
			registers.start[5] = -1;
			registers.end[5] = -1;
		}

//printGroups(buffer);

		return true;
	}	
	return false;
}

//----  This rule is for german source.
// Its cost - 0.7s out of 19s (de2en, emiele2)
bool RegularExpression::matchGermanConjugation0(const char *buffer, int startPos) {
	char *beg =(char *)buffer + startPos;
	char *cp, *anc;
	char *lb0, *rb0, *lb1, *rb1, *lb2, *rb2, *lb3, *rb3, *lb4, *rb4, *lb5, *rb5;

bool prt = false;
	while(1) {
// The idea is to find a dash and then look to the right and to the left.
// On the dash's left there has to be spaces, keyword and 1+ alphas mixed w/ dashes.
// To the right - 1+ alpha.

		cp = beg;

		bool anchorFound = false;
		char *beg1 = beg;
		while(1) {
			cp = beg1;
			cp = strchr(cp, '-');
			if(cp==NULL)
				break;

			anc = cp;	// anchor set to dash position
			if(anc<beg+4) {
				beg1 = anc + 1;
				continue;
			}

			cp--;
			if(!CharUtil::isBlank(*cp)) {
				beg1 = anc + 1;
				continue;
			}
			anchorFound = true;
			break;
		}

		if(!anchorFound)
			return false;

		lb4 = anc;
		rb4 = anc + 1;

if(prt) {
	printf("\nspace+dash found in \"%s\"\n", beg);
	fflush(stdout);
}

// move to the left until non-blank met
		bool breakOnNotBlank = false;
		while(cp>=beg) {
			if(!CharUtil::isBlank(*cp)) {
				breakOnNotBlank = true;
				break;
			}
			cp--;
		}
		if(!breakOnNotBlank) {
			beg = anc + 1;
			continue;
		}

// now look for the blanks before keyword
		bool breakOnBlank = false;
		while(cp>=beg) {
			if(CharUtil::isBlank(*cp)) {
				breakOnBlank = true;
				break;
			}
			cp--;
		}
		if(!breakOnBlank) {
			beg = anc + 1;
			continue;
		}

		cp++;
// "cp" points to first non-blank and we check against magic words list.
if(prt) {
	printf("\ncandidate in \"%s\"\n", beg);
	fflush(stdout);
}
		bool isMatch = false;
		lb3 = cp;
		if(*cp=='u') {
			if(*(cp+1)) {
				if(*(cp+2) && CharUtil::isBlank(*(cp+2))) {
					isMatch = true;
					rb3 = cp + 2;
				}
			}
		}
		if(!isMatch) {
			if(strncmp(cp, "noch ", 5)==0) {
				isMatch = true;
				rb3 = cp + 4;
			}
			else if(strncmp(cp, "prep ", 5)==0) {
				isMatch = true;
				rb3 = cp + 4;
			}
			else if(strncmp(cp, "statt ", 6)==0) {
				isMatch = true;
				rb3 = cp + 5;
			}
			else if(strncmp(cp, "und ", 4)==0) {
				isMatch = true;
				rb3 = cp + 3;
			}
			else if(strncmp(cp, "oder ", 5)==0) {
				isMatch = true;
				rb3 = cp + 4;
			}
			else if(strncmp(cp, "bis ", 4)==0) {
				isMatch = true;
				rb3 = cp + 3;
			}
			else if(strncmp(cp, "bzw ", 4)==0) {
				isMatch = true;
				rb3 = cp + 3;
			}
			else if(strncmp(cp, "als auch ", 9)==0) {
				isMatch = true;
				rb3 = cp + 8;
			}
			else if(strncmp(cp, "ebenso wie ", 10)==0) {
				isMatch = true;
				rb3 = cp + 9;
			}
			else if(strncmp(cp, "sondern auch ", 12)==0) {
				isMatch = true;
				rb3 = cp + 11;
			}
			else if(strncmp(cp, "wie auch ", 9)==0) {
				isMatch = true;
				rb3 = cp + 8;
			}
			else if(strncmp(cp, "und/oder ", 9)==0) {
				isMatch = true;
				rb3 = cp + 8;
			}
			else if(strncmp(cp, "aber auch ", 10)==0) {
				isMatch = true;
				rb3 = cp + 9;
			}
			else if(strncmp(cp, "jedoch auch ", 11)==0) {
				isMatch = true;
				rb3 = cp + 10;
			}
			else if(strncmp(cp, "oder auch ", 10)==0) {
				isMatch = true;
				rb3 = cp + 9;
			}
			else if(strncmp(cp, "und auch ", 9)==0) {
				isMatch = true;
				rb3 = cp + 8;
			}
			else if(strncmp(cp, "so wie auch ", 11)==0) {
				isMatch = true;
				rb3 = cp + 10;
			}
			else if(strncmp(cp, "ebenso wie auch ", 14)==0) {
				isMatch = true;
				rb3 = cp + 13;
			}
		}

		if(!isMatch) {
			beg = anc + 1;
			continue;
		}
if(prt) {
	printf("\ncandidate match in \"%s\"\n", beg);
	fflush(stdout);
}

		cp--;
// "cp" now points to the blank before keyword.
// move to the left until non-blank met
		breakOnNotBlank = false;
		while(cp>=beg) {
			if(!CharUtil::isBlank(*cp)) {
				breakOnNotBlank = true;
				break;
			}
			cp--;
		}
		if(!breakOnNotBlank) {
			beg = anc + 1;
			continue;
		}

		if(!CharUtil::isAlphabetic(*cp)) {
			beg = anc + 1;
			continue;
		}

if(prt) {
	printf("\ncandidate good1 in \"%s\"\n", beg);
	fflush(stdout);
}

		rb1 = cp + 1;
// move to the left until not-alpha or not-dash is met
		bool thereWasDash = false;
		while(cp>=beg) {
			if(*cp=='-') {
				thereWasDash = true;
				lb2 = cp;
			}
			if(!CharUtil::isAlphabetic(*cp) && *cp!='-') {
				break;
			}
			cp--;
		}

		lb1 = cp+1;
		lb0 = cp+1;
		if(thereWasDash) {
			rb2 = rb1;
		}

// now move to the right from dash position
		cp = anc + 1;
		lb5 = cp;
		while(1) {
			if(!CharUtil::isAlphabetic(*cp))
				break;
			cp++;
		}
		if(cp==lb5)	{	// not-alpha next to dash
			beg = anc + 1;
			continue;
		}

		rb5 = cp;
		rb0 = cp;

		regex_.re_nsub = 5;
		registers.start[0] = lb0-buffer;
		registers.end[0] = rb0-buffer;
		registers.start[1] = lb1-buffer;
		registers.end[1] = rb1-buffer;
		if(thereWasDash) {
			registers.start[2] = lb2-buffer;
			registers.end[2] = rb2-buffer;
		} else {
			registers.start[2] = -1;
			registers.end[2] = -1;
		}
		registers.start[3] = lb3-buffer;
		registers.end[3] = rb3-buffer;
		registers.start[4] = lb4-buffer;
		registers.end[4] = rb4-buffer;
		registers.start[5] = lb5-buffer;
		registers.end[5] = rb5-buffer;

//printGroups(buffer);

		return true;
	}	
	return false;
}
