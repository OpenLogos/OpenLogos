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
// File - RegularExpression.h
//
// wrapper around gnu's regex source code
//
// note: the style of regular expressions we use is
//       RE_SYNTAX_POSIX_EXTENDED representing the posix standard
//       see regex.h for explanation
//       http://funnelweb.utcc.utk.edu/~harp/gnu/regex/regex_toc.html gives gnu documentation

#ifndef __RegularExpression_h__
#define __RegularExpression_h__

#include <logos_include/lgsstring.h>
#if _MSC_VER >= 1100
   using namespace std;
#elif !defined(OS_NO_NAMESPACE)
   //using namespace OS_STD_NAMESPACE;
   using namespace std;
#endif

#include "regex.h"

// note: ']' must be 1st char in list otherwise it terminates the list
// note: '-' must be the last (or first) char in list otherwise it constitutes a range
// note: we do not escape chars in lists, and all other chars represent themselves

class RegularExpression
{
public:
   enum { MaxGroups = RE_NREGS };         // maximum number of groups
   enum Borders { Left, Right, Both, None, Contract, ElideLeft, ElideRight, ElideBoth };

   static const LgsString escapeSymbol;
   static const LgsString leftParen;
   static const LgsString rightParen;
   static const LgsString leftBoundary;
   static const LgsString leftElisionBoundary;
   static const LgsString leftContractBoundary;
   static const LgsString rightContractBoundary;
   static const LgsString rightBoundary;
   static const LgsString rightElisionBoundary;

   struct MatchPos
   {
      enum { invalid = -1 };

      MatchPos() {}

      int pos;    // starting position       - if -1 it means that no match occurred
      int length; // length of match         - if 0 it means a null LgsString matched

      MatchPos(int start, int end);
   };

   // literal(s) is a LgsString of literal characters that needs to be inserted in a regular expression
   //     but not as part of a list
   // insert an escape character '\' before any special characters that need them
   // (actually we insert escape char before all non-alphanumeric characters
   // except for some punctuation and special characters which change their meaning when
   // quoted: single and double quotes and < and >
   static LgsString escapeForm(const LgsString& literals);
   static LgsString escapeForm(char literal);

   // Construct a new regular expression - compiles the expression.
   // The same expression can be used to match multiple strings.
   // Borders may be added to the regular expression by setting the borders flags.
   //
   //     if a left border is added
   //         the expression becomes (left_border)(expression)
   //         (2 + left_groups) extra groups are introduced
   //     if a right border is added
   //         the expression becomes (expression)(right_border)
   //         (2 + right_groups) extra groups are introduced
   //     if both a left and a right border are added
   //         the expression becomes (left_border)(expression)(right_border)
   //         (3 + left_groups + right_groups) extra groups are introduced
   //     the extra groups are made invisible to client code
   //         the groups() method subtracts the extra groups
   //         the match(group) method adjusts group to allow for the extra groups
   RegularExpression(const LgsString &expression, Borders borders);

   ~RegularExpression();

   static void printTimes();

   void printGroups(const char *txt);

	// return the number of groups in the expression that was input at construction time
   // there is always at least one group - the whole regular expression
   // each set of brackets contributes one more group
   // The groups do not include the borders added
   // The groups are numbered 0 for the whole expression, 1 for the 1st left bracket, etc
   int groups() const;

   // match against a LgsString - returns true iff a match occurred
   // position is the start position
   // length is the length of the substring - if 0 match till the end of the LgsString
   bool matches(const LgsString &buffer, int startPosition = 0);

   // match against a range of characters
   bool matches(const char *buffer, int bufferLength, int startPosition = 0);

   // return the match location of the given group - group 0 represents the whole expression
   // group must be in the range from 0 up to and including groups()
   // matches() should be called first
   MatchPos match(int group = 0) const;

   // return the expression with borders excluded
   const LgsString& getExpression() const;

   // return the left and right borders; an empty LgsString denotes no border
   const LgsString& getLeftBorder() const;
   const LgsString& getRightBorder() const;

private:
	bool matchStartRuleCatchAllFirst(const char *buffer, int startPos);
	bool matchStartRuleCatchAllSecond(const char *buffer, int startPos);
	bool matchStartRuleDottedAlphanum(const char *buffer, int startPos);
	bool matchStartRuleLookupNeededFirst(const char *buffer, int startPos);
	bool matchStartRuleLookupNeededSecond(const char *buffer, int startPos);

	bool matchStartRuleHyphensequence(const char *buffer, int startPos);
	bool matchStartRuleMultiHyphenAlphanum(const char *buffer, int startPos);
	bool matchStartRuleOrdinal(const char *buffer, int startPos);
	bool matchStartRuleNumeric(const char *buffer, int startPos);
	bool matchStartRuleNumericHyphenAlphanum(const char *buffer, int startPos);
	bool matchStartRuleTime(const char *buffer, int startPos);

	bool matchGermanConjugation0(const char *buffer, int startPos);
	bool matchGermanConjugation1(const char *buffer, int startPos);

	bool matchFinishRule101(const char *buffer, int startPos);
	bool matchFinishRule205(const char *buffer, int startPos);
	bool matchFinishRule218or220(const char *buffer, int startPos);

	bool matchFinishRule206(const char *buffer, int startPos);
	bool matchFinishRule207or208(const char *buffer, int startPos);

	LgsString expression_;         // the expression to be parsed
   Borders borders_;           // whether to add borders on either side of the regular expression
   LgsString leftBorder_;         // the left border
   LgsString rightBorder_;        // the right border
   regex_t regex_;             // structure used to call gnu regular expression code
   regoff_t start_[MaxGroups]; // buffers used by registers
   regoff_t end_[MaxGroups];   // buffers used by registers

#ifdef UNICODE
#error This needs to be re-written for unicode
#endif
#define BYTEWIDTH 8

   // fastmap used in gnu regular expression code to speed up searches
   char fastmap[(1 << BYTEWIDTH)];

   re_registers registers;   // gnu regular expression registers to record group matches
   int calls_;               // number of times matches has been called

   int regid;	// id is used for identifying whether this
				// regex is expensive and subject to euristic
};

inline const LgsString& RegularExpression::getExpression() const
{
   return expression_;
}

inline const LgsString& RegularExpression::getLeftBorder() const
{
   return leftBorder_;
}

inline const LgsString& RegularExpression::getRightBorder() const
{
   return rightBorder_;
}

inline LgsString RegularExpression::escapeForm(char literal)
{
   return escapeForm(LgsString(1, literal));
}

#endif

