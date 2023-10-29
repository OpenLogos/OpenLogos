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
/*******************************************************************
 *
 *    DESCRIPTION:
 *
 *    AUTHOR:
 *
 *    HISTORY:    
 *
 *******************************************************************/

#ifndef __StemReplaceRule_Inc__
#define __StemReplaceRule_Inc__

class LLanguage;

class StemReplaceRule
{
public:
   StemReplaceRule(const LgsString& s_, const LLanguage& l_);
   virtual ~StemReplaceRule();

   void fire(LgsString* s_);

private:
   void _initialize(const LgsString& s_);
   void _match(LgsString* s_);
   void _replace(LgsString* s_);
   void _matchAnyConsonant(LgsString* s_);
   void _matchAnyVowel(LgsString* s_);
   void _matchAnyDiacritical(LgsString* s_);
   void _matchAnyCharacter(LgsString* s_);
   void _matchInSeparablePrefix(LgsString* s_);
   void _matchSeparablePrefix(LgsString* s_);
   void _matchLiteral(LgsString* s_);
   void _matchAnyAccent(LgsString* s_);
   void _replaceAnyWildCard(LgsString* s_);
   void _replaceInSeparablePrefix(LgsString* s_);
   void _replaceSeparablePrefix(LgsString* s_);
   void _replaceLiteral(LgsString* s_);
   void _replaceAccentedChar(LgsString* s_);
   void _adjustDiphthongPos(const LgsString& s_, int i_);
   void _makeNewAndOld();
   bool _isSymbol(const char c_);

   enum {_vowelCount=5, _xRefOrigin = 127, _numAccentRows = 58};

   const static int  _xRefTable[];
   const static char _accentTable[_numAccentRows][10];
   const static char _accentedUnaccentedMap[_vowelCount*2][3];
   
   const LLanguage& _language;
   
   LgsString   _lhs;    // lefthand side of the rule
   LgsString   _rhs;    // righthand side of the rule

   int      _count;
   LgsString   _old;
   LgsString   _new;
   int      _code;
   int      _startingPosition;
   int      _replaceLength;
   int      _rowPosition;
   
};

#endif


