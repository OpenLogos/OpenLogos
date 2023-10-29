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
//-------------------------------------------------------------------
// File - French.cpp
//
// Class - French
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/french.h>
#include <logos_libs/linguistic/targetsentenceunit.h>
#include <logos_libs/startrules/startruleslocale.h>

French::French()
       :LLanguage(FrenchID, "French")
{
   setVowels("aeiouy‡·‚ËÈÍÎÓÔ˘˚¸");
   setConsonants("bcdfghjklmnpqrstvwxzÁ");
   setDiacritics("aeiouy‡·‚ËÈÍÎÓÔ˘˚¸");
}

void French::findAspirePoints(TargetSentenceUnit* prev, TargetSentenceUnit* curr, LgsVector(int)* positions) const
{
   // After calling the parent class it looks for additional aspiration points for the
   // French language. In particular, all words in a phrase, after the first word,
   // should be precluded from the contracting the starting 'h' into the previous word.
   // This is done by aspiring following words that begin in 'h'.

   LLanguage::findAspirePoints(prev, curr, positions);

   int pos = 0;
   LgsString searchKey("hH");
   LgsString s = curr->surfaceExpressionAsString();

   while ((pos = s.find_first_of(searchKey, pos + 1)) != LgsString::npos)
   {
      if (CharUtil::isBlank(s[pos - 1])) // was the 'h' at the start of word
      {
         positions->push_back(pos);
      }
   }
}

bool French::isOrdinalSuffix(const LgsString& s) const
{
   return s == ".";
}

