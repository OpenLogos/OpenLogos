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
//----------------------------------------------------------------------------
// File - Variable.cpp
//
// Class - ST_Variable
//
//----------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/startrules/variable.h>
#include <logos_libs/ruleengine/util.h>

ST_Variable::ST_Variable(LWordVector& words)
            :phraseManager_(new PhraseManager),
             sourceWords_(&words)
{
   matchInfo_ = new MatchInfo;
   searchPosition_ = new RE_PhraseMgrPosition(phraseManager_, *matchInfo_);
   RE_Util::loadPhraseManager(sourceWords_, phraseManager_);
}

ST_Variable::~ST_Variable(void)
{
   if (matchInfo_)
   {
      delete matchInfo_;
      matchInfo_ = 0;
   }
   if (searchPosition_)
   {
      delete searchPosition_;
      searchPosition_ = 0;
   }
   if (phraseManager_)
   {
      delete phraseManager_;
      phraseManager_ = 0;
   }
}

bool ST_Variable::protected_or_tokenized()
{
   assert(matchInfo_->getMatch().length != 0);

   PhraseManager::Key startKey, endKey;
   getKeys(startKey, endKey);
   LWordIterator start = getStartWord() + startKey;
   LWordIterator end = getStartWord() + endKey;

   for (LgsVector(LWord)::const_iterator iter = start; iter != end; iter++)
   {
      if (iter->isProtectedWord() || iter->getTokenType() != LookupTokenType::tok_none)
         return true;
   }

   return false;
}

void ST_Variable::getMatchDetails(const RegularExpression& pattern)
{
   matchInfo_->reset(pattern, phraseManager_->getText());
}

bool ST_Variable::matches(RegularExpression& pattern)
{
   if (!pattern.matches(phraseManager_->getText(), searchPosition_->get()))
      return false;
   getMatchDetails(pattern);
   return true;
}

void ST_Variable::getKeys(PhraseManager::Key& startKey, PhraseManager::Key& endKey, 
                          int group) const
{
   RE_Util::getKeys(*matchInfo_, phraseManager_, startKey, endKey, group);
}

void ST_Variable::setTokenType(LookupTokenType::Type tokenType, LWordIterator start,
                               LWordIterator end, bool useContinuation)
{
   for (LWordIterator iter = start; iter != end; iter++)
   {
      iter->setTokenType(tokenType);
      if (useContinuation)
         tokenType = LookupTokenType::tok_continuation;
   }
}

void ST_Variable::setTokenType(LookupTokenType::Type tokenType, int group, bool useContinuation)
{
   PhraseManager::Key startKey, endKey;
   getKeys(startKey, endKey, group);
   LWordIterator start = getStartWord() + startKey;
   LWordIterator end = getStartWord() + endKey;

   setTokenType(tokenType, start, end, useContinuation);
}

void ST_Variable::setTokenType(LookupTokenType::Type tokenType, const LgsString& groups, bool useContinuation)
{
   for (LgsString::const_iterator iter = groups.begin(); iter != groups.end(); iter++)
   {
      int group = *iter - '0';
      assert(0 <= group && group < matchInfo_->getGroups());
      if (matchInfo_->getMatch(group).length > 0)
      {
         setTokenType(tokenType, group, useContinuation);
         if (useContinuation)
            tokenType = LookupTokenType::tok_continuation;
      }
   }
}

void ST_Variable::getBlankTokenRange(LWordIterator& start, LWordIterator& end)
{
   PhraseManager::Key startKey, endKey;
   getKeys(startKey, endKey);
   LWordIterator start1 = getStartWord() + startKey;
   LWordIterator end1 = getStartWord() + endKey;

   start = end1; end = start1; // to be able to check assertion

   for (LWordIterator iter = start1; iter != end1; iter++)
   {
      if (iter->getTokenType() == LookupTokenType::tok_none)
      {
         start = iter;
         for (iter++; iter != end1 && iter->getTokenType() == LookupTokenType::tok_none; iter++)
         ;
         end = iter;
         break;
      }
   }

   assert(start <= end);
}

void ST_Variable::replace(int pos, int length, const LgsString& replacement, bool synch)
{
   phraseManager_->replace(pos, length, replacement);
   searchPosition_->increment(replacement.length() - length);
   if (synch)
      synchronize();
}

void ST_Variable::synchronize()
{
   RE_Util::updateSourceWords(sourceWords_, phraseManager_);
}

void ST_Variable::remove(int group)
{
   RegularExpression::MatchPos match = getMatch(group);
   replace(match.pos, match.length, LgsString());
}

void ST_Variable::convertGroup(int group, char source, char target, bool synchFlag)
{
   RegularExpression::MatchPos match = getMatch(group);
   LgsString subString = getSubString(group);
   assert(subString.length() == match.length);

   for (int i = 0; i < match.length; i++)
   {
      if (subString[i] == source)
         replace(match.pos + i, 1, LgsString(1, target), false);
   }

   if (synchFlag)
   {
      synchronize();
   }
}
 
MatchSearchInfo ST_Variable::matchAndSearchInfo(MatchSearchInfo & iMatchSearchInfo)
{
   MatchSearchInfo retVal;

   retVal.matchInfo_ = matchInfo_;
   retVal.searchPosition_ = searchPosition_;
   if (0 == iMatchSearchInfo.matchInfo_)
   {
      iMatchSearchInfo.matchInfo_ = new MatchInfo;
   }
   matchInfo_ = iMatchSearchInfo.matchInfo_;

   if (0 == iMatchSearchInfo.searchPosition_)
   {
      iMatchSearchInfo.searchPosition_ = new RE_PhraseMgrPosition(phraseManager_, *matchInfo_);
   }
   searchPosition_ = iMatchSearchInfo.searchPosition_;

   return retVal;
}


