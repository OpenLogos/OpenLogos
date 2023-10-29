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
// Class - PM_Variable
//
//----------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/patternmatcher/variable.h>
#include <logos_libs/ruleengine/util.h>

PM_Variable::PM_Variable(LWordVector& words)
            :source_(true),
             sourceWords_(&words),
             phraseManager_(new PhraseManager),
             searchPosition_(phraseManager_, matchInfo_)
{
    RE_Util::loadPhraseManager(sourceWords_, phraseManager_);
}

PM_Variable::PM_Variable(TargetUnitVector& words)
            :source_(false),
             targetWords_(&words),
             phraseManager_(new PhraseManager),
             searchPosition_(phraseManager_, matchInfo_)
{
    RE_Util::loadPhraseManager(targetWords_, phraseManager_);
}

PM_Variable::~PM_Variable()
{
   if (phraseManager_)
   {
      delete phraseManager_;
   }
}

bool PM_Variable::matches(RegularExpression& pattern)
{
//printf("PM_Variable.matches \"%s\" in \"%s\", pos=%d\n",
//	   pattern.getExpression().c_str(), phraseManager_->getText().c_str(),
//		searchPosition_.get());
//fflush(stdout);
   if (!pattern.matches(phraseManager_->getText(), searchPosition_.get()))
      return false;
//printf("MATCH\n");
//fflush(stdout);
   getMatchDetails(pattern);
   return true;
}

void PM_Variable::getMatchDetails(const RegularExpression& pattern)
{
   matchInfo_.reset(pattern, phraseManager_->getText());
   assert(matchInfo_.getMatch().pos != -1);
}

void PM_Variable::replace(LgsString replacement)
{
   LgsString temp = toString();

   RegularExpression::MatchPos match = matchInfo_.getMatch();
   phraseManager_->replace(match.pos, match.length, replacement);

   searchPosition_.increment(replacement.length() - match.length);
   matchInfo_.adjustMatchLength(replacement.length());
}

void PM_Variable::synchronize()
{
   assert(source_);

   RE_Util::updateSourceWords(sourceWords_, phraseManager_);
}

bool PM_Variable::matchProtected() const
{
   // no protection on target side
   if (!source_)
      return false;

   RegularExpression::MatchPos match = matchInfo_.getMatch();
   if (match.length == 0)
      return false;

   PhraseManager::Key startKey, endKey;
   getKeys(startKey, endKey);

   for (LWordIterator iter = sourceWords_->begin() + startKey; iter != sourceWords_->begin() + endKey; iter++)
   {
      if (iter->isProtectedWord())
         return true;
   }

   return false;
}

void PM_Variable::protectMatch()
{
   assert(source_);

   RegularExpression::MatchPos match = matchInfo_.getMatch();
   if (match.length == 0)
      return;

   PhraseManager::Key startKey, endKey;
   getKeys(startKey, endKey);

   for (LWordIterator iter = sourceWords_->begin() + startKey; iter != sourceWords_->begin() + endKey; iter++)
      iter->setProtectedWord();
}

void PM_Variable::synchronizeTargetUnits()
{
   assert(!source_);

   PhraseManager::Key current = 0;
   for (TargetUnitIterator iter = targetWords_->begin(); iter != targetWords_->end(); ++iter, ++current)
   {
      if (phraseManager_->phraseExists(current))
      {
         if (phraseManager_->phraseChanged(current))
         {
            int blanks;
            LgsString text = phraseManager_->getPhrase(current, &blanks);
            (*iter)->surfaceExpressionFromString(text);
            (*iter)->setTrailingSpaces(blanks);
         }
      }
      else
      {
         (*iter)->surfaceExpressionFromString(LgsString());
         (*iter)->setTrailingSpaces(0);
      }
   }
}

void PM_Variable::getKeys(PhraseManager::Key& startKey, 
                          PhraseManager::Key& endKey) const
{
   RE_Util::getKeys(matchInfo_, phraseManager_, startKey, endKey, 0);
}


