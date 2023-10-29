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
#ifndef __StartVariable_h__
#define __StartVariable_h__

//-------------------------------------------------------------------
// File - Variable.h
//
// Class - ST_Variable
//
// Description - a variable class used as a template argument for
//               the rule-engine templates
//
//-------------------------------------------------------------------

#include <logos_libs/regex/regularexpression.h>
#include <logos_libs/regex/matchinfo.h>
#include <logos_libs/utility/phrasemanager.h>
#include <logos_libs/linguistic/lword.h>
#include <logos_libs/ruleengine/phrasemgrposition.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Start {

// holds source side words - stored in an LWordVector - mirrored in a phrase manager
// holds a current position to match from

struct MatchSearchInfo
{
   MatchInfo *matchInfo_;
   RE_PhraseMgrPosition *searchPosition_;
};


class ST_Variable: public Object
{
public:
   // construct a variable given a list of source words
   ST_Variable(LWordVector& words);
   ~ST_Variable(void);

   // search the current sentence from the start position
   // updates the matched region
   bool matches(RegularExpression& pattern);

   // reset the start position - so searches start at the beginning
   void resetStartPos();

   // are we past the end of the buffer
   bool pastEndPos() const;

   // increment the starting position
   // sets the new starting position beyond the matched region
   void incrementStartPos();

   // does the matched region include any protected words - or previously recognized tokens
   bool protected_or_tokenized();

   // setTokenType functions
   // set first matching LWords to the given token type
   // set following matching LWords to tok_continuation

   // set token types as described above for the whole matched expression (group 0)
   void setTokenType(LookupTokenType::Type tokenType, bool useContinuation);

   // set token types for the tokens making up the subexpression for the input group
   void setTokenType(LookupTokenType::Type tokenType, int group, bool useContinuation);

   // use iterator range to set token types as described above
   void setTokenType(LookupTokenType::Type tokenType, LWordIterator start, LWordIterator end,
                     bool useContinuation);

   // groups consists of a list of groups to set the token types
   // eg LgsString("356") will operate on groups 3, 5, and 6
   // the list of groups may not be consecutive since parentheses may be nested
   // eg for "356", group 4 may be nested inside group 3
   void setTokenType(LookupTokenType::Type tokenType, const LgsString& groups, bool useContinuation);

   // get the first range of LWords whose token Type is tok_none
   void getBlankTokenRange(LWordIterator& start, LWordIterator& end);

   // replace text in the phrase manager and the source words
   // this should occur after token token types have been set, since some
   // tokens may be removed in the process and the iterators for the tokens will be invalid
   // if synch is true synchronize will be called at the end
   //     otherwise synchronize() must be called manually (eg after a number of replacements)
   void replace(int pos, int length, const LgsString& replacement, bool synch = true);

   // synchronize the word list data from the phrase manager data
   void synchronize();

   // remove the group from the phrase manager and the source words, via replace
   // see comments for replace
   void remove(int group);

   // replace characters in group that match source - with the target
   void convertGroup(int group, char source, char target, bool synchFlag = true);

   // get the matching data, and matched substring
   RegularExpression::MatchPos getMatch(int group = 0) const;
   const LgsString& getSubString(int group = 0) const;

   // get an iterator to the start word
   LWordIterator getStartWord();
   LWordIterator getEndWord();

   // get keys for current match
   // the match will span from startKey to endKey inclusively
   void getKeys(PhraseManager::Key& startKey, PhraseManager::Key& endKey, int group = 0) const;
   MatchSearchInfo matchAndSearchInfo(MatchSearchInfo & iMatchSearchInfo);
   PhraseManager* getPhraseManager();

private:
   LWordVector* sourceWords_;
   PhraseManager* phraseManager_;
   MatchInfo *matchInfo_;
   RE_PhraseMgrPosition *searchPosition_;

   // fill in the match details from the regular expression
   void getMatchDetails(const RegularExpression& pattern);
};

inline void ST_Variable::setTokenType(LookupTokenType::Type tokenType, bool useContinuation)
{
   setTokenType(tokenType, 0, useContinuation);
}

inline bool ST_Variable::pastEndPos() const
{
   return searchPosition_->pastEnd();
}

inline void ST_Variable::resetStartPos()
{
   searchPosition_->reset();
}

inline void ST_Variable::incrementStartPos()
{
   searchPosition_->increment();
}

inline RegularExpression::MatchPos ST_Variable::getMatch(int group) const
{
   return matchInfo_->getMatch(group);
}

inline const LgsString& ST_Variable::getSubString(int group) const
{
   return matchInfo_->getSubString(group);
}

inline LWordIterator ST_Variable::getStartWord()
{
   return sourceWords_->begin();
}

inline LWordIterator ST_Variable::getEndWord()
{
   return sourceWords_->end()-1;
}

inline PhraseManager* ST_Variable::getPhraseManager()
{
   return phraseManager_;
}
//}

#endif



