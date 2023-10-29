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
#ifndef __GMatcher_h__
#define __GMatcher_h__

//-------------------------------------------------------------------
// File - GMatcher.h
//
// Class - GMatcher (interface)
//
//-------------------------------------------------------------------

#include <logos_libs/gerdem/gabstractmatcher.h>
#include <logos_libs/gerdem/gmatchcache.h>
#include <logos_libs/linguistic/linguisticfactory.h>
#include <logos_libs/linguistic/lroot.h>
#include <logos_libs/gerdem/phrasematchsequencer.h>

class GMatcher: public GAbstractMatcher
{
public:
   GMatcher();
   virtual ~GMatcher();

   //---------------------------------------------------------------
   // The FindWordMatch() member function returns a pointer to a
   // DWordMatch object that is considered the best match for the
   // vector of GWord objects passed in as an arguement.
   //---------------------------------------------------------------

   // not const due to phrase cache
   virtual LDictionaryTokenVector* FindPhraseMatch(const LWordIterator& begin,
                                                   const LWordIterator& end,
                                                   const LgsString& firstWord,
                                                   const LgsString& secondWord,
                                                   short noSpaces,
                                                   PhraseMatchSequencer::RootAnalysisCode analysisCode);

   // not const due to phrase cache
   virtual DWordPhraseVector* findPhraseCandidates(const LgsString& first, short firstSpaceCount,
                                                   const LgsString& second);
   virtual LDictionaryTokenVector* FindWordMatch(const LTextualComponent&, bool BOS, bool bAllCapitalWords);
   virtual DWordPhraseVector* FindCachedMatch(const LgsString&, bool BOS) const;
   virtual void InsertMatchIntoCache(const LgsString&, DWordPhraseVector*, bool BOS);
   virtual void Factory(LinguisticFactory*);
   virtual LinguisticFactory& Factory();
   virtual LinguisticFactory& Factory() const;
   void CleanupMemory();

protected:
   void populateCandidates(DWordPhraseVector* pMatches, LDictionaryTokenVector* pCandidates,
                           const LWordIterator& beginSource, const LWordIterator& endSource,
                           PhraseMatchSequencer::RootAnalysisCode analysisCode,
                           const LgsString& firstWord);

   DWordPhraseIterator EndOfWordCountRange(DWordPhraseIterator& begin,
                                           DWordPhraseIterator& end)  const;

   void ParseIntoWords(DWordPhraseIterator& wordMatch, LgsVector(LgsString)& resultWords,
                       const LLanguage* aLanguage) const;

   void MultiExactMatch(LDictionaryTokenVector* pCandidates,
                        DWordPhraseIterator& begin, DWordPhraseIterator& end,
                        const LWordIterator& beginSource, const LWordIterator& endSource) const;

   void MultiInflectMatch(LDictionaryTokenVector* pCandidates, DWordPhraseIterator& begin,
                          DWordPhraseIterator& end, const LWordIterator& beginSource) const;

   void AdjNPmatch(LDictionaryTokenVector* pResults, DWordPhraseIterator& rangeBegin,
                   DWordPhraseIterator& rangeEnd, const LWordIterator& beginSource,
					    const LgsString& firstWord) const;
   void determinePhraseWordCount(DWordPhraseVector*) const;
   void orderPhrases(DWordPhraseVector*) const;

private:
   GMatchCache* v_matchCache;

   LinguisticFactory* p_factory;

   // simplistic phrase cache here - needs to be expanded to 2 maps
   DWordPhraseVector* v_phraseCache[2];
   DWordPhraseVector* getCachedPhrases(const LgsString& first, short spaceCount,
                                       const LgsString& second, int cacheMatch) const;
   void setCachedPhrases(const LgsString& first, const LgsString& second,
                         DWordPhraseVector* pMatches, int cacheMatch);
};

//---------------------------------------------------------------------
inline DWordPhraseVector* GMatcher::FindCachedMatch(const LgsString& s, bool BOS) const
{
   return v_matchCache->FindMatch(s, BOS);
}
//---------------------------------------------------------------------
inline void GMatcher::InsertMatchIntoCache(const LgsString& s, DWordPhraseVector* m, bool BOS)
{
   v_matchCache->InsertMatch(s, m, BOS);
}
//---------------------------------------------------------------------
inline void GMatcher::Factory(LinguisticFactory* f)
{
   p_factory = f;
}
//---------------------------------------------------------------------
inline LinguisticFactory& GMatcher::Factory()
{
   assert(p_factory);
   return *p_factory;
}
//---------------------------------------------------------------------
inline LinguisticFactory& GMatcher::Factory() const
{
   assert(p_factory);
   return *p_factory;
}
//---------------------------------------------------------------------
inline void GMatcher::CleanupMemory()
{
   v_matchCache->CleanupMemory();
}

#endif // __GMatcher_h__

