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
#ifndef __dictionaryentrybuilder_h__
#define __dictionaryentrybuilder_h__

//-------------------------------------------------------------------
// File - dictionaryentrybuilder.h
//
// Class - DictionaryEntryBuilder
//
// Description - This is a part of an application of the "Builder"
//      pattern.
//
// Patterns
//      Builder
//
//-------------------------------------------------------------------

class IniParser;


#include <logos_libs/linguistic/entrybuilder.h>
#include <logos_libs/gerdem/gabstractmatcher.h>
#include <logos_libs/linguistic/lsemantosyntacticunit.h>
#include <logos_libs/linguistic/linflection.h>
#include <logos_libs/linguistic/lsubjectmatter.h>
#include <logos_libs/entity/dderivedform.h>
#include <logos_libs/gerdem/gwordcache.h>
#include <logos_libs/gerdem/gentrycache.h>
#include <logos_libs/gerdem/cardinalprototypemap.h>
#include <logos_libs/gerdem/patternprototypemap.h>
#include <logos_libs/linguistic/ldictionarytoken.h>
#include <logos_libs/entity/dwordphrase.h>
#include <logos_libs/linguistic/linguisticfactory.h>
#include <logos_libs/linguistic/ldictionaryentry.h>
#include <logos_libs/linguistic/lgerman.h>
#include <logos_libs/gerdem/wordgroupmanager.h>

class HNounDepWrdLookup;
class SsuList;
class GermanTokenWord;

class DictionaryEntryBuilder: public EntryBuilder
{
public:
   enum WordCategory
   {
      NonGerman,
      GermanNonHead,
      GermanHead,
      GermanWord
   };

   //---------------------------------------------------------------
   // The destructor will perhaps do nothing for this class but it
   // needs to be declared virtual so that the delete of a pointer
   // to this abstract class will cause the subclass's destructor to
   // be executed.
   //---------------------------------------------------------------
   DictionaryEntryBuilder();
   virtual ~DictionaryEntryBuilder();

   GAbstractMatcher& matcher();
   virtual LinguisticFactory& factory();

   void setMatcher(GAbstractMatcher*);
   virtual void setFactory(LinguisticFactory*);

   virtual LDictionaryEntry* buildEntry(const LWordIterator& begin, const LWordIterator& end,
                                        bool BOS, LWordVector * srcWrdVector,
                                        bool bAllCapitalWords);
   virtual LDictionaryEntry* buildProtectedEntry(const LWord&);
   virtual LDictionaryEntry* buildProtectedWordEntry(const LWordIterator& word, LgsString& protectedString);
   virtual LDictionaryEntry* buildPunctuationEosEntry(const LWord&);
   virtual LDictionaryEntry* buildSpecialEntry(const LWordIterator& begin, const LWordIterator& end );

   virtual LDictionaryEntry* bosEntry();
   virtual LDictionaryEntry* eosEntry();
   virtual LDictionaryEntry* unfoundEntry(UnfoundTYPE typeOfUnfound, const LWord &word);

   virtual LDictionaryEntry* createTempBosEntry(const LgsString&);
   virtual LDictionaryEntry* createTempEosEntry(const LgsString&);

   void loadWordList();
   void CleanupMemory();

   //---------------------------------------------------------------
   // The following methods provide access to the various caches used by the builder.
   // These would be protected accept they are used for output during development.
   //
   // UnfoundWordCachePre() - returns a cache that contains unfound words that are
   //                         unrelated to phrases.
   // UnfoundWordCachePost() - returns a cache that contains unfound words that are
   //                          related to phrases.
   // NoPhraseWordCache() -
   //---------------------------------------------------------------
   GWordCache& unfoundWordCachePre();
   GWordCache& noPhraseWordCache();
   GWordCache& phraseWordCache();
   GEntryCache& entryCache();

   int wordCount()const;
   LDictionaryEntry *matchSingleWord(const LWordIterator & beginSource, bool BOS,
                                     WordCategory wrdCategory,
                                     LWordVector * srcWrdVector, bool halfNounDepLookup,
                                     bool bAllCapitalWords);
   LDictionaryEntry* matchText(const LTextualComponent&, bool BOS, WordCategory wrdCategory = NonGerman,
                               bool cacheEntry = true, bool bAllCapitalWords = false,
							   GermanTokenWord* wordPart = 0);
   LDictionaryEntry* matchRoot(const LWord&, WordCategory wrdCategory, bool BOS, bool bAllCapitalWords);
   LDictionaryEntry* matchNothing(const LWord&, bool BOS = false);
   bool populateCompoundDE(LCompoundDictionaryEntry * dictEntry, LWord &pWord, bool headPresent,
                           bool hyphenated, bool beginsUpperCase, bool BOS, bool bAllCapitalWords);

protected:
   //---------------------------------------------------------------
   // for a non-head word - select a single ssu out of the list
   // remove all other ssu's in the list
   // see swork.txt file for details
   //---------------------------------------------------------------
   void adjustSsuList(LgsVector(LSemantoSyntacticUnit)& ssuList, GermanTokenWord * wordPart);
   void removeInconsistentSsus(LgsVector(LSemantoSyntacticUnit)& ssuList, GermanTokenWord *wordPart);
   void adjustSSUBasedOnOvrlpPriority(LgsVector(int)& ovrlpPriorityLst,
                                      LgsVector(LSemantoSyntacticUnit)& ssuList);
   bool limitSSUsForWClass(LgsVector(LSemantoSyntacticUnit)& ssuList, int wordClass);
   bool limitSSUsStoredHalfNoun(LgsVector(LSemantoSyntacticUnit)& ssuList);
   void loadValue(IniParser & argParser, const LgsString & argDesc, int & wcParameter);
   void loadOverlapPriorNos(IniParser & argParser, const LgsString & argDesc,
                            LgsVector(int) & ovrlpPriorNos);

   static int storedHN;
   static int nounWC;
   static int verbWC;
   static int adjWC;
   static int storedHNprior;

   LDictionaryEntry* matchPrePhrase(const LWordIterator& beginSource, const LWordIterator& endSource);
   LDictionaryEntry* matchPhrase(const LWordIterator& beginSource, const LWordIterator& endSource,
                                 bool BOS);
   LDictionaryEntry* matchCompound(LWordIterator, bool BOS, bool hyphenated,
                                   LWordIterator endIter, bool bAllCapitalWords);

   void mergeCaseSensitivity(LDictionaryTokenVector&  bestWord, LDictionaryTokenIterator begin,
                             LDictionaryTokenIterator end) const;
   void mergeCompanyWords(LDictionaryTokenVector& bestWord, LDictionaryTokenIterator begin,
                          LDictionaryTokenIterator end) const;

   bool screenInflections(LDictionaryEntry*) const;
   LDictionaryEntry* makeAlphanumericEntry();

   LDictionaryEntry* createEntry(LDictionaryTokenVector* candidates, const LTextualComponent &matchText,
                                 bool BOS, WordCategory wrdCategory = NonGerman, bool cacheEntry = true,
								 GermanTokenWord* wordPart = 0);

   LDictionaryEntry* createRootEntry(LDictionaryTokenVector* candidates, const LTextualComponent* matchText,
                                     const LTextualComponent* sourceWord, WordCategory wrdCategory, bool BOS);

   void processMidSntnceCapWrd(SsuList & ssuLst, bool phraseEntry = false);
   void processMidSntnceSmallWrd(SsuList & ssuLst);
   LDictionaryEntry* createPhraseEntry(LDictionaryTokenVector* candidates, const LWordIterator& beginSource,
                                       const LWordIterator& endSource, bool BOS);
   void initializeWordGroupManager(LWordVector * srcWords);
   void updateWordPosition(int wordCount, LWordIterator startSeq);

   bool isPhrase( const LWordIterator& beginSource, const LWordIterator& endSource );
   void removeSuffix(const LWordIterator & wordIter, const LgsString & suffix);
   void replaceThousandSeparator(const LWordIterator & beginWord, const LWordIterator & endWord, short decimalPos);
   short replaceDecimal(const LWordIterator & beginWord, const LWordIterator & endWord);

private:
   GEntryCache v_entryCache;
   GEntryCache v_rootEntryCache;
   GEntryCache v_compoundEntryCache;
   GWordCache v_noPhraseWordCache;
   GWordCache v_phraseWordCache;
   GWordCache v_unfoundWordCachePre;
   GWordCache v_unfoundWordCachePost;
   int v_unfoundWordID;

   GAbstractMatcher* p_matcher;
   LinguisticFactory* p_factory;
   PatternPrototypeMap v_punctuationPatternPrototypes;
   int v_wordCount;
   WordGroupManager v_wgManager;
   bool v_possibleEOS;

   LgsSet(LgsString) m_firstWordList;
};
//---------------------------------------------------------------------
inline GEntryCache& DictionaryEntryBuilder::entryCache()
{
   return v_entryCache;
}
//---------------------------------------------------------------------
inline GWordCache& DictionaryEntryBuilder::noPhraseWordCache()
{
   return v_noPhraseWordCache;
}
//---------------------------------------------------------------------
inline GWordCache& DictionaryEntryBuilder::phraseWordCache()
{
   return v_phraseWordCache;
}
//---------------------------------------------------------------------
inline GWordCache& DictionaryEntryBuilder::unfoundWordCachePre()
{
   return v_unfoundWordCachePre;
}
//---------------------------------------------------------------------
inline GAbstractMatcher& DictionaryEntryBuilder::matcher()
{
   return *p_matcher;
}
//---------------------------------------------------------------------
inline LinguisticFactory& DictionaryEntryBuilder::factory()
{
   return *p_factory;
}
//---------------------------------------------------------------------
inline int DictionaryEntryBuilder::wordCount()const
{
   return v_wordCount;
}
//---------------------------------------------------------------------
inline void DictionaryEntryBuilder::CleanupMemory()
{
   v_wgManager.CleanupMemory();
   v_entryCache.CleanupMemory();
   v_rootEntryCache.CleanupMemory();
   v_compoundEntryCache.CleanupMemory();
}

#endif // __dictionaryentrybuilder_h__

