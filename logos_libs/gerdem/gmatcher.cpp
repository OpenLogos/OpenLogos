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
//---------------------------------------------------------------------
// File - GMatcher.cpp
//
// Class - GMatcher (implementation)
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/gmatcher.h>
#include <logos_libs/linguistic/linguisticfactory.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/linguistic/ldictionary.h>
#include <logos_libs/gerdem/grootedword.h>
#include <logos_libs/gerdem/gphrasequery.h>
#include <logos_libs/translutility/translcommonobjects.h>

#ifdef HAVE_STRNCASECMP
#define strnicmp strncasecmp
#endif

//-------------------------------------------------------------------
GMatcher::GMatcher()
         :v_matchCache(0)
{
   v_matchCache = new GMatchCache();
   v_phraseCache[0] = v_phraseCache[1] = 0;
}
//-------------------------------------------------------------------
GMatcher::~GMatcher()
{
   delete v_matchCache;
   if (v_phraseCache[0])
   {
      (v_phraseCache[0])->clear();
      delete v_phraseCache[0];
   }
   if (v_phraseCache[1])
   {
      (v_phraseCache[1])->clear();
      delete v_phraseCache[1];
   }
}
//-------------------------------------------------------------------
//-------------------------------------------------------------------
LDictionaryTokenVector* GMatcher::FindPhraseMatch(const LWordIterator& beginSource,
                                                  const LWordIterator& endSource,
                                                  const LgsString& firstWord,
                                                  const LgsString& secondWord,
                                                  short noSpaces,
                                                  PhraseMatchSequencer::RootAnalysisCode analysisCode)
{
   // Creates the set of candidate match words for the words that
   //    are sent into this member function.
   // It Builds a GRootedWord object from first word sent in. The
   //    GRootedWord object contains the first word and all of its
   //    possible roots.
   // This memberthen asks the factory to give it an ordered
   //    collection of all the possible DWordPhrase objects
   //    for the primary search key in the new GRootedWord object.
   // If the factory returns no objects this function returns an
   //    empty list of candidates.
   // Otherwise, the DWordPhrase objects are broken down into groups
   //    based upon their respective WordCount value (e.g., all the
   //    objects that are made up of 4 constituent words are grouped
   //    and processed together by the "rangeBegin" and "rangeEnd"
   //    iterators, then all the objects with 3 constituent words
   //    processed together, et.). Note, this algorithm depends upon
   //    the factory returning the DWordPhrase objects in order by
   //    descending WordCount.
   // Each subgroup of DWordPhrase object is searched first for all
   //    of the exact matches and then is searched a second time for
   //    all of the inflected matches. The groups with the higher
   //    WordCounts are searched first. This guarantees the correct
   //    ordering of all possible candidates.
   // Note that the DWordPhrase group made where the WordCount is 1
   //    is handled as a special case. This is for performance
   //    reasons.
   // There are some inter-dependencies to this code. This function
   //    depends on the factories ordering of the DWordPhrase objects,
   //    etc.., This is necessary for performance reasons.

   LDictionaryTokenVector* pCandidates = new LDictionaryTokenVector;
   DWordPhraseVector* pMatches = 0;

   pMatches = 	findPhraseCandidates(firstWord, noSpaces, secondWord);
   if (pMatches && !pMatches->empty())
   {
      populateCandidates(pMatches, pCandidates, beginSource, endSource, analysisCode, firstWord);
   }
   if (pMatches)
   {
      delete pMatches;
      pMatches = 0;
   }
   return pCandidates;
}

void GMatcher::populateCandidates(DWordPhraseVector* pMatches, LDictionaryTokenVector* pCandidates,
                                  const LWordIterator& beginSource, const LWordIterator& endSource,
                                  PhraseMatchSequencer::RootAnalysisCode analysisCode,
                                  const LgsString& firstWord)
{
   if (!pMatches->empty())
   {
      DWordPhraseIterator wordMatchEnd = pMatches->end();

      DWordPhraseIterator rangeBegin = pMatches->begin();
      DWordPhraseIterator rangeEnd   = wordMatchEnd;

      while (rangeBegin->WordCount() >= 1)
      {
         rangeEnd = EndOfWordCountRange(rangeBegin, wordMatchEnd);
         if (rangeBegin->WordCount() <= (endSource - beginSource))
         {
            MultiExactMatch(pCandidates, rangeBegin, rangeEnd, beginSource, endSource);
            if (PhraseMatchSequencer::ADJ_NP == analysisCode)
            {
               AdjNPmatch(pCandidates, rangeBegin, rangeEnd, beginSource, firstWord);
            }
            else
            {
               MultiInflectMatch(pCandidates, rangeBegin, rangeEnd, beginSource);
            }
         }
         rangeBegin = rangeEnd;
         if (rangeBegin == wordMatchEnd) // search is at end of Vector
         {
            break;
         }
      }
   }
}
//---------------------------------------------------------------------
void GMatcher::determinePhraseWordCount (DWordPhraseVector* pMatches) const
{
   for (DWordPhraseIterator i =  pMatches->begin(); i != pMatches->end(); i++)
   {
      int count = count_if (i->Word().begin(), i->Word().end(), bind2nd(equal_to<char>(), ' '));
      count += 2 * count_if (i->Word().begin(), i->Word().end(), bind2nd(equal_to<char>(), '-'));
      i->WordCount (count + 1);
   }
}
//---------------------------------------------------------------------
bool greaterWordCount(DWordPhrase first, DWordPhrase second)
{
   return first.WordCount() > second.WordCount();
}
//---------------------------------------------------------------------
void GMatcher::orderPhrases(DWordPhraseVector* pMatches) const
{
   sort(pMatches->begin(), pMatches->end(), greaterWordCount);
}
//---------------------------------------------------------------------
DWordPhraseVector* GMatcher::findPhraseCandidates(const LgsString& first, short firstSpaceCount,
                                                  const LgsString& second)
{
   DWordPhraseVector* pMatches = 0;
   LinguisticFactory& factory = Factory();
   const LLanguage& language = factory.Dictionary().sourceLanguage();

   // there are 2 types of word phrase caches - one for the first word only eg "in%"
   //                                         - one for first and second together eg "to be%"
   enum CacheMatch { noCache, cacheOnFirst, cacheOnBoth } cacheMatch = noCache;

   // check if the caching is applicable for the given word(s) in the language
   // if so get the phrases out of cache
   // if a null is returned the cache is empty and will need to be filled later
   if (language.inPhraseCache(first))
   {
      // first + " " - starts a phrase that should be cached eg "in "
      cacheMatch = cacheOnFirst;
   }
   else if (language.inPhraseCache(first, second))
   {
      // first + " " + second - starts a phrase that should be cached eg "to be"
      cacheMatch = cacheOnBoth;
   }

   if (cacheMatch != noCache)
   {
      pMatches = getCachedPhrases(first, firstSpaceCount, second, int(cacheMatch));
   }

   if (!pMatches)
   {
      // get phrases from the database
      switch (cacheMatch)
      {
      case noCache:
         if (StringUtil::beginsUpperCase(first))
            pMatches = p_factory->CreatePhraseCapMatches(first, firstSpaceCount, second);
         else
            pMatches = p_factory->CreatePhraseMatches(first, firstSpaceCount, second);
         break;
      case cacheOnFirst:
         pMatches = p_factory->CreatePhraseCapMatches(first, firstSpaceCount, LgsString());
         break;
      case cacheOnBoth:
         pMatches = p_factory->CreatePhraseCapMatches(first, firstSpaceCount, second);
         break;
      }

      // count words and sort on word count - descending order
      determinePhraseWordCount(pMatches);
      orderPhrases(pMatches);

      // synchronize the cache - if the cache was empty fill it with retrieved phrases
      // if the cache is only on the first word we need to call getCachedPhrase again
      // in order to get a subset of the list of phrases - since the cache was built
      // using only the first word
      switch (cacheMatch)
      {
      case noCache:
         break;
      case cacheOnFirst:
         setCachedPhrases(first, second, pMatches, int(cacheMatch));
         pMatches = getCachedPhrases(first, firstSpaceCount, second, cacheMatch);
         break;
      case cacheOnBoth:
         setCachedPhrases(first, second, pMatches, int(cacheMatch));
         break;
      }
   }
   return pMatches;
}
//---------------------------------------------------------------------
LDictionaryTokenVector* GMatcher::FindWordMatch(const LTextualComponent& source, bool BOS, bool bAllCapitalWords)
{
   // This member matches single source words to DWordPhrase objects.
   // It first attempts to match the source word with already existing
   // source-to-DWordPhrase mappings in the cache. Ifthis fails then it
   // goes to the factory to do a database fetch. Note that if the
   // word is in upper-case a different fetch is used.

   LDictionaryTokenVector* pTokens = new LDictionaryTokenVector;
   DWordPhraseVector* pMatches = FindCachedMatch(source, BOS);

   bool pMatchesCached = false;

   if (pMatches->empty())
   {
      if (StringUtil::beginsUpperCase(source))
      {
         bool isGerman = (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID);
         LgsString lower = source;
         StringUtil::toLower(lower);
         pMatches = p_factory->CreateWordMatches(source);
         DWordPhraseVector* pLowerCaseMatches = p_factory->CreateWordMatches(lower);
         if (StringUtil::isAllUpperCase(source))
         {
            DWordPhraseVector* pInitialCapMatches = 0;
            if (lower.length() > 1)
            {
               lower[0] = CharUtil::upper(lower[0]);
               pInitialCapMatches = p_factory->CreateWordMatches(lower);
            }
            if (bAllCapitalWords)
            {
               if (pInitialCapMatches)
                  pMatches->insert(pMatches->begin(), pInitialCapMatches->begin(), pInitialCapMatches->end());
               pMatches->insert(pMatches->begin(), pLowerCaseMatches->begin(), pLowerCaseMatches->end());
            }
            else
            {
               pMatches->insert(pMatches->end(), pLowerCaseMatches->begin(), pLowerCaseMatches->end());
               if (pInitialCapMatches)
                  pMatches->insert(pMatches->end(), pInitialCapMatches->begin(), pInitialCapMatches->end());
            }
            if (pInitialCapMatches)
            {
               delete pInitialCapMatches;
               pInitialCapMatches=0;
            }
         }
         else
         {
            if (BOS && !isGerman)
               pMatches->insert(pMatches->begin(), pLowerCaseMatches->begin(), pLowerCaseMatches->end());
            else
               pMatches->insert(pMatches->end(), pLowerCaseMatches->begin(), pLowerCaseMatches->end());
         }
         delete pLowerCaseMatches;
         pLowerCaseMatches = 0;
      }
      else
      {
         pMatches = p_factory->CreateWordMatches(source);
      }
      if (pMatches->empty())
      {
         delete pMatches;
         pMatches = 0;
      }
      else if (!StringUtil::isAllUpperCase(source))
      {
		  pMatchesCached = true;
         InsertMatchIntoCache(source, pMatches, BOS);
      }
   } else {
		pMatchesCached = true;
   }
   if (pMatches)
   {
      for (DWordPhraseIterator i = pMatches->begin(); i != pMatches->end(); i++)
      {
         LDictionaryToken token(i->Word());
         pTokens->push_back(token);
         LDictionaryToken& rToken = pTokens->back();
         rToken.wordPhrase(new DWordPhrase(*i));
         rToken.inflectionUsed(source.inflectionUsed());
      }

	  if(!pMatchesCached)
		delete pMatches;
   }

   return pTokens;
}
//---------------------------------------------------------------------
DWordPhraseIterator GMatcher::EndOfWordCountRange(DWordPhraseIterator& rangeBegin,
                                                  DWordPhraseIterator& vectorEnd) const
{
   // This member returns the end of the range of DWordPhrase objects
   // that have the same number of words. This member is of real value
   // only when the list of DWordPhrase objects are ordered by
   // WordCount.
   //
   // Following the convention set by STL, the end of a range is not
   // inclusive. Therefore the index that is returned is the first
   // occurence beyond the inclusive range. Note that the end defaults
   // to being the end of the vector itself.

   //DWordPhraseIterator result = rangeBegin;

   for (DWordPhraseIterator i = rangeBegin; i != vectorEnd; i++)
   {
      if (i->WordCount() != rangeBegin->WordCount())
      {
         return i;
      }
   }
   return vectorEnd;
}
//---------------------------------------------------------------------
void GMatcher::ParseIntoWords(DWordPhraseIterator& phrase, LgsVector(LgsString)& sourceWords,
                              const LLanguage* aLanguage) const
{
   // This member is responsible for converting a DWordPhrase object
   // into aset of LWords.
   LgsString wordPhrase = phrase->Word();
   // Special case when the phrase is ".)", ".]", ".}", "!)", "!]", "!}", "?)", "?]", or "?}".
   /* This special case has been covered in the else section,
      because ')', ']' and '}' are delimiters now
   if ((wordPhrase.length() == 2) && ((wordPhrase[0] == '.') || (wordPhrase[0] == '!') || (wordPhrase[0] == '?')) &&
       ((wordPhrase[1] == ')') || (wordPhrase[1] == ']') || (wordPhrase[1] == '}')))
   {
      sourceWords.push_back(wordPhrase.substr(0, 1));
      sourceWords.push_back(wordPhrase.substr(1, 1));
   }
   else*/
   {
      const char *delimiters = "-/()[]{}";    // list of all the delimiters (they are considered as words also)
      LgsString::const_iterator i
        , startPos = wordPhrase.end(); // there was no word yet
      for( i = wordPhrase.begin(); i != wordPhrase.end(); i++ )
      {
         if( ' ' == *i )
         {
            // Have got a space
            if( wordPhrase.end() != startPos )
            {
               // Found end of the word
               sourceWords.push_back(LgsString(startPos, i));
               startPos = wordPhrase.end();
            }
         }
         else if( strchr(delimiters, *i) != NULL )
         {
            // Have got a delimiter
            if( wordPhrase.end() != startPos )
            {
               // Found end of the word
               sourceWords.push_back(LgsString(startPos, i));
               startPos = wordPhrase.end();
            }
            sourceWords.push_back(LgsString(i, i+1)); // delimiter is a word too
         }
         else
         {
            // Have got a normal char
            if( wordPhrase.end() == startPos )
            {
               // Found beginning of the word
               startPos = i; // set start position of the word
            }
         }
      }
      // Check the last word
      if( wordPhrase.end() != startPos )
      {
         sourceWords.push_back(LgsString(startPos, i));
      }
   }
/*
   printf("ntokens=%d, \"%s\"\n", sourceWords.size(), wordPhrase.c_str());
   fflush(stdout);
   LgsVector(LgsString)::iterator j;
   for(j=sourceWords.begin();j<sourceWords.end();j++) {
	   printf("\"%s\"\n", (*j).c_str());
	   fflush(stdout);
   }
*/
}
//---------------------------------------------------------------------
void GMatcher::MultiExactMatch(LDictionaryTokenVector* pResults,
                               DWordPhraseIterator& rangeBegin, DWordPhraseIterator& rangeEnd,
                               const LWordIterator& beginSource, const LWordIterator& endSource) const
{
   // Accross the range of DWordPhrase objects (presumeably all having
   // the same number of words) this member reduces each DWordPhrase
   // object into its constituent words then tests each of the
   // constituent words to match the source word at the respective
   // position. If each word has a successful match the DWordPhrase
   // object is accepted as a candidate.

   const bool ignoreCase = StringUtil::beginsUpperCase(*beginSource);
   for (DWordPhraseIterator i = rangeBegin; i != rangeEnd; i++)
   {
      LgsVector(LgsString) wordsInMatch;
      ParseIntoWords(i, wordsInMatch, beginSource->language());

      bool isMatchGood = true;
      LWordIterator source = beginSource;
      LgsVector(LgsString)::iterator w = wordsInMatch.begin();
      short hyphenCount = 0;
      while (w != wordsInMatch.end())
      {
         if ((LookupTokenType::tok_none != source->getTokenType()) &&
             (0 == source->trailingSpaces()) && (endSource != (source + 1)))
         {
            LWordIterator nextSource = source + 1;
            if ((LookupTokenType::tok_FT_Sym == nextSource->getTokenType()) ||
                (LookupTokenType::tok_IN_Sym == nextSource->getTokenType()))
            {
               LgsString combinedWord = *source + *nextSource;
               if (!ignoreCase)
               {
                  if (combinedWord != *w)
                  {
                     isMatchGood = false;
                     break;
                  }
               }
               else if (!StringUtil::equalsIgnoreCase(combinedWord, *w))
               {
                  isMatchGood = false;
                  break;
               }
               w++;
               source = nextSource + 1;
               hyphenCount--;
               continue;
            }
         }

         if (!ignoreCase)
         {
            if (*source != *w)
            {
               isMatchGood = false;
               break;
            }
         }
         else if (!StringUtil::equalsIgnoreCase(*source, *w))
         {
            isMatchGood = false;
            break;
         }
         if ((w + 1) != wordsInMatch.end() && (*source).isHyphenated())
         {
            if (*(w + 1) != "-")
            {
               isMatchGood = false;
               break;
            }
            if (*(source+1) == "-")
            {
               source++;
            }
            else
            {
               hyphenCount++;
            }
            w++;
         }
         source++;
         w++;
      }
      if (isMatchGood)
      {
         LDictionaryToken token(i->Word());
         pResults->push_back(token);
         pResults->back().wordPhrase(new DWordPhrase(*i));
         pResults->back().setWordsUsedInMatch(wordsInMatch.size()-hyphenCount);
      }
   }
}
//---------------------------------------------------------------------
void GMatcher::MultiInflectMatch(LDictionaryTokenVector* pResults, DWordPhraseIterator& rangeBegin,
                                 DWordPhraseIterator& rangeEnd, const LWordIterator& beginSource) const
{
   // Accross the range of DWordPhrase objects (presumeably all having
   // the same number of words) this member reduces each DWordPhrase
   // object into its constituent words then tests each of the
   // constituent words to match any of the possible roots of the
   // source word at the respective position. If each word has a
   // successful match the DWordPhrase object is accepted as a
   // candidate.

   for (DWordPhraseIterator i = rangeBegin; i != rangeEnd; i++)
   {
      LgsVector(LgsString) wordsInMatch;
      ParseIntoWords(i, wordsInMatch, beginSource->language());

      bool isMatchGood = false;
      LgsVector(LgsString)::iterator matchWord = wordsInMatch.begin();
      LWordIterator sourceWord = beginSource;
      int positionInPhrase = 0;
      const LInflection* headInflection = 0;
      short hyphenCount = 0;
      while (matchWord != wordsInMatch.end())
      {
         if (!headInflection)
         {
            ++positionInPhrase;
         }
         if (StringUtil::equalsIgnoreCase(*sourceWord, *matchWord))
         {
            isMatchGood = true;
         }
         else
         {
            GRootedWord rootedWord(&(*sourceWord));
            const LRoot* matchRoot;
            if (matchRoot = rootedWord.rootMatchedBy(*matchWord))
            {
               isMatchGood = true;
               headInflection = matchRoot->inflectionUsed();
            }
            else
            {
               isMatchGood = false;
               break;
            }
         }
         if ((matchWord+1) != wordsInMatch.end() && (*sourceWord).isHyphenated())
         {
            if (*(matchWord+1) != "-")
            {
               isMatchGood = false;
               break;
            }
            if (*(sourceWord+1) == "-")
            {
               sourceWord++;
               positionInPhrase++;
            }
            else
            {
               hyphenCount++;
            }
            matchWord++;
         }
         matchWord++;
         sourceWord++;
      }
      if (isMatchGood)
      {
         LDictionaryToken token(i->Word());
         token.inflectionUsed(headInflection);
         if (headInflection)
         {
            token.setProposedHeadWord(positionInPhrase);
         }
         pResults->push_back(token);
         pResults->back().wordPhrase(new DWordPhrase(*i));
         pResults->back().setWordsUsedInMatch(wordsInMatch.size()-hyphenCount);
      }
   }
}
//---------------------------------------------------------------------
void GMatcher::AdjNPmatch(LDictionaryTokenVector* pResults, DWordPhraseIterator& rangeBegin,
                          DWordPhraseIterator& rangeEnd, const LWordIterator& beginSource,
                          const LgsString& firstWord) const
{
   // Accross the range of DWordPhrase objects (presumeably all having
   // the same number of words) this member reduces each DWordPhrase
   // object into its constituent words then tests each of the
   // constituent words to match any of the possible roots of the
   // source word at the respective position. If each word has a
   // successful match the DWordPhrase object is accepted as a
   // candidate.

   LgsString srcEnding((*beginSource).substr(firstWord.length()));
   for (DWordPhraseIterator i = rangeBegin; i != rangeEnd; i++)
   {
      LgsVector(LgsString) wordsInMatch;
      ParseIntoWords(i, wordsInMatch, beginSource->language());

      bool isMatchGood = false;
      LgsVector(LgsString)::iterator matchWord = wordsInMatch.begin();
      LWordIterator sourceWord = beginSource;
      int positionInPhrase = 0;
      const LInflection* headInflection = 0;
      LgsString dbEnding((*matchWord).substr(firstWord.length()));
      if (!PhraseMatchSequencer::isValidNPending(dbEnding) || !(*i).EndingLength())
      {
         continue;
      }
      while (matchWord != wordsInMatch.end() - 1)
      {
         ++positionInPhrase;
         int rootLength = (*sourceWord).length() - srcEnding.length();
         int dbRootLength = (*matchWord).length() - dbEnding.length();
         if (rootLength > 0 && dbRootLength > 0 &&	rootLength == dbRootLength &&
             !srcEnding.compare((*sourceWord).substr(rootLength)) &&
             !dbEnding.compare((*matchWord).substr(rootLength)) &&
             StringUtil::equalsIgnoreCase((*sourceWord).substr(0,rootLength), (*matchWord).substr(0,rootLength)))
         {
            isMatchGood = true;
            ++matchWord;
            ++sourceWord;
         }
         else
         {
            isMatchGood = false;
            break;
         }
      }
      ++positionInPhrase;
      if (!isMatchGood)
      {
         continue;
      }
      bool bInflectedMatch = false;
      if (StringUtil::equalsIgnoreCase(*sourceWord, *matchWord))
      {
         ++matchWord;
         ++sourceWord;
      }
      else
      {
         GRootedWord rootedWord(&(*sourceWord));
         const LRoot* matchRoot;
         if (matchRoot = rootedWord.rootMatchedBy(*matchWord))
         {
            bInflectedMatch = true;
            ++matchWord;
            ++sourceWord;
            headInflection = matchRoot->inflectionUsed();
         }
         else
         {
            isMatchGood = false;
         }
      }

      if (isMatchGood)
      {
         LDictionaryToken token(i->Word());
         if (bInflectedMatch)
         {
            token.inflectionUsed(headInflection);
         }
         token.setProposedHeadWord(positionInPhrase);
         pResults->push_back(token);
         pResults->back().wordPhrase(new DWordPhrase(*i));
         pResults->back().setWordsUsedInMatch(wordsInMatch.size());
      }
   }
}
//---------------------------------------------------------------------
DWordPhraseVector* GMatcher::getCachedPhrases(const LgsString& first, short spaceCount,
                                              const LgsString& second, int cacheMatch) const
{
   // NB: assumption - there is only caching for English
   // NB: assumption - there is only caching for one phrase with cacheMatch = 1 ("in")
   // NB: assumption - there is only caching for one phrase with cacheMatch = 2 ("to be")
   // NB: assumption - for "to be" there are no source phrases
   // NB: if any of these assumptions change the code needs to be changed

   assert(inrange(cacheMatch, 1, 2));

   // NB: using specific information that the phrase "to be%" does not
   //     occur as a source phrase - so we can return an empty vector
   if (cacheMatch == 2)
      return new DWordPhraseVector;

   if (v_phraseCache[cacheMatch - 1] == 0)
      return 0;

   char buf[256];
   GPhraseQuery::CombineWords(first, spaceCount, second, buf);
   // remove final '%' char
   if (strlen(buf) != 0)
      buf[strlen(buf) - 1] = 0;

   DWordPhraseVector* ret = new DWordPhraseVector;
   for (DWordPhraseIterator iter = v_phraseCache[cacheMatch - 1]->begin();
        iter != v_phraseCache[cacheMatch - 1]->end(); iter++)
   {
      if (strnicmp(iter->AsString().c_str(), buf, strlen(buf)) == 0)
         ret->push_back(*iter);
   }

   return ret;
}

//---------------------------------------------------------------------
void GMatcher::setCachedPhrases(const LgsString& first, const LgsString& second,
                                DWordPhraseVector* pMatches, int cacheMatch)
{
   assert(inrange(cacheMatch, 1, 2));

   // do a deep copy of the vector
   v_phraseCache[cacheMatch - 1] = new DWordPhraseVector;
   *v_phraseCache[cacheMatch - 1] = *pMatches;
}

