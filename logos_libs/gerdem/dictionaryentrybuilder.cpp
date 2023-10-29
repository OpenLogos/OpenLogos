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
// File - dictionaryentrybuilder.cpp
//
// Class - DictionaryEntryBuilder
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/utility/iniparser.h>
#include <logos_libs/gerdem/dictionaryentrybuilder.h>
#include <logos_libs/gerdem/grootedword.h>
#include <logos_libs/linguistic/linflection.h>
#include <logos_libs/linguistic/lenglish.h>
#include <logos_libs/linguistic/lgerman.h>
#include <logos_libs/linguistic/ldictionaryentry.h>
#include <logos_libs/linguistic/ldictionarytoken.h>
#include <logos_libs/gerdem/gfactory.h>
#include <logos_libs/gerdem/gmatcher.h>
#include <logos_libs/gerdem/patternprototypebuilder.h>
#include <logos_libs/gerdem/punctuationprototypebuilder.h>
#include <logos_libs/startrules/entrymap.h>
#include <logos_libs/translutility/translcommonobjects.h>
#include <logos_libs/halfnoun/germantoken.h>
#include <logos_libs/halfnoun/germansworkinfo.h>
#include <logos_libs/gerdem/hndepwrdlookup.h>
#include <logos_libs/regex/charutil.h>
#include <logos_libs/halfnoun/germantoken.h>
#include <logos_libs/gerdem/phrasematchsequencer.h>
#include <logos_libs/linguistic/lookuptokentype.h>
#include <logos_libs/halfnoun/germansearch.h>
#include <configdatafileinterface/configdatainterfacemain.h>

int DictionaryEntryBuilder::storedHN = 4;
int DictionaryEntryBuilder::nounWC = 1;
int DictionaryEntryBuilder::verbWC = 2;
int DictionaryEntryBuilder::adjWC = 4;
int DictionaryEntryBuilder::storedHNprior = 32;

DictionaryEntryBuilder::DictionaryEntryBuilder()
                       :p_matcher(0),
                        p_factory(0),
                        v_entryCache(20000),
                        v_rootEntryCache(20000),
                        v_compoundEntryCache(20000),
                        v_wordCount(0),
                        v_wgManager(*this),
                        v_possibleEOS(false)
{
   // There is a huge limit on the entry cache. Note that the issue
   // of cleaning up entries is not complete yet so if this limit is
   // exceeded entries will not be garbaged collected and will not
   // exist in any cache -- therefore causing a memory leak. I'll
   // will resolve this after vacation. -- RAM 4/26/1996.

   ST_EntryMap::Initialize();       // loads dictionary entries for token types

   PunctuationPrototypeBuilder punctuationPrototypeBuilder;
   punctuationPrototypeBuilder.Build(v_punctuationPatternPrototypes);
}

// --------------------------------------------------------------------------
DictionaryEntryBuilder::~DictionaryEntryBuilder()
{
   ST_EntryMap::Cleanup();
}

// --------------------------------------------------------------------------
void DictionaryEntryBuilder::loadWordList()
{
   char wordlistFile[MAX_FILEPATH_LEN];
   GetConfigData("sourcedata", "search_filter_wordlist", wordlistFile, MAX_FILEPATH_LEN);

   // Open and read file
   ifstream stream(wordlistFile);
   if (stream.good())
   {   
      // Clear list
      m_firstWordList.clear();

      LgsString str;
      while (!stream.eof())
      {
         stream >> str;
         if (!str.empty())
         {
            //StringUtil::toLower( str );
            m_firstWordList.insert(str);
         }
      }
   }
}

// --------------------------------------------------------------------------
// This member returns an LDictionaryEntry object that is made up
// of only one source word. It returns 0, if no dictionary match can be found.
// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::matchText(const LTextualComponent &text, 
                                                    bool BOS, WordCategory wrdCategory, 
                                                    bool cacheEntry, bool bAllCapitalWords,
                                                    GermanTokenWord *wordPart)
{
   LDictionaryEntry* pEntry = 0;

   LDictionaryTokenVector* pCandidates = p_matcher->FindWordMatch(text, BOS, bAllCapitalWords);
   if (!pCandidates->empty())
   {
      pEntry = createEntry(pCandidates, text, BOS, wrdCategory, cacheEntry, wordPart);
   }
   delete pCandidates;
   return pEntry;
}

// --------------------------------------------------------------------------
// The EntityBuilder object requires a matcher object. It needs to
// be a passed in pointer for polymorhism sake. A matcher that
// matches the GAbstractMatcher interface can be passed in here.
// --------------------------------------------------------------------------
void DictionaryEntryBuilder::setMatcher(GAbstractMatcher* aMatcher)
{
   p_matcher = aMatcher;
   p_matcher->Factory(p_factory);
}

// --------------------------------------------------------------------------
// The EntityBuilder object requires a matcher object. It needs to
// be a passed in pointer for polymorhism sake. A matcher that
// matches the GAbstractMatcher interface can be passed in here.
// --------------------------------------------------------------------------
void DictionaryEntryBuilder::setFactory(LinguisticFactory* aFactory)
{
   p_factory = aFactory;
   if (p_matcher)
   {
      p_matcher->Factory(p_factory);
   }
}

// --------------------------------------------------------------------------
// Initialize the word group manager (to find longest match) with the given sequence
// of source words.
// --------------------------------------------------------------------------
void DictionaryEntryBuilder::initializeWordGroupManager(LWordVector * srcWords)
{
   v_wgManager.initialize(srcWords);
}

// --------------------------------------------------------------------------
// Update the word position in the initial sequence of source words for the next match.
// --------------------------------------------------------------------------
void DictionaryEntryBuilder::updateWordPosition(int wordCount, LWordIterator startSeq)
{
   v_wgManager.updateWordPos(wordCount, startSeq);
}

// --------------------------------------------------------------------------
// Directs the assembly of a LDictionaryEntry object. It is created as quickly as possible without 
// sacrificing accuracy. The steps that are followed in attempting to create the LDictionaryEntry
// object are in increasing time expense. Once an object is successfully created, the remaining steps 
// can be avoided. The steps are as follows:
// 1. Attempt to create object without doing any database accesses (MatchPrePhrase).
// 2. Attempt to create object by matching it as a phrase (MatchPhrase).
// 3. Attempt to match a single word (MatchWord).
// 4. Attempt to match a single word root (MatchRoot).
// 5. If the word cannot be resolved in any of the above steps it is designated as a new unfound 
//    word (MatchNothing).
// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::buildEntry(const LWordIterator& beginSource,
                                                     const LWordIterator& endSource,
                                                     bool BOS, LWordVector* srcWrdVector,
                                                     bool bAllCapitalWords)
{
    WordCategory wrdCategory = 
        (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)? GermanWord : NonGerman;
    if (StringUtil::beginsUpperCase(*beginSource) && 
       ( !BOS && (*(beginSource-1))[(*(beginSource-1)).length()-1] == '.'))
   {
      v_possibleEOS = true;
   }
   LDictionaryEntry* pEntry = 0;
   if (v_wgManager)
   {
      pEntry = (v_wgManager)++;
   }

   ++v_wordCount;
   if (!pEntry && !(((beginSource + 2) == endSource) && (*(beginSource + 1)).isExcludedFromTranslation()))
   {
        if (!(pEntry = matchPhrase(beginSource, endSource, BOS)))
        {
            if (!CharUtil::isJoiningCharacter((*beginSource)[0]) &&
                StringUtil::containsJoiningCharacter(*beginSource) &&
                (LookupTokenType::tok_Unfound_Alpha_Num == (*beginSource).getTokenType() ||
                 LookupTokenType::tok_Unfound_Agent == (*beginSource).getTokenType()))
            {
                pEntry = matchText(*beginSource, BOS, wrdCategory, true, bAllCapitalWords);
                if (!pEntry)
                {
                    pEntry = matchRoot(*beginSource, wrdCategory, BOS, bAllCapitalWords);
                }
            }
            if (!pEntry)
            {
                pEntry = matchPrePhrase(beginSource, endSource);
            }
            else
            {
                pEntry->wordsUsedInMatch(1);
            }
        }
        if (pEntry)
        {
            pEntry->derivedFormCode();
        }
   }

   if (!pEntry && v_wgManager.matchPattern(beginSource))
   {
      v_wgManager.lookup(BOS, bAllCapitalWords);
      v_wgManager.restart();
      pEntry = v_wgManager++;
   }

   if (!pEntry)
   {
      pEntry = matchSingleWord(beginSource, BOS, 
                   wrdCategory, srcWrdVector, false, bAllCapitalWords);
   }

   v_possibleEOS = false;
   return pEntry;
}

// --------------------------------------------------------------------------
// Attempt a match as a single word
// --------------------------------------------------------------------------
LDictionaryEntry *DictionaryEntryBuilder::matchSingleWord(const LWordIterator& beginSource,
                                                          bool BOS, WordCategory wrdCategory,
                                                          LWordVector* srcWrdVector,
                                                          bool halfNounDepLookup,
                                                          bool bAllCapitalWords)
{
   LDictionaryEntry *pEntry = 0;

   bool bChangeWordClass = false;
   bool notHyphenated = false;
   LWordVector::iterator endIter;
   if (srcWrdVector)
   {
      endIter = srcWrdVector->end();
   }
   if (!srcWrdVector ||
       (notHyphenated = (TranslCommonObjects::GetSourceLanguage()->id() != LLanguage::GermanID) ||
        (beginSource + 1 == endIter) || (beginSource + 2 == endIter) || (*(beginSource + 1) != "-") ||
        ((*(beginSource + 1)).precedingSpaces() != 0) || ((*(beginSource + 2)).precedingSpaces() != 0) ||
        (!StringUtil::isAllAlpha(*(beginSource + 2)) &&
        ((*(beginSource + 2)).getTokenType() != LookupTokenType::tok_lookup))))
   {
      int wordsUsed = 1;
      if (!halfNounDepLookup && srcWrdVector && beginSource+1 != endIter && wrdCategory == GermanWord)
      {
         LWordIterator startWord = beginSource;
         if (StringUtil::isAllAlpha(*(beginSource)) && (*(beginSource + 1) == "-") &&
             ((*(beginSource + 1)).precedingSpaces() == 0) && (beginSource + 2 != endIter))
         {
            wrdCategory = GermanNonHead;
            wordsUsed = 2;
            bChangeWordClass = true;
         } 
         else if (StringUtil::isAllAlpha(*(beginSource + 1)) && (*(beginSource) == "-") &&
                  ((*(beginSource+1)).precedingSpaces() == 0))

         {
            startWord = beginSource+1;
            wrdCategory = GermanHead;
            wordsUsed = 2;
         }
         pEntry = matchText(*startWord, BOS, wrdCategory, true, bAllCapitalWords);
         if (pEntry)
         {
            if (bChangeWordClass)
            {
               bChangeWordClass = false;
               SsuList& ssuList = pEntry->semantoSyntacticUnits();
               adjustSsuList(ssuList, 0);
               SsuList::iterator endIter = ssuList.end();
               for (SsuList::iterator iter = ssuList.begin(); iter != endIter; iter++)
               {
                  iter->setWordClassCode(4);
               }
               pEntry->derivedFormCode(true);
            }
            pEntry->wordsUsedInMatch(wordsUsed);
         }
      }
      else 
      {
         pEntry = matchText(*beginSource, BOS, wrdCategory, true, bAllCapitalWords);
      }
      if (!pEntry)
      {
         pEntry = matchRoot(*beginSource, wrdCategory, BOS, bAllCapitalWords);
         wordsUsed = 1;
      }

      if (pEntry)
      {
         if (wrdCategory != GermanNonHead)
         {
            pEntry->derivedFormCode();
         }
         pEntry->wordsUsedInMatch(wordsUsed);
      }
   } 
   if (!pEntry)
   {
      if (!(pEntry = matchCompound(beginSource, BOS, !notHyphenated, srcWrdVector->end(), bAllCapitalWords)))
      {
         if (((beginSource == srcWrdVector->end() - 2) && (*(beginSource + 1) == ".")) ||
             ((beginSource != srcWrdVector->end() - 1) && (beginSource != srcWrdVector->end() - 2) &&
              (*(beginSource + 1) == ".") && ((*(beginSource + 2) == ")") || (*(beginSource + 2) == "]") ||
                                              (*(beginSource + 2) == "}"))))
         {
            LWord tempWord = *beginSource;
            (*beginSource) += *(beginSource+1);
            pEntry = matchText(*beginSource, BOS, wrdCategory, true, bAllCapitalWords);
            if (!pEntry)
            {
               *beginSource = tempWord;
            } 
            else
            {
               srcWrdVector->erase(beginSource+1);
            }
         }
      }
      if (!pEntry)
      {
         pEntry = matchNothing(*beginSource, BOS);
         pEntry->wordsUsedInMatch(1);
      }
   }
   if (bChangeWordClass && pEntry)
   {
      SsuList& ssuList = pEntry->semantoSyntacticUnits();
      adjustSsuList(ssuList, 0);
      SsuList::iterator endIter = ssuList.end();
      for (SsuList::iterator iter = ssuList.begin(); iter != endIter; iter++)
      {
         iter->setWordClassCode(4);
      }
      pEntry->derivedFormCode(true);
   }
   return pEntry;
}

// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::buildPunctuationEosEntry(const LWord& word)
{
   LDictionaryEntry* pEntry = 0;
   pEntry = v_punctuationPatternPrototypes.EntryByPattern(word);
   return pEntry;
}

// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::buildProtectedWordEntry(const LWordIterator& word, LgsString& protectedString)
{
   LDictionaryEntry* pEntry = new LDictionaryEntry();

   LSemantoSyntacticUnit ssu;

   LWordIterator index = word;
   int trailingSpaces = 0;
   int wordCount = 0;
   while ((*index).isProtectedWord())
   {
      if (trailingSpaces > 0)
      {
         protectedString += " ";
      }
      protectedString += (*index);
      trailingSpaces = (*index).trailingSpaces();
      index++;
      wordCount++;
   }
   ssu.setWord(protectedString);
   ssu.setTargetWord(protectedString);
   ssu.setWordCount(wordCount);
   ssu.setWordID(LSemantoSyntacticUnit::unfoundWordID());

   ssu.setWordClassCode(1);

   ssu.setSetID(1);
   ssu.setSubSetID(228);
   ssu.setSuperSetID(1);
   ssu.setFormCode(33);

   pEntry->addSsu(ssu);

   pEntry->setToUnfoundWord();
   pEntry->wordsUsedInMatch(wordCount);

   return pEntry;
}

// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::buildProtectedEntry(const LWord& word)
{
   LDictionaryEntry* pEntry = new LDictionaryEntry();

   LSemantoSyntacticUnit ssu;

   ssu.setWord(word);
   ssu.setTargetWord(word);
   ssu.setWordCount(1);
   ssu.setWordID(LSemantoSyntacticUnit::unfoundWordID());

   ssu.setWordClassCode(1);

   ssu.setSetID(1);
   ssu.setSubSetID(228);
   ssu.setSuperSetID(1);
   ssu.setFormCode(33);

   pEntry->addSsu(ssu);

   pEntry->setToUnfoundWord();

   return pEntry;
}

// --------------------------------------------------------------------------
// This is the most efficient way to return an LDictionaryEntry object. If the object can be 
// resolved here there is no need for a database fetch. The conditions that avoid a database 
// fetch are as follows:
// 1. The object is special (number, date, etc.)
// 2. The object has already been determined to be unfound.
// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::matchPrePhrase(const LWordIterator& beginSource,
                                                         const LWordIterator& endSource)
{
   LDictionaryEntry* pEntry = buildSpecialEntry(beginSource, endSource);

   return pEntry;
}

// --------------------------------------------------------------------------
// This member serves as a filter for source words that cannot be found in the dictionary
// because they are numbers, etc.
// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::buildSpecialEntry(const LWordIterator& begin,
                                                            const LWordIterator& end)
{
    LookupTokenType::Type tokenType = begin->getTokenType();
  
    LDictionaryEntry* entry = (tokenType == LookupTokenType::tok_none) ? 0 :
                              ST_EntryMap::getEntryForToken(tokenType, begin->language()->id());
    if (entry != 0)
    {
        LWordIterator wordEnd;
        // count tokens used - for the dictionary entry
        for (wordEnd = begin + 1; wordEnd != end; wordEnd++)
        {
            if (wordEnd->getTokenType() != LookupTokenType::tok_continuation)
                break;
        }
        short decimalPos = 0;
        switch (tokenType)
        {
            case LookupTokenType::tok_No_Decimal:
                replaceDecimal(begin, wordEnd);
                break;
            case LookupTokenType::tok_No_Mixed:
            case LookupTokenType::tok_No_16_14:
                decimalPos = replaceDecimal(begin, wordEnd);
            case LookupTokenType::tok_No_Range_1000_1399:
            case LookupTokenType::tok_No_Range_1400_2100:
            case LookupTokenType::tok_No_Range_2101_:
                replaceThousandSeparator(begin, wordEnd, decimalPos);
                break;
            case LookupTokenType::tok_AdjAdv_Num_1st_E:
            case LookupTokenType::tok_AdjAdv_Num_st_E:
            case LookupTokenType::tok_AdjAdv_Num_1st_E_Hyphen:
            case LookupTokenType::tok_AdjAdv_Num_st_E_Hyphen:
                removeSuffix(wordEnd-1, "st");
                break;                
            case LookupTokenType::tok_AdjAdv_Num_nd_E:
            case LookupTokenType::tok_AdjAdv_Num_nd_E_Hyphen:
                removeSuffix(wordEnd-1, "nd");
                break;                
            case LookupTokenType::tok_AdjAdv_Num_rd_E:
            case LookupTokenType::tok_AdjAdv_Num_rd_E_Hyphen:
                removeSuffix(wordEnd-1, "rd");
                break;                
            case LookupTokenType::tok_AdjAdv_Num_th_E:
            case LookupTokenType::tok_AdjAdv_Num_th_E_Hyphen:
                removeSuffix(wordEnd-1, "th");
                break;                
            case LookupTokenType::tok_Adj_Num_1_G:
            case LookupTokenType::tok_Adj_Num_21_G:
            case LookupTokenType::tok_Adj_Num_2_G:
            case LookupTokenType::tok_Adj_Num_3_G:
            case LookupTokenType::tok_Adj_Num_4_G:
                removeSuffix(wordEnd-1, ".");
                break;
            default:
                break;
        }
      begin->setHyphenated(false);
      LDictionaryToken* dictionaryToken = new LDictionaryToken;
      dictionaryToken->setWordsUsedInMatch(wordEnd - begin);
      entry->dictionaryToken(*dictionaryToken);
	  delete dictionaryToken;
   }
   return entry;
}

short DictionaryEntryBuilder::replaceDecimal(const LWordIterator & beginWord, const LWordIterator & endWord)
{
    ST_Locale & sourceLocale = TranslCommonObjects::GetSourceLocale();              
    ST_Locale & targetLocale = TranslCommonObjects::GetTargetLocale();
    short retPos = 0;
    int decimalPos = 0;
    LWordIterator wordIter = endWord;
    do
    {
        wordIter--;
        decimalPos = (*wordIter).find_last_of(sourceLocale.dec_sep_);
        if (decimalPos != LgsString::npos)
        {
            retPos += (*wordIter).length()-decimalPos-1;
            (*wordIter).replace(decimalPos, 1, 1, targetLocale.dec_sep_);
            return retPos;
        }
        retPos += (*wordIter).length();
    }
    while (wordIter != beginWord);
    return 0;
}

void DictionaryEntryBuilder::replaceThousandSeparator(const LWordIterator & beginWord, const LWordIterator & endWord, short decimalPos)
{
    ST_Locale & sourceLocale = TranslCommonObjects::GetSourceLocale();              
    ST_Locale & targetLocale = TranslCommonObjects::GetTargetLocale();
    // Position the word iterator at the word which contains decimal separator
    LWordIterator wordIter;
    for (wordIter = endWord-1; decimalPos >= (*wordIter).length(); wordIter--, decimalPos -= (*wordIter).length());
    int prevThousSepPos = (*wordIter).length() - decimalPos - 1;
    int thousSepPos = 0;
    wordIter++;

    do
    {
        wordIter--;
        while (prevThousSepPos != 0 && thousSepPos != LgsString::npos)
        {
            thousSepPos = (*wordIter).find_last_of(sourceLocale.thou_sep_, prevThousSepPos-1);
            if (thousSepPos != LgsString::npos)
            {
                (*wordIter).replace(thousSepPos, 1, 1, targetLocale.thou_sep_);          
            }
            prevThousSepPos = thousSepPos;
        }
        if (wordIter != beginWord)
        {
            prevThousSepPos = (*(wordIter-1)).length()-1;
        }
    }
    while (wordIter != beginWord);
}

void DictionaryEntryBuilder::removeSuffix(const LWordIterator & wordIter, const LgsString & suffix)
{
    LWordIterator word = wordIter;
    int suffixPos = (*word).length()-suffix.length();
    if (LgsString::npos != (*word).find_first_of(suffix, suffixPos))
    {
        (*word).erase(suffixPos);
    }
}

// --------------------------------------------------------------------------
// Simply records the word as an unfound term in the cache and creates a default LDictionary object.
// This method has grown over time (unfortunately). There are now a set of different unfound 
// dictionary entries, depending upon capitalization etc.,.
// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::matchNothing(const LWord &word, bool BOS)
{
   unfoundWordCachePre().Insert(word);
   LDictionaryEntry *dictEntry = 0;
   if ((word.length() > 1) && (word.at(word.length() - 1) == 's'))
   {
      if ((word.length() > 2) && (word.at(word.length() - 2) == '\''))
      {
         if (StringUtil::isAllUpperCase(LgsString(word.substr(0, (word.length() - 2)))))
         {
            dictEntry = unfoundEntry(AllCapsApostS, word);
         }
         else if (StringUtil::beginsUpperCase(word))
         {
            dictEntry = unfoundEntry(BeginCapApostS, word);
         }
         else
         {
            dictEntry = unfoundEntry(EndingApostS, word);
         }
      }
      else
      {
         if (StringUtil::isAllUpperCase(LgsString(word.substr(0, (word.length() - 1)))))
         {
            dictEntry = unfoundEntry(AllCapsS, word);
         }
         else if (StringUtil::beginsUpperCase(word))
         {
            if (BOS)
            {
				   dictEntry = unfoundEntry(BeginCapSBOS, word);
            }
            else
            {	
				   dictEntry = unfoundEntry(BeginCapS, word);
            }
         }
         else
         {
            dictEntry = unfoundEntry(EndingS, word);
         }
      }
   }
   else
   {
      if (StringUtil::containsAlphabetic(word) && StringUtil::containsDigit(word))
      {
         dictEntry = makeAlphanumericEntry();
      }
      else if (StringUtil::isAllUpperCase(word)) 
      {
         dictEntry = unfoundEntry(AllCaps, word);
      }
      else if (StringUtil::beginsUpperCase(word))
      {
         if (word.length() > 1)
         {
            if (CharUtil::isUpper((word)[1]))
            {
               dictEntry = unfoundEntry(UnfoundAgent, word);
            }
            else if (word.length() > 2)
            {
               LWord::const_iterator charVal;
               for (charVal = word.begin() + 2; (charVal != word.end()) && (!CharUtil::isUpper(*charVal)); charVal++);

               if (charVal != word.end())
               {
                  dictEntry = unfoundEntry(UnfoundAgent, word);
               }
            }
         }

         if (!dictEntry)
         {
            if (BOS)
            {
               dictEntry = unfoundEntry(BeginCapBOS, word);
            }
            else
            {
               dictEntry = unfoundEntry(BeginCap, word);
            }
         }
      }
      else
      {
         dictEntry = unfoundEntry(Unfound, word);
      }
   }
   if (dictEntry)
   {
	   LDictionaryToken dictToken(word);
	   dictEntry->dictionaryToken(dictToken);
   }
   return dictEntry;
}

// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::unfoundEntry(EntryBuilder::UnfoundTYPE typeOfUnfound, const LWord &word)
{
   // Initialize ssu with common attributes for all dictionary entries.
   LSemantoSyntacticUnit ssu;

   ssu.setWordCount(1);
   ssu.setWord(word);
   ssu.setWordID(LSemantoSyntacticUnit::unfoundWordID());
   ssu.setWordClassCode(1);
   ssu.setSetID(1);

   switch (typeOfUnfound)
   {
   case AllCaps:
      ssu.setSubSetID(228);
      ssu.setFormCode(33);
      break;

   case AllCapsS:
      ssu.setSubSetID(228);
      ssu.setFormCode(2);
      break;

   case AllCapsApostS:
      ssu.setSubSetID(228);
      ssu.setFormCode(14);
      break;

   case BeginCap:
      ssu.setSubSetID(859);
      ssu.setFormCode(33);
      break;

   case BeginCapBOS:
      ssu.setSubSetID(001);
      ssu.setFormCode(33);
      break;

   case BeginCapS:
      ssu.setSubSetID(859);
      ssu.setFormCode(33);
      break;

   case BeginCapSBOS:
      ssu.setSubSetID(865);
      ssu.setFormCode(33);
      break;

   case BeginCapApostS:
      ssu.setSubSetID(859);
      ssu.setFormCode(4);
      break;

   case EndingS:
      ssu.setSubSetID(865);
      ssu.setFormCode(33);
      break;

   case EndingApostS:
      ssu.setSubSetID(1);
      ssu.setFormCode(4);
      break;

   case UnfoundAgent:
      ssu.setSubSetID(228);
      ssu.setFormCode(33);
      break;

   case Unfound:
   default:
      ssu.setSubSetID(1);
      ssu.setFormCode(33);
      break;
   }

   LDictionaryEntry* theUnfoundEntry = new LDictionaryEntry();

   theUnfoundEntry->addSsu(ssu);
   theUnfoundEntry->setToUnfoundWord();
   //theUnfoundEntry->setCached(true);
   return theUnfoundEntry;
}

// --------------------------------------------------------------------------
// This member function attempts to build an LDictionaryEntry object from a phrase. 
// This would be the match with the greatest priority but it comes with a certain overhead 
// that can be avoided on some occassions.
// Note that the query to determine matches is based on the lowest form of root of the word.
// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::matchPhrase(const LWordIterator& beginSource,
                                                      const LWordIterator& endSource, bool BOS)
{
   LDictionaryEntry* pEntry = 0;

   if( isPhrase( beginSource, endSource ) )
   {
		PhraseMatchSequencer matchSequencer(beginSource);
		for (;!pEntry && matchSequencer; matchSequencer++)
		{
			LDictionaryTokenVector* pCandidates = 
				p_matcher->FindPhraseMatch(beginSource, endSource, matchSequencer.firstWord(),
										   matchSequencer.secondWord(), matchSequencer.noSpaces(),
										   matchSequencer.analysisCode());
			if (!pCandidates->empty())
			{
				pEntry = createPhraseEntry(pCandidates, beginSource, endSource, BOS);
			}
			delete pCandidates;
		}
   }
   return pEntry;
}

// --------------------------------------------------------------------------
bool DictionaryEntryBuilder::isPhrase( const LWordIterator& beginSource, const LWordIterator& endSource )
{
   // 1. Check count of words
   if( (endSource - beginSource) < 2 )
   {
      return false; // less then two words => it CAN NOT be a phrase
   }

   // 2. Check if second word has token
   //if( (beginSource + 1)->getTokenType() != LookupTokenType::tok_none )
   //{
   //   return false; // second word has token => it CAN NOT be a phrase
   //}

   // 3. Check if first word is in List
   if( !m_firstWordList.empty() )
   {
      // Do search
      if( m_firstWordList.find( *beginSource ) != m_firstWordList.end() )
      {
         return false; // found => it CAN NOT be a phrase
      }
   }
   if ( !(*beginSource).empty() && ((*beginSource)[0] == '%' || (*beginSource)[0] == '_'))
   {
		return false;
   }

   return true; // not found => it can be a phrase
}

// --------------------------------------------------------------------------
// For German source, invokes the decomposition logic to try to split the
// word (comound) word into parts
// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::matchCompound(LWordIterator word, bool BOS,
                                                        bool hyphenated, LWordIterator endIter,
                                                        bool bAllCapitalWords)
{
   if (TranslCommonObjects::GetSourceLanguage()->id() != LLanguage::GermanID)
   {
      return 0;
   }

   LCompoundDictionaryEntry* compoundEntry = new LCompoundDictionaryEntry(word->language());
   bool beginsUpperCase = StringUtil::beginsUpperCase(*word);

   LWord lookupWord = *word;
   LgsString wordString = *word;
   int wordCount = 1;
   if (hyphenated)
   {
      // Go thru the rest of the word list and join word that are seperated 
      // by hyphens
      // Also build a vector of strings for decomposition logic
      for (LWordIterator currIter = word; currIter + 1 != endIter && currIter + 2 != endIter && 
           *(currIter + 1) == "-" && (*(currIter + 1)).precedingSpaces() == 0 &&
           (*(currIter + 2)).precedingSpaces() == 0 && StringUtil::isAllAlpha(*(currIter + 2));
           currIter += 2)
      {
         wordCount += 2;
      }

      for (int i = 0; i < wordCount - 2; i += 2)
      {
         if (!populateCompoundDE(compoundEntry, *(word + i), false, true, beginsUpperCase, BOS, bAllCapitalWords))
         {
            LDictionaryEntry *pEntry = matchNothing(*(word + i), BOS);
            SsuList& ssuList = pEntry->semantoSyntacticUnits();
            adjustSsuList(ssuList, 0);
            SsuList::iterator endIter = ssuList.end();
            for (SsuList::iterator iter = ssuList.begin(); iter != endIter; iter++)
            {
               iter->setWordClassCode(4);
               iter->setFormCode(9);
            }
			pEntry->setAsNonHead();
            pEntry->wordsUsedInMatch(2);
            compoundEntry->addDictionaryEntry(pEntry);
         }
      }
      if (!populateCompoundDE(compoundEntry, *(word + wordCount - 1), true, true, beginsUpperCase, BOS, bAllCapitalWords))
      {
         LDictionaryEntry *pEntry = matchNothing(*(word + wordCount - 1), BOS);
		 pEntry->setAsHead();
         compoundEntry->addDictionaryEntry(pEntry);
      }
   } 
   else
   {
	  if (word+1 != endIter && word+2 != endIter && *(word+1) == "-" && *(word+2) == ",")
	  {
		wordCount++; // include "-"
		populateCompoundDE(compoundEntry, *word, false, false, beginsUpperCase, BOS, bAllCapitalWords);
		if (compoundEntry->numberOfEntries() > 0)
		{
			compoundEntry->wordsUsedInMatch(wordCount);
		}
	  }
	  else
	  {
		populateCompoundDE(compoundEntry, *word, true, false, beginsUpperCase, BOS, bAllCapitalWords);
	  }
   }
   if (compoundEntry->numberOfEntries() > 0 && hyphenated)
   {
      compoundEntry->wordsUsedInMatch(wordCount);
   }
   if (compoundEntry->numberOfEntries() == 0)
   {
      delete compoundEntry;
      compoundEntry = 0;
   }
   return compoundEntry;
}

// --------------------------------------------------------------------------
bool DictionaryEntryBuilder::populateCompoundDE(LCompoundDictionaryEntry * dictEntry, LWord &pWord,
                                                bool headPresent, bool hyphenated,
                                                bool beginsUpperCase, bool BOS,
                                                bool bAllCapitalWords)
{
   LDictionaryEntry * pEntry = 0;
   if (hyphenated)
   {
      pEntry = matchText(pWord, false, headPresent? GermanHead : GermanNonHead, false, bAllCapitalWords);
      if (!pEntry && headPresent)
      {
         pEntry = matchRoot(pWord, headPresent? GermanHead : GermanNonHead, false, bAllCapitalWords);
      }
      if (pEntry)
      {
         if (!headPresent)
         {
            SsuList& ssuList = pEntry->semantoSyntacticUnits();
            adjustSsuList(ssuList, 0);
            SsuList::iterator endIter = ssuList.end();
            for (SsuList::iterator iter = ssuList.begin(); iter != endIter; iter++)
            {
               iter->setWordClassCode(4);
               iter->setFormCode(9);
            }
			pEntry->setAsNonHead();
            pEntry->wordsUsedInMatch(2);
         }
         else
         {
            pEntry->wordsUsedInMatch(1);
            pEntry->derivedFormCode();
			pEntry->setAsHead();
         }
         dictEntry->addDictionaryEntry(pEntry);
         return true;
      }
   }
   LCompoundDictionaryEntry tempCompound(pWord.language());
   GermanTokenList decomposition;

   // split word
   // - if word is at start of sentence (uppercase) - noun or adjective
   // - if word in middle of sentence with uppercase - noun
   // - if word is lowercase - adjective

   bool decomposed = false;
   decomposed = TranslCommonObjects::GetGermanSearch().decompose(
   pWord,
   decomposition,
   headPresent? nounAdjHeadWord: noHeadWord,
   StringUtil::isAllUpperCase(pWord));

   if (decomposed)
   {
      LgsString compound = pWord;
      int pos = 0;    // position of partial word in compound word

      GermanTokenList::iterator lastElem = decomposition.end() - 1;
      GermanTokenTrialWord * trialWord = 
      dynamic_cast<GermanTokenTrialWord *>(*lastElem);
      if (trialWord)
      {
         //delete trialWord;
         decomposition.erase(lastElem);
      }

      // get head word info if present
      GermanTokenWord* headWord = 0;
      GermanTokenSuffix* suffix = 0;

      if (headPresent)
      {
         headWord = decomposition.getHeadWord();
         suffix = decomposition.getSuffix();
      }

      // iterate through non-head-nouns
      GermanTokenList::const_iterator iterToken = decomposition.begin();
      GermanTokenWord* wordPart;

      for (wordPart = decomposition.getNextWord(iterToken); wordPart != headWord;
           wordPart = decomposition.getNextWord(iterToken))
      {
         // iterate from the current position (pos) in the compound word
         // matching against the word part
         // note that the compound word is not normalized, while the word part is
         // - the difference is that eszett characters in the compound word becomes
         //   'ss' in the word part
         int prevPos = pos;
         int length = GermanUtil::wordPartLength(compound, pos, wordPart->text_);

         // invalid splitting - return unfound - cleanup interim entry
         if (length == -1)
         {
            pWord.setText(compound);
            return false;
         }

         pos += length;
         assert(pos <= compound.length());

         // set up word to lookup
         LgsString lookupWordString(compound.substr(prevPos, pos-prevPos));
		 if (wordPart->isDoubleEnded())
		 {
			lookupWordString += lookupWordString[lookupWordString.length()-1];
		 }
         pWord.setText(lookupWordString);

         // skip past connector
         GermanTokenConnector* connector = decomposition.getNextConnector(iterToken);
         if (connector) pos += connector->text_.length();
            assert(pos <= compound.length());

         // now build an entry for the word

         // call matchText with initial lowercase
		 LDictionaryEntry* entryValue = 0;
		 if (wordPart->matchWithLowerCase())
		 {
			pWord[0] = CharUtil::lower(pWord[0]);
			entryValue = matchText(pWord, false, GermanNonHead, false, bAllCapitalWords, wordPart);
		 }

         // since the above returned entry may be cached, save its contents
         // to entry1 and use it (to be modified later)
         LDictionaryEntry* entry1 = 0;
         if (entryValue)
         {
            entry1 = new LDictionaryEntry(*entryValue);
         }

         // call matchText with initial uppercase
		 LDictionaryEntry* entry2 = 0;
		 if (wordPart->matchWithUpperCase())
		 {
			pWord[0] = CharUtil::upper(pWord[0]);
			entry2 = matchText(pWord, false, GermanNonHead, false, bAllCapitalWords, wordPart);
		 }

         // if nothing found - whole compound is unfound - so cleanup
         if (entry1 == 0 && entry2 == 0)
         {
            pWord.setText(compound);
            return false;
         }

         // if entry1 is 0 - swap them around
         if (entry1 == 0)
         {
            entry1 = entry2;
            entry2 = 0;
         }

         // select only one ssu out of a maximum of six (3 for each entry),
         // and store in entry1
         SsuList& ssuList1 = entry1->semantoSyntacticUnits();
         if (entry2 != 0)
         {
            SsuList& ssuList2 = entry2->semantoSyntacticUnits();
            for (SsuList::const_iterator iter = ssuList2.begin(); iter != ssuList2.end(); iter++)
            {
               ssuList1.push_back(*iter);
            }
         }
		 // remove SSU information that belong to word classes that are not
		 // part of decomposed component (from the cache)
		 removeInconsistentSsus(ssuList1, wordPart);
         adjustSsuList(ssuList1, wordPart);
         SsuList::iterator endIter = ssuList1.end();
         for (SsuList::iterator iter = ssuList1.begin(); iter != endIter; iter++)
         {
            iter->setWordClassCode(4);
            if (hyphenated)
            {
               iter->setFormCode(9);
            }
         }
         entry1->derivedFormCode(true);
		 entry1->setAsNonHead();

         // discard entry2 - and use entry1 as the entry for the non-head-word
         if (entry2 && !(entry2->isCached()))
         {
            delete entry2;
         }
         if (entryValue && !(entryValue->isCached()))
         {
            delete entryValue;
         }
         tempCompound.addDictionaryEntry(entry1);
      }

      if (headPresent)
      {
         // now insert the head-word entry into the compound entry
         LgsString headWordString(compound.substr(pos));
         pWord.setText(headWordString);
         if (beginsUpperCase)
            pWord[0] = CharUtil::upper(pWord[0]);

         LDictionaryEntry* headEntry;
         if (suffix != 0)
            headEntry = matchRoot(pWord, GermanHead, false, bAllCapitalWords);
         else
            headEntry = matchText(pWord, false, GermanHead, false, bAllCapitalWords);

         // if head word not found - whole compound is unfound - so cleanup
         if (headEntry == 0)
         {
            pWord.setText(compound);
            return false;
         }

		 headEntry->setAsHead();
         headEntry->derivedFormCode();
         tempCompound.addDictionaryEntry(headEntry);
      }
      pWord.setText(compound);
   } 
   else
   {
      return false;
   }
   // Transfer all elements from tempCompound to compoundEntry
   int numEntries = tempCompound.numberOfEntries();
   LDictionaryEntry * temp = 0;
   for (int i = 0; i < numEntries - 1; i++)
   {
      temp = tempCompound.getEntry(i);
      temp->wordsUsedInMatch(0);
      dictEntry->addDictionaryEntry(temp);
   }
   temp = tempCompound.getEntry(numEntries - 1);
   temp->wordsUsedInMatch(1);
   dictEntry->addDictionaryEntry(temp);

   return true;
}

// --------------------------------------------------------------------------
// This performs the same work as MatchWord except it does it for all possible variations (roots) 
// of the word. It is only resorted to if the word itself is not found. A RootedWord object is a
// Word that also contains all of its roots (created from Inflection objects in the Language object). 
// This method iterates through the roots of the word (they are prioritized) and attempts to find a 
// valid DictionaryEntry object.
// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::matchRoot(const LWord &word, WordCategory wrdCategory,
													bool BOS, 
                                                    bool bAllCapitalWords)
{
   LDictionaryEntry* pEntry = 0;

   GRootedWord rootedWord(&word);
   const LRootVector* roots = rootedWord.roots();
   for (LRootVector::const_iterator r =  roots->begin(); r != roots->end(); r++)
   {
      LDictionaryTokenVector* pCandidates = p_matcher->FindWordMatch(*r, BOS, bAllCapitalWords);
      if (!pCandidates->empty())
      {
         pEntry = createRootEntry(pCandidates, &(*r), &word, wrdCategory, BOS);
         if (pEntry)
         {
            delete pCandidates;
            break;
         }
      }
      delete pCandidates;

   }
   return pEntry;
}

// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::makeAlphanumericEntry()
{
   LSemantoSyntacticUnit ssu;

   ssu.setCompanyCode("LOG");
   ssu.setWordCount(1);
   ssu.setWordID(LSemantoSyntacticUnit::unfoundWordID());

   ssu.setWordClassCode(1);

   ssu.setSetID(1);
   ssu.setSubSetID(989);
   ssu.setSuperSetID(1);
   ssu.setFormCode(33);

   LDictionaryEntry* alphanumericEntry = new LDictionaryEntry();
   alphanumericEntry->addSsu(ssu);
   alphanumericEntry->setToUnfoundWord();
// The following line caused memory leaks. Entries w/ "cached"
// flag set were not deleted because of the assumption that
// whatever cache is holding pointer to that entry, this cache
// is responsible for cleaning it up. But, such a cache for
// unfound entries does not exist as of moment of this writing.
// XXX Might be that similar problems exist for patternprototypebuilder
// and punctuationprototypebuilder (later, later ...)
//   alphanumericEntry->setCached(true);
   return alphanumericEntry;
}

// --------------------------------------------------------------------------
// Builds the prototype object for the standard BOS entry. Note the SAL values. Check with Liz if 
// there are any problems with these values.
// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::bosEntry()
{
   LSemantoSyntacticUnit ssu;

   ssu.setWord("bos");
   ssu.setTargetWord("bos");
   ssu.setCompanyCode("LOG");
   ssu.setWordCount(0);
   ssu.setWordID(LSemantoSyntacticUnit::unfoundWordID());

   ssu.setPatNumber(0);
   ssu.setSourceStemNumber(0);
   ssu.setWordClassCode(LLanguage::PUNCTUATION);

   ssu.setSetID(1);
   ssu.setFormCode(1);
   ssu.setSubSetID(0);
   ssu.setSuperSetID(1);

   LDictionaryEntry* bosEntry = new LDictionaryEntry();
   bosEntry->addSsu(ssu);
   bosEntry->setToUnfoundWord();
   //bosEntry->setCached(true);
   return bosEntry;
}

// --------------------------------------------------------------------------
// Creates an dictionary entry that marks the beginning of a temporary sentence. A temporary 
// sentence is actually part of a real sentence that is too large for fortran to handle (70
// unit maximum).
// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::createTempBosEntry(const LgsString& s)
{
   LSemantoSyntacticUnit ssu;

   ssu.setWord("bos");
   ssu.setTargetWord("bos");
   ssu.setCompanyCode("LOG");
   ssu.setWordCount(0);
   ssu.setWordID(LSemantoSyntacticUnit::unfoundWordID());

   ssu.setPatNumber(0);
   ssu.setSourceStemNumber(0);
   ssu.setWordClassCode(LLanguage::PUNCTUATION);

   ssu.setSetID(1);
   ssu.setFormCode(1);
   ssu.setSuperSetID(1);

   if (s == ";")
   {
      ssu.setSubSetID(185);
   }
   else if (s == ":")
   {
      ssu.setSubSetID(186);
   }
   else if (s == ",")
   {
      ssu.setSubSetID(188);
   }
   else if (s == "-")
   {
      ssu.setSubSetID(189);
   }
   else if ((s == "i.e.") || (s == "e.g.") || (s == "cf."))
   {
      ssu.setSubSetID(181);
   }
   else if ((3 == s.length()) && ('(' == s.at(0)) && (')' == s.at(2)))
   {
      ssu.setSubSetID(182);
   }
   else
   {
      ssu.setSubSetID(180);
   }

   LDictionaryEntry* pBosEntry = new LDictionaryEntry();
   pBosEntry->addSsu(ssu);
   pBosEntry->setToUnfoundWord();

   return pBosEntry;
}

// --------------------------------------------------------------------------
// Creates an dictionary entry that marks the termination of a temporary sentence. A temporary 
// sentence is actually part of a real sentence that is too large for fortran to handle (70
// unit maximum).
// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::createTempEosEntry(const LgsString& s)
{
   LSemantoSyntacticUnit ssu;

   ssu.setWord("");
   ssu.setTargetWord("");
   ssu.setCompanyCode("LOG");
   ssu.setWordCount(0);
   ssu.setWordID(LSemantoSyntacticUnit::unfoundWordID());

   ssu.setPatNumber(0);
   ssu.setSourceStemNumber(0);
   ssu.setWordClassCode(LLanguage::PUNCTUATION);

   ssu.setMeaningID(10011);
   ssu.setSetID(51);
   ssu.setFormCode(1);
   ssu.setSuperSetID(10);

   if (s == ";")
   {
      ssu.setSubSetID(185);
   }
   else if (s == ":")
   {
      ssu.setSubSetID(186);
   }
   else if (s == ",")
   {
      ssu.setSubSetID(188);
   }
   else if (s == "-")
   {
      ssu.setSubSetID(189);
   }
   else if ((s == "i.e.") || (s == "e.g.") || (s == "cf."))
   {
      ssu.setSubSetID(181);
   }
   else if ((3 == s.length()) && ('(' == s.at(0)) && (')' == s.at(2)))
   {
      ssu.setSubSetID(182);
   }
   else if (s == "}" || s == "]" || s == ")")
   {
      ssu.setSubSetID(187);
   }
   else
   {
      ssu.setSubSetID(180);
   }

   LDictionaryEntry* pEosEntry = new LDictionaryEntry();
   pEosEntry->addSsu(ssu);
   pEosEntry->setToUnfoundWord();

   return pEosEntry;
}

// --------------------------------------------------------------------------
inline LDictionaryEntry* DictionaryEntryBuilder::eosEntry()
{
   LSemantoSyntacticUnit ssu;

   ssu.setWord("");
   ssu.setTargetWord("");
   ssu.setCompanyCode("LOG");
   ssu.setWordCount(0);
   ssu.setWordID(LSemantoSyntacticUnit::unfoundWordID());

   ssu.setPatNumber(0);
   ssu.setSourceStemNumber(0);
   ssu.setWordClassCode(LLanguage::PUNCTUATION);

   ssu.setMeaningID(10011);
   ssu.setSetID(10);
   ssu.setFormCode(1);
   ssu.setSubSetID(10);
   ssu.setSuperSetID(10);

   LDictionaryEntry* eosEntry = new LDictionaryEntry();
   eosEntry->addSsu(ssu);
   eosEntry->setToUnfoundWord();
   //eosEntry->setCached(true);
   return eosEntry;
}

// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::createEntry(LDictionaryTokenVector* pCandidates,
                                                      const LTextualComponent &pText,
                                                      bool BOS, WordCategory wrdCategory,
                                                      bool cacheEntry,
													  GermanTokenWord* wordPart)
{
   LDictionaryEntry* pEntry = 0;

   LDictionaryTokenIterator theEnd = pCandidates->end();
   for (LDictionaryTokenIterator i = pCandidates->begin(); i != theEnd; i++)
   {
      if (GermanNonHead != wrdCategory && cacheEntry && !StringUtil::isAllUpperCase(pText) && (pEntry = entryCache().FindEntry(*i)))
      {
         if (((wrdCategory == GermanHead) || (wrdCategory == GermanWord)) && !BOS)
         {
            // since the above returned entry is cached, save its contents
            // to temp and use pEntry (to be modified)
            pEntry = new LDictionaryEntry(*pEntry);
            SsuList & ssuLst = pEntry->semantoSyntacticUnits();

            if (StringUtil::beginsUpperCase(pText))
            {
               processMidSntnceCapWrd(ssuLst);
            }
            else
            {
               if( (ssuLst.size() == 1) && ((ssuLst.begin())->wordClassCode() == 1) &&
                   (pText.length() > 1) && CharUtil::isUpper(pText[1]) )
               {
                  // Skip special German nouns. See incident 2861
               }
               else
               {
                  processMidSntnceSmallWrd(ssuLst);
               }
            }
         }

         if (0 != pEntry->countOfSemantoSyntacticUnits())
         {
            return pEntry;
         }
         pEntry = 0;
      }
      else if (p_factory->validDictionaryToken(*i, false))
      {
         LDictionaryTokenVector bestWord;

         bestWord.push_back(*i);

         mergeCompanyWords(bestWord, i, theEnd);
         mergeCaseSensitivity(bestWord, i, theEnd);

         pEntry = new LDictionaryEntry(pText.language());

         pEntry->dictionaryToken(*i);

         p_factory->completeEntryComponents(pEntry, bestWord);

		 if (wrdCategory == GermanHead || wrdCategory == GermanNonHead)
		 {
			pEntry->removeTitleUnits();
            if (pEntry->countOfSemantoSyntacticUnits() == 0)
            {
               delete pEntry;
               pEntry = 0;
               continue;
            }
		 }

         if (wrdCategory == GermanWord || wrdCategory == GermanHead)
         {
            pEntry->removeNonHeadUnits();
            if (pEntry->countOfSemantoSyntacticUnits() == 0)
            {
               delete pEntry;
               pEntry = 0;
               continue;
            }
         }

         pEntry->eliminateUnitsForBadContexts();
         if (pEntry->countOfSemantoSyntacticUnits() == 0)
         {
            delete pEntry;
            pEntry = 0;
            continue;
         }
         if (wrdCategory != GermanNonHead)
         {
            pEntry->eliminateUnitsByDerivedForms();
         }
         if (pEntry->countOfSemantoSyntacticUnits() == 0)
         {
            delete pEntry;
            pEntry = 0;
            continue;
         }
         for (LSsuIterator ssu = pEntry->semantoSyntacticUnits().begin();
              ssu != pEntry->semantoSyntacticUnits().end(); ssu++)
         {
            ssu->setIsSourceTransferred(p_factory->IsSsuTransferred(*ssu));
         }
         pEntry->limitUnitsByTransfer();
         if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::EnglishID)
         {
            pEntry->limitUnitsByTransitivity();
         }
         if (wrdCategory == GermanNonHead)
         {
            SsuList & ssuLst = pEntry->semantoSyntacticUnits();
            if (wordPart)
            {
               removeInconsistentSsus(ssuLst, wordPart);
            }
            adjustSsuList(ssuLst, wordPart);			
         }

         pEntry->limitUnitsWithinPriorityOrder();
         if (pEntry->countOfSemantoSyntacticUnits() == 0)
         {
            delete pEntry;
            pEntry = 0;
            continue;
         }
         if (wrdCategory != GermanNonHead)
         {
            pEntry->limitUnitsByConflictingPriorityOrder();
         }
         if (pEntry->countOfSemantoSyntacticUnits() == 0)
         {
            delete pEntry;
            pEntry = 0;
            continue;
         }

         // since the above returned entry may be cached, save its contents
         // to temp and use pEntry (to be modified)
         LDictionaryEntry* temp = pEntry;
         if (((wrdCategory == GermanHead) || (wrdCategory == GermanWord)) && (!BOS) &&
             (!StringUtil::isAllUpperCase(pText)))
         {
            pEntry = new LDictionaryEntry(*temp);

            SsuList & ssuLst = pEntry->semantoSyntacticUnits();
            if (StringUtil::beginsUpperCase(pText))
            {
               processMidSntnceCapWrd(ssuLst);
            }
            else
            {
               if( (ssuLst.size() == 1) && ((ssuLst.begin())->wordClassCode() == 1) &&
                   (pText.length() > 1) && CharUtil::isUpper(pText[1]) )
               {
                  // Skip special German nouns. See incident 2861
               }
               else
               {
                  processMidSntnceSmallWrd(ssuLst);
               }
            }
         }

         if (temp)
         {
            if (GermanNonHead != wrdCategory && cacheEntry && !StringUtil::isAllUpperCase(pText))
            {
               entryCache().InsertEntry(*i, temp);
            } else {
// not deleting temp caused memory leak.
				if (((wrdCategory == GermanHead) || 
					(wrdCategory == GermanWord)) && (!BOS) &&
					(!StringUtil::isAllUpperCase(pText)))
						delete temp;
			}

            if (pEntry->countOfSemantoSyntacticUnits() == 0)
            {
               delete pEntry;
               pEntry = 0;
            }
            else
            {
               return pEntry;
            }
         }
      }
   }
   return pEntry;
}

// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::createRootEntry(LDictionaryTokenVector* pCandidates,
                                                          const LTextualComponent* pText,
                                                          const LTextualComponent* pSourceWord, 
														  WordCategory wrdCategory,
                                                          bool BOS)
{
   LDictionaryEntry* pEntry = 0;

   LgsString inflection(pSourceWord->substr(pText->length()));
   LDictionaryTokenIterator theEnd = pCandidates->end();
   for (LDictionaryTokenIterator i = pCandidates->begin(); i != theEnd; i++)
   {
      if (!((inflection == "S") && (i->wordPhrase().WordTypeCode() == 2)))
      {
         if ((GermanNonHead != wrdCategory) && (pEntry = v_rootEntryCache.FindEntry(*pSourceWord)))
         {
            if ((TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID) && (!BOS))
            {
               // since the above returned entry is cached, save its contents
               // to temp and use pEntry (to be modified)
               pEntry = new LDictionaryEntry(*pEntry);

               SsuList & ssuLst = pEntry->semantoSyntacticUnits();
               if (StringUtil::beginsUpperCase(*pSourceWord))
               {
                  processMidSntnceCapWrd(ssuLst);
               }
               else
               {
                  processMidSntnceSmallWrd(ssuLst);
               }
            }
            if (pEntry->countOfSemantoSyntacticUnits() != 0)
            {
               return pEntry;
            }
            pEntry = 0;
         }
         else
         {
            pEntry = entryCache().FindEntry(*pSourceWord);
            if (pEntry)
            {
               LDictionaryEntry* pTemp = new LDictionaryEntry(*pEntry);
               pEntry = pTemp;
            }
            else if (p_factory->validDictionaryToken(*i, false))
            {
               LDictionaryTokenVector bestWord;

               bestWord.push_back(*i);

               mergeCompanyWords(bestWord, i, theEnd);
               mergeCaseSensitivity(bestWord, i, theEnd);

               pEntry = new LDictionaryEntry(pText->language());
               pEntry->dictionaryToken(*i);
               p_factory->completeEntryComponents(pEntry, bestWord);
               pEntry->eliminateUnitsForBadContexts();

               if (pEntry->countOfSemantoSyntacticUnits() == 0)
               {
                  delete pEntry;
                  pEntry = 0;
               }
            }
            if (pEntry)
            {
               pEntry->eliminateUnitsByDerivedForms();

               if (pEntry->countOfSemantoSyntacticUnits() == 0)
               {
                  delete pEntry;
                  pEntry = 0;
                  continue;
               }

               for (LSsuIterator ssu  = pEntry->semantoSyntacticUnits().begin();
                    ssu != pEntry->semantoSyntacticUnits().end(); ssu++)
               {
                  ssu->setIsSourceTransferred(p_factory->IsSsuTransferred(*ssu));
               }
               pEntry->limitUnitsByTransfer();
			   if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::EnglishID)
			   {
					pEntry->limitUnitsByTransitivity();
			   }
               pEntry->limitUnitsWithinPriorityOrder();

               if (pEntry->countOfSemantoSyntacticUnits() == 0)
               {
                  delete pEntry;
                  pEntry = 0;
                  continue;
               }
               pEntry->limitUnitsByConflictingPriorityOrder();

               if (pEntry->countOfSemantoSyntacticUnits() == 0)
               {
                  delete pEntry;
                  pEntry = 0;
                  continue;
               }
               LDictionaryEntry* temp = pEntry;

               if ((GermanNonHead != wrdCategory) && (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID) && (!BOS))
               {
                  // since the above returned entry may be cached, save its contents
                  // to temp and use pEntry (to be modified)
                  pEntry = new LDictionaryEntry(*temp);

                  SsuList & ssuLst = pEntry->semantoSyntacticUnits();
                  if (StringUtil::beginsUpperCase(*pSourceWord))
                  {
                     processMidSntnceCapWrd(ssuLst);
                  }
                  else
                  {
                     processMidSntnceSmallWrd(ssuLst);
                  }
               }

               if (temp)
               {
                   if (GermanNonHead != wrdCategory)
                   {
                        v_rootEntryCache.InsertEntry(*pSourceWord, temp);
                   }
                  if (pEntry->countOfSemantoSyntacticUnits() == 0)
                  {
                     delete pEntry;
                     pEntry = 0;
                  }
                  else
                  {
                     return pEntry;
                  }
               }
            }
         }
      }
   }
   return pEntry;
}

// --------------------------------------------------------------------------
// This function removes SSUs for words at middle of sentence that start
// with capital letter and that are not word classes 1 OR 4
// --------------------------------------------------------------------------
void DictionaryEntryBuilder::processMidSntnceCapWrd(SsuList & ssuLst, bool phraseEntry)
{
    SsuList::iterator endIter = ssuLst.end();
    SsuList::iterator i = ssuLst.begin();
    if (v_possibleEOS)
    {
        for ( ;i != endIter; i++)
        {
            if (phraseEntry? (2 != i->wordClassCode()) : 
                           ((1 == i->wordClassCode()) || (4 == i->wordClassCode()) ||
                            (5 == i->wordClassCode()) || (14 == i->wordClassCode())))
            {
                break;            
            }
        }
    }
    if (i == endIter)
    {
        return;
    }
    SsuList::iterator beginIter = ssuLst.begin();
    for (SsuList::iterator j = ssuLst.end()-1; j >= beginIter; j--)
    {
        if (phraseEntry? (2 == j->wordClassCode()) : 
                       ((1 != j->wordClassCode()) && (4 != j->wordClassCode()) &&
                        (5 != j->wordClassCode()) && (14 != j->wordClassCode())))
        {
            ssuLst.erase(j);
        }
    }
}

// --------------------------------------------------------------------------
// This function removes SSUs for words at middle of sentence that start
// with small letter and that is word class 1
// --------------------------------------------------------------------------
void DictionaryEntryBuilder::processMidSntnceSmallWrd(SsuList & ssuLst)
{
   SsuList::iterator beginIter = ssuLst.begin();
   for (SsuList::iterator j = ssuLst.end()-1; j >= beginIter; j--)
   {
      // Erase only if word class is 1 and NOT an abbreviation and NOT an acronym
      if (1 == j->wordClassCode() && 3 != j->wordTypeCode() && 2 != j->wordTypeCode())
      {
         ssuLst.erase(j);
      }
   }
}

// --------------------------------------------------------------------------
// Creates a dictionary entry for a phrase object. This method differs from the other builder methods 
// in that its components use a different part of the factory and that a headword needs to be tested 
// for correctness with the database. Also note that phrases are not, at this time, cached.
// --------------------------------------------------------------------------
LDictionaryEntry* DictionaryEntryBuilder::createPhraseEntry(LDictionaryTokenVector* pCandidates,
                                                            const LWordIterator& beginSource,
                                                            const LWordIterator& endSource, bool BOS)
{
   LDictionaryEntry* pEntry = 0;

   LDictionaryTokenIterator theEnd = pCandidates->end();
   for (LDictionaryTokenIterator i = pCandidates->begin(); i != theEnd; i++)
   {
      if (p_factory->validDictionaryToken(*i, true))
      {
         LDictionaryTokenVector bestWord;

         bestWord.push_back(*i);

         mergeCompanyWords   (bestWord, i, theEnd);
         mergeCaseSensitivity(bestWord, i, theEnd);

         pEntry = new LDictionaryEntry(beginSource->language());

         pEntry->dictionaryToken(*i);

         p_factory->completePhraseComponents(pEntry, bestWord);
         if (pEntry->countOfSemantoSyntacticUnits() == 0)
         {
            delete pEntry;
            pEntry = 0;
            continue;
         }

         if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::EnglishID)
         {
            if (i->proposedHeadWord() && i->proposedHeadWord() != pEntry->headWord())
            {
               delete pEntry;
               pEntry = 0;
               continue;
            }
         }
         pEntry->eliminateUnitsForBadContexts();
         if (pEntry->countOfSemantoSyntacticUnits() == 0)
         {
            delete pEntry;
            pEntry = 0;
            continue;
         }
         pEntry->eliminateUnitsByDerivedForms();
         if (pEntry->countOfSemantoSyntacticUnits() == 0)
         {
            delete pEntry;
            pEntry = 0;
            continue;
         }
         for (LSsuIterator ssu = pEntry->semantoSyntacticUnits().begin();
              ssu != pEntry->semantoSyntacticUnits().end(); ssu++)
         {
            ssu->setIsSourceTransferred(p_factory->IsSsuTransferred(*ssu));
         }
         pEntry->limitUnitsByTransfer();
         if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::EnglishID)
         {
            pEntry->limitUnitsByTransitivity();
         }
         pEntry->limitUnitsWithinPriorityOrder();
         if (pEntry->countOfSemantoSyntacticUnits() == 0)
         {
            delete pEntry;
            pEntry = 0;
            continue;
         }
         pEntry->limitUnitsByConflictingPriorityOrder();
         if (pEntry->countOfSemantoSyntacticUnits() == 0)
         {
            delete pEntry;
            pEntry = 0;
         }

         if ((TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID) && (!BOS))
         {
            SsuList & ssuLst = pEntry->semantoSyntacticUnits();

            if (StringUtil::beginsUpperCase(*(beginSource+pEntry->headWord() - 1)))
            {
               processMidSntnceCapWrd(ssuLst, true);
            }
            else
            {
               processMidSntnceSmallWrd(ssuLst);
            }
         }

         if (pEntry->countOfSemantoSyntacticUnits() == 0)
         {
            delete pEntry;
            pEntry = 0;
         }

         if (pEntry)
         {
            return pEntry;
         }
      }
   }
   return pEntry;
}

// --------------------------------------------------------------------------
// Merges any word candidate that is the same as the target one (the target is pointed at by the
// "begin" parameter) except for the case of its lettering.
// It simply proceeds through the match candidates following the target and tests for equality 
// after the target and the tested match have been reduced to lower case.
// If the test succeeds the tested object joins the target match in the set of words that are going
// to constitute the new entry.
// --------------------------------------------------------------------------
void DictionaryEntryBuilder::mergeCaseSensitivity(LDictionaryTokenVector& bestWord,
                                                  LDictionaryTokenIterator begin,
                                                  LDictionaryTokenIterator end) const
{
   for (LDictionaryTokenIterator i = begin + 1; i != end; i++)
   {
      if (StringUtil::equalsIgnoreCase(*i, *begin))
      {
         bestWord.push_back(*i);
      }
   }
}

// --------------------------------------------------------------------------
// Merges any word candidate that is the same as the target one (the target is pointed at by the 
// "begin" parameter) except for the identity of the word candidate's company.
// It simply proceeds through the match candidates following the target and tests for equality 
// word text.
// If the test succeeds the tested object joins the target match in the set of words that are 
// going to constitute the new entry.
// Since the set of matches is a mathematical set no object will be entered more than once.
// --------------------------------------------------------------------------
void DictionaryEntryBuilder::mergeCompanyWords(LDictionaryTokenVector& bestWord,
                                               LDictionaryTokenIterator begin,
                                               LDictionaryTokenIterator end) const
{
   for (LDictionaryTokenIterator i = begin + 1; i != end; i++)
   {
      if (*i == *begin)
      {
         bestWord.push_back(*i);
      }
   }
}


// --------------------------------------------------------------------------
void DictionaryEntryBuilder::loadValue(IniParser & argParser, const LgsString & argDesc,
                                       int & wcParameter)
{
   const Argument & argVal = argParser.GetArgument(argDesc);
   if (!argVal.IsNull())
   {
      wcParameter = atoi(argVal.Value().c_str());
   }
}

// --------------------------------------------------------------------------
void DictionaryEntryBuilder::loadOverlapPriorNos(IniParser & argParser, const LgsString & argDesc,
                                                 LgsVector(int) & ovrlpPriorNos)
{
   const Argument & argVal = argParser.GetArgument(argDesc);
   if (!argVal.IsNull())
   {
      LgsVector(LgsString) arrayValues;
      StringUtil::parseInto(argVal.Value(), arrayValues, ',');
      for (LgsVector(LgsString)::iterator i = arrayValues.begin(); i != arrayValues.end(); i++)
      {
         int priorityNo = atoi(i->c_str());
         ovrlpPriorNos.push_back(priorityNo);
      }
   }
}

// --------------------------------------------------------------------------
void DictionaryEntryBuilder::removeInconsistentSsus(LgsVector(LSemantoSyntacticUnit)& ssuList, GermanTokenWord *wordPart)
{
   LgsVector(LSemantoSyntacticUnit)::iterator currElement = ssuList.begin();
   for (;currElement != ssuList.end();)
   {
	   if (!wordPart->allowedWordClass(currElement->wordClassCode(), currElement->patNumber()))
	   {
			currElement = ssuList.erase(currElement);
	   }
	   else
	   {
			currElement++;
	   }
   }	
}

// --------------------------------------------------------------------------
void DictionaryEntryBuilder::adjustSsuList(LgsVector(LSemantoSyntacticUnit)& ssuList, GermanTokenWord *wordPart)
{
   LgsVector(LSemantoSyntacticUnit)::iterator currElement = ssuList.begin();
   if (wordPart)
   {
		for (;currElement != ssuList.end();)
		{
			if (wordPart && !wordPart->allowedAsNonHead(currElement->wordClassCode(), currElement->patNumber()))
			{
				currElement = ssuList.erase(currElement);
			}
			else
			{
				currElement++;
			}
		}
   }

   IniParser argParser;
   char pgmConstantsFile[MAX_FILEPATH_LEN];

   GetConfigData("engine", "pgm_constants", pgmConstantsFile, MAX_FILEPATH_LEN);
   argParser.open(pgmConstantsFile, "language");
   loadValue(argParser, "noun", nounWC);
   loadValue(argParser, "verb", verbWC);
   loadValue(argParser, "adjective", adjWC);
   loadValue(argParser, "storedhalfnoun", storedHN);
   loadValue(argParser, "storedHNprior", storedHNprior);
   LgsVector(int) ovrlpPriorityNos;

   if (limitSSUsStoredHalfNoun(ssuList))
   {
      loadOverlapPriorNos(argParser, "ovrlp_prior_storedHN", ovrlpPriorityNos);
   }
   else if (limitSSUsForWClass(ssuList, adjWC))
   {
      loadOverlapPriorNos(argParser, "ovrlp_prior_adj", ovrlpPriorityNos);
   }
   else if (limitSSUsForWClass(ssuList, nounWC))
   {
      loadOverlapPriorNos(argParser, "ovrlp_prior_noun", ovrlpPriorityNos);
   }
   else if (limitSSUsForWClass(ssuList, verbWC))
   {
      loadOverlapPriorNos(argParser, "ovrlp_prior_verb", ovrlpPriorityNos);
   }

   if (!ovrlpPriorityNos.empty())
   {
      adjustSSUBasedOnOvrlpPriority(ovrlpPriorityNos, ssuList);
   }

   LgsVector(LSemantoSyntacticUnit)::iterator i = 
	   (ssuList.begin() == ssuList.end())? ssuList.end(): ssuList.begin()+1;
   while (i != ssuList.end())
   {
		i = ssuList.erase(i);
   }
}

// --------------------------------------------------------------------------
// This function adjusts the SSU list by looking for elements other than word
// class 4 with priority # 32 and removing them
// --------------------------------------------------------------------------
bool DictionaryEntryBuilder::limitSSUsStoredHalfNoun(LgsVector(LSemantoSyntacticUnit)& ssuList)
{
   LgsVector(LSemantoSyntacticUnit)::iterator currElement = ssuList.begin();
   for (;(currElement != ssuList.end()) && 
         ((currElement->wordClassCode() != storedHN) || (currElement->priorityOrder() != storedHNprior));
         currElement++);
   if (currElement != ssuList.end())
   {
      LgsVector(LSemantoSyntacticUnit)::iterator i = currElement - 1;
      while (i >= ssuList.begin())
      {
         ssuList.erase(i);
         i--;
      }
      i++;
      i++;
      while (i != ssuList.end())
      {
         if ((i->wordClassCode() != storedHN) || (i->priorityOrder() != storedHNprior))
         {
            i = ssuList.erase(i);
         }
         else
         {
            i++;
         }
      }
      return true;
   }
   else
   {
      return false;
   }
}

// --------------------------------------------------------------------------
// This function looks for elements in SSU that do not belong to word class
// passed as a parameter and removes them
// --------------------------------------------------------------------------
bool DictionaryEntryBuilder::limitSSUsForWClass(LgsVector(LSemantoSyntacticUnit)& ssuList, int wordClass)
{
   LgsVector(LSemantoSyntacticUnit)::iterator currElement = ssuList.begin();
   for (;(currElement != ssuList.end()) && (currElement->wordClassCode() != wordClass);
        currElement++);
   if (currElement != ssuList.end())
   {
      LgsVector(LSemantoSyntacticUnit)::iterator i = currElement - 1;
      while (i >= ssuList.begin())
      {
         ssuList.erase(i);
         i--;
      }
      i++;
      i++;
      while (i != ssuList.end())
      {
         if (i->wordClassCode() != wordClass)
         {
            i = ssuList.erase(i);
         }
         else
         {
            i++;
         }
      }
      return true;
   }
   else
   {
      return false;
   }
}

// --------------------------------------------------------------------------
// This removes the elements in SSU list that have overlap priority #s
// --------------------------------------------------------------------------
void DictionaryEntryBuilder::adjustSSUBasedOnOvrlpPriority(LgsVector(int)& ovrlpPriorityLst,
                                                           LgsVector(LSemantoSyntacticUnit)& ssuList)
{
   LgsVector(LSemantoSyntacticUnit)::iterator i = ssuList.end() - 1;
   while((ssuList.size() > 1) && (i >= ssuList.begin()))
   {
      LgsVector(int)::iterator j;
      for (j = ovrlpPriorityLst.begin();
           (j != ovrlpPriorityLst.end()) && (i->priorityOrder() != (*j)); j++);
      if (j != ovrlpPriorityLst.end())
      {
         ssuList.erase(i);
      }
      --i;
   }
}
