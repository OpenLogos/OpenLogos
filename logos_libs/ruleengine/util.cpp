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
// File - Util.cpp
//
// Class - RE_Util
//
//----------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/ruleengine/util.h>
#include <logos_libs/utility/stringutil.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace RuleEngine {

void RE_Util::loadPhraseManager(LWordVector* sourceWords, PhraseManager* phraseManager)
{
   int words = 0;
   for (LWordIterator iter = sourceWords->begin(); iter != sourceWords->end(); iter++)
   {
      const LgsString& word = *iter;
      assert(word.length() != 0);

      int spaces;
      LWordIterator next = iter + 1;
      if (next == sourceWords->end())
         spaces = 0;
      else
         spaces = next->precedingSpaces();

      phraseManager->appendPhrase(word, words++, spaces);
   }
}

void RE_Util::loadPhraseManager(TargetUnitVector* targetWords, PhraseManager* phraseManager)
{
   int words = 0;
   for (TargetUnitIterator iter = targetWords->begin(); iter != targetWords->end(); iter++)
   {
      phraseManager->appendPhrase((*iter)->surfaceExpressionAsString(), words++,
                                  (*iter)->trailingSpaces());
   }
}

void RE_Util::updateSourceWords(LWordVector* sourceWords, PhraseManager* phraseManager,
                                bool updatePhraseManager)
{
   int currentPhrase;              // index of current phrase in phrase manager
   int currentWord;                // index of current word in sourceWords
   bool dirty = false;             // one ore more phrase has changed

   int precedingSpaces = 0;
   int totWords = sourceWords->size();

   for (currentPhrase = 0, currentWord = 0; currentPhrase < totWords; ++currentPhrase, ++currentWord)
   {
      bool dirtyPhrase = false;   // current phrase has been changed

      if (!phraseManager->phraseExists(currentPhrase))
      {
         dirtyPhrase = true;
         LWord& currWord = *(sourceWords->begin() + currentWord);

         if ((currWord.trailingSpaces() == 0) && (currWord.precedingSpaces() == 0))
         {
            if (currentWord != 0)
            {
               (*(sourceWords->begin() + currentWord - 1)).setTrailingSpaces(0);
            }
            if (currentWord != (sourceWords->size() - 1))
            {
               (*(sourceWords->begin() + currentWord + 1)).setPrecedingSpaces(0);
            }
         }
         else
         {
            if (currentWord != 0)
            {
               (*(sourceWords->begin() + currentWord - 1)).setTrailingSpaces(1);
            }
            if (currentWord != (sourceWords->size() - 1))
            {
               (*(sourceWords->begin() + currentWord + 1)).setPrecedingSpaces(1);
            }
         }

         sourceWords->erase(sourceWords->begin() + currentWord--);
      }
      else
      {
         if (phraseManager->phraseChanged(currentPhrase))
         {
            dirtyPhrase = true;

            // get the current phrase including trailing spaces
            LgsString phrase = phraseManager->getFullPhrase(currentPhrase);

            // split phrase into words
            LgsVector(LgsString) tokens;
            LgsVector(int) spaces;
            StringUtil::parseInto(phrase, tokens, spaces);
            assert(spaces.size() == tokens.size() + 1);

            int tokenNo = 0;
            if (tokens.size() == 0)
            {
               if (spaces[0])
               {
                  if (currentWord != 0)
                  {
                     (*(sourceWords->begin() + currentWord - 1)).setTrailingSpaces(spaces[0]);
                  }
                  if (currentWord != (sourceWords->size() - 1))
                  {
                     (*(sourceWords->begin() + currentWord + 1)).setPrecedingSpaces(spaces[0]);
                  }
               }

               // must have been a blank word, with or without trailing spaces
               sourceWords->erase(sourceWords->begin() + currentWord--);
            }
            else
            {
               LWord& currWord = *(sourceWords->begin() + currentWord);

               // replace word in the vector sourceWords with the first word in the list
               currWord.setText(tokens[tokenNo]);
               int noSpaces = spaces[tokenNo];
               if (tokenNo == 0)
               {
                  noSpaces += precedingSpaces;
                  if (noSpaces && (currentWord != 0))
                  {
                     (*(sourceWords->begin() + currentWord - 1)).setTrailingSpaces(noSpaces);
                  }
               }
               tokenNo++;
               currWord.setPrecedingSpaces(noSpaces);
               currWord.setTrailingSpaces(spaces[tokenNo]);

               // if there are more words in the list
               // insert the remaining words into the sourceWords vector
               for (; tokenNo < tokens.size(); tokenNo++)
               {
                  currentWord++;
                  LWord nextWord = currWord;

                  // set correct token type
                  if (nextWord.getTokenType() != LookupTokenType::tok_none)
                  {
                     nextWord.setTokenType(LookupTokenType::tok_continuation);
                  }

                  nextWord.setText(tokens[tokenNo]);
                  nextWord.setPrecedingSpaces(spaces[tokenNo]);
                  nextWord.setTrailingSpaces(spaces[tokenNo + 1]);
                  sourceWords->insert(sourceWords->begin() + currentWord, nextWord);
               }
            }
            LWordVector::iterator nextWord = sourceWords->begin() + currentWord + 1;
            if (nextWord != sourceWords->end())
            {
               nextWord->setPrecedingSpaces(spaces[tokenNo]);
            }
         }
      }

      dirty |= dirtyPhrase;

      // store trailing spaces of current phrase in precedingSpaces variable
      // for use by the next phrase when the loop increments currentPhrase
      if (phraseManager->phraseExists(currentPhrase))
      {
         phraseManager->getPhrase(currentPhrase, &precedingSpaces);
      }
   }

   if (dirty && updatePhraseManager)
   {
      // reload phraseManager so that id's are consecutive
      phraseManager->erase();
      loadPhraseManager(sourceWords, phraseManager);
   }
}

void RE_Util::getKeys(MatchInfo matchInfo, PhraseManager* phraseManager, PhraseManager::Key& startKey, 
                      PhraseManager::Key& endKey, int group)
{
   RegularExpression::MatchPos match = matchInfo.getMatch(group);
   assert(match.length != 0);
   startKey = phraseManager->getPhraseAtPos(match.pos);
   endKey = phraseManager->getPhraseAtPos(match.pos + match.length - 1) + 1;
}

