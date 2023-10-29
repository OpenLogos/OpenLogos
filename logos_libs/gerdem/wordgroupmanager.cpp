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
// WordGroupManager.cpp: implementation of the WordGroupManager class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/dictionaryentrybuilder.h>
#include <logos_libs/translutility/translcommonobjects.h>
#include <logos_libs/gerdem/hndepwrdlookup.h>
#include <logos_libs/gerdem/wordgroupmanager.h>

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

// Instantiates the various WordGroupLookup classes and stores them in the vector
WordGroupManager::WordGroupManager(DictionaryEntryBuilder & entryBuilder) :
                  _matchedGroup(0), _initialized(false), _entryBuilder(entryBuilder)
{
}

// Destroys the WordGroupLookup class instances in the vector
WordGroupManager::~WordGroupManager()
{
}

void WordGroupManager::CleanupMemory()
{
   for (WordGroupLookupIterator wordGroup = _wordGroups.begin(); wordGroup != _wordGroups.end();)
   {
      WordGroupLookup * temp = (*wordGroup);
      wordGroup = _wordGroups.erase(wordGroup);
      delete temp;
   }
   WordGroupLookup::destroyST_Variable();
}

void WordGroupManager::initialize(LWordVector * srcWords)
{
   WordGroupLookup::setST_Variable(srcWords);
   if (!_initialized)
   {
      WordGroupLookup *wordGroup = 0;
      if (LLanguage::GermanID == TranslCommonObjects::GetSourceLanguage()->id())
      {
         wordGroup = new HNounDepWrdLookup(_entryBuilder, HNounDepWrdLookup::HNounBeginPattern);
         _wordGroups.push_back(wordGroup);
         wordGroup = new HNounDepWrdLookup(_entryBuilder, HNounDepWrdLookup::HNounEndPattern);
         _wordGroups.push_back(wordGroup);
      }
      _initialized = true;
   }
   WordGroupLookupIterator endIter = _wordGroups.end();
   for (WordGroupLookupIterator wordGroup = _wordGroups.begin(); 
        wordGroup != endIter; wordGroup++)
   {
           (*wordGroup)->initialize();
   }
}

void WordGroupManager::lookup(bool BOS, bool bAllCapitalWords)
{
   _matchedGroup->lookup(BOS, bAllCapitalWords);
}

void WordGroupManager::restart(void)
{
   if (_matchedGroup)
   {
      _matchedGroup->restart();
   }
}

WordGroupManager::operator bool(void)
{
   bool retVal = false;
   if (_matchedGroup)
   {
      retVal = (*_matchedGroup);
      if (!retVal)
      {
         _matchedGroup = 0;
      }
   }
   return retVal;
}

LDictionaryEntry * WordGroupManager::operator++(int)
{
   LDictionaryEntry *retVal = 0;
   if (_matchedGroup)
   {
      retVal = (*_matchedGroup)++;
      if (!retVal)
      {
         _matchedGroup = 0;
      }
   }
   return retVal;
}

bool WordGroupManager::matchPattern(const LWordIterator & iWord)
{
   WordGroupLookupIterator wordGroup, endIter = _wordGroups.end();
   for (wordGroup = _wordGroups.begin(); 
        wordGroup != endIter && !(*wordGroup)->matchPattern(iWord);
        wordGroup++);
   if (wordGroup == endIter)
   {
      return false;
   }
   _matchedGroup = *wordGroup;
   return true;
}

void WordGroupManager::updateWordPos(int iNoWords, const LWordIterator & iWord)
{
   WordGroupLookupIterator endIter = _wordGroups.end();
   for (WordGroupLookupIterator wordGroup = _wordGroups.begin(); 
        wordGroup != endIter; wordGroup++)
   {
      (*wordGroup)->updateWordPos(iNoWords, iWord);
   }
}

