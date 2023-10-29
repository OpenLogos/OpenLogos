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
#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/hndepwrdlookup.h>
#include <logos_libs/utility/iniparser.h>
#include <logos_libs/linguistic/lword.h>
#include <logos_libs/translutility/translcommonobjects.h>
#include <logos_libs/halfnoun/germantoken.h>
#include <configdatafileinterface/configdatainterfacemain.h>

LgsString HNounDepWrdLookup::_conjPattern;
bool HNounDepWrdLookup::_initialized = false;
LgsVector(LgsString) HNounDepWrdLookup::_prefixv;
const int HNounDepWrdLookup::MaxEntries = 3;

HNounDepWrdLookup::HNounDepWrdLookup(DictionaryEntryBuilder & iEntryBuilder, PatternType iPattType)
                  :_builder(iEntryBuilder),
                   _pattType(iPattType),
                   _compNoWords(0),
                   _noWordsInWord(0),
                   _conjNoWords(0)
{
	if (!_initialized)
	{
		// Initialize Conjugate List
		IniParser argumentParser;
      char generalTableFileName[MAX_FILEPATH_LEN];

      GetConfigData("sourcedata", "generaltbl", generalTableFileName, MAX_FILEPATH_LEN);
		argumentParser.open(generalTableFileName, "Language");

		const Argument& arg = argumentParser.GetArgument("ConjugateList");
		if (!arg.IsNull())
		{
			LgsVector(LgsString) arrayValues;
			StringUtil::parseInto(arg.Value(), arrayValues, ',');
			_conjPattern = '(';

			LgsVector(LgsString)::iterator j, lastElement = arrayValues.end() - 1;
			for (j = arrayValues.begin();
				 j != lastElement;
				 j++)
			{
				_conjPattern += (*j);
				_conjPattern += '|';
			}
			// Process the last element and terminate the pattern
			_conjPattern += (*j);
			_conjPattern += ')';
		}
		const Argument & prefixArg = argumentParser.GetArgument("PrefixList");
		if (!arg.IsNull())
		{
			StringUtil::parseInto(prefixArg.Value(), _prefixv, ',');
		}
		_initialized = true;
	}
   
	if (iPattType == HNounBeginPattern)
	{
		_pattern = "([[:alpha:]]+(-[[:alpha:]]+)*) +";
		_pattern += _conjPattern;
		_pattern += " +(-)([[:alpha:]]+)";
		_wordGroupNo = 5;
		_hyphenGroupNo = 4;
		_conjGroupNo = 3;
		_compoundGroupNo = 1;
		_wordPos = 2;
		_conjPos = 1;
		_compoundPos = 0;
	}
   else
   {
		_pattern = "([[:alpha:]]+)(-) +";
		_pattern += _conjPattern;
		_pattern += " +([[:alpha:]]+(-[[:alpha:]]+)*)";
		_wordGroupNo = 1;
		_hyphenGroupNo = 2;
		_conjGroupNo = 3;
		_compoundGroupNo = 4;
		_wordPos = 0;
		_conjPos = 1;
		_compoundPos = 2;
	}
	initializeRE();
}


HNounDepWrdLookup::~HNounDepWrdLookup(void)
{
}


// lookup will call dictionary().createSentenceUnit(word+iMergeWord)
void HNounDepWrdLookup::_lookup(bool BOS, bool bAllCapitalWords)
{
   LDictionaryEntry * dictEntries[MaxEntries];
	// First do a halfnoun (compound) word lookup

   GetWordBoundaries(_wordStart, _wordEnd, _wordGroupNo);
   GetWordBoundaries(_conjStart, _conjEnd, _conjGroupNo);
   GetWordBoundaries(_compoundStart, _compoundEnd, _compoundGroupNo);

	LWordIterator lookupWord = _compoundStart;
	LgsString origStr(*lookupWord);
	LgsString compoundStr(*lookupWord);
   LWordIterator currIter = _compoundStart;
   if (currIter != _compoundEnd)
   {
      do
      {
         currIter++;
		   LgsString componentPart((*currIter).c_str());
		   StringUtil::toLower(componentPart);
		   if (componentPart != "-")
			   compoundStr += componentPart;
      } while(currIter != _compoundEnd);
   }

	LDictionaryEntry *dictEntry = _builder.matchSingleWord(lookupWord, BOS, DictionaryEntryBuilder::GermanWord, sourceWords(), true, bAllCapitalWords);
	(*lookupWord).setText(origStr);
	_compNoWords = (_compoundEnd-_compoundStart)+1;
	dictEntries[_compoundPos] = dictEntry;
	
	// Extract head word from _compoundDE
	LgsString mergeWord;

	LDictionaryEntry *wordDE = 0;
	lookupWord = _wordStart;
	origStr = (*lookupWord);
	bool prefixSuffix = false;
				
	// Process Prefix/Suffix pattern here
	LgsVector(LgsString)::iterator endOfVector = _prefixv.end();
	LgsVector(LgsString)::iterator prefix;
	LgsString tempStr(compoundStr);
	if (StringUtil::beginsUpperCase(tempStr))
	{
		StringUtil::toLower(tempStr);
	}

   for (prefix = _prefixv.begin(); 
        prefix != endOfVector && tempStr.compare(0, prefix->length(), *prefix); prefix++);

	LgsString tempWrd = (*lookupWord);
	if (StringUtil::beginsUpperCase(tempWrd))
	{
		StringUtil::toLower(tempWrd);
	}

	if (prefix != endOfVector && binary_search(_prefixv.begin(), endOfVector-1, tempWrd))
	{
		prefixSuffix = true;
		mergeWord = tempStr.substr(prefix->length());
		(*lookupWord) += mergeWord;
		wordDE = _builder.matchSingleWord(lookupWord, BOS, DictionaryEntryBuilder::GermanWord, sourceWords(), true, bAllCapitalWords);
	}

	if (!dictEntry->isUnfoundWord() && !prefixSuffix)
	{
		if (dictEntry->isCompound())
		{

			LCompoundDictionaryEntry & compoundEntry = dynamic_cast<LCompoundDictionaryEntry &>(*dictEntry);

			int entryCount = compoundEntry.numberOfEntries();
			int componentPos, incValue, finalValue, matchPos;

         if (_pattType == HNounBeginPattern)
			{
				componentPos = entryCount - 1;
				incValue = -1;
				finalValue = 0;
				matchPos = compoundStr.length() - 1;
			} else
			{
				componentPos = 1;
				incValue = 1;
				finalValue = entryCount;
				matchPos = 0;
			}
			for (; (!wordDE || wordDE->isUnfoundWord()) && componentPos != finalValue;
				    componentPos+= incValue)
			{
				dictEntry = compoundEntry.getEntry(componentPos);
				if (dictEntry->isUnfoundWord())
				{
					continue;
				}
				lookupWord->setText(origStr);
				const LDictionaryToken & dictToken = dictEntry->dictionaryToken();
				DWordPhrase & wordPhrase = dictToken.wordPhrase();
				LgsString wordValue(wordPhrase.Word());
				if (_pattType == HNounBeginPattern)
				{
					StringUtil::toLower(wordValue);
					matchPos = compoundStr.rfind(wordValue.c_str(), matchPos);
					mergeWord = compoundStr.substr(0, matchPos);
					(*lookupWord).insert(0, mergeWord.c_str());
				}
            else 
				{
					StringUtil::toLower(wordValue);
					matchPos = compoundStr.find(wordValue.c_str(), matchPos);
					mergeWord = compoundStr.substr(matchPos, compoundStr.length() - matchPos);
					matchPos += wordValue.length()-1;
					StringUtil::toLower(mergeWord);
					(*lookupWord) += mergeWord;
				}
				wordDE = _builder.matchText(*lookupWord, BOS, DictionaryEntryBuilder::GermanWord, true, bAllCapitalWords);
				if (!wordDE)
				{
				   wordDE = _builder.matchRoot(*lookupWord, DictionaryEntryBuilder::GermanWord, BOS, bAllCapitalWords);
				}
				//wordDE = _builder.matchSingleWord(*lookupWord, BOS, DictionaryEntryBuilder::GermanWord, sourceWords(), true, bAllCapitalWords);
			}
		}
      else 
		{
			int startOfHead = dictEntry->hashLocation()-1;
			if (startOfHead && startOfHead!=-1)
			{
				if (_pattType == HNounBeginPattern)
				{
					mergeWord = compoundStr.substr(0, startOfHead);
					(*lookupWord).insert(0, mergeWord.c_str());
				}
				else
				{
					mergeWord = compoundStr.substr(startOfHead);
					(*lookupWord)  += mergeWord;
				}
				wordDE = _builder.matchText(*lookupWord, BOS, DictionaryEntryBuilder::GermanWord, true, bAllCapitalWords);
				if (!wordDE)
				{
				   wordDE = _builder.matchRoot(*lookupWord, DictionaryEntryBuilder::GermanWord, BOS, bAllCapitalWords);
				}
			} 
		}
      if (wordDE)
      {
         wordDE->derivedFormCode();
         if (_pattType == HNounBeginPattern)
         {
            lookupWord->setModificationType(LWord::REMOVE_NON_HEAD); 
            _compoundEnd->setModificationType(LWord::REMOVE_NON_HEAD);
         }
         else
         {
            lookupWord->setModificationType(LWord::REMOVE_HEAD); 
            _compoundEnd->setModificationType(LWord::REMOVE_HEAD);
         }
      }
	}
   bool wordNotFound;
	(*lookupWord).setText(origStr);
	if (wordNotFound = !wordDE || wordDE->isUnfoundWord())
	{
		if (StringUtil::beginsUpperCase(compoundStr))
		{
			origStr[0] = CharUtil::upper(origStr[0]);
		}
		(*lookupWord).setText(origStr);
		DictionaryEntryBuilder::WordCategory wrdCategory = DictionaryEntryBuilder::GermanWord;
		if (!prefixSuffix)
		{			
			wrdCategory = (_pattType == HNounBeginPattern)? DictionaryEntryBuilder::GermanHead
			:DictionaryEntryBuilder::GermanNonHead;
		}
		wordDE = _builder.matchSingleWord(lookupWord, BOS, wrdCategory, sourceWords(), true, bAllCapitalWords);
		bool nonHeadDecompose = false;
		if (wordDE->isUnfoundWord() && (wrdCategory == DictionaryEntryBuilder::GermanNonHead))
		{
			bool beginsUpperCase = StringUtil::beginsUpperCase(*lookupWord);
			LCompoundDictionaryEntry *compoundEntry =  new LCompoundDictionaryEntry(lookupWord->language());
			if (!_builder.populateCompoundDE(compoundEntry, *lookupWord, false, false,
                                          beginsUpperCase, BOS, bAllCapitalWords))
			{
				delete compoundEntry;
				compoundEntry = 0;
			}
			else
			{
				nonHeadDecompose = true;
				delete wordDE;
				wordDE = compoundEntry;
			}
		}
		if (!nonHeadDecompose && (wrdCategory == DictionaryEntryBuilder::GermanNonHead || prefixSuffix))
		{
			SsuList& ssuList = wordDE->semantoSyntacticUnits();
			SsuList::iterator endIter = ssuList.end();

			for (SsuList::iterator iter = ssuList.begin(); iter != endIter; iter++)
			{
				iter->setWordClassCode(4);
				iter->setFormCode(9);
			}
		}
	}
	_noWordsInWord = 2;
	dictEntries[_wordPos] = wordDE;

	// Do lookup for conjunctions
	lookupWord = _conjStart;
	origStr = (*lookupWord);
	lookupWord->setText(getSubString(_conjGroupNo));
	dictEntry = _builder.matchSingleWord(lookupWord, BOS, DictionaryEntryBuilder::GermanWord, sourceWords(), true, bAllCapitalWords);
	_conjNoWords = _conjEnd - _conjStart + 1;
	dictEntries[_conjPos] = dictEntry;
	lookupWord->setText(origStr);

	if (!wordNotFound)
	{
		SsuList& ssuList = dictEntries[_conjPos]->semantoSyntacticUnits();
		SsuList::iterator endIter = ssuList.end();

		for (SsuList::iterator iter = ssuList.begin(); iter != endIter; iter++)
      {
         iter->setOverflow3b(4);
      }
	}
   for (int posNo = 0; posNo < MaxEntries; addEntry(dictEntries[posNo]), posNo++);
}

void HNounDepWrdLookup::SetWordCount(LDictionaryEntry * dictEntry, short dictIndex)
{
   if (!dictEntry)
   {
      return;
   }
   if (dictIndex == _compoundPos)
      dictEntry->wordsUsedInMatch(_compNoWords);
   else if (dictIndex == _conjPos)
      dictEntry->wordsUsedInMatch(_conjNoWords);
   else if (dictIndex == _wordPos)
      dictEntry->wordsUsedInMatch(_noWordsInWord);
}
