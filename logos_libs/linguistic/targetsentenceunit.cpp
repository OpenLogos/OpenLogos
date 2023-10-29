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
//-------------------------------------------------------------------
// File - targetsentenceunit.cpp
//
// Class - TargetSentenceUnit (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/targetsentenceunit.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/linguistic/blackhole.h>
#include <logos_libs/linguistic/lsentence.h>
#include <logos_libs/translutility/translcommonobjects.h>

//-------------------------------------------------------------------
TargetSentenceUnit::TargetSentenceUnit()
                   :v_initialUpperCaseFlag(false),
                    v_allUpperCaseFlag(false),
                    v_trailingSpaces(1),
                    v_hadSpaceClosed(false),
                    v_isProperName(false),
                    v_isProtected(false),
                    v_acceptTransfer(false),
                    v_phraseModifier(LWord::NO_REMOVE),
                    v_merged(false),
                    v_prevMergeUnit(NULL),
                    v_nextMergeUnit(NULL)
{
}
//-------------------------------------------------------------------
TargetSentenceUnit::TargetSentenceUnit(LDictionary& dictionary)
                   :SentenceUnit(dictionary),
                    v_initialUpperCaseFlag(false),
                    v_allUpperCaseFlag(false),
                    v_trailingSpaces(1),
                    v_hadSpaceClosed(false),
                    v_isProperName(false),
                    v_isProtected(false),
                    v_acceptTransfer(false),
                    v_phraseModifier(LWord::NO_REMOVE),
                    v_merged(false),
                    v_prevMergeUnit(NULL),
                    v_nextMergeUnit(NULL)
{
}
//-------------------------------------------------------------------
TargetSentenceUnit::TargetSentenceUnit(const TargetSentenceUnit& rhs)
                   :SentenceUnit(rhs),
                    sconTable(rhs.sconTable),
                    v_opadr(rhs.v_opadr),
                    v_sourcePrimarySsuPosition(rhs.v_sourcePrimarySsuPosition),
                    v_initialUpperCaseFlag(rhs.v_initialUpperCaseFlag),
                    v_allUpperCaseFlag(rhs.v_allUpperCaseFlag),
                    v_hadSpaceClosed(rhs.v_hadSpaceClosed),
                    v_trailingSpaces(rhs.v_trailingSpaces),
                    sconPointerVector_(rhs.sconPointerVector_),
                    v_isProperName(rhs.v_isProperName),
                    v_isProtected(rhs.v_isProtected),
                    v_acceptTransfer(rhs.v_acceptTransfer),
                    v_phraseModifier(rhs.v_phraseModifier),
                    v_companyCode(rhs.v_companyCode),
                    v_merged(rhs.v_merged),
                    v_prevMergeUnit(rhs.v_prevMergeUnit),
                    v_nextMergeUnit(rhs.v_nextMergeUnit)
{
}
//-------------------------------------------------------------------
TargetSentenceUnit::~TargetSentenceUnit()
{
   if (v_prevMergeUnit)
   {
      delete v_prevMergeUnit;
   }
   if (v_nextMergeUnit)
   {
      delete v_nextMergeUnit;
   }
}
//-------------------------------------------------------------------
void TargetSentenceUnit::adjustAsEos()
{
   v_trailingSpaces = 0;
}
//---------------------------------------------------------------------
bool TargetSentenceUnit::allowsPrecedingSpaces() const
{
   LgsString& str = (LgsString&) surfaceExpressionAsString();
   if (LLanguage::PUNCTUATION == wordClassCode())
   {
      if ((str == "(") || (str == "[") || (str == "{") || (str[0] == '-'))
         return true;
      else
         return false;
   }
   else
   {
      if ((str.length() >= 1) && (str.find_first_of(".,;:?!%]})>/'\"", 0) == 0))
         return false;
   }
   return true;
}
//---------------------------------------------------------------------
bool TargetSentenceUnit::allowsTrailingSpaces() const
{
   if (LLanguage::PUNCTUATION == wordClassCode())
   {
      LgsString& str = (LgsString&) surfaceExpressionAsString();
      if ((str == "(") || (str == "[") || (str == "{") || (str == "/"))
         return false;
   }
   return true;
}
//---------------------------------------------------------------------
bool TargetSentenceUnit::isOpenBracketPunctuation() const
{
   if (LLanguage::PUNCTUATION == wordClassCode())
   {
      LgsString& str = (LgsString&) surfaceExpressionAsString();
      if ((str == "(") || (str == "[") || (str == "{"))
         return true;
   }
   return false;
}
//---------------------------------------------------------------------
bool TargetSentenceUnit::isInhibitCapConstant() const
{
   return false;
}
//---------------------------------------------------------------------
int TargetSentenceUnit::precedingSpaces() const
{
   return 0;
}
//---------------------------------------------------------------------
void TargetSentenceUnit::assignMarkupToWords()
{
   for (LWordIterator i = surfaceWords().begin(); i != surfaceWords().end(); i++)
   {
      i->markupId(wordMarkup()->id());
      i->markup(*wordMarkup());
   }
}
//-------------------------------------------------------------------
int TargetSentenceUnit::blackHoleSentenceAddress() const
{
   return 0;
}
//-------------------------------------------------------------------
void TargetSentenceUnit::insertBlackHole(const BlackHole*)
{
   // This may eventually be turned into an exception --
   // in any case -- the only unit that receives this message
   // should be of the TranslatedSentenceUnit subclass

   assert (0);
}
//-------------------------------------------------------------------
bool TargetSentenceUnit::isBlackHoleStart() const
{
   return false;
}
//-------------------------------------------------------------------
bool TargetSentenceUnit::isBlackHoleEnd() const
{
   return false;
}
//-------------------------------------------------------------------
bool TargetSentenceUnit::isFunctionalConstant() const
{
   return false;
}
//-------------------------------------------------------------------
bool TargetSentenceUnit::isCloseSpaceFunctionalConstant(TargetUnitIterator prev) const
{
   return false;
}
//-------------------------------------------------------------------
int TargetSentenceUnit::patNumber() const
{
   return 0;
}
//-------------------------------------------------------------------
int TargetSentenceUnit::usageID() const
{
   return 0;
}
//---------------------------------------------------------------------
void TargetSentenceUnit::generateDictionaryEntry()
{
   checkSconValues();
}
//---------------------------------------------------------------------
void TargetSentenceUnit::generateDictionaryEntry(LgsString& theString)
{
}
//-------------------------------------------------------------------
void TargetSentenceUnit::generateStem()
{
}
//-------------------------------------------------------------------
void TargetSentenceUnit::findAssociatedSourceUnit(SourceUnitVector&)
{
}
//---------------------------------------------------------------------
void TargetSentenceUnit::reconcileMarkup()
{
}
//---------------------------------------------------------------------
void TargetSentenceUnit::makeAspiration(TargetSentenceUnit* prev)
{
}
//---------------------------------------------------------------------
void TargetSentenceUnit::setSurfaceExpressionFromDictionary()
{
}
//---------------------------------------------------------------------
void TargetSentenceUnit::capitalize(int offset)
{
   if (!v_acceptTransfer)
   {
      if ((offset == 1) && (caseState() != SentenceUnit::AllUpperCase) && !isProtected())
      {
         for (SourceUnitIterator i = sentence().sourceSentenceUnits().begin();
              i != sentence().sourceSentenceUnits().end(); i++)
         {
            if (((*i)->sentenceAddress() != SentenceUnit::BosIdentifier) &&
                (*i)->isValidFirstWord())
            {
               if (((*i)->caseState() == SentenceUnit::BeginsUpperCase) ||
                   ((*i)->caseState() == SentenceUnit::AllUpperCase) ||
                   (*i)->isProtected())
               {
                  setCaseState(SentenceUnit::BeginsUpperCase);
               }
               break;
            }
         }
      }
      if (sentence().isTargetAllUpperCase() && (surfaceExpressionAsString() != "^p"))
      {
         setCaseState(SentenceUnit::AllUpperCase);
      }
      if (caseState() == SentenceUnit::AllUpperCase)
      {
         LgsString& s = (LgsString&) surfaceExpressionAsString();
   	   if (TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::GermanID)
            StringUtil::toUpperGerman(s);
         else
            StringUtil::toUpper(s);
         surfaceExpressionFromString(s);
         return;
      }
      if (caseState() == SentenceUnit::BeginsUpperCase)
      {
         LgsString& s = (LgsString&) surfaceExpressionAsString();
         StringUtil::capitalize(s);
         surfaceExpressionFromString(s);
      }
   }
}
//-------------------------------------------------------------------
int TargetSentenceUnit::trailingSpaces() const
{
   return v_trailingSpaces;
}
//-------------------------------------------------------------------
void TargetSentenceUnit::updateSourcePrimarySsu()
{
}
//-------------------------------------------------------------------
void TargetSentenceUnit::mergeMarkupAndSpaces(const TargetSentenceUnit& unit)
{
   wordMarkup()->orMask(unit.markup());
   v_trailingSpaces += unit.v_trailingSpaces;
   if (unit.v_hadSpaceClosed)
      v_hadSpaceClosed = true;
}
//-------------------------------------------------------------------
void TargetSentenceUnit::persistOut(ostream& stream)
{
   SentenceUnit::persistOut(stream);
}
//-------------------------------------------------------------------
void TargetSentenceUnit::persistIn(istream& stream)
{
   SentenceUnit::persistIn(stream);
}
//-------------------------------------------------------------------
TargetUnitVector::~TargetUnitVector()
{
}
//-------------------------------------------------------------------
void TargetUnitVector::removeAndDelete(TargetUnitIterator start, TargetUnitIterator end)
{
   for (iterator i = start; i != end; i++)
   {
      TargetSentenceUnit* p = *i;
      delete p;
   }
   erase(start, end);
}
//-------------------------------------------------------------------
void TargetUnitVector::removeAndDeleteAll()
{
   removeAndDelete(begin(), end());
}
//-------------------------------------------------------------------
int TargetSentenceUnit::wordClassCode() const
{
   return sconTable.wordClass();
}
//-------------------------------------------------------------------
void TargetSentenceUnit::setWordClassCode(int value)
{
   sconTable.setWordClass(value);
}
//-------------------------------------------------------------------
void TargetSentenceUnit::setSconTable(const SconTable& newSconTable)
{
   sconTable = newSconTable;
}
//-------------------------------------------------------------------
const SconTable& TargetSentenceUnit::getSconTable() const
{
   return sconTable;
}
//-------------------------------------------------------------------
bool TargetSentenceUnit::isValidFirstWord()
{
   // If punctuation then not valid.
   if (isPunctuation() || (surfaceExpressionAsString() == "ø"))
   {
      return false;
   }
   else if (sourceSentenceUnit())
   {
      for (SourceUnitIterator i = sentence().sourceSentenceUnits().begin();
           i != sentence().sourceSentenceUnits().end(); i++)
      {
         if (((*i)->sentenceAddress() != SentenceUnit::BosIdentifier) &&
             (*i)->isValidFirstWord())
         {
            if ((sourceSentenceUnit()->originalSentencePosition() == 0) ||
                (sourceSentenceUnit()->originalSentencePosition() >= (*i)->originalSentencePosition()))
            {
               return true;
            }
            else
            {
               return false;
            }
         }
      }
   }
   return true;
}
//-------------------------------------------------------------------
void TargetSentenceUnit::sconPointer(int x, bool appendBack)
{
   if (appendBack || sconPointerVector_.empty())
   {
      sconPointerVector_.push_back(x);
   }
   else
   {
      sconPointerVector_.insert(sconPointerVector_.begin(), x);
   }
}
//-------------------------------------------------------------------
void TargetSentenceUnit::sconPointerVector(LgsVector(int)& x, bool appendBack)
{
   LgsVector(int)::iterator iter;

   if (appendBack || sconPointerVector_.empty())
   {
      for (iter = x.begin(); iter != x.end(); iter++)
      {
         sconPointerVector_.push_back(*iter);
      }
   }
   else
   {
      for (iter = (x.end() - 1); iter >= x.begin(); iter--)
      {
         sconPointerVector_.insert(sconPointerVector_.begin(), *iter);
      }
   }
}

//-------------------------------------------------------------------
// Display on console content of this vector (for testing)
//-------------------------------------------------------------------
void TargetUnitVector::display()
{
	cout << endl << "*** TARGET SENTENCE UNITS ***" << endl;
	cout << "#\tSSUpos\tsconPtr\tTSU" << endl;
	TargetUnitIterator tsu;
	int n = 0;
	for (tsu = begin(); tsu != end(); tsu++) 
	{
		cout << ++n;
		cout << "\t" << (*tsu)->sourceUnitPosition();
		cout << "\t" << (*tsu)->sconPointer();
		cout << "\t" << (*tsu)->surfaceExpressionAsString();
		cout << endl;
	}
}
//-------------------------------------------------------------------
// Check the scon pointer vector for a match.
//-------------------------------------------------------------------
bool TargetSentenceUnit::sconPointerFound(int sconPointer)
{
	LgsVector(int)::iterator sconPtrVecItr;
	bool bfound = false;
	for (sconPtrVecItr = sconPointerVector_.begin(); ((!bfound) && (sconPtrVecItr != sconPointerVector_.end()) ); sconPtrVecItr++)
	{
		if (*sconPtrVecItr == sconPointer)
      {
         bfound = true;
      }
	}
	return bfound;

}
//-------------------------------------------------------------------
void TargetSentenceUnit::setCompanyCode(LgsString& newCC)
{
   if ((newCC == "   ") || (newCC.empty()))
   {
      v_companyCode = "LOG";
   }
   else
   {
      v_companyCode = newCC;
   }
}
//-------------------------------------------------------------------
void TargetSentenceUnit::setCompanyCode(char* newCC)
{
   LgsString temp = newCC;
   setCompanyCode(temp);
}
//-------------------------------------------------------------------
void TargetSentenceUnit::resolveSpaces()
{
   LWordVector::iterator wordListBegin = surfaceWords().begin();
   LWordVector::iterator wordListEnd = surfaceWords().end();
   for (LWordVector::iterator word = wordListBegin; word != wordListEnd; word++)
   {
      if (word == wordListBegin)
      {
         (*word).setPrecedingSpaces(precedingSpaces());
      }
      if (word == (wordListEnd - 1))
      {
         (*word).setTrailingSpaces(trailingSpaces());
      }
      else
      {
         (*word).setTrailingSpaces((*(word + 1)).precedingSpaces());
      }
   }
}
//-------------------------------------------------------------------
bool TargetSentenceUnit::isFlagUnfoundWord()
{
   // Skip dictionary, punctuation and protected units
   if( !isTargetDictionaryUnit() &&
       (wordClassCode() != LLanguage::PUNCTUATION) &&
       (surfaceExpressionAsString() != "^p") )
   {
      // Check is it unfound word
      if( sourceSentenceUnit() && sourceSentenceUnit()->isUnfoundWord() )
      {
         // Check is it tokenized by start rules
         LWordVector &words = sourceSentenceUnit()->surfaceWords();
         if( (words.begin() != words.end())
             && (words.begin()->getTokenType() == LookupTokenType::tok_none) )
         {
            // Check is it word with all alpha symbols or alpha with "'","'s" at the end
            LgsString str = *(words.begin());
            int size = str.size();
            if( (size > 1) && (str[size-1] == '\'') )
            {
               size--;
            }
            else if( (size > 2) && (str.substr(size-2, 2) == "'s") )
            {
               size-= 2;
            }
            if( size && StringUtil::isAllAlpha(str.substr(0, size)) )
            {
               return true; // unit must be flagged as unfound
            }
         }
      }
   }

   return false;
}

//-------------------------------------------------------------------
const LLanguage& TargetSentenceUnit::language() const
{
   return dictionary().targetLanguage();
}
