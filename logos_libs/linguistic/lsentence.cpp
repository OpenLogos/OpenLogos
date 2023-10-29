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
// -------------------------------------------------------------------
// File - LSentence.cpp
//
// Class - LSentence (implementation)
//
// -------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/lsentence.h>
#include <logos_libs/linguistic/ldictionary.h>
#include <logos_libs/linguistic/punctuationconstantsentenceunit.h>
#include <logos_libs/linguistic/functionconstantsentenceunit.h>
#include <logos_libs/linguistic/constantsentenceunit.h>
#include <logos_libs/linguistic/unfoundsentenceunit.h>
#include <logos_libs/linguistic/swork.h>
#include <logos_libs/linguistic/lword.h>
#include <logos_libs/linguistic/sentenceexception.h>
#include <logos_libs/utility/argumentexception.h>
#include <logos_libs/linguistic/blackhole.h>
#include <logos_libs/utility/phrasemanagerstrategy.h>
#include <logos_libs/linguistic/linkexception.h>
#include <logos_libs/entity/dwordphrase.h>
#include <logos_libs/translutility/translcommonobjects.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <logos_libs/linguistic/untranslatedsentenceunit.h>
#include <logos_libs/PatternRecognition/ProperNameRecognizer.h>
#include <logos_libs/utility/stringutil.h>
#include <transl/lgstraninterface.h>

int LSentence::st_baseUnfoundWordAddress = -11;

#ifdef TRANSLAPP
	#include <transl/interface.h>
	#include <transl/translthrman.h>
	#include <logos_libs/multithreadlib/comminterface.h>
	LgsMessage* tranMsg = 0;
        static LgsMessage* lookupMsgRes = 0;

// -------------------------------------------------------------------
void LSentence::createMessaging()
{
   tranMsg = new LgsMessage;
   lookupMsgRes = new LgsMessage;
}
// -------------------------------------------------------------------
void LSentence::cleanupMessaging()
{
   delete lookupMsgRes;
   lookupMsgRes = 0;
   delete tranMsg;
   tranMsg = 0;
}
#endif
// -------------------------------------------------------------------
LSentence::LSentence()
          :translationState_(DoTranslate),
           partOf_(0),
           numberOfParts_(0),
           caseState_(SentenceUnit::UndeterminedCase),
           bold_(false),
           italic_(false),
           underlined_(false),
           singleQuoted_(false),
           doubleQuoted_(false),
           bDifferentSentence_(false)
{
   v_sourceWords.reserve(maximumSourceAddress);
   v_sourceSentenceUnits.reserve(maximumSourceAddress);
   v_targetSentenceUnits.reserve(maximumSourceAddress);
}


// -------------------------------------------------------------------
LSentence::LSentence(LDictionary& dictionary)
          :TranslationObject(dictionary),
           translationState_ (DoTranslate),
           partOf_(0),
           numberOfParts_(0),
           caseState_(SentenceUnit::UndeterminedCase),
           bold_(false),
           italic_(false),
           underlined_(false),
           singleQuoted_(false),
           doubleQuoted_(false),
           bDifferentSentence_(false)
{
   v_sourceWords.reserve(maximumSourceAddress);
   v_sourceSentenceUnits.reserve (maximumSourceAddress);
   v_targetSentenceUnits.reserve (maximumSourceAddress);
}


// -------------------------------------------------------------------
LSentence::LSentence(const LSentence& rhs)
          :TranslationObject(rhs),
           translationState_(rhs.translationState_),
           partOf_(rhs.partOf_),
           numberOfParts_(rhs.numberOfParts_),
           v_markup(rhs.v_markup),
           caseState_(rhs.caseState_),
           bold_(rhs.bold_),
           italic_(rhs.italic_),
           underlined_(rhs.underlined_),
           singleQuoted_(rhs.singleQuoted_),
           doubleQuoted_(rhs.doubleQuoted_),
           bDifferentSentence_(rhs.bDifferentSentence_)
{
   // Copy constructor -- important for passing by value.
   // This is here to satisfy STL. If sentences are to be passed by
   // value work needs to be done here.
}


// -------------------------------------------------------------------
LSentence::~LSentence()
{
}


// -------------------------------------------------------------------
void LSentence::adjustFinalSpaces()
{
   // This method determines how many spaces exist after each
   // unit. Each target unit assumes the number of trailing spaces
   // that exist as the number of preceding spaces for the following
   // unit (the following units gets its preceding spaces from its
   // corresponding source unit.
   // Exceptions to this rule are where the current unit has been
   // told that it cannot have trailing spaces or where the
   // following unit has been told that it cannot have preceding
   // spaces.

   if (1 >= v_targetSentenceUnits.size())
   {
      return;
   }
   for (TargetUnitIterator i = v_targetSentenceUnits.begin(); i != v_targetSentenceUnits.end(); ++i)
   {
      if ((*i)->surfaceExpressionAsString().empty())
      {
         continue;
      }

      if (!(*i)->allowsTrailingSpaces() || (*i)->hadSpaceClosed())
      {
         (*i)->setTrailingSpaces(0);
      }
      else if (i != (v_targetSentenceUnits.end() - 1))
      {
         TargetUnitIterator next = i + 1;
         while ((next != (v_targetSentenceUnits.end() - 1)) && (*next)->surfaceExpressionAsString().empty())
         {
            next++;
         }
         if (!(*next)->allowsPrecedingSpaces())
         {
            if (!(*next)->isMerged())
            {
               (*i)->setTrailingSpaces(0);
            }
         }
         else if ((*next)->precedingSpaces() == 0)
         {
            if ((((*i)->opadr() > 0) && (((*i)->opadr() + 1) == (*next)->opadr())) ||
                ((*i)->sourceSentenceUnit() && (*next)->sourceSentenceUnit() &&
                 ((*i)->sourceSentenceUnit()->originalSentencePosition() != 0) &&
                 (((*i)->sourceSentenceUnit()->originalSentencePosition() + 1) ==
                   (*next)->sourceSentenceUnit()->originalSentencePosition())))
            {
               (*i)->setTrailingSpaces(0);
            }
         }
         else if ((*i)->sourceSentenceUnit() && !((*next)->sourceSentenceUnit()) &&
                  ((*i)->sourceSentenceUnit()->trailingSpaces() == 0) && !((*i)->surfaceExpressionAsString().empty()) &&
                   ((*i)->surfaceExpressionAsString().at((*i)->surfaceExpressionAsString().length() - 1) == '-'))
         {
            (*i)->setTrailingSpaces(0);
         }
      }
      if ((*i)->trailingSpaces() > 0)
      {
         (*i)->setTrailingSpaces(1);
      }
   }

   // If the sentence has be broken up because of its length into 2 or more sentences and this is not
   // the last sentence part, then make sure a space will separate the sentence parts. For one whole
   // sentence, numberOfParts == partOf == 0.
   if (numberOfParts_ != partOf_)
   {
      if ((*(v_targetSentenceUnits.end() - 1))->surfaceExpressionAsString() == "")
         (*(v_targetSentenceUnits.end() - 2))->setTrailingSpaces(1);
      else
         (*(v_targetSentenceUnits.end() - 1))->setTrailingSpaces(1);
   }
   else
      v_targetSentenceUnits.back()->adjustAsEos();

   // Remove any spaces preceding the first word of the sentence. Be careful of multiple sentence parts.
   if (((numberOfParts_ == 0) && (partOf_ == 0)) || ((numberOfParts_ > 0) && (partOf_ == 1)))
   {
      if (SentenceUnit::BosIdentifier == (*(v_targetSentenceUnits.begin()))->opadr())
         (*(v_targetSentenceUnits.begin()))->setTrailingSpaces(0);
   }
}


// -------------------------------------------------------------------
SentenceUnit::CaseState LSentence::caseState()
{
   // If the case state for this sentence has already been
   // determined, this member is just accessor. However, if it
   // has not been determined, determine the case state through
   // an analysis of the sentence's units (via the "isSource"
   // methods.

   if (SentenceUnit::UndeterminedCase == caseState_)
   {
      if (isSourceAllUpperCase())
      {
         caseState_ = SentenceUnit::AllUpperCase;
      }
      else if (isSourceBeginsUpperCase())
      {
         caseState_ = SentenceUnit::BeginsUpperCase;
      }
      else
      {
         caseState_ = SentenceUnit::LowerCase;
      }
   }
   return caseState_;
}


// -------------------------------------------------------------------
void LSentence::concatenate(LSentence* sentence)
{
   // This member concatenates another sentence onto the current
   // object. Note that it removes the adjacent EOS/BOS pair.
   // This member is primarily used to rebuild a sentence that
   // has been partitioned because it was too large for the trans.

   for (LWordVector::const_iterator w = sentence->v_sourceWords.begin();
        w != sentence->v_sourceWords.end(); ++w)
   {
      v_sourceWords.push_back(*w);
   }

   SourceSentenceUnit* lastUnit = v_sourceSentenceUnits.back();

   if (lastUnit->surfaceExpressionAsString() == "")
   {
      v_sourceSentenceUnits.pop_back();
	  lastUnit->deleteEntry();
      delete lastUnit;
   }

   SourceSentenceUnit* bos = sentence->v_sourceSentenceUnits.front();
   if (SentenceUnit::BosIdentifier == bos->sentenceAddress())
   {
		bos->deleteEntry();
		sentence->v_sourceSentenceUnits.erase(sentence->v_sourceSentenceUnits.begin());
		delete bos;
   }

   for (SourceUnitVector::const_iterator s = sentence->v_sourceSentenceUnits.begin();
        s != sentence->v_sourceSentenceUnits.end(); ++s)
   {
      v_sourceSentenceUnits.push_back(*s);
   }
   TargetSentenceUnit* pEos = v_targetSentenceUnits.back();
   if (pEos->surfaceExpressionAsString() == "")
   {
      v_targetSentenceUnits.pop_back();
      delete pEos;
   }
   if (SentenceUnit::BosIdentifier == sentence->v_targetSentenceUnits.front()->opadr())
   {
      delete sentence->v_targetSentenceUnits.front();
      sentence->v_targetSentenceUnits.erase(sentence->v_targetSentenceUnits.begin());
   }

   for (TargetUnitVector::const_iterator t = sentence->v_targetSentenceUnits.begin();
        t != sentence->v_targetSentenceUnits.end(); ++t)
   {
      v_targetSentenceUnits.push_back(*t);
   }
}


// -------------------------------------------------------------------
bool LSentence::isSourceAllUpperCase() const
{
   // Returns true if the whole sentence begins in upper case terms.
   bool result = true;

   for (SourceUnitVector::const_iterator i = v_sourceSentenceUnits.begin();
        i != v_sourceSentenceUnits.end(); ++i)
   {
      if (1 == (*i)->surfaceExpression().length())
      {
         if ((SentenceUnit::BeginsUpperCase != (*i)->caseState()) &&
             (SentenceUnit::AllUpperCase != (*i)->caseState()) &&
             (SentenceUnit::IrrelevantCase != (*i)->caseState()) &&
             (!(*i)->isProtected()))
         {
            result = false;
            break;
         }
      }
      else if ((SentenceUnit::AllUpperCase != (*i)->caseState()) &&
               (SentenceUnit::IrrelevantCase != (*i)->caseState()) &&
               (!(*i)->isProtected()))
      {
         result = false;
         break;
      }
   }
   return result;
}


// -------------------------------------------------------------------
bool LSentence::isSourceBeginsUpperCase() const
{
   // Returns true if the whole sentence begins in upper case terms.
   // This is true if all the units are either "begins upper" or
   // case is considered irrelevant (i.e., punctuation).

   bool result = true;

   for (SourceUnitVector::const_iterator i = v_sourceSentenceUnits.begin();
        i != v_sourceSentenceUnits.end(); ++i)
   {
      if ((SentenceUnit::BeginsUpperCase != (*i)->caseState()) &&
          (SentenceUnit::IrrelevantCase != (*i)->caseState()) &&
          (!(*i)->isProtected()))
      {
         result = false;
         break;
      }
   }
   return result;
}


// -------------------------------------------------------------------
bool LSentence::isSourceOverSized() const
{
   // Do the number of units in this sentence exceed the maximum
   // number of units that can be handled by the fortran (70)

   return maximumSourceAddress < v_sourceSentenceUnits.size();
}


// -------------------------------------------------------------------
bool LSentence::isLastOfOversizedTarget()
{
   // e.g. -- if this is the fourth part of a sentence that has 4
   // parts -- return true.

   return partOf_ == numberOfParts_;
}


// -------------------------------------------------------------------
void LSentence::splitOverSized(LSentencePVector* v)
{
   if (isSourceOverSized())
   {
      LSentence* oldSentence = this;
      LSentence* newSentence = 0;

      while (0 != (newSentence = oldSentence->reduceToValidSize()))
      {
         v->push_back(newSentence);
         oldSentence = newSentence;
      }
   }
}

// -------------------------------------------------------------------
// Break at positions where the filter ASSUMED a Proper Name 
// BUT it is NOT a Proper Name.
// Since the filter does not do any Dictionary Lookup
// , only Lookup can analyse the sentence and split it
// if it is really NOT  a proper name
void LSentence::splitSentence(LSentencePVector* v)
{
   LSentence* oldSentence = this;
   LSentence* newSentence = 0;
   newSentence = oldSentence->breakSentence(true);
   if (!newSentence)
   {
      return;
   }
   do
   {
      v->push_back(newSentence);
      oldSentence = newSentence;
   }
   while (0 != (newSentence = oldSentence->breakSentence(false)));
}


// -------------------------------------------------------------------
void LSentence::reconcilePrecedingSpaces()
{
   // Delegate real work to each corresponding unit.
   for (SourceUnitIterator i =  v_sourceSentenceUnits.begin();
        i != v_sourceSentenceUnits.end(); i++)
   {
      (*i)->reconcilePrecedingSpaces();
   }
}


// -------------------------------------------------------------------
LSentence* LSentence::reduceToValidSize()
{
   if (!isSourceOverSized())
   {
      renumberFoundUnits  ();
      renumberUnFoundUnits();
      SourceSentenceUnit* pLastEosUnit = v_sourceSentenceUnits.back();
      pLastEosUnit->updateSetID(52);
      return 0;
   }

   LSentence* newSentence = new LSentence(dictionary());

   bool isBreakPositionInNew = false;
   SourceUnitIterator breakPosition =
   findSentenceBreak(v_sourceSentenceUnits.begin(),
   v_sourceSentenceUnits.end(),
   &isBreakPositionInNew);

   SourceUnitIterator firstUnitOfNew = breakPosition;
   if (!isBreakPositionInNew)
   {
      --firstUnitOfNew;
   }
   SourceSentenceUnit* pUnit = dictionary().createTempBosUnit((*(firstUnitOfNew))->surfaceExpressionAsString());
   pUnit->setSentence (newSentence);
   newSentence->v_sourceSentenceUnits.push_back(pUnit);

   for (SourceUnitIterator i = breakPosition; i != v_sourceSentenceUnits.end(); i++)
   {
      newSentence->v_sourceSentenceUnits.push_back(*i);
   }
   LgsString surfaceString((*(firstUnitOfNew))->surfaceExpressionAsString());
   pUnit = dictionary().createTempEosUnit(surfaceString);

   if (surfaceString == ";" || surfaceString == ":" || surfaceString == ",")
   {
		breakPosition--;
		LWordVector & wrdVector = (*(firstUnitOfNew))->surfaceWords();
		pUnit->addSurfaceWords(wrdVector.begin(), wrdVector.end());
   }

   v_sourceSentenceUnits.erase(breakPosition, v_sourceSentenceUnits.end());
   pUnit->setSentence (newSentence);
   pUnit->setSentenceAddress (maximumSourceAddress);
   v_sourceSentenceUnits.push_back(pUnit);
   renumberFoundUnits();
   renumberUnFoundUnits();

   return newSentence;
}

// -------------------------------------------------------------------
LSentence* LSentence::breakSentence(bool bFirstTime)
{
   // find the break position

   SourceUnitIterator breakPosition;
   for (breakPosition = v_sourceSentenceUnits.begin(); 
        breakPosition != v_sourceSentenceUnits.end() && !sentenceCanBeSplit(breakPosition); 
        breakPosition++);

   if (v_sourceSentenceUnits.end() == breakPosition || 
       v_sourceSentenceUnits.end() == breakPosition+1)
   {
      if (!bFirstTime)
      {
         renumberFoundUnits  ();
         renumberUnFoundUnits();
         bDifferentSentence_ = true;
      }
      return 0;
   }
   bDifferentSentence_ = true;

   SourceUnitIterator firstUnitOfNew = breakPosition+1;
   LSentence* newSentence = new LSentence(dictionary());

   SourceSentenceUnit* pUnit = dictionary().createBosUnit();
   pUnit->setSentence (newSentence);
   newSentence->v_sourceSentenceUnits.push_back(pUnit);

   for (SourceUnitIterator i = firstUnitOfNew; i != v_sourceSentenceUnits.end(); i++)
   {
      newSentence->v_sourceSentenceUnits.push_back(*i);
   }

   if ((*breakPosition)->surfaceExpressionAsString().length() > 1)
   {
      pUnit = dictionary().createEmptyEosUnit();
      pUnit->position((*breakPosition)->sentenceAddress());
      pUnit->setSentenceAddress((*breakPosition)->sentenceAddress());
   }
   else
   {
      LgsString surfaceString((*breakPosition)->surfaceExpressionAsString());
      pUnit = dictionary().createTempEosUnit(surfaceString);
   }

   v_sourceSentenceUnits.erase(firstUnitOfNew, v_sourceSentenceUnits.end());
   pUnit->setSentence (newSentence);
   v_sourceSentenceUnits.push_back(pUnit);
   renumberFoundUnits();
   renumberUnFoundUnits();

   return newSentence;
}

// -------------------------------------------------------------------
bool LSentence::sentenceCanBeSplit(SourceUnitIterator currentUnit) const
{
   LgsString currentUnitString((*currentUnit)->surfaceExpressionAsString());
   const int lenCurString = currentUnitString.length();
   if( (lenCurString < 1) || ('.' != currentUnitString[lenCurString - 1]) )
   {
      return false;
   }
   SourceUnitIterator nextUnit = currentUnit + 1;
   if( v_sourceSentenceUnits.end() == nextUnit )
   {
      return false;
   }
   LgsString nextUnitString((*nextUnit)->surfaceExpressionAsString());
   if( "(" == nextUnitString )
   {
      nextUnit = nextUnit + 1;
      if( v_sourceSentenceUnits.end() == nextUnit )
      {
         return false;
      }
      nextUnitString = (*nextUnit)->surfaceExpressionAsString();
   }

   return (CharUtil::isUpper(nextUnitString[0]) &&
           (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::EnglishID && !(*currentUnit)->isProperName() && 
           lenCurString < 3 || 
           TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID && 1 != (*nextUnit)->wordClassCode() && 
           !(4 ==(*nextUnit)->wordClassCode() && 9 ==(*nextUnit)->formCode())));
}

// -------------------------------------------------------------------
SourceUnitIterator LSentence::findSentenceBreak(SourceUnitIterator begin, SourceUnitIterator end,
                                                bool* isBreakPositionInNew)
{
   // If the sentence is within the allowable range, this
   // method returns its true end.
   // If the sentence is too big, this method finds the last
   // semi-colon within an allowable range and returns the position
   // following the semi-colon.
   // Having failed to find a semi-colon, it attempts to do the same
   // with a colon and then with a comma.
   // If all else fails this function breaks off the sentence after
   // the 70th element.

   *isBreakPositionInNew = false;

   if (maximumSourceAddress >= (end - begin))
   {
      return end;
   }
   SourceUnitIterator worstEnd = begin + maximumSourceAddress - 1;

   LgsVector(LgsString) s;
   LgsString matchedStr;
   s.push_back(")");
   s.push_back("]");
   s.push_back("}");
   s.push_back("-");

   s.push_back(";");
   SourceUnitIterator parentheticalEnd = worstEnd;
   SourceUnitIterator newEnd =
   findLastSourceSurfaceExpression(begin, worstEnd, s);
   s.erase(s.end()-1);
   if (newEnd != worstEnd)
   {
	   if ((*newEnd)->surfaceExpressionAsString() != ";")
	   {
			parentheticalEnd = newEnd;
	   }
	   else
	   {
			return newEnd + 1;
	   }
   }
   newEnd = findLastSourceSurfaceExpression(begin, worstEnd, ":");
   if (newEnd != worstEnd)
   {
	   if (parentheticalEnd != worstEnd &&
		   parentheticalEnd > newEnd)
	   {
			newEnd = parentheticalEnd;
	   }
	   return newEnd + 1;
   }
   newEnd = findLastSourceSurfaceExpression(begin, worstEnd, ",");
   if (newEnd != worstEnd)
   {
	   if (parentheticalEnd != worstEnd &&
		   parentheticalEnd > newEnd)
	   {
			newEnd = parentheticalEnd;
	   }
	   return newEnd + 1;
   }
   newEnd = findLastSourceSurfaceExpression(begin, worstEnd, "i.e.");
   if (newEnd != worstEnd)
   {
		if (parentheticalEnd != worstEnd &&
			parentheticalEnd > newEnd)
		{
			newEnd = parentheticalEnd+1;
		}
		else
		{
			*isBreakPositionInNew = true;
		}
		return newEnd;
   }
   newEnd = findLastSourceSurfaceExpression(begin, worstEnd, "e.g.");
   if (newEnd != worstEnd)
   {
		if (parentheticalEnd != worstEnd &&
			parentheticalEnd > newEnd)
		{
			newEnd = parentheticalEnd+1;
		}
		else
		{
			*isBreakPositionInNew = true;
		}
		return newEnd;
   }
   newEnd = findLastSourceSurfaceExpression(begin, worstEnd, "cf.");
   if (newEnd != worstEnd)
   {
		if (parentheticalEnd != worstEnd &&
			parentheticalEnd > newEnd)
		{
			newEnd = parentheticalEnd+1;
		}
		else
		{
			*isBreakPositionInNew = true;
		}
		return newEnd;
   }

   newEnd = findLastSourceSurfaceExpression(begin, worstEnd, "(");
   if (newEnd != worstEnd)
   {
		if (parentheticalEnd != worstEnd &&
			parentheticalEnd > newEnd)
		{
			newEnd = parentheticalEnd+1;
		}
		else
		{
			*isBreakPositionInNew = true;
		}
		return newEnd;
   }
   return worstEnd;
}


// -------------------------------------------------------------------
SourceUnitIterator LSentence::findLastSourceSurfaceExpression(SourceUnitIterator begin,
                                                              SourceUnitIterator end,
                                                              const LgsString& s)
{
   // This method simply returns the last sentence unit within
   // a range of units that has a matching surface expression.
   // Note that range is not inclusive of either the beginning
   // or end.

   for (SourceUnitIterator i = end - 1; i >= begin; i--)
   {
      if ((*i)->surfaceExpressionAsString() == s)
      {
         return i;
      }
   }
   return end;
}


// -------------------------------------------------------------------
SourceUnitIterator LSentence::findLastSourceSurfaceExpression(SourceUnitIterator begin, SourceUnitIterator end,
                                                              LgsVector(LgsString) & s)
{
   // This method simply returns the last sentence unit within
   // a range of units that has a matching surface expression.
   // Note that range is not inclusive of either the beginning
   // or end.

	SourceUnitIterator retVal=end;
	LgsVector(LgsString)::iterator endIter = s.end();
	for (LgsVector(LgsString)::iterator currIter = s.begin(); currIter != endIter; 
		 currIter++)
	{
		for (SourceUnitIterator i = end - 1; i >= begin; i--)
		{
			if ( ((*i)->surfaceExpressionAsString() == (*currIter)) &&
				 ((retVal == end) || (retVal < i)) )
			{
				retVal = i;	
			}
		}
	}

	return retVal;
}


// -------------------------------------------------------------------
void LSentence::preLookupSetTranslationState()
{
   try
   {
      translationState_ = DoTranslate;

      // check for untranslatable sentence eg a formula
      for (LWordIterator i =  sourceWords().begin(); i != sourceWords().end(); ++i)
      {
         const LgsString& word = *i;

         int alphaChars = 0;
         bool hasDigit = false;
		 bool tokenAssigned = false;
         for (LgsString::const_iterator j = word.begin(); j != word.end(); j++)
         {
            if (CharUtil::isNumeric(*j))
            {
               hasDigit = true;
               break;
            }
            if (CharUtil::isAlphaNumeric(*j))
               alphaChars++;
			if ((*i).getTokenType() != LookupTokenType::tok_none)
			{
				tokenAssigned = true;
			}
         }

         if ((!hasDigit && alphaChars > 1) || tokenAssigned) // found a non-numeric word with more than one alpha character, so not a formula
            return;
      }

      // reached here, so sentence is untranslatable
      translationState_ = DoNotTranslate;
   }
   catch(...)
   {
      TranslCommonObjects::GetDiagnostic()->writeLine("\n*SENTENCE EXCEPTION THROWN.*");
      TranslCommonObjects::GetDiagnostic()->writeLine (LgsString("In prelookup for Sentence #") +
                                                       const_cast<SentenceMarkup&>(markup()).idAsString());
      TranslCommonObjects::GetDiagnostic()->writeLine("*Sentence Not Translated.*\n");
      translationState_ = DoNotTranslate;
   }

}


// -------------------------------------------------------------------
void LSentence::postLookupSetTranslationState()
{
   if (DoTranslate == translationState_)
   {
      for (SourceUnitIterator i =  v_sourceSentenceUnits.begin();
           i != v_sourceSentenceUnits.end(); ++i)
      {
         if ((*i)->isTranslateable())
         {
            return;
         }
      }
      // reached here, so sentence is untranslatable
      translationState_ = DoNotTranslate;
   }
}


// -------------------------------------------------------------------
// Dictionary lookup phase. The task is to construct a vector of
// source sentence units based on longest matches of source surface 
// words with dictionary entries.
// Requests that MakeWords() isolate all the words in the sentence,
// then call MakeSentenceUnits() convert the words into LSentenceUnit
// objects.
// Performs the actual dictionary matching of the source words.
// This includes ordering of SSU's based upon criteria such as priority number, 
// SMC and company code.
// -------------------------------------------------------------------
void LSentence::lookup() throw (SentenceException)
{
  try
    {
      if (!(sourceWords().empty()))
        {
          makeSourceSentenceUnits();
          reconcilePrecedingSpaces();
          removeDividingUnits();
          assignOrigSentencePosition();
          if (isQuestion())
            {
              (*(sourceSentenceUnits().begin()))->makeBeginningOfQuestion();
            }
        }
    }
  catch(...)
    {
      throw SentenceException ("lookup");
    }
}


// -------------------------------------------------------------------
// This is the case when a sentenc eis not to be translated.
// This method creates a source sentence unit for each source surface
// word and marks each as protected from translation.
// -------------------------------------------------------------------
void LSentence::makeAllSourceUnitsProtected()
{
   LWordIterator beginWord = sourceWords().begin();

   int unfoundAddress = st_baseUnfoundWordAddress;

   SourceSentenceUnit* pBosUnit = dictionary().createBosUnit();
   pBosUnit->setSentence(this);
   v_sourceSentenceUnits.push_back(pBosUnit);

   int foundAddress = 2;
   for (LWordIterator i =  sourceWords().begin(); i != sourceWords().end(); ++i)
   {
      SourceSentenceUnit* pUnit = dictionary().createProtectedUnit(i);
      pUnit->setSentence (this);

      pUnit->addSurfaceWords(i, i + 1);

      pUnit->position(foundAddress);

      pUnit->setSentenceAddress(foundAddress);

      v_sourceSentenceUnits.push_back(pUnit);
      ++foundAddress;
   }
   SourceSentenceUnit* pEosUnit = dictionary().createEmptyEosUnit();
   pEosUnit->setSentence(this);
   pEosUnit->setSentenceAddress(foundAddress);
   v_sourceSentenceUnits.push_back(pEosUnit);
}


// -------------------------------------------------------------------
void LSentence::makeAllTargetUnitsProtected()
{
   dictionary().createProtectedTargetUnits (v_sourceSentenceUnits, v_targetSentenceUnits);
   for (TargetUnitVector::iterator i =  v_targetSentenceUnits.begin();
        i != v_targetSentenceUnits.end(); ++i)
   {
      (*i)->setSentence (this);
   }
}



// -------------------------------------------------------------------
void LSentence::recognizeHyphenatedWords()
{
   for (LWordIterator i =  v_sourceWords.begin() + 1; i != v_sourceWords.end(); ++i)
   {
      if ((i->precedingSpaces() == 0) && (*i == "-") && ((*(i - 1)) != "-"))
      {
         if (i == (v_sourceWords.end() - 1))
         {
            (i - 1)->setHyphenated();
         }
         else if ((*(i + 1)) != "-")
         {
            (i - 1)->setHyphenated();
         }
      }
   }
}


// -------------------------------------------------------------------
void LSentence::removeDividingUnits()
{
   bool isAdjusted = false;

   if (1 < v_sourceSentenceUnits.size())
   {
      for (SourceUnitIterator i = v_sourceSentenceUnits.end() - 2;
           i >= v_sourceSentenceUnits.begin(); --i)
      {
         if ((*i)->isHyphenated())
         {
            if (StringUtil::containsAlphanumeric((*i)->surfaceExpressionAsString()))
            {
               if ((*(i + 1))->surfaceExpressionAsString() == "-")
               {
                  if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
                  {
                     (*i)->setUnitFormCode(9);
                  }
                  else
                  {
                     (*i)->setUnitFormCode(6);
                  }
                  if (i != (v_sourceSentenceUnits.end() - 2))
                  {
                     if (!((*(i + 2))->isEndOfSentence()))
                     {
                        if ((*(i + 2))->surfaceExpressionAsString().find_first_of(".,;:?!%]})>/'\"", 0) == NPOS)
                        {
                           (*i)->setTrailingSpaces(1);
                           (*(i + 2))->setPrecedingSpaces(1);
                        }
                        else
                        {
                           (*i)->setTrailingSpaces(0);
                           (*(i + 2))->setPrecedingSpaces(0);
                        }
                     }
                     else
                     {
                        (*i)->setTrailingSpaces(0);
                     }
                  }
                  (*(i + 1))->deleteEntry();
                  v_sourceSentenceUnits.removeAndDelete(i + 1, i + 2);
               }
               else if ((*(i + 1))->surfaceExpressionAsString()[0] == '-')
               {
                  // This handles the case in German where the word was formed from a multi-hyphenated word
                  // and the inner hyphens become part of the word.
                  if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
                  {
                     (*i)->setUnitFormCode(9);
                  }
                  else
                  {
                     (*i)->setUnitFormCode(6);
                  }
                  if ((*(i + 1))->surfaceExpressionAsString().find_first_of(".,;:?!%]})>/'\"", 1) == NPOS)
                  {
                     (*i)->setTrailingSpaces(1);
                     (*(i + 1))->setPrecedingSpaces(1);
                  }
               }
               isAdjusted = true;
            }
            else if ((*i)->surfaceExpressionAsString() == "-")
            {
               (*i)->setHyphenatedHyphen(true);
               (*(i + 1))->deleteEntry();
               v_sourceSentenceUnits.removeAndDelete(i + 1, i + 2);
               isAdjusted = true;
            }
         }
      }
   }
   if (isAdjusted)
   {
      renumberFoundUnits();
   }
}
//-------------------------------------------------------------------
void LSentence::renumberFoundUnits()
{
   int newAddress = 1;

   for (SourceUnitIterator i = v_sourceSentenceUnits.begin();
        i != v_sourceSentenceUnits.end(); ++i)
   {
      (*i)->position(newAddress);
      if (minimumSourceAddress <= (*i)->sentenceAddress())
      {
         (*i)->setSentenceAddress(newAddress);
      }
      ++newAddress;
   }
}
//-------------------------------------------------------------------
void LSentence::renumberUnFoundUnits()
{
   int newAddress = -11;

   for (SourceUnitIterator i = v_sourceSentenceUnits.begin();
        i != v_sourceSentenceUnits.end(); ++i)
   {
      if (SentenceUnit::BosIdentifier > (*i)->sentenceAddress())
      {
         (*i)->setSentenceAddress(newAddress);
         --newAddress;
      }
   }
}


// -------------------------------------------------------------------
// The number of words or phrases, either matched or unmatched within this sentence.
// -------------------------------------------------------------------
int LSentence::translatedWordCount() const
{
   // Following the Composite design pattern, the word count for
   // the sentence is derived from the word count of all of the
   // sentence units.

   int count = 0;

   for (SourceUnitVector::const_iterator i = v_sourceSentenceUnits.begin();
        i != v_sourceSentenceUnits.end(); ++i)
   {
      count += (*i)->translatedWordCount();
   }
   return count;
}


// -------------------------------------------------------------------
bool LSentence::isTranslated() const
{
   return (translationState_ == DoTranslate);
}


// -------------------------------------------------------------------
void LSentence::setSurfaceExpressionFromDictionary()
{
   // Delegate real work to each corresponding unit.
   for (TargetUnitIterator i = v_targetSentenceUnits.begin();
        i != v_targetSentenceUnits.end(); ++i)
   {
      (*i)->setSurfaceExpressionFromDictionary();
   }
   checkForPhraseModification();
}


// -------------------------------------------------------------------
void LSentence::checkForPhraseModification()
{
   // This only occurs with German source
   if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
   {
      for (TargetUnitIterator i = v_targetSentenceUnits.begin(); i != v_targetSentenceUnits.end(); ++i)
      {
         short phraseModifier = (*i)->phraseModifier();

         // If one of the modification flags have been set
         if ((phraseModifier == LWord::REMOVE_NON_HEAD) || (phraseModifier == LWord::REMOVE_HEAD))
         {
            // Find matching target unit for this action (has the same flag set)
            for (TargetUnitIterator j = i + 1; j != v_targetSentenceUnits.end(); ++j)
            {
               // If a following unit has the same flag set
               if (phraseModifier == (*j)->phraseModifier())
               {
                  int cntr;

                  // First get the left most phrase to be removed base on whether it's to be the
                  // head word or the non-head words.
                  LgsString leftPhrase;
                  LWordVector& leftWords = (*i)->surfaceWords();
                  LWordIterator leftWord = leftWords.begin();
                  int leftTotalWords = leftWords.size();

                  if (leftTotalWords == 1)
                  {
                     leftPhrase += *leftWord;
                  }
                  else
                  {
                     for (cntr = 1; cntr <= leftTotalWords; cntr++, leftWord++)
                     {
                        if (((phraseModifier == LWord::REMOVE_HEAD) && ((*i)->headWord() == cntr)) ||
                            ((phraseModifier == LWord::REMOVE_NON_HEAD) && ((*i)->headWord() != cntr)))
                        {
                           if ((leftPhrase.length() > 0) && ((*leftWord).precedingSpaces() != 0))
                           {
                              leftPhrase += " ";
                           }
                           leftPhrase += *leftWord;
                        }
                     }
                  }

                  // Next get the right most phrase to be removed base on whether it's to be the
                  // head word or the non-head words.
                  LgsString rightPhrase;
                  LWordVector& rightWords = (*j)->surfaceWords();
                  LWordIterator rightWord = rightWords.begin();
                  int rightTotalWords = rightWords.size();

                  if (rightTotalWords == 1)
                  {
                     rightPhrase += *rightWord;
                  }
                  else
                  {
                     for (cntr = 1; cntr <= rightTotalWords; cntr++, rightWord++)
                     {
                        if (((phraseModifier == LWord::REMOVE_HEAD) && ((*j)->headWord() == cntr)) ||
                            ((phraseModifier == LWord::REMOVE_NON_HEAD) && ((*j)->headWord() != cntr)))
                        {
                           if ((rightPhrase.length() > 0) && ((*rightWord).precedingSpaces() != 0))
                           {
                              rightPhrase += " ";
                           }
                           rightPhrase += *rightWord;
                        }
                     }
                  }

                  // Now compare the two phrases or words, if they match then the left most can
                  // have the word / phrase in question removed.
                  if (leftPhrase == rightPhrase)
                  {
                     LgsString newPhrase;

                     // If the head is to be removed, remove it from the left most unit otherwise,
                     // for non-head removal, remove it from the right most unit.
                     if (phraseModifier == LWord::REMOVE_HEAD)
                     {
                        leftWord = leftWords.begin();

                        if (leftTotalWords > 1)
                        {
                           for (int cntr = 1; cntr <= leftTotalWords; cntr++, leftWord++)
                           {
                              if ((*i)->headWord() != cntr)
                              {
                                 if ((newPhrase.length() > 0) && ((*leftWord).precedingSpaces() != 0))
                                 {
                                    newPhrase += " ";
                                 }
                                 newPhrase += *leftWord;
                              }
                           }
                        }
                        (*i)->surfaceExpressionFromString(newPhrase);
                     }
                     else
                     {
                       rightWord = rightWords.begin();

                        if (rightTotalWords > 1)
                        {
                           for (int cntr = 1; cntr <= rightTotalWords; cntr++, rightWord++)
                           {
                              if ((*j)->headWord() == cntr)
                              {
                                 if ((newPhrase.length() > 0) && ((*rightWord).precedingSpaces() != 0))
                                 {
                                    newPhrase += " ";
                                 }
                                 newPhrase += *rightWord;
                              }
                           }
                        }
                        (*j)->surfaceExpressionFromString(newPhrase);
                     }
                  }
                        // Get ready to skip to the next unit in the sentence and look for more combinations.
                  i = j;
                  break;
               }
            }
         }
      }
   }
}

// -------------------------------------------------------------------
void LSentence::updateSourcePrimarySsu()
{
   // Delegate real work to each corresponding unit.
   for (TargetUnitIterator i = v_targetSentenceUnits.begin();
        i != v_targetSentenceUnits.end(); ++i)
   {
      (*i)->updateSourcePrimarySsu();
   }
}


// -------------------------------------------------------------------
void LSentence::generateStems()
{
   // Delegate real work to each corresponding unit.
   for (TargetUnitIterator i = v_targetSentenceUnits.begin(); i != v_targetSentenceUnits.end(); ++i)
   {
      if ((*i)->isProperNameTitle() || !((*i)->isProperName()))
      {
         (*i)->generateStem();
      }
   }
}


// -------------------------------------------------------------------
void LSentence::makeAspirations()
{
   // Delegate real work to each corresponding unit.
   for (TargetUnitIterator i = v_targetSentenceUnits.begin();
        i != v_targetSentenceUnits.end(); ++i)
   {
      if (i == v_targetSentenceUnits.begin())
      {
         (*i)->makeAspiration((TargetSentenceUnit*) 0);
      }
      else
      {
         (*i)->makeAspiration(*(i - 1));
      }
   }
}


// -------------------------------------------------------------------
void LSentence::reconcileMarkup()
{
   TargetUnitIterator i;

   // Delegate real work to each corresponding unit.
   for (i = v_targetSentenceUnits.begin(); i != v_targetSentenceUnits.end(); i++)
   {
      (*i)->reconcileMarkup();
   }

   // Make sure all markup id's of -1 are set to the id of the sentence unit preceding it.
   for (i = v_targetSentenceUnits.begin(); i != v_targetSentenceUnits.end(); i++)
   {
      // Skip the BOS unit.
      if ((*i)->opadr() == SentenceUnit::BosIdentifier)
         continue;

      // If the markup id is set to -1 indicating an inserted sentence unit, then we have to
      // do some processing on it.
      if ((*i)->wordMarkup()->id() == -1)
      {
         // If the unit has a source sentence unit associated with it, use the markup associated
         // with that common sentence unit.
         if ((*i)->sourceUnitPosition() > 0)
         {
            for (SourceUnitVector::const_iterator j = v_sourceSentenceUnits.begin(); j != v_sourceSentenceUnits.end(); j++)
            {
               if ((*j)->position() == (*i)->sourceUnitPosition())
               {
                  const LWordVector& sourceWords = (*j)->surfaceWords();

                  if (sourceWords.size())
                  {
                     (*i)->wordMarkup()->setId(sourceWords.begin()->markupId());

                     for (LWordVector::const_iterator iter = sourceWords.begin(); iter != sourceWords.end(); iter++)
                     {
                        (*i)->wordMarkup()->orMask(iter->markup());
                     }
                  }
                  break;
               }
            }
         }
      }

      // If no matching source sentence unit was found then the next step is to see if the two words on
      // either side have common markup (ex both Bold). If so, apply it to this sentence unit.
      if ((*i)->wordMarkup()->id() == -1)
      {
         // If it's the first sentence unit after the BOS.
         if ((*(i - 1))->opadr() == SentenceUnit::BosIdentifier)
         {
            // If there's more than one word in the sentence, take the markup information of
            // the first sentence unit after this unit that has valid markup.
            if ((i + 1) != v_targetSentenceUnits.end())
            {
               // Get the markup id of the first sentence unit that has a valid one.
               for (TargetUnitIterator j = (i + 1); j != v_targetSentenceUnits.end(); j++)
               {
                  if ((*j)->wordMarkup()->id() != -1)
                  {
                     (*i)->wordMarkup()->orMask((*j)->markup());
                     (*i)->wordMarkup()->setId((*j)->wordMarkup()->id());
                     break;
                  }
               }
            }
            else
            {
               (*i)->wordMarkup()->setId(1);
            }
         }
         else
         {
            // No change in markup if it's the last unit in the sentence so only work on those units
            // before the end of the sentence.
            bool markupAssigned = false;
            TargetUnitIterator nextUnit = (i + 1);
            while ((nextUnit != v_targetSentenceUnits.end()) && ((*nextUnit)->wordMarkup()->id() == -1))
            {
               nextUnit++;
            }
            if (nextUnit != v_targetSentenceUnits.end())
            {
               // Check the markup of the sentence units on either side. If they have like markup, then
               // apply that to this sentence unit.
               // Check for bold.
               if ((*(i - 1))->wordMarkup()->isBold() && (*nextUnit)->wordMarkup()->isBold())
               {
                  (*i)->wordMarkup()->setBold();
                  markupAssigned = true;
               }

               // Check for italic.
               if ((*(i - 1))->wordMarkup()->isItalic() && (*nextUnit)->wordMarkup()->isItalic())
               {
                  (*i)->wordMarkup()->setItalic();
                  markupAssigned = true;
               }

               // Check for underlined.
               if ((*(i - 1))->wordMarkup()->isUnderlined() && (*nextUnit)->wordMarkup()->isUnderlined())
               {
                  (*i)->wordMarkup()->setUnderlined();
                  markupAssigned = true;
               }

               // Check for single quotes.
               if ((*(i - 1))->wordMarkup()->isSingleQuoted() && (*nextUnit)->wordMarkup()->isSingleQuoted())
               {
                  (*i)->wordMarkup()->setSingleQuoted();
                  markupAssigned = true;
               }
 
               // Check for double quotes.
               if ((*(i - 1))->wordMarkup()->isDoubleQuoted() && (*nextUnit)->wordMarkup()->isDoubleQuoted())
               {
                  (*i)->wordMarkup()->setDoubleQuoted();
                  markupAssigned = true;
               }
 
               // Check for French single quotes.
               if ((*(i - 1))->wordMarkup()->isFrenchSingleQuoted() && (*nextUnit)->wordMarkup()->isFrenchSingleQuoted())
               {
                  (*i)->wordMarkup()->setFrenchSingleQuoted();
                  markupAssigned = true;
               }
 
               // Check for French double quotes.
               if ((*(i - 1))->wordMarkup()->isFrenchDoubleQuoted() && (*nextUnit)->wordMarkup()->isFrenchDoubleQuoted())
               {
                  (*i)->wordMarkup()->setFrenchDoubleQuoted();
                  markupAssigned = true;
               }

               // Now assign a markup id based on whether markup was already assigned to this sentence unit and
               // whether the following sentence unit has a valid markup id.
               if (!markupAssigned && ((*nextUnit)->wordMarkup()->id() != -1))
               {
                  if (((*(i - 1))->wordMarkup()->hasMarkup() || ((*(i - 1))->surfaceExpressionAsString() == "^p")) &&
                      !((*nextUnit)->wordMarkup()->hasMarkup()))
                  {
                     // Set the markup id to the sentence unit that follows this one.
                     (*i)->wordMarkup()->setId((*nextUnit)->wordMarkup()->id());
                  }
                  else
                  {
                     // Set the markup id to the sentence unit that preceded this one.
                     (*i)->wordMarkup()->setId((*(i - 1))->wordMarkup()->id());
                  }
               }
               else
               {
                  // Set the markup id to the sentence unit that preceded this one.
                  (*i)->wordMarkup()->setId((*(i - 1))->wordMarkup()->id());
               }
            }
            else
            {
               // Set the markup id to the sentence unit that preceded this one.
               (*i)->wordMarkup()->setId((*(i - 1))->wordMarkup()->id());
            }
         }
      }
   }
   
   // If the target language is Spanish, check to make sure any inverted question marks (ø)
   // are quoted the same way as the word that follows them.
   if (TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::SpanishID)
   {
      for (i = v_targetSentenceUnits.begin(); i != v_targetSentenceUnits.end(); i++)
      {
         if ((*i)->isFunctionalConstant() &&
             ((*i)->opadr() == FunctionConstantSentenceUnit::InvertedQuestion) &&
             ((i + 1) != v_targetSentenceUnits.end()))
         {
            if ((*(i + 1))->markup().isDoubleQuoted())
            {
               (*i)->wordMarkup()->setDoubleQuoted(true);
            }
            if ((*(i + 1))->markup().isSingleQuoted())
            {
               (*i)->wordMarkup()->setSingleQuoted(true);
            }
         }
      }
   }

   // Check to see if last sentence unit is punctuation. If so, it must not have markup unless the
   // preceding unit has the same markup.
   if (((*(v_targetSentenceUnits.end() - 1))->wordClassCode() == LLanguage::PUNCTUATION) &&
       (v_targetSentenceUnits.size() > 1) && ((*(v_targetSentenceUnits.end() - 2))->opadr() != SentenceUnit::BosIdentifier))
   {
      TargetUnitIterator prevUnit = v_targetSentenceUnits.end() - 2;
      TargetUnitIterator lastUnit = v_targetSentenceUnits.end() - 1;
      if ((*lastUnit)->wordMarkup()->isBold() && !((*prevUnit)->wordMarkup()->isBold()))
         (*lastUnit)->wordMarkup()->setBold(false);

      // Check for italic.
      if ((*lastUnit)->wordMarkup()->isItalic() && !((*prevUnit)->wordMarkup()->isItalic()))
         (*lastUnit)->wordMarkup()->setItalic(false);

      // Check for underlined.
      if ((*lastUnit)->wordMarkup()->isUnderlined() && !((*prevUnit)->wordMarkup()->isUnderlined()))
         (*lastUnit)->wordMarkup()->setUnderlined(false);

      // Check for single quotes.
      if ((*lastUnit)->wordMarkup()->isSingleQuoted() && !((*prevUnit)->wordMarkup()->isSingleQuoted()))
         (*lastUnit)->wordMarkup()->setSingleQuoted(false);

      // Check for double quotes.
      if ((*lastUnit)->wordMarkup()->isDoubleQuoted() && !((*prevUnit)->wordMarkup()->isDoubleQuoted()))
         (*lastUnit)->wordMarkup()->setDoubleQuoted(false);

      // Check for French single quotes.
      if ((*lastUnit)->wordMarkup()->isFrenchSingleQuoted() && !((*prevUnit)->wordMarkup()->isFrenchSingleQuoted()))
         (*lastUnit)->wordMarkup()->setFrenchSingleQuoted(false);

      // Check for French double quotes.
      if ((*lastUnit)->wordMarkup()->isFrenchDoubleQuoted() && !((*prevUnit)->wordMarkup()->isFrenchDoubleQuoted()))
         (*lastUnit)->wordMarkup()->setFrenchDoubleQuoted(false);
   }

   // Assign the markup amongst the words that make up the surface words.
   assignMarkupToWords();

   // Assign markup for words that are joined so that each word's markup is
   // a combination of both.
   for (i = v_targetSentenceUnits.begin(); i != (v_targetSentenceUnits.end() - 1); i++)
   {
      // If not the BOS and neither of the words are punctuation.
      if (((*i)->opadr() != SentenceUnit::BosIdentifier) && ((*i)->wordClassCode() != LLanguage::PUNCTUATION) &&
          ((*(i + 1))->wordClassCode() != LLanguage::PUNCTUATION) && ((*i)->trailingSpaces() == 0) &&
          ((*i)->surfaceExpressionAsString() != "^p") && ((*(i + 1))->surfaceExpressionAsString() != "^p"))
      {
         // Combine the markup but don't mess with the markup id's. These should be proper at this point.
         if (!((*i)->surfaceWords().empty()) && !((*(i + 1))->surfaceWords().empty()))
         {
            ((*i)->surfaceWords().end() - 1)->orMarkupMask(*((*(i + 1))->surfaceWords().begin()));
            (*(i + 1))->surfaceWords().begin()->orMarkupMask(*((*i)->surfaceWords().end() - 1));
         }
      }
   }

   // Last minute cleanup that had to wait for markup to be reconciled.
   // Remove any redundant periods at the end of the sentence due to the rearrangement of the
   // original source words. If there are supposed to be double periods then leave them.
   if ((v_targetSentenceUnits.size() >= 2) && (v_sourceSentenceUnits.size() >= 2))
   {
      TargetUnitIterator lastUnit = v_targetSentenceUnits.end() - 1;
      TargetUnitIterator prevUnit = v_targetSentenceUnits.end() - 2;
      LgsString last = (*lastUnit)->surfaceExpressionAsString();
      LgsString prev = (*prevUnit)->surfaceExpressionAsString();
      if ((last == ".") && (prev.length() > 0) && (prev.substr(prev.length() - 1, 1) == ".") &&
          ((*lastUnit)->wordMarkup()->isBold() == (*prevUnit)->wordMarkup()->isBold()) &&
          ((*lastUnit)->wordMarkup()->isItalic() == (*prevUnit)->wordMarkup()->isItalic()) &&
          ((*lastUnit)->wordMarkup()->isUnderlined() == (*prevUnit)->wordMarkup()->isUnderlined()) &&
          ((*lastUnit)->wordMarkup()->isSingleQuoted() == (*prevUnit)->wordMarkup()->isSingleQuoted()) &&
          ((*lastUnit)->wordMarkup()->isDoubleQuoted() == (*prevUnit)->wordMarkup()->isDoubleQuoted()) &&
          ((*lastUnit)->wordMarkup()->isFrenchSingleQuoted() == (*prevUnit)->wordMarkup()->isFrenchSingleQuoted()) &&
          ((*lastUnit)->wordMarkup()->isFrenchDoubleQuoted() == (*prevUnit)->wordMarkup()->isFrenchDoubleQuoted()))
      {
         LgsString orgLast = (*(v_sourceSentenceUnits.end() - 1))->surfaceExpressionAsString();
         LgsString orgPrev = (*(v_sourceSentenceUnits.end() - 2))->surfaceExpressionAsString();
         if (!((orgLast == ".") && (orgPrev.substr(orgPrev.length() - 1, 1) == ".")))
         {
            delete *(v_targetSentenceUnits.end() - 1);
            v_targetSentenceUnits.erase((v_targetSentenceUnits.end() - 1));
         }
      }
   }
}


// -------------------------------------------------------------------
void LSentence::generateDictionaryEntries()
{
   // Delegate real work to each corresponding unit.
   // Sentence asks each unit to get its own dictionary entry.
   // Because of polymorphism, the only units that do anything
   // here are those that are suppose to have information in the
   // dictionary.

   LgsString prevWord = "";

   for (TargetUnitIterator i = v_targetSentenceUnits.begin();
        i != v_targetSentenceUnits.end(); ++i)
   {
      if ((*i)->isFunctionalConstant())
         (*i)->generateDictionaryEntry(prevWord);
      else
         (*i)->generateDictionaryEntry();
      prevWord = (*i)->word();
   }
}


// -------------------------------------------------------------------
void LSentence::assignMarkupToWords()
{
   // Delegate real work to each corresponding unit.
   for (TargetUnitIterator i = v_targetSentenceUnits.begin();
        i != v_targetSentenceUnits.end(); ++i)
   {
      (*i)->assignMarkupToWords();
   }
}


// -------------------------------------------------------------------
void LSentence::outputTarget()
{
#ifdef TRANSLAPP
	//  +-------------------------------------------------------+
	// -| Calculate size of message to be sent to LGSSGML Merge |
	//  +-------------------------------------------------------+
	int msgSize = 0;
   TargetUnitIterator iter;
   LWordVector::const_iterator wordListBegin;
   LWordVector::const_iterator wordListEnd;
   LWordVector::const_iterator word;

   msgSize += sizeof(ILgsSentInfo); // Sentence ID & number of words in sentence
   int wordCount = 0;

   for (iter = v_targetSentenceUnits.begin(); iter != v_targetSentenceUnits.end(); iter++)
   {
      wordCount += (*iter)->surfaceExpression().words().size();
      wordListBegin = (*iter)->surfaceExpression().words().begin();
      wordListEnd = (*iter)->surfaceExpression().words().end();
      for (word = wordListBegin; word != wordListEnd; word++)
      {
         msgSize += (*word).length(); // Size of each individual word
      }
   }

   msgSize += wordCount * sizeof(ILgsWordMarkup);

	//  +-------------------------------+
	// -| construct the message to send |
	//  +-------------------------------+
   TranslThreadManager& thrManager = TranslThreadManager::singleton();
   const LgsSgmlMerger* mergerThread = dynamic_cast<const LgsSgmlMerger*>(thrManager.GetThread(TranslThreadManager::LGSSGML_MERGE.c_str()));
   LgsMessage outMsg(GenerateMsg, msgSize, 0, mergerThread->threadId());
   char* dataPtr = outMsg.dataPtr();    // the message content

   
	ILgsSentInfo* sentInfo = reinterpret_cast<ILgsSentInfo *>(dataPtr);
   sentInfo->_id = v_markup.id();
   sentInfo->_totWords = wordCount;
   ILgsWordMarkup* wordInfo = reinterpret_cast<ILgsWordMarkup *>(dataPtr + sizeof(ILgsSentInfo));
   char* theWord = 0;

   for (iter = v_targetSentenceUnits.begin(); iter != v_targetSentenceUnits.end(); iter++)
   {
      (*iter)->resolveSpaces();
      wordListBegin = (*iter)->surfaceExpression().words().begin();
      wordListEnd = (*iter)->surfaceExpression().words().end();
      for (word = wordListBegin; word != wordListEnd; word++)
      {
         (*word).fillWordInfo(*wordInfo);
	      char* theWord = (reinterpret_cast<char *>(wordInfo)) + sizeof(ILgsWordMarkup);
         memcpy(theWord, (*word).c_str(), (*word).length());
         wordInfo = reinterpret_cast<ILgsWordMarkup *>(theWord + (*word).length());
      }
   }
	//  +------------------+
	// -| send the message |
	//  +------------------+
	CommunicationInterface::singleton().sendMsg(mergerThread->generateOutHandle(), outMsg);
#endif
}


// -------------------------------------------------------------------
// This is the primary member function of the LSentence class.
// The primary purpose of a Sentence object is to reduce itself
// into an ordered set of SentenceUnit objects. The Gerdem subsystem
// creates a SentenceUnit as a wrapper around an LDictionaryEntry
// object.
// -------------------------------------------------------------------
void LSentence::makeSourceSentenceUnits()
{
   LWordIterator beginWord = sourceWords().begin();
   LWordIterator endOfWord = sourceWords().end();

   bool bAllCapitalWords=true;
   for (LWordIterator oCurrent = beginWord; oCurrent != endOfWord && true == bAllCapitalWords;
   bAllCapitalWords = StringUtil::isAllUpperCaseOrNonAlpha(*oCurrent), oCurrent++);

   int unfoundAddress = st_baseUnfoundWordAddress;

   SourceSentenceUnit* pBosUnit = dictionary().createBosUnit();
   v_sourceSentenceUnits.push_back(pBosUnit);

	dictionary().initializeWordGroupManager(&sourceWords());

   int foundAddress;
   bool BOS = true;
   for (foundAddress = 2; beginWord != sourceWords().end(); foundAddress++)
   {
      // The following line needs adjustment for a sentence that
      // starts with one or more parentheses
      SourceSentenceUnit* pUnit = dictionary().createSentenceUnit(beginWord, endOfWord, BOS, sourceWords(), bAllCapitalWords);
      if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
      {
         dictionary().updateWordPosition(pUnit->dictionaryEntry().wordsUsedInMatch(), beginWord);
      }

      pUnit->setSentence (this);
      int advanceLength = pUnit->dictionaryEntry().wordsUsedInMatch() > 0
                          ? pUnit->dictionaryEntry().wordsUsedInMatch() : 1;
      LWordIterator nextUnusedWord = beginWord + advanceLength;

      if (pUnit->isCompound())
      {
         // See if the phrase has the phrase modification flag set.
         short phraseModifier = LWord::NO_REMOVE;

         for (LWordIterator word = beginWord; word != nextUnusedWord; word++)
         {
            if ((word->getModificationType() == LWord::REMOVE_HEAD) ||
                (word->getModificationType() == LWord::REMOVE_NON_HEAD))
            {
               phraseModifier = word->getModificationType();
            }
         }

         LCompoundDictionaryEntry & compoundEntry =
                                 dynamic_cast<LCompoundDictionaryEntry &>(pUnit->dictionaryEntry());

         int entryCount = compoundEntry.numberOfEntries();
         LDictionaryEntry *dictEntry = 0;
         int entryNumber = 0;
         LWordIterator currentWord = beginWord;
         const CompositeWord * surfaceWords = 0;
         for (entryNumber = 0;
              (entryNumber < entryCount) && (0 != (dictEntry = compoundEntry.getEntry(entryNumber)));
              entryNumber++)
         {
            SourceSentenceUnit *pSentenceUnit = new SourceSentenceUnit(dictionary());
            assert(pSentenceUnit);
            pSentenceUnit->dictionaryEntry(dictEntry);
			   if (0 == entryNumber)
			   {
				   pSentenceUnit->addSurfaceWords(beginWord, (beginWord + advanceLength));
				   surfaceWords = &pSentenceUnit->surfaceExpression();
			   }
			   else
			   {
				   pSentenceUnit->setSurfaceExpression(*surfaceWords);
			   }


            if ((entryNumber + 1) == entryCount && (currentWord-beginWord) != (advanceLength-1))
            {
               currentWord->setTrailingSpaces((beginWord + advanceLength - 1)->trailingSpaces());
            }
            else
            {
               currentWord->setTrailingSpaces(1);
            }
            if (entryNumber > 0)
            {
               currentWord->setPrecedingSpaces(1);
            }

			   pSentenceUnit->setTrailingSpaces(currentWord->trailingSpaces());
			   pSentenceUnit->setPrecedingSpaces(currentWord->precedingSpaces());

            // Make sure if the phrase modification flag has been set that it gets
            // assigned to the correct LWord unit.
            if (((phraseModifier == LWord::REMOVE_HEAD) && dictEntry->headUnit()) ||
                ((phraseModifier == LWord::REMOVE_NON_HEAD) && !dictEntry->headUnit()))
            {
               currentWord->setModificationType(phraseModifier);
            }
            else
            {
               currentWord->setModificationType(LWord::NO_REMOVE);
            }

            if (!dictEntry->isUnfoundWord())
            {
               const LDictionaryToken & dictToken = dictEntry->dictionaryToken();
               DWordPhrase & wordPhrase = dictToken.wordPhrase();
               const LgsString & wordValue = wordPhrase.Word();

               // Borrow all properties from the compound word and then set
               // the text part to the compound word's component text
               LWord tempWord(*currentWord);
               tempWord.setText(wordValue);
               if ((entryNumber == 0) && StringUtil::beginsUpperCase(*currentWord))
               {
                  StringUtil::capitalize(tempWord);
               }
               if (StringUtil::isAllUpperCase(*currentWord))
               {
                  StringUtil::toUpper(tempWord);
               }
               if (entryNumber > 0)
               {
                  tempWord.setPrecedingSpaces(1);
               }
               pSentenceUnit->getMarkupFromWords();
               pSentenceUnit->setSentenceAddress(foundAddress);
            }
            else
            {
               pSentenceUnit->setSentenceAddress(unfoundAddress--);
            }

            if (dictEntry->wordsUsedInMatch())
            {
               currentWord += dictEntry->wordsUsedInMatch();
            }

            // Set isHyphenated to false
            // Assumption: compound word components are never hyphenated
            pSentenceUnit->setHyphenated (false);
            pSentenceUnit->position(foundAddress);
            v_sourceSentenceUnits.push_back(pSentenceUnit);
            foundAddress++;

            if (BOS && pSentenceUnit->isValidFirstWord())
            {
               BOS = false;
            }
         }
         foundAddress--;
         pUnit->deleteEntry();
         delete pUnit;
      }
      else
      {
         pUnit->addSurfaceWords(beginWord, nextUnusedWord);
         pUnit->getMarkupFromWords();
         pUnit->setHyphenated((*(pUnit->surfaceWords().end() - 1)).isHyphenated());
         pUnit->position(foundAddress);
         if (pUnit->isUnfoundWord())
         {
            pUnit->setSentenceAddress(unfoundAddress--);
         }
         else
         {
            pUnit->setSentenceAddress(foundAddress);
         }
         v_sourceSentenceUnits.push_back(pUnit);

         if (BOS && pUnit->isValidFirstWord())
         {
            BOS = false;
         }
      }

      beginWord = nextUnusedWord;
   }
   if (!(v_sourceSentenceUnits.empty()) && !((*(v_sourceSentenceUnits.end() - 1))->isEndOfSentence()) &&
       !((*(v_sourceSentenceUnits.end() - 2))->isEndOfSentence()))
   {
      SourceSentenceUnit* pEosUnit = dictionary().createEmptyEosUnit();
      pEosUnit->position(foundAddress);
      pEosUnit->setSentenceAddress(foundAddress);
      v_sourceSentenceUnits.push_back(pEosUnit);
   }
}


// -------------------------------------------------------------------
void LSentence::associateTargetToSource()
{
    // Have each target unit find its corresponding source
    // unit. Polymorphism is at work here -- types of target
    // units that do not have source units do nothing. If a match
    // fails for a unit that needs a match -- that unit is
    // degraded to functional constant (an exception is caught).

   for (TargetUnitIterator i = v_targetSentenceUnits.begin();
        i != v_targetSentenceUnits.end(); ++i)
   {
      try
      {
         (*i)->findAssociatedSourceUnit(v_sourceSentenceUnits);
         if ((*i)->isUntranslatedSentenceUnit())
            (*i)->findOrigSourceWord(v_origSourceWords);
      }
      catch(LinkException x)
      {
         replaceWithUnresolvedUnit(&i);
      }
   }
}


// -------------------------------------------------------------------
void LSentence::removeExtraneousTargetUnits()
{
   // A German Comma constant is often a superfluous unit. When it immediately follows
   // a BOS unit, it should be removed. First, any target sentence units that precede the BOS
   // are removed, then if a German Comma constant starts the sentence, it is removed. Finally,
   // this routine determines if this is German Comma constant followed by a punctuation unit.
   // The German Comma must be removed.
   if (TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::GermanID)
   {
      bool isBosFound = false;
      TargetUnitIterator currUnit;

      for (currUnit = v_targetSentenceUnits.begin(); currUnit != v_targetSentenceUnits.end(); ++currUnit)
      {
         if (SentenceUnit::BosIdentifier == (*currUnit)->opadr())
         {
            isBosFound = true;
            break;
         }
      }

      if (isBosFound  && (currUnit != v_targetSentenceUnits.begin()))
      {
         for (TargetUnitIterator i =  v_targetSentenceUnits.begin(); i != currUnit; ++i)
         {
            delete (*i);
         }
         v_targetSentenceUnits.erase (v_targetSentenceUnits.begin(), currUnit);
      }

      currUnit = v_targetSentenceUnits.begin();
      if ((*currUnit)->opadr() == SentenceUnit::BosIdentifier)
         ++currUnit;
      if ((currUnit != v_targetSentenceUnits.end()) &&
          ((*currUnit)->opadr() == PunctuationConstantSentenceUnit::GmComma))
      {
         delete (*currUnit);
         v_targetSentenceUnits.erase(currUnit);
      }

      for (TargetUnitIterator i = v_targetSentenceUnits.end() - 2; i > v_targetSentenceUnits.begin(); --i)
      {
         if (PunctuationConstantSentenceUnit::GmComma == (*i)->opadr())
         {
            if ((*(i + 1))->isPunctuation() || (*(i - 1))->isOpenBracketPunctuation())
            {
               delete (*i);
               v_targetSentenceUnits.erase(i);
            }
         }
      }
   }

   // When the word is the single character '*' and the word class of the word is a verb, then
   // remove the target unit because it comes under the heading of extraneous.
   for (TargetUnitIterator j = v_targetSentenceUnits.begin(); j != v_targetSentenceUnits.end(); ++j)
   {
      if (((*j)->word() == "*") && ((*j)->wordClassCode() == LLanguage::VERB))
      {
         delete (*j);
         v_targetSentenceUnits.erase(j);
      }
   }

   // Remove any commas which are followed by any other punctuation including any commas that
   // start the sentence.
   for (TargetUnitIterator iter = v_targetSentenceUnits.begin(); iter != v_targetSentenceUnits.end(); iter++)
   {
      bool doDelete = false;
      if ((*iter)->isComma())
      {
         if (iter == v_targetSentenceUnits.begin())
         {
            doDelete = true;
         }
         else if (((*(iter - 1))->opadr() == SentenceUnit::BosIdentifier) ||
                  ((iter != (v_targetSentenceUnits.end() - 1)) && (*(iter + 1))->isPunctuation() &&
                   ((*(iter + 1))->surfaceExpressionAsString() != "") && !(*(iter + 1))->isOpenBracketPunctuation()))
         {
            doDelete = true;
         }
         if (doDelete)
         {
            delete (*iter);
            v_targetSentenceUnits.erase(iter);
         }
      }
   }
}


// -------------------------------------------------------------------
void LSentence::replaceWithUnresolvedUnit (TargetUnitIterator* i)
{
   TargetSentenceUnit* pOld = **i;
   TargetSentenceUnit* pNew =
                  new FunctionConstantSentenceUnit(FunctionConstantSentenceUnit::UnresolvedTarget);

   pNew->setOpadr(pOld->opadr());
   pNew->setSentenceAddress(pOld->sentenceAddress());
   pNew->setDictionary(pOld->dictionary());
   pNew->setSconTable(pOld->getSconTable());
   pNew->setWordClassCode(LLanguage::NOUN);
   pNew->setTransferSwitch(0);
   pNew->setSentence(this);
   delete pOld;
   **i = pNew;
}


// -------------------------------------------------------------------
void LSentence::deleteEntries()
{
   // Delegate real work to each corresponding unit.
   // Requests that each of the LSentenceUnits that make up the LSentence
   // attempt to delete its internal DictionaryEntry.

   for (SourceUnitIterator i = v_sourceSentenceUnits.begin(); i != v_sourceSentenceUnits.end(); ++i)
   {
      (*i)->deleteEntry();
   }
}


// -------------------------------------------------------------------
void LSentence::deleteSourceUnits()
{
   for (SourceUnitIterator i = v_sourceSentenceUnits.begin(); i != v_sourceSentenceUnits.end(); ++i)
   {
      delete (*i);
   }
   v_sourceSentenceUnits.erase(v_sourceSentenceUnits.begin(), v_sourceSentenceUnits.end());
}


// -------------------------------------------------------------------
void LSentence::deleteTargetUnits()
{
   for (TargetUnitIterator i = v_targetSentenceUnits.begin(); i != v_targetSentenceUnits.end(); ++i)
   {
      delete (*i);
   }
   v_targetSentenceUnits.erase(v_targetSentenceUnits.begin(), v_targetSentenceUnits.end());
}

struct contraction {
	char *s;	// here goes expression we're looking for
	char *t[2];	// one or two words of replacement
};
static struct contraction cntra[] = { 
	{"I'd", "I", "had/would"},
	{"I'll", "I", "will"},
	{"I'm", "I", "am"},
	{"I've", "I", "have"},
	{"you'd", "you", "had/would"},
	{"you'll", "you", "will"},
	{"you're", "you", "are"},
	{"you've", "you", "have"},
	{"he'd", "he", "had/would"},
	{"he'll", "he", "will"}, 
	{"he's", "he", "is/has"}, 
	{"she'd", "she", "had/would"}, 
	{"she'll", "she", "will"}, 
	{"she's", "she", "is/has"},
	{"it'd", "it", "had/would"},
	{"it'll", "it", "will"},
	{"it's", "it", "is/has"},
	{"we'd", "we", "had/would"},
	{"we'll", "we", "will"},
	{"we're", "we", "are"},
	{"we've", "we", "have"},
	{"they'd", "they", "had/would"}, 
	{"they'll", "they", "will"}, 
	{"they're", "they", "are"},
	{"they've", "they", "have"}, 
	{"enough's", "enough", "is"}, 
	{"e'er", "ever", NULL}, 
	{"everything's", "everything", "is"}, 
	{"here's", "here", "is"},
	{"how'd", "how", "had/would"}, 
	{"how's", "how", "is"}, 
	{"let's", "let", "us"}, 
	{"ne'er", "never", NULL},
	{"nothing's", "nothing", "is"},
	{"something's", "something", "is"}, 
	{"that's", "that", "is/has"}, 
	{"there'd", "there", "had/would"}, 
	{"there's", "there", "is"},
	{"what'd", "what", "had/would"}, 
	{"what's", "what", "is/has"}, 
	{"whatever's", "whatever", "is"}, 
	{"when's", "when", "is"},
	{"when'd", "when", "did"}, 
	{"where's", "where", "is"}, 
	{"where'd", "where", "did"}, 
	{"whichever's", "whichever", "is"},
	{"who'd", "who", "had/would"}, 
	{"who'll", "who", "will"}, 
	{"who's", "who", "is/has"}, 
	{"whoever's", "whoever", "is"},
	{"why'd", "why", "had/would"}, 
	{"why's", "why", "is"}, 
	{"aren't", "are", "not"}, 
	{"can't", "cannot", NULL}, 
	{"couldn't", "could", "not"},
	{"didn't", "did", "not"}, 
	{"don't", "do", "not"}, 
	{"doesn't", "does", "not"}, 
	{"shouldn't", "should", "not"}, 
	{"won't", "will", "not"},
	{"wouldn't", "would", "not"}, 
	{"hasn't", "has", "not"}, 
	{"haven't", "have", "not"}, 
	{"isn't", "is", "not"}, 
	{"mayn't", "may", "not"},
	{"mightn't", "might", "not"}, 
	{"mustn't", "must", "not"}, 
	{"wasn't", "was", "not"}, 
	{"weren't", "were", "not"}, 
	{"needn't", "need", "not"},
// The same w/ the 1-st word in upper case
	{"You'd", "You", "had/would"},
	{"You'll", "You", "will"},
	{"You're", "You", "are"},
	{"You've", "You", "have"},
	{"He'd", "He", "had/would"},
	{"He'll", "He", "will"}, 
	{"He's", "He", "is/has"}, 
	{"She'd", "She", "had/would"}, 
	{"She'll", "She", "will"}, 
	{"She's", "She", "is/has"},
	{"It'd", "It", "had/would"},
	{"It'll", "It", "will"},
	{"It's", "It", "is/has"},
	{"We'd", "We", "had/would"},
	{"We'll", "We", "will"},
	{"We're", "We", "are"},
	{"We've", "We", "have"},
	{"They'd", "They", "had/would"}, 
	{"They'll", "They", "will"}, 
	{"They're", "They", "are"},
	{"They've", "They", "have"}, 
	{"Enough's", "Enough", "is"}, 
	{"E'er", "Ever", NULL}, 
	{"Everything's", "Everything", "is"}, 
	{"Here's", "Here", "is"},
	{"How'd", "How", "had/would"}, 
	{"How's", "How", "is"}, 
	{"Let's", "Let", "us"}, 
	{"Ne'er", "Never", NULL},
	{"Nothing's", "Nothing", "is"},
	{"Something's", "Something", "is"}, 
	{"That's", "That", "is/has"}, 
	{"There'd", "There", "had/would"}, 
	{"There's", "There", "is"},
	{"What'd", "What", "had/would"}, 
	{"What's", "What", "is/has"}, 
	{"Whatever's", "Whatever", "is"}, 
	{"When's", "When", "is"},
	{"When'd", "When", "did"}, 
	{"Where's", "Where", "is"}, 
	{"Where'd", "Where", "did"}, 
	{"Whichever's", "Whichever", "is"},
	{"Who'd", "Who", "had/would"}, 
	{"Who'll", "Who", "will"}, 
	{"Who's", "Who", "is/has"}, 
	{"Whoever's", "Whoever", "is"},
	{"Why'd", "Why", "had/would"}, 
	{"Why's", "Why", "is"}, 
	{"Aren't", "Are", "not"}, 
	{"Can't", "Cannot", NULL}, 
	{"Couldn't", "Could", "not"},
	{"Didn't", "Did", "not"}, 
	{"Don't", "Do", "not"}, 
	{"Doesn't", "Does", "not"}, 
	{"Shouldn't", "Should", "not"}, 
	{"Won't", "Will", "not"},
	{"Wouldn't", "Would", "not"}, 
	{"Hasn't", "Has", "not"}, 
	{"Haven't", "Have", "not"}, 
	{"Isn't", "Is", "not"}, 
	{"Mayn't", "May", "not"},
	{"Mightn't", "Might", "not"}, 
	{"Mustn't", "Must", "not"}, 
	{"Wasn't", "Was", "not"}, 
	{"Weren't", "Were", "not"}, 
	{"Needn't", "Need", "not"}
};
static int ncntra = sizeof(cntra)/sizeof(struct contraction);

// -------------------------------------------------------------------
// This method reads in the information about the sentence (id number and number of words)
// from the filter in file and then reads in and fills a vector of source units representing
// the actual source words from the input document.
// -------------------------------------------------------------------
bool LSentence::makeSourceWords()
{
#ifdef TRANSLAPP
	LgsMessage splitterMsg;
   TranslThreadManager& thrManager = TranslThreadManager::singleton();
   const LgsLookup* lookupThread = dynamic_cast<const LgsLookup*>(thrManager.GetThread(TranslThreadManager::LOOKUP.c_str()));
	const QHandle* qHandle = lookupThread->lookupHandle();
	CommunicationInterface& commInterface = CommunicationInterface::singleton();
	commInterface.receiveMsg(qHandle, splitterMsg);

	// If End Of Document; return
	if ((splitterMsg.msgType() == EOD) || (splitterMsg.msgType() == CLOSEFILE))
	{
   	short resId = getThreadId(thrManager.threadList(), TranslThreadManager::RES.c_str());
		commInterface.sendMsg(resId, splitterMsg);
		return false;
	}

	char * dataPtr = splitterMsg.dataPtr();

	ILgsSentInfo* sentInfo = reinterpret_cast<ILgsSentInfo *>(dataPtr);

   (const_cast<SentenceMarkup&>(markup())).id(sentInfo->_id);

   if (sentInfo->_totWords > 0)
   {
      ILgsWordMarkup* wordInfo = reinterpret_cast<ILgsWordMarkup *>(dataPtr + sizeof(ILgsSentInfo));
      for (int cntr = 0; cntr < sentInfo->_totWords; cntr++)
      {
	      char* theWord = (reinterpret_cast<char *>(wordInfo)) + sizeof(ILgsWordMarkup);
         char* wordBuffer = new char[wordInfo->_sizeOfWord + 1];
         memcpy(wordBuffer, theWord, wordInfo->_sizeOfWord);
         wordBuffer[wordInfo->_sizeOfWord] = '\0';

         LgsString actualWord(wordBuffer);
         delete[] wordBuffer;

         while (actualWord.length() > 0)
         {
            LWord word(*wordInfo, actualWord, &(sourceLanguage()), true);

// Contraction
			bool cntraFired = false;
			if (sourceLanguage().id()==
                            //LLanguage::ID::EnglishID)
                            LLanguage::EnglishID)
         {
				for (int ii=0;ii<ncntra;ii++)
            {
					if (word.compare(cntra[ii].s)==0)
               {
						LgsString t0(cntra[ii].t[0]);
						word.assign(t0);
						if (sourceWords().size() > 0)
                  {
							sourceWords().back().setTrailingSpaces(word.precedingSpaces());
							origSourceWords().back().setTrailingSpaces(word.precedingSpaces());
						}
						sourceWords().push_back(word);
						origSourceWords().push_back(word);
						if (cntra[ii].t[1])
                  {
							LgsString t1(cntra[ii].t[1]);
							word.assign(t1);
							word.setPrecedingSpaces(1);
							sourceWords().back().setTrailingSpaces(1);
							origSourceWords().back().setTrailingSpaces(1);
							sourceWords().push_back(word);
							origSourceWords().push_back(word);
						}
						cntraFired = true;
						break;	// we'got a match, no need to check further
					}
				}
			}
			if (cntraFired)
         {	// contraction has been found and replaced
				continue;		// therefore there is no need to check for
			}					// quotes and other misc chars.

            #ifndef FILTER_SPLITS_QUOTES
            // The filters need to be modified to split off quotes as separate units
            // As soon as that happens this code can be removed
            // fix other characters at the same time
            bool firstFragment = true;
            const LgsString strChars("\"')]}>.?!,");
            while (word.length() > 0)
            {
               LgsString::size_type quote = word.find_first_of(strChars);
               if (quote == LgsString::npos)
               {
                  if (!firstFragment)
                  {
                     word.setPrecedingSpaces(0);
                     sourceWords().back().setTrailingSpaces(0);
                     origSourceWords().back().setTrailingSpaces(0);
                  }
                  break;
               }

               // found a quote
               const LgsString duplicate(word);
               if (quote > 0)
               {
                  // split off LgsString to left of the quote
                  word.erase(quote);                // remove from and including the quote to end of LgsString
                  if (firstFragment)
                  {
                     firstFragment = false;
                  }
                  else
                  {
                     word.setPrecedingSpaces(0);
                  }
                  if (sourceWords().size() > 0)
                  {
                     sourceWords().back().setTrailingSpaces(word.precedingSpaces());
                     origSourceWords().back().setTrailingSpaces(word.precedingSpaces());
                  }
                  sourceWords().push_back(word);    // append
                  origSourceWords().push_back(word);
               }

               // split off the quote by itself
               word.assign(1, duplicate[quote]);     // set LgsString to quote by itself
               if (firstFragment)
               {
                  firstFragment = false;
               }
               else
               {
                  word.setPrecedingSpaces(0);
               }
               if (sourceWords().size() > 0)
               {
                  sourceWords().back().setTrailingSpaces(word.precedingSpaces());
                  origSourceWords().back().setTrailingSpaces(word.precedingSpaces());
               }
               sourceWords().push_back(word);        // append
               origSourceWords().push_back(word);

               // remove from the beginning of the LgsString up to and including the quote
               word.assign(duplicate.begin() + quote + 1, duplicate.end());
            }
            #endif

            if (word.length() > 0)
            {
               sourceWords().push_back(word);
               origSourceWords().push_back(word);
            }
         }
         wordInfo = reinterpret_cast<ILgsWordMarkup *>(theWord + wordInfo->_sizeOfWord);
      }
      // Determine the markup that applies to the entire sentence for res.
      determineSentenceMarkup();

      // Determine if the sentence is bracketed by "()" or "[]" or "{}".
      determineIfBracketed();
   }
#endif
return true;
}


// -------------------------------------------------------------------
void LSentence::patternMatch(RE_Engine<PM_Variable>* engine, bool source)
{
   if (source)
   {
      // create a pattern match variable, and evaluate the engine on it
      PM_Variable variable(sourceWords());
      engine->evaluate(variable);
   }
   else
   {
      // create a pattern match variable, and evaluate the engine on it
      PM_Variable variable(targetSentenceUnits());
      engine->evaluate(variable);
      variable.synchronizeTargetUnits();
   }
}


// -------------------------------------------------------------------
void LSentence::doStartRules()
{
   sourceLanguage().doStartRules(sourceWords());
   LWordVector::iterator iter;
   LWordVector::iterator orig;

   // iterate through the words - replacing tok_lookup token types with tok_none
   for (iter = v_sourceWords.begin(); iter != v_sourceWords.end(); iter++)
   {
      if (iter->getTokenType() == LookupTokenType::tok_lookup)
         iter->setTokenType(LookupTokenType::tok_none);
   }

   if (v_sourceWords.size() < 2)
      return;

   // iterate through the words - joining words if they have alpha/numeric/single-quotes
   // also join period to preceeding alpha/numeric/single-quotes word, unless
   //     * the period is the last token
   //     * the period is followed by a closing bracket
   //     * the period is the last quoted token
   // go backwards since we may be erasing elements, and the iterators are invalid after the erasure

   // Initialize original source word list to match that of the source word list
   orig = v_origSourceWords.end();
	for (iter--, orig--; iter != v_sourceWords.begin(); iter--, orig--)
	{
		LWord& curr = *iter;
		LWord& prev = *(iter - 1);
      LWord& origCurr = *orig;
      LWord& origPrev = *(orig - 1);
		if (curr.getTokenType() != LookupTokenType::tok_none ||
			prev.getTokenType() != LookupTokenType::tok_none || curr.precedingSpaces() > 0)
		{
			continue;
		}

		assert(prev.length() > 0);
		assert(curr.length() > 0);
		char c1 = prev[0];
		char c1_end = prev[prev.length() - 1];
		char c2 = curr[0];
      bool join = false;

		if ((c1 == '\'' || CharUtil::isAlphaNumeric(c1)))
		{
			if (c2 == '\'' || CharUtil::isAlphaNumeric(c2))
			{
				join = true;
			}
			else if (c2 == '.' && iter != v_sourceWords.end() - 1)
			{
				LWord& next = *(iter + 1);
				assert(next.length() > 0);
				char c3 = next[0];
				if ( (c3 != ')' && c3 != '}' && c3 != ']') ||
					 (iter+1) != (v_sourceWords.end() - 1))
				{
					int currQuote = 0;
					if (curr.isSingleQuoted())
						currQuote++;
					if (curr.isDoubleQuoted())
						currQuote++;

					int nextQuote = 0;
					if (next.isSingleQuoted())
						nextQuote++;
					if (next.isDoubleQuoted())
						nextQuote++;

					if (currQuote <= nextQuote)
						join = true;
				}
			}
		}
      else if (((curr == "'s") || (curr == "s'")) && (curr.precedingSpaces() == 0) && (prev.trailingSpaces() == 0))
      {
         join = true;
      }
		if (join)
		{
			prev += curr;
         prev.setTrailingSpaces(curr.trailingSpaces());
         prev.setHyphenated(curr.isHyphenated());
         origPrev += origCurr;
         origPrev.setTrailingSpaces(origCurr.trailingSpaces());
			iter = v_sourceWords.erase(iter);
         orig = v_origSourceWords.erase(orig);
		}
	}
	for (iter = (v_sourceWords.end() - 1), orig = (v_origSourceWords.end() - 1);
        iter != v_sourceWords.begin(); iter--, orig--)
	{
		LWord& curr = *iter;
		LWord& prev = *(iter - 1);
      LWord& origCurr = *orig;
      LWord& origPrev = *(orig - 1);
		if (curr.getTokenType() != LookupTokenType::tok_none ||
			prev.getTokenType() != LookupTokenType::tok_none || curr.precedingSpaces() > 0)
		{
			continue;
		}
		if (prev[prev.length()-1] == '.')
		{
         LgsString ending;
         
         if (curr.length() > 2)
         {
            ending = curr.substr(curr.length() - 2, 2);
         }
			if (StringUtil::isAllAlphaNumericAndPeriods(curr) || (ending == "'s") || (ending == "s'"))
			{
				prev += curr;
            prev.setTrailingSpaces(curr.trailingSpaces());
            prev.setHyphenated(curr.isHyphenated());
            origPrev += origCurr;
            origPrev.setTrailingSpaces(origCurr.trailingSpaces());
				iter = v_sourceWords.erase(iter);
            orig = v_origSourceWords.erase(orig);
			} 
		} 
	}
    iter = v_sourceWords.begin();
    orig = v_origSourceWords.begin();

	while (iter != v_sourceWords.end() && (iter+1) != v_sourceWords.end()) 
	{
        LWord& fw = *iter;
        LWord& sw = *(iter+1);
        LWord& origFw = *orig;
        LWord& origSw = *(orig+1);
        if (0 == sw.precedingSpaces() &&
            StringUtil::isAllNumeric(fw) &&
            StringUtil::isEnglishSuffix(sw))
        {
            fw += sw;
            fw.setTrailingSpaces(sw.trailingSpaces());
            fw.setHyphenated(sw.isHyphenated());
            origFw += origSw;
            origFw.setTrailingSpaces(origSw.trailingSpaces());
            origFw.setHyphenated(origSw.isHyphenated());
            v_sourceWords.erase(iter+1);
            v_origSourceWords.erase(orig+1);
            continue;
        } 
        iter++;
        orig++;
	}
    iter = v_sourceWords.begin();
    orig = v_origSourceWords.begin();

	while (iter != v_sourceWords.end() && (iter+1) != v_sourceWords.end() && (iter+2) != v_sourceWords.end()) 
	{
        LWord& fw = *iter;
        LWord& sw = *(iter+1);
        LWord& tw = *(iter+2);
        LWord& origFw = *orig;
        LWord& origSw = *(orig+1);
        LWord& origTw = *(orig+2);
        if (1 == sw.length() && CharUtil::isJoiningCharacter(sw[0]) &&
            0 == sw.precedingSpaces() && 0 == tw.precedingSpaces() &&
            !CharUtil::isWordSeparator(fw[fw.length()-1]) &&
            !CharUtil::isWordSeparator(tw[0]) &&
            StringUtil::isAllAlphaNumericAndJoiningCharacters(fw) &&
            StringUtil::isAllAlphaNumericAndJoiningCharacters(tw))
        {
            fw += sw;
            fw += tw;
            fw.setTrailingSpaces(tw.trailingSpaces());
            fw.setHyphenated(tw.isHyphenated());
            origFw += origSw;
            origFw += origTw;
            origFw.setTrailingSpaces(origTw.trailingSpaces());
            origFw.setHyphenated(origTw.isHyphenated());
            v_sourceWords.erase(v_sourceWords.erase(iter+1));
            v_origSourceWords.erase(v_origSourceWords.erase(orig+1));
            continue;
        } 
        iter++;
        orig++;
	}
   iter = v_sourceWords.end();
}

// -------------------------------------------------------------------
bool LSentence::isQuestion() const
{
   // If the sends ends in a question mark, the sentence itself is a question.
   bool result = false;

   if (!v_sourceSentenceUnits.empty())
   {
      const SourceSentenceUnit* pEndOfSentence = *(v_sourceSentenceUnits.end() - 1);
      result = (const_cast<SourceSentenceUnit*>(pEndOfSentence))->isQuestionMark();
   }
   return result;
}


// -------------------------------------------------------------------
// Construct the Lookup message to send to further TransL processes
// -------------------------------------------------------------------
void LSentence::persistOut(void)
{
#ifdef TRANSLAPP
	//  +---------------------------------------------+
	// -| Calculate size of message to be sent to Res |
	//  +---------------------------------------------+
	int msgSize = 0;
   LWordIterator orig;

	// number of bytes needed to store info about the source sentence units
	for (SourceUnitIterator ws = v_sourceSentenceUnits.begin(); ws != v_sourceSentenceUnits.end(); ws++)
	{
		short ssuCount = (*ws)->ssuCount();
      if (ssuCount > 3)
      {
         ssuCount = 3;
      }
		msgSize += ssuCount * sizeof(ISWorkInfo) + sizeof(ISWorkInfoHeader);
		if ((*ws)->isUnfoundWord())
         msgSize += (*ws)->surfaceExpressionAsString().length();
		else
         msgSize += (*ws)->word().length();
      for (int cntr = 0; cntr < ssuCount; cntr++)
      {
         msgSize += (*ws)->ssuAt(cntr).word().length();
         msgSize += (*ws)->ssuAt(cntr).targetWord().length();
      }
		msgSize += sizeof(int);
		msgSize += (*ws)->surfaceWords().size() * sizeof(IWordMarkup);
		for (LWordIterator s = (*ws)->surfaceWords().begin(); s != (*ws)->surfaceWords().end(); s++)
		{
			msgSize += s->length();
		}
	}

   // Now add the number of bytes needed to store the original source words.
   msgSize += sizeof(int);
   msgSize += origSourceWords().size() * sizeof(IWordMarkup);
   for (orig = origSourceWords().begin(); orig != origSourceWords().end(); orig++)
   {
      msgSize += orig->length();
   }
	msgSize += sizeof(ISentenceInfo);

	//  +-------------------------------+
	// -| construct the message to send |
	//  +-------------------------------+
	TranslThreadManager & thrManager = TranslThreadManager::singleton();
   const LgsRes* resThread = dynamic_cast<const LgsRes*>(thrManager.GetThread(TranslThreadManager::RES.c_str()));
	short msgType = LookupMsg;
	if (translationState_ != DoTranslate) 
	{
		msgType = UntranslatedLookupMsg;
	}
	LgsMessage outMsg(msgType, msgSize, 0, resThread->threadId());
	char* dataPtr = outMsg.dataPtr();					// the message content

	// general info on the sentence
	ISentenceInfo* sentInfo = reinterpret_cast<ISentenceInfo *>(dataPtr);
	sentInfo->_isObject = true;
	LSyntaxUnit::persistOut(&sentInfo->_position);
	sentInfo->_ssuListSize = v_sourceSentenceUnits.size();
	sentInfo->_partOf = partOf_;
	sentInfo->_numberOfParts = numberOfParts_;;
	sentInfo->_translationState = translationState_;
	sentInfo->_caseState = caseState();
	sentInfo->_sentMarkup = v_markup.id();
	sentInfo->_bold = bold_;
	sentInfo->_italic = italic_;
	sentInfo->_underlined = underlined_;
	sentInfo->_singleQuoted = singleQuoted_;
	sentInfo->_doubleQuoted = doubleQuoted_;
   sentInfo->_bDifferentSentence = bDifferentSentence_;
	char* wordSize = dataPtr + sizeof(ISentenceInfo);

	// info on each source sentence unit
	int opadrAdj = 0;
   int wsize = 0;
   IWordMarkup* wrdMarkup = 0;
	for (SourceUnitIterator w = v_sourceSentenceUnits.begin(); w != v_sourceSentenceUnits.end(); w++)
	{
		bool excludeFromTranslation = false;
		wsize = (*w)->surfaceWords().size();
		memcpy(wordSize, (const char *)&wsize, sizeof(wsize));
		wrdMarkup = reinterpret_cast<IWordMarkup *>(wordSize + sizeof(int));
		for (LWordIterator s = (*w)->surfaceWords().begin(); s != (*w)->surfaceWords().end(); s++, wrdMarkup++)
		{
			// Adjust the opadr address on units in sentence bounded by brackets.
			if (s->isExcludedFromTranslation() && !excludeFromTranslation)
			{
				opadrAdj++;
				excludeFromTranslation = true;
			}
			s->persistOut(wrdMarkup);
			wrdMarkup = reinterpret_cast<IWordMarkup *>((reinterpret_cast<char *>(wrdMarkup) + s->length()));
		}
		char* sworkPtr = reinterpret_cast<char *>(wrdMarkup);
		SWork swork(**w);

		// Adjust the opadr address on units in sentence bounded by brackets.
		if (!excludeFromTranslation) 
		{
			swork.SourcePosition(swork.SourcePosition() - opadrAdj);
		}

		// streamOutSWorkInfo returns new position
		wordSize = streamOutSWorkInfo(sworkPtr, swork);
	}

	wsize = origSourceWords().size();
	memcpy(wordSize, (const char *)&wsize, sizeof(wsize));
   wrdMarkup = reinterpret_cast<IWordMarkup *>(wordSize + sizeof(int));
	for (orig = origSourceWords().begin(); orig != origSourceWords().end(); orig++, wrdMarkup++)
	{
		// Adjust the opadr address on units in sentence bounded by brackets.
		orig->persistOut(wrdMarkup);
		wrdMarkup = reinterpret_cast<IWordMarkup *>((reinterpret_cast<char *>(wrdMarkup) + orig->length()));
	}

	//  +------------------+
	// -| send the message |
	//  +------------------+
	CommunicationInterface::singleton().sendMsg(resThread->threadId(), outMsg);
#endif
}


// -------------------------------------------------------------------
// Read the content of a message
// -------------------------------------------------------------------
bool LSentence::persistIn(void)
{
#ifdef TRANSLAPP
	CommunicationInterface& commInterface = CommunicationInterface::singleton();
	TranslThreadManager& thrManager = TranslThreadManager::singleton();
	const QHandle* qHandle;

	if (JobControlArguments::translationMode == LgsDBCommonObjects::GetJobControlArguments().RunMode())
	{
      const LgsGenerate* generateThread = dynamic_cast<const LgsGenerate*>(thrManager.GetThread(TranslThreadManager::GENERATE.c_str()));
		qHandle = generateThread->generateHandle();
	}
	else
	{
      const LgsTermSearch* termSearchThread = dynamic_cast<const LgsTermSearch*>(thrManager.GetThread(TranslThreadManager::TERM_SEARCH.c_str()));
		qHandle = termSearchThread->termSearchHandle();
	}
	commInterface.receiveMsg(qHandle, *lookupMsgRes);

	// If End Of Document; return
	if ((lookupMsgRes->msgType() == EOD) ||(lookupMsgRes->msgType() == CLOSEFILE))
	{
		return false;
	}

	switch(lookupMsgRes->msgType())
	{
	case LookupMsg:
		commInterface.receiveMsg(qHandle, *tranMsg);
		break;
	case Tran4Msg:
	case Tran1Msg:
		*tranMsg = *lookupMsgRes;
		commInterface.receiveMsg(qHandle, *lookupMsgRes);
	case UntranslatedLookupMsg:
		break;
	}

	char * dataPtr = lookupMsgRes->dataPtr();


	ISentenceInfo * sentInfo = reinterpret_cast<ISentenceInfo *>(dataPtr);

	if (!sentInfo->_isObject)
	{
		return false;
	}
	LSyntaxUnit::persistIn(&sentInfo->_position);
	int suSize = 0;
	suSize = sentInfo->_ssuListSize;
	partOf_ = sentInfo->_partOf;
	numberOfParts_ = sentInfo->_numberOfParts;
    bDifferentSentence_ = sentInfo->_bDifferentSentence;

	switch (sentInfo->_translationState)
	{
	case 0:
		translationState_ = DoTranslate;
		break;
	case 1:
		translationState_ = DoNotTranslate;
		break;
	default:
		translationState_ = DoTranslate;
		break;
	}

	switch (sentInfo->_caseState)
	{
	case -1:
		caseState_ = SentenceUnit::IrrelevantCase;
		break;
	case 0:
		caseState_ = SentenceUnit::LowerCase;
		break;
	case 1:
		caseState_ = SentenceUnit::BeginsUpperCase;
		break;
	case 2:
		caseState_ = SentenceUnit::AllUpperCase;
		break;
	default : caseState_ = SentenceUnit::UndeterminedCase;
		break;
	}
	v_markup.id(sentInfo->_sentMarkup);

   // Get the markup information for the sentence.
   bold_ = sentInfo->_bold;
   italic_ = sentInfo->_italic;
   underlined_ = sentInfo->_underlined;
   singleQuoted_ = sentInfo->_singleQuoted;
   doubleQuoted_ = sentInfo->_doubleQuoted;

	// The wordSize pointer marks the start of a new block of SSU info
	// which includes - SWork Information, Markup information for the unit.
	char * wordSize = dataPtr + sizeof(ISentenceInfo);
   int opadrAdj = 0;
   bool excludeFromTranslation = false;
	int size = 0;
	IWordMarkup* wrdMarkup = 0;
   
   for (int w = 0; w < suSize; w++)
	{
		memcpy((char *)&size, wordSize, sizeof(size));
		wrdMarkup = reinterpret_cast<IWordMarkup *>(wordSize + sizeof(int));

		// Calculate SWORK pointer
		IWordMarkup * tempWrdMarkup = wrdMarkup;
		char * sworkPtr = reinterpret_cast<char *>(wrdMarkup);
		for (int wordNo = 0; wordNo < size; wordNo++)
		{	
			sworkPtr += sizeof(IWordMarkup) + tempWrdMarkup->_sizeOfWord;
			tempWrdMarkup = reinterpret_cast<IWordMarkup *>(sworkPtr);
		}

		SWork swork;

		//update start of next ssu block pointer
		wordSize = streamInSWorkInfo(sworkPtr, swork);

		SourceSentenceUnit* pUnit = new SourceSentenceUnit(dictionary(), swork);
		pUnit->setSentence(this);

		for (int i = 0; i < size; i++, wrdMarkup++)
		{
			LWord word;
			word.persistIn(wrdMarkup);

         // Adjust the opadr for those units in a bracketed sentence that were adjust before going to res.
         if (word.isExcludedFromTranslation() && !excludeFromTranslation)
         {
            opadrAdj++;
            excludeFromTranslation = true;
         }

			char* temp = reinterpret_cast<char *>(wrdMarkup);
			wrdMarkup = reinterpret_cast<IWordMarkup *>(temp + wrdMarkup->_sizeOfWord);
			pUnit->surfaceWords().push_back(word);

         // Retrieve the original sentence position of the unit
         if (i == 0)
         {
            pUnit->setOriginalSentencePosition(word.originalSentencePosition());
         }

         // Retrieve the trialing spaces
         if (i == (size - 1))
         {
            pUnit->setTrailingSpaces(word.trailingSpaces());
         }
		}

		// Adjust the opadr for those units in a bracketed sentence that were adjust before going to res.
		if (!excludeFromTranslation)
		{
			pUnit->setSentenceAddress(pUnit->sentenceAddress() + opadrAdj);
		}

		// add this newly constructed source sentence unit to the vector of SSU's
		v_sourceSentenceUnits.push_back(pUnit);
	}

	memcpy((char *)&size, wordSize, sizeof(size));
	wrdMarkup = reinterpret_cast<IWordMarkup *>(wordSize + sizeof(int));
	for (int i = 0; i < size; i++, wrdMarkup++)
	{
		LWord origWord;
		origWord.persistIn(wrdMarkup);

		char* temp = reinterpret_cast<char *>(wrdMarkup);
		wrdMarkup = reinterpret_cast<IWordMarkup *>(temp + wrdMarkup->_sizeOfWord);
		origSourceWords().push_back(origWord);
	}
#endif
	return true;
}


// -------------------------------------------------------------------
void LSentence::deleteEmptyUnits(bool termSearchOnly)
{
   bool prevDeleted = false;
   TargetUnitIterator i;
   if (v_targetSentenceUnits.size() > 2)
   {
      if (termSearchOnly)
      {
         for (i = v_targetSentenceUnits.end() - 2; i > v_targetSentenceUnits.begin(); --i)
         {
            if (((*i)->surfaceExpressionAsString() == "~QZQD~") ||
                ((*i)->surfaceExpression().isEmpty() && (*i)->isFunctionalConstant() && (!(*i)->isInhibitCapConstant()) ) )
            {
               (*(i - 1))->mergeMarkupAndSpaces(**i);
               if (((*i)->isCloseSpaceFunctionalConstant(i - 1) && !prevDeleted) || (*i)->hadSpaceClosed())
               {
                  (*(i - 1))->setHadSpaceClosed(true);
               }
               if (!(*i)->isFunctionalConstant() || ((*i)->opadr() == FunctionConstantSentenceUnit::GerAdjInflection))
               {
                  prevDeleted = true;
               }
               delete (*i);
               v_targetSentenceUnits.erase(i);
            }
         }
      }
      else
      {
         for (i =  v_targetSentenceUnits.end() - 2; i > v_targetSentenceUnits.begin(); --i)
         {
            if (((*i)->surfaceExpression().isEmpty() && !(*i)->isInhibitCapConstant()) ||
                ((*i)->surfaceExpressionAsString() == "~QZQD~"))
            {
               (*(i - 1))->mergeMarkupAndSpaces(**i);
               if (((*i)->isCloseSpaceFunctionalConstant(i - 1) && !prevDeleted) || (*i)->hadSpaceClosed())
               {
                  (*(i - 1))->setHadSpaceClosed(true);
               }
               if (!(*i)->isFunctionalConstant() ||
                   ((*i)->opadr() == FunctionConstantSentenceUnit::GerAdjInflection))
               {
                  prevDeleted = true;
               }
               delete (*i);
               v_targetSentenceUnits.erase(i);
            }
            else
               prevDeleted = false;
         }
      }
   }
}


// -------------------------------------------------------------------
void LSentence::mergeTargetUnits()
{
   if (v_targetSentenceUnits.size() > 2)
   {
      TargetUnitIterator i;

      for (i = v_targetSentenceUnits.end() - 2; i > v_targetSentenceUnits.begin(); --i)
      {
         if ((*(i - 1))->hadSpaceClosed())
         {
            LgsString prevWords = (*(i - 1))->surfaceExpressionAsString();
            LgsString currWords = (*i)->surfaceExpressionAsString();
            StringUtil::rightTrim(prevWords, ' ');
            StringUtil::leftTrim(prevWords, ' ');
            TargetSentenceUnit* newUnit;
            if (((*(i - 1))->opadr() < 0) && ((*i)->opadr() > (*(i - 1))->opadr()))
            {
               (*i)->setHadSpaceClosed(false);
               if ((*i)->isLinkedTargetUnit())
               {
                  newUnit = new LinkedTargetUnit(*(dynamic_cast<LinkedTargetUnit*>(*i)));
               }
               else
               {
                  newUnit = new TargetSentenceUnit(*(*i));
               }
               (*i)->surfaceExpressionFromString(prevWords + currWords);
               (*(i - 1))->reconcileMarkup();
               (*i)->wordMarkup()->orMask((*(i - 1))->markup());
               (*i)->sconPointerVector((*(i - 1))->sconPointerVector(), false);
               (*i)->setMerged(true);
               (*i)->setPrevMergedUnit((*(i - 1)));
               (*i)->setNextMergedUnit(newUnit);
               --i;
               v_targetSentenceUnits.erase(i);
            }
            else
            {
               (*(i - 1))->setHadSpaceClosed(false);
               if ((*(i - 1))->isLinkedTargetUnit())
               {
                  newUnit = new LinkedTargetUnit(*(dynamic_cast<LinkedTargetUnit*>(*(i - 1))));
               }
               else
               {
                  newUnit = new TargetSentenceUnit(*(*(i - 1)));
               }
               (*(i - 1))->surfaceExpressionFromString(prevWords + currWords);
               (*(i - 1))->setTrailingSpaces((*i)->trailingSpaces());
               (*i)->reconcileMarkup();
               (*(i - 1))->wordMarkup()->orMask((*i)->markup());
               (*(i - 1))->sconPointerVector((*i)->sconPointerVector(), false);
               (*(i - 1))->setMerged(true);
               (*(i - 1))->setPrevMergedUnit(newUnit);
               (*(i - 1))->setNextMergedUnit(*i);
               v_targetSentenceUnits.erase(i);
            }
         }
      }
   }
}


// -------------------------------------------------------------------
void LSentence::processBlackHoles()
{
   for (TargetUnitIterator i = v_targetSentenceUnits.begin(); i != v_targetSentenceUnits.end(); ++i)
   {
      if ((*i)->isBlackHoleStart())
      {
         TargetUnitIterator holeEnd = findNextBlackHoleEnd(i, v_targetSentenceUnits.end());

         // See if the black hole address is 0
         if ((*i)->blackHoleTargetAddress() == 0)
         {
            // If the address is 0 then remove the start and end markers and leave everything else
            // in between. Note: order of removal is important!
            v_targetSentenceUnits.removeAndDelete(holeEnd, holeEnd + 1);
            v_targetSentenceUnits.removeAndDelete(i, i + 1);
         }
         else
         {
            BlackHole* pHole = createBlackHole(i, holeEnd);
            insertBlackHole(pHole);
            v_targetSentenceUnits.removeAndDelete(i, holeEnd + 1);
            delete pHole;
         }
         --i;
      }
   }
}


// -------------------------------------------------------------------
void LSentence::capitalize()
{
   // Used primarily at the target end. Tells most of its units to capitalize themselves.
   // Each unit needs to know if it is the first real word in the sentence. "realWordCount"
   // is incremented for each word that is not BOS or in some other is disqualified as a
   // real word.
   int realWordCount = 0;
   int wordSentPos = 0;
   bool acceptTransfer = false;

   for (TargetUnitIterator i = v_targetSentenceUnits.begin(); i != v_targetSentenceUnits.end(); ++i)
   {
      // Skip over the BOS unit and any inhibit cap constant function constant units.
      if ((SentenceUnit::BosIdentifier == (*i)->opadr()) || (*i)->isInhibitCapConstant())
      {
         continue;
      }
      
      wordSentPos++;

      // Increment word count if this is a valid word.
      if ((*i)->isValidFirstWord())
      {
         ++realWordCount;
         if ((realWordCount == 1) && acceptTransfer)
         {
            (*i)->acceptTransfer(true);
         }
      }
      else
      {
         if ((wordSentPos == 1) && (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID) &&
             (*i)->sourceSentenceUnit() && ((*i)->sourceSentenceUnit()->originalSentencePosition() == 1) &&
             ((*i)->sourceSentenceUnit()->wordClassCode() == LLanguage::ARITHMATE))
         {
            acceptTransfer = true;
         }
      }

      // If this is not the first unit of the sentence.
      if (i != v_targetSentenceUnits.begin())
      {
         // If the previous unit was an inhibit cap constant then use dictionary transfer.
         if ((*(i - 1))->isInhibitCapConstant() && (caseState_ != SentenceUnit::AllUpperCase))
         {
            continue;
         }
      }

      // Make sure if this is a multi-part sentence, that only the first part is treated as it should
      // with a BOS unit.
      if ((realWordCount == 1) && (numberOfParts_ > 0) && (partOf_ != 1) && !bDifferentSentence_)
      {
         ++realWordCount;
      }

      (*i)->capitalize(realWordCount);

      // Some preparation must be done to handle German words that are merged later on. For all other
      // languages, similar preparation must be done to handle words that are merged.
      if (((i + 1) != v_targetSentenceUnits.end()) && (*i)->hadSpaceClosed() &&
          ((*i)->caseState() == SentenceUnit::AllUpperCase))
      {
         if (TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::GermanID)
         {
            if ((*(i + 1))->opadr() == FunctionConstantSentenceUnit::CloseSpaceLeft)
            {
               (*(i + 1))->setCaseState(SentenceUnit::AllUpperCase);
            }
         }
         else if (!(*(i + 1))->sourceSentenceUnit())
         {
            (*(i + 1))->setCaseState(SentenceUnit::AllUpperCase);
         }
      }
   }

   // Remove the inhibit cap constant functionconstant units.
   for (TargetUnitIterator di =  v_targetSentenceUnits.end() - 1;
        di >= v_targetSentenceUnits.begin(); --di)
   {
      if (FunctionConstantSentenceUnit::InhibitFullCap == (*di)->opadr())
      {
         delete (*di);
         v_targetSentenceUnits.erase(di);
      }
   }

   // Check for constants that are surrounded by all capped words.
   for (TargetUnitIterator iter = v_targetSentenceUnits.end() - 2; iter > v_targetSentenceUnits.begin(); --iter)
   {
      // If the previous unit is the BOS unit then skip.
      if (SentenceUnit::BosIdentifier == (*(iter - 1))->opadr())
      {
         continue;
      }

      // If the current unit is a constant ((opadr < -35) || (opadr > 70)) and the previous and next
      // sentence units are all capped, then fully cap the constant.
      TargetUnitIterator prev = (iter - 1);
      while ((prev != v_targetSentenceUnits.begin()) && (*prev)->isConstantSentenceUnit() &&
             !(*prev)->sourceSentenceUnit())
      {
         prev--;
      }

      if ((*iter)->isConstantSentenceUnit() && !(*iter)->sourceSentenceUnit() &&
          StringUtil::isAllUpperCase((*prev)->surfaceExpressionAsString()) &&
          StringUtil::isAllUpperCase((*(iter + 1))->surfaceExpressionAsString()))
      {
         (*iter)->setCaseState(SentenceUnit::AllUpperCase);
         LgsString& s = (LgsString&) (*iter)->surfaceExpressionAsString();
   	   if (TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::GermanID)
            StringUtil::toUpperGerman(s);
         else
            StringUtil::toUpper(s);
         (*iter)->surfaceExpressionFromString(s);
      }
   }
}


// -------------------------------------------------------------------
void LSentence::elide()
{
   targetLanguage().elide(targetSentenceUnits());
}


// -------------------------------------------------------------------
TargetUnitIterator LSentence::findNextBlackHoleEnd(TargetUnitIterator start,
                                                   TargetUnitIterator end) const
{
   assert(start != end);

   for (TargetUnitIterator i = start; i != end; i++)
   {
      if ((*i)->isBlackHoleEnd())
      {
         return i;
      }
   }
   assert(0);
   return end;
}


// -------------------------------------------------------------------
BlackHole* LSentence::createBlackHole(TargetUnitIterator start, TargetUnitIterator end)
{
   BlackHole* pHole = new BlackHole();
   pHole->setTargetUnitAddress((*start)->blackHoleTargetAddress());

   bool startSeen = false;

   for (TargetUnitIterator i = (start + 1); i != end; i++)
   {
      if (startSeen)
         pHole->addText(" ");
      else
         startSeen = true;
      pHole->addText((*i)->surfaceExpressionAsString());
   }
   return pHole;
}


// -------------------------------------------------------------------
void LSentence::insertBlackHole(const BlackHole* pHole)
{
   for (TargetUnitIterator i = v_targetSentenceUnits.begin(); i != v_targetSentenceUnits.end (); ++i)
   {
      if ((*i)->blackHoleSentenceAddress() == pHole->targetUnitAddress())
      {
         (*i)->insertBlackHole (pHole);
         break;
      }
   }
}


// -------------------------------------------------------------------
void LSentence::writeSourceWords(ostream& stream)
{
   // Delegate real work to each corresponding unit.
   for (LWordIterator i =  v_sourceWords.begin(); i != v_sourceWords.end(); ++i)
   {
      stream << *i << endl;
   }
}


// --------------------------------------------------------------------------
// Finalize the surface form of the vector of target units
// - for translation jobs: call beginFinalizeTransfer() and finishFinalizeTransfer() 
//   without interuption -> see LDocument::generateTranslation()
// - for term search jobs: call beginFinalizeTransfer() and finishFinalizeTransfer() 
//   with interuption for processing -> see LDocument::termSearch()
// --------------------------------------------------------------------------
void LSentence::finalizeTransfer(RuleEngine* patternMatcherEngine)
{
	beginFinalizeTransfer();
	finishFinalizeTransfer(patternMatcherEngine);
}
// --------------------------------------------------------------------------
void LSentence::beginFinalizeTransfer()
{
	generate();
	associateTargetToSource();
	updateSourcePrimarySsu();
	generateDictionaryEntries();
	setSurfaceExpressionFromDictionary();
	filterLastNames();
	removeExtraneousTargetUnits();

	if (isTranslated())
   {
		TranslCommonObjects::GetDiagnostic()->writeLine("\n*Scon Information*");
		TranslCommonObjects::GetDiagnostic()->sconCompare(*this);
	}
	TranslCommonObjects::GetDiagnostic()->write("\n*From Dictionary*\n");
	TranslCommonObjects::GetDiagnostic()->generateSentence(*this);

	generateStems();
	TranslCommonObjects::GetDiagnostic()->write("\n*After Stemgen*\n");
	TranslCommonObjects::GetDiagnostic()->generateSentence(*this);

	makeAspirations();
	processBlackHoles();
	TranslCommonObjects::GetDiagnostic()->write("\n*After Black Hole*\n");
	TranslCommonObjects::GetDiagnostic()->generateSentence(*this);

	elide();
	TranslCommonObjects::GetDiagnostic()->write("\n*After Finish Rules*\n");
	TranslCommonObjects::GetDiagnostic()->generateSentence(*this);
}
//------------------------------------------------------------------------
void LSentence::finishFinalizeTransfer(RuleEngine* patternMatcherEngine)
{
	deleteEmptyUnits();
	TranslCommonObjects::GetDiagnostic()->write("\n*After deleteEmptyUnits*\n");
	TranslCommonObjects::GetDiagnostic()->generateSentence(*this);

   capitalize();
   TranslCommonObjects::GetDiagnostic()->write("\n*After Capitalization*\n");
   TranslCommonObjects::GetDiagnostic()->generateSentence(*this);

   mergeTargetUnits();
	adjustFinalSpaces();
	TranslCommonObjects::GetDiagnostic()->write("\n*After adjustFinalSpaces*\n");
	TranslCommonObjects::GetDiagnostic()->generateSentence(*this);

	TranslCommonObjects::GetDiagnostic()->write("\n*After Pattern Matcher*\n");
	if (patternMatcherEngine->hasRules())
   {
		patternMatch(patternMatcherEngine,false);
		TranslCommonObjects::GetDiagnostic()->generateSentence(*this);
	}
	else
	{
      TranslCommonObjects::GetDiagnostic()->writeAlways("\nPattern matcher has no rules\n");
	}
}
// --------------------------------------------------------------------------
// The Generate stems should be called after termsaerch resets scons.

void LSentence::beginFinalizeTransferForTermSearch()
{
	generate();
	associateTargetToSource();
	updateSourcePrimarySsu();
	generateDictionaryEntries();
	setSurfaceExpressionFromDictionary();
	filterLastNames();
	removeExtraneousTargetUnits();

	if (isTranslated())
   {
		TranslCommonObjects::GetDiagnostic()->writeLine("\n*Scon Information*");
		TranslCommonObjects::GetDiagnostic()->sconCompare(*this);
	}
	TranslCommonObjects::GetDiagnostic()->write("\n*From Dictionary*\n");
	TranslCommonObjects::GetDiagnostic()->generateSentence(*this);
}
//-------------------------------------------------------------------------
void LSentence::Finish_beginFinalizeTransferForTermSearch()
{
   generateStems();
	TranslCommonObjects::GetDiagnostic()->write("\n*After Stemgen*\n");
	TranslCommonObjects::GetDiagnostic()->generateSentence(*this);

	makeAspirations();
	processBlackHoles();
	TranslCommonObjects::GetDiagnostic()->write("\n*After Black Hole*\n");
	TranslCommonObjects::GetDiagnostic()->generateSentence(*this);

	elide();
	TranslCommonObjects::GetDiagnostic()->write("\n*After Finish Rules*\n");
	TranslCommonObjects::GetDiagnostic()->generateSentence(*this);
}
//------------------------------------------------------------------------
void LSentence::finishFinalizeTransferForTermSearch(RuleEngine* patternMatcherEngine)
{
	deleteEmptyUnits(true);
	TranslCommonObjects::GetDiagnostic()->write("\n*After deleteEmptyUnits*\n");
	TranslCommonObjects::GetDiagnostic()->generateSentence(*this);

//   capitalize();
//   TranslCommonObjects::GetDiagnostic()->write("\n*After Capitalization*\n");
//   TranslCommonObjects::GetDiagnostic()->generateSentence(*this);

//   mergeTargetUnits();
	adjustFinalSpaces();
	TranslCommonObjects::GetDiagnostic()->write("\n*After adjustFinalSpaces*\n");
	TranslCommonObjects::GetDiagnostic()->generateSentence(*this);

	TranslCommonObjects::GetDiagnostic()->write("\n*After Pattern Matcher*\n");
	if (patternMatcherEngine->hasRules())
   {
		patternMatch(patternMatcherEngine, false);
		TranslCommonObjects::GetDiagnostic()->generateSentence(*this);
	}
	else
	{
      TranslCommonObjects::GetDiagnostic()->writeAlways("\nPattern matcher has no rules\n");
	}
}


// -------------------------------------------------------------------
void LSentence::completeGenerate()
{
   // This member function is used to address any last minute adjustments to the sentence at the
   // end of the transl generate phase.
   TargetUnitIterator iter;
   LgsString orgWord;

   // Remove any preceeding spaces from the first target unit word of the sentence.
   int offset = 0;
   if ((SentenceUnit::BosIdentifier == (*(v_targetSentenceUnits.begin()))->opadr()) &&
       (v_targetSentenceUnits.size() > 1))
   {
      offset = 1;
   }

   if (!(*(v_targetSentenceUnits.begin() + offset))->surfaceExpression().words().empty())
      const_cast<LWord&>((*((*(v_targetSentenceUnits.begin() + offset))->surfaceExpression().words().begin()))).setPrecedingSpaces(0);


   // If the target language is Spanish and the sentence ends in either a ? or ! then a cooresponding
   // ø or ° must be appended to the beginning of the sentence.
   if (TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::SpanishID)
   {
      TargetUnitIterator last = v_targetSentenceUnits.end() - 1;

      if ((*last)->surfaceExpressionAsString() == "?")
      {
         // Check to make sure the inverted question hasn't already been handled through the use
         // of a functional constant used for specifically this purpose.
         bool hasInvertQuestFuncConst = false;
         for (iter = v_targetSentenceUnits.begin(); iter != v_targetSentenceUnits.end(); iter++)
         {
            if ((*iter)->isFunctionalConstant() &&
                ((*iter)->opadr() == FunctionConstantSentenceUnit::InvertedQuestion))
            {
               hasInvertQuestFuncConst = true;
               break;
            }
         }

         if (!hasInvertQuestFuncConst)
         {
            for (iter = (v_targetSentenceUnits.begin() + 1); iter != v_targetSentenceUnits.end (); iter++)
            {
               if ((*iter)->isValidFirstWord())
               {
                  orgWord = (*(*iter)->surfaceExpression().words().begin());
                  orgWord = "ø" + orgWord;
                  const_cast<LWord&>(*((*iter)->surfaceExpression().words().begin())).setText(orgWord);
                  break;
               }
            }
         }
      }
      else if ((*last)->surfaceExpressionAsString() == "!")
      {
         for (iter = (v_targetSentenceUnits.begin() + 1); iter != v_targetSentenceUnits.end (); iter++)
         {
            if ((*iter)->isValidFirstWord())
            {
               orgWord = (*(*iter)->surfaceExpression().words().begin());
               orgWord = "°" + orgWord;
               const_cast<LWord&>(*((*iter)->surfaceExpression().words().begin())).setText(orgWord);
               break;
            }
         }
      }
   }

   // If the target language is French then we must make sure that any ':' found in the sentence are
   // preceded by a none breaking space and followed by a space (which may be breakable). To accomplish
   // this, a space and ':' are appended to the previous word so that the space is "hard wired" into
   // the output, so to speak. Exception to this rule, no spaces are inserted if a sentence unit preceding
   // the : is punctuation.
   if (TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::FrenchID)
   {
      for (iter =  v_targetSentenceUnits.end() - 1; iter > v_targetSentenceUnits.begin(); --iter)
      {
         if (((*iter)->surfaceExpressionAsString() == ":") &&
             (!((*(iter - 1))->isPunctuation()) || ((*(iter - 1))->surfaceExpressionAsString() == ")") ||
              ((*(iter - 1))->surfaceExpressionAsString() == "]") || ((*(iter - 1))->surfaceExpressionAsString() == "}")))
         {
			   LWordVector& words = (*iter)->surfaceWords();
            LWordIterator word = words.begin();
            (*word).setText(" :");
         }
      }
   }

   // If the target language is anything other than English and the target unit is a proper name,
   // then strip off any apostrophe or apostrophe 's' combinations when the proper name is possessive.
   for (iter = v_targetSentenceUnits.begin(); iter != v_targetSentenceUnits.end (); iter++)
   {
      if ((*iter)->isProperName())
      {
         LgsString words = (*iter)->surfaceExpressionAsString();
         int strLen = words.length();

         // See if the proper name ends with a apostrophe
         if (words[strLen - 1] == '\'')
         {
            words.erase((strLen - 1), 1);
            (*iter)->surfaceExpressionFromString(words);
         }
         else
         {
            if ((strLen > 1) && (words[strLen - 2] == '\'') && (words[strLen - 1] == 's'))
            {
               words.erase((strLen - 2), 2);
               (*iter)->surfaceExpressionFromString(words);
            }
         }
      }
   }

   // Mark unfound words
   markUnfoundWords();

   // See if there is any last minute markup that must be applied to the target units.
   // Test to try inserting superscript and superscript with underlining for certain ordinal
   // abbreviations in French, Spanish and Italian.
//   for (TargetUnitIterator index = v_targetSentenceUnits.begin();
//        index < v_targetSentenceUnits.end(); index++)
//   {
//      (*index)->completeGenerate();
//   }
}


// -------------------------------------------------------------------
// Mark unfound words by adding a question mark in front of ones
// -------------------------------------------------------------------
void LSentence::markUnfoundWords()
{
   // Retrieve unfound words flag from job control info
   JobControlArguments &jobCntrlArgs = LgsDBCommonObjects::GetJobControlArguments();
   bool flagUnfoundWords = (jobCntrlArgs.WordSearchOptions() & 0x0100) == 0x0100;
   if( !flagUnfoundWords )
   {
      return; // flag is not set => nothing to do
   }

   // Go through target units
   TargetUnitIterator iter;
   for( iter = v_targetSentenceUnits.begin(); iter != v_targetSentenceUnits.end(); iter++ )
   {
      // Check is it unfound word
      bool bUnfoundWords = false;
      int posUnfoundWords = 0; // position to insert a question mark
      if( !(*iter)->isMerged() )
      {
         // Not merged unit
         bUnfoundWords = (*iter)->isFlagUnfoundWord();
      }
      else
      {
         // Unit is merged, check left and right units
         TargetSentenceUnit *prev = (*iter)->prevMergedUnit();
         TargetSentenceUnit *next = (*iter)->nextMergedUnit();
         assert((prev != 0 ) && (next != 0)); // merged unit must has both
         if( prev->isFlagUnfoundWord() )
         {
            bUnfoundWords = true; // left unit is unfound
         }
         else if( next->isFlagUnfoundWord() )
         {
            bUnfoundWords = true; // right unit is unfound
            LWordVector::const_iterator wordListBegin = prev->surfaceExpression().words().begin();
            LWordVector::const_iterator wordListEnd = prev->surfaceExpression().words().end();
            for (LWordVector::const_iterator word = wordListBegin; word != wordListEnd; word++)
            {
               posUnfoundWords += (*word).length();
            }
         }
      }
      if( !bUnfoundWords )
      {
         continue; // it is not unfound word => go to next
      }

      // Add a question mark
      LWordVector::const_iterator wordListBegin = (*iter)->surfaceExpression().words().begin();
      LWordVector::const_iterator wordListEnd = (*iter)->surfaceExpression().words().end();
      for( LWordVector::const_iterator word = wordListBegin; word != wordListEnd; word++ )
      {
         if( posUnfoundWords < (*word).length() )
         {
            LgsString newWord = *word;
            newWord.insert( posUnfoundWords, "?" );
            (const_cast<LWord &>(*word)).setText( newWord );
            break;
         }
         else
         {
            posUnfoundWords -= (*word).length();
         }
      }
   }
}


// -------------------------------------------------------------------
// Read in TranMsg sent by Tran process to retrieve target info
// If the job is a term search job, then the message comes from Tran1 and contains
// additional info about all the NPs identified by Tran1.
// Currently receives source objects from the lookup() method
// (they have been persisted out), and SCON information from the
// trans (from a persistence file). This method merges this
// information into an ordered set of Target Sentence Units.
// -------------------------------------------------------------------
void LSentence::generate() throw (SentenceException)
{
	try
   {
		// get the target info from the message (always present in message)
		makeTargetSentenceUnits();
	}
	catch(...)
   {
		throw SentenceException("generate");
	}
}


// -------------------------------------------------------------------
// Get the target info from TranMsg message
// -------------------------------------------------------------------
void LSentence::makeTargetSentenceUnits()
{
   // The dictionary knows how to generate the actual sentence
   // units. However, each new unit must be given a pointer to the
   // sentence itself.

   dictionary().createTargetSentenceUnits(v_sourceSentenceUnits, v_targetSentenceUnits);

   for (TargetUnitVector::iterator i = v_targetSentenceUnits.begin();
        i != v_targetSentenceUnits.end(); ++i)
   {
      (*i)->setSentence (this);
   }
}


// -------------------------------------------------------------------
void LSentence::determineSentenceMarkup()
{
   bool isBold = true;
   bool isItalic = true;
   bool isUnderlined = true;
   bool isSingleQuoted = true;
   bool isDoubleQuoted = true;

   // Determine if entire sentence is bold, italic, underlined, single quoted or double quoted.
   for (LWordIterator w = v_sourceWords.begin(); w != v_sourceWords.end(); w++)
	{
      if (!((*w).isBold()))
         isBold = false;
      if (!((*w).isItalic()))
         isItalic = false;
      if (!((*w).isUnderlined()))
         isUnderlined = false;
      if (!((*w).isSingleQuoted()))
         isSingleQuoted = false;
      if (!((*w).isDoubleQuoted()))
         isDoubleQuoted = false;
   }

   // Set the member variables
   bold_ = isBold;
   italic_ = isItalic;
   underlined_ = isUnderlined;
   singleQuoted_ = isSingleQuoted;
   doubleQuoted_ = isDoubleQuoted;
}


// -------------------------------------------------------------------
void LSentence::determineIfBracketed()
{
   if (isBracketedBy("(", ")") || isBracketedBy("[", "]") || isBracketedBy("{", "}") ||
       isBracketedBy("<", ">"))
   {
      (*(v_sourceWords.begin())).setExcludedFromTranslation();
      (*(v_sourceWords.end() - 1)).setExcludedFromTranslation();
   }
}


// -------------------------------------------------------------------
bool LSentence::isBracketedBy(LgsString beginBracket, LgsString endBracket)
{
   bool valid = false;
   
   // Get the first and last source words of the sentence and see if they
   // are brackets bounding the sentence.
   if ((*(v_sourceWords.begin()) == beginBracket) && (*(v_sourceWords.end() - 1) == endBracket))
   {
      // Make sure sentence is only bracketed or has nested brackets. [words] [words] not allowed.
      valid = true;
      int sentinal = 1;

	   for (LWordIterator w = v_sourceWords.begin() + 1; w != v_sourceWords.end() - 1; w++)
	   {
         if ((*w) == beginBracket)
         {
            sentinal += 1;
         }
         else
         {
            if ((*w) == endBracket)
            {
               sentinal -= 1;
            }
         }
         if (sentinal == 0)
         {
            valid = false;
            break;
         }
      }
   }
   return valid;
}


// -------------------------------------------------------------------
// Recognize proper names from given patterns in the vector of SSU. Mark each element
// of the recognized proper name as such.
// -------------------------------------------------------------------
void LSentence::recognizeProperNames(ProperNameRecognizer& properNameRecognizer)
{
	properNameRecognizer.recognizePatternsIn(&v_sourceSentenceUnits);
}


// -------------------------------------------------------------------
// Decide if any target unit should use the source expression form instead 
// of the transfer form. This is the case for source sentence units that
// have been marked as last names (last names are not translated).
// This method is used during the generation phase; main entry call in generate().
// -------------------------------------------------------------------
void LSentence::filterLastNames()
{
	TargetUnitVector::iterator targetUnit;
	SourceUnitVector::iterator sourceUnit;

	for (targetUnit=v_targetSentenceUnits.begin();targetUnit!=v_targetSentenceUnits.end();targetUnit++)
	{
		findAssociatedSourceSentenceUnit(*targetUnit,sourceUnit);
		if ((*sourceUnit)->keepSourceExpression())
		{
			// this unit has been recognized as a last name that must not be translated
			(*targetUnit)->surfaceExpressionFromString((*sourceUnit)->surfaceExpressionAsString());
		}
	}
}


// --------------------------------------------------------------------------
// Return a pointer in the given SSU vector of the SSU that is associated with 
// the given TSU.
// --------------------------------------------------------------------------
void LSentence::findAssociatedSourceSentenceUnit(TargetSentenceUnit* tsu, SourceUnitIterator& ssui)
{
	try 
	{
		ssui = v_sourceSentenceUnits.begin();		// start with first element in vector
		int ssuElementNumber=1;				// this is element number 1
		// go to the next element in the vector until the correct one is found
		while ((ssui != v_sourceSentenceUnits.end()) && (ssuElementNumber < (*tsu).sourceUnitPosition()))
      {
			ssuElementNumber++;
			ssui++;
		}
		// at the end of the loop, either the element was found (and is pointed to by ssui)
		// or it was not found (and ssui points to end of vector - no element).
		// It is assumed that there is always a corresponding SSU element to any TSU (e.g., the method
		// TSU::sourceUnitPosition() has always a positive or null value).
	}
	catch (...) 
	{
		throw SentenceException("LSentence::findAssociatedSourceSentenceUnit");
	}
}

// -------------------------------------------------------------------
void LSentence::assignOrigSentencePosition()
{
   int sentPosition = 0;
   for (SourceUnitIterator i = v_sourceSentenceUnits.begin(); i != v_sourceSentenceUnits.end(); ++i)
   {
      (*i)->setOriginalSentencePosition(sentPosition);
      sentPosition++;
   }
}

// -------------------------------------------------------------------
LSentenceVector::~LSentenceVector()
{
}


// -------------------------------------------------------------------
void LSentenceVector::removeAndDelete(LSentenceIterator start, LSentenceIterator end)
{
   for (iterator i = start; i != end; i++)
   {
      LSentence* p = *i;
      delete p;
   }
   erase(start, end);
}


// -------------------------------------------------------------------
void LSentenceVector::removeAndDeleteAll()
{
   removeAndDelete(begin(), end());
}

