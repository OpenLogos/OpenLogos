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
// File - LLanguage.cpp
//
// Class - LLanguage
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/linguistic/targetsentenceunit.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/elision/variable.h>
#include <logos_libs/startrules/factory.h>
#include <logos_libs/startrules/variable.h>
#include <logos_libs/linguistic/functionconstantsentenceunit.h>
#include <logos_libs/translutility/translcommonobjects.h>
#include <logos_libs/linguistic/noninflectingwordcache.h>

LInflection LLanguage::v_nullInflection;

LLanguage::LLanguage(ID id, const LgsString& description)
          :id_(id),
           description_(description),
           p_inflections(0),
//           p_derivedForms(0),
           p_derivedFormsMap(0),
           p_context(0),
           p_elisionEngine(0),
           p_startRulesEngine(0)
{
}

LLanguage::~LLanguage()
{
   if (p_elisionEngine)
   {
      delete p_elisionEngine;
      p_elisionEngine = 0;
   }
   if (p_startRulesEngine)
   {
      delete p_startRulesEngine;
      p_startRulesEngine = 0;
   }
/*
   if (p_derivedForms) {
      delete p_derivedForms;
      p_derivedForms = 0;
   }
*/

   if (p_derivedFormsMap) {
      delete p_derivedFormsMap;
      p_derivedFormsMap = 0;
   }
   if(p_inflections) {
	   delete p_inflections;
	   p_inflections = 0;
   }
}

LgsString LLanguage::applyCombiningFormCode(int code, LgsString& target)
{
   return target;
}

const LgsString* LLanguage::combiningForm(int)
{
   return 0;
}

LRootVector* LLanguage::createRootsFromWord(const LWord& aWord) const
{
   // Creates a vector of roots from a given word. Of course, the roots must be valid according
   // to the rules of the language. The rules of the language are specified in the collection of
   // inflections that the language object contains. Therefore, this method iterates through the
   // language object's inflections and attempts to create a word by removing the inflection from
   // the word.
   LRootVector* pRoots = new LRootVector;

   // Check if word is in NonInflectingWordCache
   if( NonInflectingWordCache::GetObject()->inCache( aWord ) )
   {
      return pRoots;
   }

   for (LInflectionVector::iterator i = p_inflections->begin(); i != p_inflections->end(); i++)
   {
      LRoot* p = (*i).createRoot(aWord);
      if (p)
      {
         if (p->size() >= 2)
         {
            pRoots->push_back(*p);
         }
         delete p;
      }
   }
   return pRoots;
}

LWordVector* LLanguage::createWordsFromRoot(const LRoot& aRoot) const
{
   // Creates a vector of words from a given root by adding every
   // possible inflection that is valid within the language object.
   LWordVector* pWords = new LWordVector;

   for (LInflectionVector::iterator i = p_inflections->begin(); i != p_inflections->end(); i++)
   {
      const LWord* p = (*i).createWord(aRoot);
      if (p)
      {
         pWords->push_back(*p);
      }
   }
   return pWords;
}

void LLanguage::findAspirePoints(TargetSentenceUnit* prev, TargetSentenceUnit* curr, LgsVector(int)* positions) const
{
   // The default is to return position 0 if the word is aspired and nothing otherwise.
   positions->erase(positions->begin(), positions->end());

   if (curr->isAspired())
   {
      positions->push_back(0);
   }
}

const DDerivedForm* LLanguage::findDerivedForm(int patNumber, int stemNumber,
                                               const LInflection* inflection) const
{
/* iterating through vector is way too slow for German source. */
/*
   if (inflection)
   {
      for (DDerivedFormVector::iterator i = p_derivedForms->begin(); i != p_derivedForms->end(); i++)
      {
         if ((i->PatNumber() == patNumber) && (i->StemNumber() == stemNumber) &&
            (*inflection == i->Ending()))
         {
//printf("FOUND pn=%d, sn=%d, end=\"%s\"\n", 
//	   patNumber, stemNumber, inflection->c_str());
//fflush(stdout);
            return i;
         }
      }
   }
//printf("NOTFOUND pn=%d, sn=%d, end=\"%s\"\n", 
//	   patNumber, stemNumber, inflection->c_str());
//fflush(stdout);
   return 0;
*/

	if(inflection) {
		DerivedFormKey key(patNumber, stemNumber, inflection->c_str());
		DDerivedFormMap::iterator i = p_derivedFormsMap->find(key);
		if(i!=p_derivedFormsMap->end()) {

//printf("FOUND pn=%d, sn=%d, end=\"%s\"\n", 
//	   patNumber, stemNumber, inflection->c_str());
//fflush(stdout);

			return & i->second;
		}
	}

//printf("NOTFOUND pn=%d, sn=%d, end=\"%s\"\n", 
//	   patNumber, stemNumber, inflection->c_str());
//fflush(stdout);

	return 0;

}

void LLanguage::elide(TargetUnitVector& units) const
{
   if (p_elisionEngine == 0)
      return;

   PhraseManager phraseManager;
   EL_Variable variable(phraseManager);
   int count = 0;

   // fill variable with target sentence units
   TargetUnitIterator i;
   for (i = units.begin(); i != units.end(); ++i)
   {
      int trailingSpaces = (*i)->trailingSpaces();

      // If a functional constant starting with @ is not separated by spaces from the preceding
      // and following words the finish rules may fail to fire.
      if ((*i)->isFunctionalConstant() && (((*i)->surfaceExpressionAsString())[0] == '@'))
      {
         trailingSpaces = 1;
      }
      else
      {
         if ((i != (units.end() - 1)) &&
             !((*i)->isFunctionalConstant() && (*i)->surfaceExpressionAsString().empty()))
         {
            bool checkSpaces = true;
            int iter = 1;

            while (((i + iter) != units.end()) && (*(i + iter))->isFunctionalConstant() &&
                   (((*(i + iter))->surfaceExpressionAsString())[0] == '@'))
            {
               iter++;
            }

            if ((i + iter) == units.end())
            {
               checkSpaces = false;
            }
            TargetSentenceUnit* nextUnit = *(i + iter);

            if (checkSpaces)
            {
               if (nextUnit->allowsPrecedingSpaces() && ((*i)->trailingSpaces() == 0) &&
                   (*i)->allowsTrailingSpaces() && !(*i)->hadSpaceClosed())
               {
                  if ((*i)->sourceSentenceUnit() && nextUnit->sourceSentenceUnit())
                  {
                     if ((((*i)->sourceSentenceUnit()->originalSentencePosition() + 1) !=
                          nextUnit->sourceSentenceUnit()->originalSentencePosition()) ||
                         nextUnit->isConstantSentenceUnit())
                     {
                        LgsString word = (*i)->surfaceExpressionAsString();
                        if (word.length() && (word[word.length() - 1] != '-'))
                        {
                           (*i)->setTrailingSpaces(1);
                           trailingSpaces = 1;
                        }
                     }
                  }
                  else
                  {
                     if ((((*i)->opadr() + 1) != nextUnit->opadr()) || nextUnit->isConstantSentenceUnit())
                     {
                        (*i)->setTrailingSpaces(1);
                        trailingSpaces = 1;
                     }
                  }
               }
               else if (((*i)->trailingSpaces() == 1) &&
                        (((*(i + 1))->isPunctuationConstant() && !(*(i + 1))->allowsPrecedingSpaces()) ||
                         ((*i)->isPunctuationConstant() && !(*i)->allowsTrailingSpaces())))
               {
                  (*i)->setTrailingSpaces(0);
                  trailingSpaces = 0;
               }
            }
         }
      }
      phraseManager.appendPhrase((*i)->surfaceExpressionAsString(), count++, trailingSpaces);
   }

   // apply the rule engine to the variable
   p_elisionEngine->evaluate(variable);

   phraseManager.moveLeadingSpaces();

   // save target sentence units back to the sentence
   int current = 0;
   for (i = units.begin(); i != units.end(); ++i, ++current)
   {
      if (phraseManager.phraseExists(current))
      {
         if (phraseManager.phraseChanged(current))
         {
            int blanks;
            LgsString text = phraseManager.getPhrase(current, &blanks);

            // If this is a functional constant which initially caps the next unit or lower cases
            // the next unit then the results are reversed and we have to handle the processing of
            // the units in the reverse order.
            if ((*i)->isFunctionalConstant() &&
                (((*i)->opadr() == FunctionConstantSentenceUnit::InitialCapFlag) ||
                 ((*i)->opadr() == FunctionConstantSentenceUnit::InitialCapFlag2) ||
                 ((*i)->opadr() == FunctionConstantSentenceUnit::LowercaseRight)))
            {
               (*i)->surfaceExpressionFromString("");
               (*i)->setTrailingSpaces(0);
               i++;
               current++;
               if ((phraseManager.phraseExists(current)) && (phraseManager.phraseChanged(current)))
               {
                  int additionalBlanks;
                  LgsString additionalText = phraseManager.getPhrase(current, &additionalBlanks);
                  if (additionalText.length() > 0)
                  {
                     if (blanks > 0)
                     {
                        text += " " + additionalText;
                     }
                     else
                     {
                        text += additionalText;
                     }
                     blanks = additionalBlanks;
                  }
               }
               (*i)->surfaceExpressionFromString(text);

               if (((*i)->trailingSpaces() > 0) && (blanks == 0))
               {
                  (*i)->setHadSpaceClosed(true);
               }
               (*i)->setTrailingSpaces(blanks);
            }
            else
            {
               (*i)->surfaceExpressionFromString(text);

               if (((*i)->trailingSpaces() > 0) && (blanks == 0) && (text != ""))
               {
                  (*i)->setHadSpaceClosed(true);
               }
               (*i)->setTrailingSpaces(blanks);
            }
         }
      }
      else
      {
         (*i)->surfaceExpressionFromString(LgsString());
         (*i)->setTrailingSpaces(0);
      }
   }
}

void LLanguage::loadStartRulesEngine(const LgsString& startRulesFileName)
{
   // create start rule engine
   if (startRulesFileName.length() == 0)
      throw(LgsString("Missing start rules file"));
   ifstream input(startRulesFileName.c_str());
   if (!input.good())
      throw(LgsString("Error opening start rules file"));
   ST_Factory factory("start-rules", &input,
   &TranslCommonObjects::GetSourceLocale(), &TranslCommonObjects::GetTargetLocale());
   p_startRulesEngine = (RE_Engine<ST_Variable>*)factory.createRuleEngine();
   input.close();
}

void LLanguage::doStartRules(LWordVector& words) const
{
   if (words.size() == 0)
      return;

   // create a start rules variable, andevaluate the engine on it
   ST_Variable variable(words);
   p_startRulesEngine->evaluate(variable);
}

void LLanguage::logStartRuleStatistics(void) const
{
   LgsString scratchDir = LgsDBCommonObjects::GetServerProperties().ScratchFileDirectory();
	char logFileName[100];
	sprintf(logFileName, "%s/%d.startrules", scratchDir.c_str(),
                                            LgsDBCommonObjects::GetJobControlArguments().JobID());
	ofstream logStream(logFileName);
 	p_startRulesEngine->logStatistics(logStream);
	logStream.flush();
	logStream.close();
}

void LLanguage::logElisionStatistics(void) const
{
   LgsString scratchDir = LgsDBCommonObjects::GetServerProperties().ScratchFileDirectory();
   char logFileName[100];
   sprintf(logFileName, "%s/%d.elision", scratchDir.c_str(),
                                         LgsDBCommonObjects::GetJobControlArguments().JobID());
   ofstream logStream(logFileName);
   p_elisionEngine->logStatistics(logStream);
   logStream.flush();
   logStream.close();
}

bool LLanguage::isRomanceLanguage() const
{
   if ((id_ == LLanguage::FrenchID) || (id_ == LLanguage::SpanishID) || (id_ == LLanguage::ItalianID)
	   || (id_ == LLanguage::PortugueseID))
      return true;
   else
      return false;
}


//-------------------------------------------------------------------
// return a two-letter description of the language
//-------------------------------------------------------------------
LgsString LLanguage::shortDescription() const {
	LgsString description="--";
	switch(id_) {
	case GermanID		: description="DE"; break;
	case EnglishID		: description="EN"; break;
	case FrenchID		: description="FR"; break;
	case SpanishID		: description="ES"; break;
	case ItalianID		: description="IT"; break;
	case PortugueseID	: description="PT"; break;
	default				: description="--";
	}
	return description;
}
