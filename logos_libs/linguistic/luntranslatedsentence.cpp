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
// File - LUntranslatedSentence.cpp
//
// Class - LUntranslatedSentence (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/luntranslatedsentence.h>
#include <logos_libs/linguistic/ldictionary.h>
#include <logos_libs/linguistic/untranslatedsentenceunit.h>
#include <logos_libs/linguistic/linkexception.h>
#include <logos_libs/linguistic/bossentenceunit.h>

//-------------------------------------------------------------------
LUntranslatedSentence::LUntranslatedSentence(LSentence& rhs)
                      :LSentence(rhs)
{
   // Copy constructor -- important for passing by value.
   // This is here to satisfy STL. If sentences are to be passed by
   // value work needs to be done here.
   v_sourceSentenceUnits = rhs.sourceSentenceUnits();
	v_sourceWords = rhs.sourceWords();
   v_origSourceWords = rhs.origSourceWords();
}
//-------------------------------------------------------------------
LUntranslatedSentence::~LUntranslatedSentence()
{
}
//-------------------------------------------------------------------
int LUntranslatedSentence::translatedWordCount() const
{
   // Following the Composite design pattern, the word count for
   // the sentence is derived from the word count of all of the
   // sentence units.
   return 0;
}
//-------------------------------------------------------------------
bool LUntranslatedSentence::isTranslated() const
{
   return false;
}
//-------------------------------------------------------------------
void LUntranslatedSentence::generateStems()
{
}
//-------------------------------------------------------------------
void LUntranslatedSentence::makeAspirations ()
{
}
//-------------------------------------------------------------------
void LUntranslatedSentence::makeTargetSentenceUnits()
{
   // The dictionary knows how to generate the actual sentence
   // units. However, each new unit must be given a pointer to the
   // sentence itself.
   targetSentenceUnits().push_back(new BosSentenceUnit());
   TargetSentenceUnit* p = targetSentenceUnits().back();
   p->setSourceUnitPosition(-1);
   p->setOpadr(-1);
   p->setSentence(this);
   p->setDictionary(dictionary());
   p->setWordClassCode(LLanguage::PUNCTUATION);

   if (v_sourceSentenceUnits.size() > 2)
   {
      for (SourceUnitVector::iterator i =  v_sourceSentenceUnits.begin() + 1;
           i != v_sourceSentenceUnits.end(); ++i)
      {
         targetSentenceUnits().push_back(new UntranslatedSentenceUnit());
         p = targetSentenceUnits().back();
         p->setSourceUnitPosition((*i)->position());
         p->setOpadr((*i)->position());
         p->setSentence(this);
         p->setDictionary(dictionary());
         p->setWordClassCode((*i)->wordClassCode());
      }
   }
}
//---------------------------------------------------------------------
void LUntranslatedSentence::removeExtraneousTargetUnits()
{
}
//---------------------------------------------------------------------
void LUntranslatedSentence::inhibitCapConstant()
{
}
//---------------------------------------------------------------------
void LUntranslatedSentence::processBlackHoles()
{
}
//-------------------------------------------------------------------
void LUntranslatedSentence::capitalize()
{
}
//-------------------------------------------------------------------
void LUntranslatedSentence::elide()
{
}
