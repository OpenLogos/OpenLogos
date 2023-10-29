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
// DictionaryUnfoundTerm.cpp: implementation of the DictionaryUnfoundTerm class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/DictionaryUnfoundTerm.h>
#include <TermMiner/TermSearchSentenceUnit.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/linguistic/targetsentenceunit.h>
#include <logos_libs/translutility/translcommonobjects.h>

// --------------------------------------------------------------------------
// Create this object and define all necessary information for unfound terms 
// for the term search report.
// --------------------------------------------------------------------------
DictionaryUnfoundTerm::DictionaryUnfoundTerm(TermSearchSentenceUnit *sentenceUnit) {
  if (sentenceUnit != 0)
    {
      reset(sentenceUnit);
      doGeneralSettings();
      searchStatus_ = UNFOUND_TERM;
      sourcePOS_ = sentenceUnit->headElement().sourceSentenceUnit()->primarySsu().wordClassCode();
      setSourceGender();
      setTransferGender();		// set targetGender_

      // source term - surface form of the single unfound word as it is in the text
      // (no change in its form but the initial capitalization)
      sourceSurfaceForm_ = sentenceUnit->headElement().sourceSentenceUnit()->surfaceExpressionAsString();

      // the canonical form is the dictionary match
      sourceHeadTerm_ = sentenceUnit->headElement().sourceSentenceUnit()->primarySsu().word();

      sourceHeadWord_ = computeSourceHeadWord(sentenceUnit->headElement().sourceSentenceUnit(),sourceHeadTerm_);
      // the canonical form is the dictionary match
      sourceHeadInCanonicalForm_ = sourceHeadTerm_;
      sourceTermInCanonicalForm_ = sourceHeadInCanonicalForm_;
      // the canonical form of the whole term is the canonical form of the head since we have a single element
      sourceCanonicalForm_ = sourceTermInCanonicalForm_;

      // settings pertaining to German source
      if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
        {
          // use info of the word or the compound head
          sourceHeadTerm_ = sourceCanonicalForm_;		// since there is a single unit, these 2 fields are the same
        }

      // use dictionary info for the transfer
      targetSurfaceForm_ = sentenceUnit->headElement().targetSentenceUnit()->surfaceExpressionAsString();

      setCorrectly_ = !hasMissingFields();
    }
  else
    {
      setCorrectly_ = false;
    }
}

// --------------------------------------------------------------------------
DictionaryUnfoundTerm::~DictionaryUnfoundTerm()
{
}

// --------------------------------------------------------------------------
// do not set the source gender if the source language is English
// For German source, if the head element is an unfound noun, then set gender to default gender
// (masculine), else get the gender from the dictionary match.
// --------------------------------------------------------------------------
void DictionaryUnfoundTerm::setSourceGender()
{
	if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
	{
		if (sentenceUnit_->headElement().sourceSentenceUnit()->primarySsu().wordClassCode() == LLanguage::NOUN)
		{
			sourceGender_ = 1;
		}
		else
		{
			sourceGender_ = sentenceUnit_->headElement().sourceSentenceUnit()->primarySsu().genderCode();
		}
	}
}

