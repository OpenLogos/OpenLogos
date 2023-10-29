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
// Tran1GermanCompoundTerm.cpp: implementation of the Tran1GermanCompoundTerm class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/Tran1GermanCompoundTerm.h>
#include <TermMiner/TermSearchSentenceUnit.h>
#include <TermMiner/GermanSourceCompoundWord.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>

// --------------------------------------------------------------------------
Tran1GermanCompoundTerm::Tran1GermanCompoundTerm(TermSearchSentenceUnit* sentenceUnit) :
sourceCompoundWord_(0)
{
	reset(sentenceUnit);
	cleanBeginningOfTerm();
	doGeneralSettings();
	searchStatus_ = UNFOUND_TERM;
	sourcePOS_ = sentenceUnit_->headElement().sourceSentenceUnit()->primarySsu().wordClassCode();
	setSourceGender();			// set sourceGender_
	setTransferGender();		// set targetGender_

	// define head of compound (if any)
	sourceHeadTerm_ = sentenceUnit_->headElement().sourceSentenceUnit()->primarySsu().word();
	sourceHeadWord_ = sourceHeadTerm_;

	defineSourceCompoundWord();		// set sourceCompoundWord_
	computeSourceCanonicalForms();
	computeSourceSurfaceForm_GermanSource();		// set sourceSurfaceForm_
	computeTargetCanonicalForm();					// set targetSurfaceForm_
												
	setCorrectly_ = !hasMissingFields();
}

// --------------------------------------------------------------------------
Tran1GermanCompoundTerm::~Tran1GermanCompoundTerm()
{
	delete sourceCompoundWord_;
}

// --------------------------------------------------------------------------
// Load all the elements into a compound object that will compute the canonical form.
// --------------------------------------------------------------------------
void Tran1GermanCompoundTerm::defineSourceCompoundWord()
{
	if (!sentenceUnit_->elementsInSourceSequenceOrder().empty())
	{
		sourceCompoundWord_ = new GermanSourceCompoundWord();

		for (TermElementIterator element=sentenceUnit_->elementsInSourceSequenceOrder().begin(); element!=sentenceUnit_->elementsInSourceSequenceOrder().end(); element++)
		{
			sourceCompoundWord_->push_back(element->sourceSentenceUnit());
		}
	}
}

// --------------------------------------------------------------------------
// Update sourceCanonicalForm_.
// Use sourceHeadInCanonicalForm_ for the head of the compound.
// Agree any adjective to the noun head if any.
// --------------------------------------------------------------------------
void Tran1GermanCompoundTerm::computeSourceCanonicalForms()
{
	// the canonical form is the dictionary match filtered by the following method
	sourceHeadInCanonicalForm_ = filterCanonicalForm(sourceHeadWord_,sentenceUnit_->headElement().sourceSentenceUnit());
	sourceTermInCanonicalForm_ = sourceHeadInCanonicalForm_;

	// generate the canonical form of the compound word
	if (sourceCompoundWord_ != 0)
	{
		sourceCanonicalForm_ = sourceCompoundWord_->canonicalForm(sourceHeadInCanonicalForm_,0);
	}
	else
	{
		sourceCanonicalForm_ = "";
	}
}

// --------------------------------------------------------------------------
// gender of compound is gender of the head
// If the head element is an unfound noun, then set gender to default gender
// (masculine), else get the gender from the dictionary match.
// --------------------------------------------------------------------------
void Tran1GermanCompoundTerm::setSourceGender()
{
	if (sentenceUnit_ != 0)
	{
		if (   sentenceUnit_->headElement().sourceSentenceUnit()->isUnfoundWord()
			&& sentenceUnit_->headElement().sourceSentenceUnit()->primarySsu().wordClassCode() == LLanguage::NOUN
		   )
		{
			sourceGender_ = 1;
		}
		else
		{
			sourceGender_ = sentenceUnit_->headElement().sourceSentenceUnit()->primarySsu().genderCode();
		}
	}
}

