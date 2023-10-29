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
// DictionaryFoundTerm.cpp: implementation of the DictionaryFoundTerm class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/DictionaryFoundTerm.h>
#include <TermMiner/TermSearchSentenceUnit.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/utility/stringutil.h>
#include <logos_libs/translutility/translcommonobjects.h>

// --------------------------------------------------------------------------
// A term that corresponds to a dictionary match has only one element (possibly several
// words) defined in a single source sentence unit and a single corresponding target
// sentence unit. The given TermSearchSentenceUnit object contains this information.
// --------------------------------------------------------------------------
DictionaryFoundTerm::DictionaryFoundTerm(TermSearchSentenceUnit* sentenceUnit)
{
	if (sentenceUnit != 0)
	{
		reset(sentenceUnit);
		doGeneralSettings();
		searchStatus_ = FOUND_TERM;
		sourcePOS_ = sentenceUnit_->headElement().sourceSentenceUnit()->primarySsu().wordClassCode();
		while (removeFirstElementInTarget());		// modify number of target elements
		setSourceAndTransferGenders();
		targetStatus_ = sentenceUnit->transferStatus();
		companyCode_.set(sentenceUnit_->headElement().sourceSentenceUnit()->companyCode());
		smc_ = sentenceUnit_->headElement().sourceSentenceUnit()->subjectMatterCode();

		// compute the different forms of the source term
		SourceSentenceUnit* sourceUnit = sentenceUnit_->headElement().sourceSentenceUnit();
		if (sourceUnit != 0)
		{
			// - source surface form of the whole term as it is found in the text
			computeSourceSurfaceForm(sourceUnit);

			// - canonical form of the main word of the term
			// the sequence of words containing the main word
			sourceHeadTerm_ = sourceUnit->primarySsu().word();
			// the canonical form is the dictionary match filtered by the following method
			sourceHeadInCanonicalForm_ = filterCanonicalForm(sourceHeadTerm_,sourceUnit);
			// extract the main word of the term from the filtered canonical form
			sourceHeadWord_ = computeSourceHeadWord(sourceUnit,sourceHeadInCanonicalForm_);
			// synthesis from the above
			reconstructHeadTerm();		// set sourceTermInCanonicalForm_
			sourceHeadInCanonicalForm_ = sourceHeadWord_;

			// - canonical form of the whole term
			// the whole source term with its head in canonical form
			sourceCanonicalForm_ = sourceTermInCanonicalForm_;
		}

		// in case of a merged unit, always discard the inserted unit (the one at left)
		targetSurfaceForm_ = sentenceUnit_->headElement().sourceSentenceUnit()->primarySsu().targetWord();

		setCorrectly_ = !hasMissingFields();
	}
	else
	{
		setCorrectly_ = false;
	}
}

// --------------------------------------------------------------------------
DictionaryFoundTerm::~DictionaryFoundTerm()
{
}

// --------------------------------------------------------------------------
// Return the source surface form: sourceSurfaceForm_.
// Update the case.
// Since it is a found entry, it is contained in a single source sentence unit
// (no change in its form but the initial capitalization)
// --------------------------------------------------------------------------
void DictionaryFoundTerm::computeSourceSurfaceForm(SourceSentenceUnit* sourceUnit)
{
	sourceSurfaceForm_ = sourceUnit->surfaceExpressionAsString();
	LgsString dictionaryMatch = sourceUnit->primarySsu().word();

	// if the word is not all upper case and the dictionary match is not initially capped
	// then change to lowercase
	if (!StringUtil::isAllUpperCase(sourceSurfaceForm_) && !StringUtil::beginsUpperCase(dictionaryMatch))
	{
		StringUtil::toLower(sourceSurfaceForm_);
	}
}

// --------------------------------------------------------------------------
void DictionaryFoundTerm::setSourceAndTransferGenders()
{
	// report gender of nouns only
	if (sourcePOS_ == LLanguage::NOUN)
	{
		// do not set the source gender if the source language is English
		if (TranslCommonObjects::GetSourceLanguage()->id() != LLanguage::EnglishID)
		{
			sourceGender_ = sentenceUnit_->headElement().sourceSentenceUnit()->primarySsu().genderCode();
		}

		// set targetGender_
		setTransferGender();
	}
}

