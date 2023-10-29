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
// Tran1NounPhraseTerm.cpp: implementation of the Tran1NounPhraseTerm class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/Tran1NounPhraseTerm.h>
#include <TermMiner/TermSearchSentenceUnit.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/linguistic/targetsentenceunit.h>

// --------------------------------------------------------------------------
// Default constructor need only for subclasses - should not be used.
// --------------------------------------------------------------------------
Tran1NounPhraseTerm::Tran1NounPhraseTerm() :
isSingleElement_(false)
{
	reset(0);
}

// --------------------------------------------------------------------------
Tran1NounPhraseTerm::~Tran1NounPhraseTerm()
{
}

// --------------------------------------------------------------------------
void Tran1NounPhraseTerm::resetNP(TermSearchSentenceUnit* sentenceUnit)
{
	isSingleElement_ = false;
	reset(sentenceUnit);
}

// --------------------------------------------------------------------------
// Return whether this element is a valid noun phrase.
// Only if there are more than 1 elements in the vector of elements in source sequence order.
// --------------------------------------------------------------------------
bool Tran1NounPhraseTerm::isNounPhrase()
{
	bool result = false;

	if (sentenceUnit_ != 0)
	{
		result = sentenceUnit_->elementsInSourceSequenceOrder().size() > 1;
	}

	return result;
}

// --------------------------------------------------------------------------
// This is not a noun phrase anymore. It must be changed to a found or unfound term 
// by the caller
// --------------------------------------------------------------------------
void Tran1NounPhraseTerm::setAsNotANounPhrase()
{
	isSingleElement_ = true;
	setCorrectly_ = false;
}

// --------------------------------------------------------------------------
void Tran1NounPhraseTerm::doGeneralSettings()
{
	if (sentenceUnit_ != 0)
	{
		Term::doGeneralSettings();
		searchStatus_ = UNFOUND_TERM;	// all Tran1 NPs are unfound terms
		sourcePOS_ = LLanguage::NOUN;	// all unfound noun phrase terms are nouns by default
		computeTargetCanonicalForm();		// set targetSurfaceForm_
		sourceHeadTerm_ = sentenceUnit_->headElement().sourceSentenceUnit()->primarySsu().word();		// head is the dictionary match
		sourceHeadWord_ = computeSourceHeadWord(sentenceUnit_->headElement().sourceSentenceUnit(),sourceHeadTerm_);							// set sourceHeadWord_ (the main word)
		sourceHeadInCanonicalForm_ = filterCanonicalForm(sourceHeadWord_,sentenceUnit_->headElement().sourceSentenceUnit());
		reconstructHeadTerm();		// update sourceTermInCanonicalForm_
	}
}

// --------------------------------------------------------------------------
bool Tran1NounPhraseTerm::isSingleElement()
{
	return isSingleElement_;
}

// --------------------------------------------------------------------------
// Decide whether the head of the candidate NP is really a noun.
// Compute the word class code of the head. Word class can be from the 
// chosen semanto-syntactic unit (primarySSU) or from the scon1 after tran1.
// Sometimes RES/Tran changes the WC of a unit.
// See incident 3177 - we want to keep the process nouns (-ing) that are WC4 in 
// the dictionary but changed by RES/Tran to WC because the head noun of the NP.
// In these cases, the word class info should be read from the scon1 after Tran1
// and not from the primarySSU (dictionary info). These nouns are identified by 
// the superset value: 15.
// --------------------------------------------------------------------------
bool Tran1NounPhraseTerm::headIsNoun()
{
	int headWordClassCode = 0;

	if (sentenceUnit_ != 0)
	{
		// get info about the word class code of the head element
		SourceSentenceUnit* headSourceUnit = sentenceUnit_->headElement().sourceSentenceUnit();
		TargetSentenceUnit* headTargetUnit = sentenceUnit_->headElement().targetSentenceUnit();
		int headWordClassCodeAtPrimarySSU = 0;
		int headWordClassCodeAfterTran1 = 0;
		// get the word class code info from the dictionary match
		if (headSourceUnit != 0)
		{
			headWordClassCodeAtPrimarySSU = headSourceUnit->primarySsu().wordClassCode();
		}
		// get the word class from the scon table (scon 1) at the end of Tran1
		if (headTargetUnit != 0)
		{
			const SconTable sconTable = headTargetUnit->getSconTable();
			headWordClassCodeAfterTran1 = sconTable.getScon(SconTable::SOURCE_WORD_CLASS_INDEX);
		}

		// determine the word class code of the head element
		if (   headSourceUnit->superSetID() == 15 
			&& headWordClassCodeAtPrimarySSU == LLanguage::ADJECTIVE
			&& headWordClassCodeAfterTran1 == LLanguage::NOUN
			)
		{
			// this is a processed noun (-ing changed from WC4 to WC1 by RES/Tran)
			// this should be a N then
			headWordClassCode = LLanguage::NOUN;
		}
		else
		{
			// this is not a processed noun
			// the word class code is the one from the chosen dictionary match
			headWordClassCode = headWordClassCodeAtPrimarySSU;
		}
	}

	return headWordClassCode == LLanguage::NOUN;
}

