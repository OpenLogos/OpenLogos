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
// GermanSourceCompoundWord.cpp: implementation of the GermanSourceCompoundWord class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/GermanSourceCompoundWord.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/utility/stringutil.h>

// --------------------------------------------------------------------------
GermanSourceCompoundWord::GermanSourceCompoundWord() :
headElement_(0)
{
}

// --------------------------------------------------------------------------
GermanSourceCompoundWord::~GermanSourceCompoundWord()
{
}

// --------------------------------------------------------------------------
// Add a component of the component to the definition of the entire compound.
// --------------------------------------------------------------------------
void GermanSourceCompoundWord::push_back(SourceSentenceUnit* sourceUnit)
{
	elements_.push_back(sourceUnit);

	// check if this unit is the head element of the compound word
	if (sourceUnit->headUnit())
	{
		// found the head of the compound
		headElement_ = sourceUnit;
	}
}

// --------------------------------------------------------------------------
// Return the canonical form of the compound word.
// There are 2 major types of compounds depending where they are found. Compounds can be by themselves
// and thus are a Tran1GermanCompound object or can be part of a tran1 noun phrase, and thus are part
// of a Tran1NounPhrase object. In the latter, we need to know the gender of the head noun of the noun
// phrase for agreement of any adjective to this noun. The gender is null if the compound has not been
// found in a NP.
// --------------------------------------------------------------------------
LgsString GermanSourceCompoundWord::canonicalForm(LgsString sourceHeadInCanonicalForm, int genderNounPhraseHeadNoun)
{
	LgsString canonicalForm = "";

	if (!elements_.empty())
	{
		if (sourceHeadInCanonicalForm == "" || headElement_ == 0)
		{
			// the head has no canonical form defined, so use the surface form
			canonicalForm = (*elements_.begin())->surfaceExpressionAsString();
		}
		else
		{
			// replace the head by its canonical form - drop the ending
			LgsString compoundHead = headElement_->primarySsu().word();
			StringUtil::toLower(compoundHead);
			LgsString sourceSurfaceForm = (*elements_.begin())->surfaceExpressionAsString();
			LgsString surfaceFormToLower = sourceSurfaceForm;
			StringUtil::toLower(surfaceFormToLower);
			StringUtil::toLower(sourceHeadInCanonicalForm);

			// extract the LgsString before the head of the compound
			LgsString sequenceBefore = "";
			char ctemp= 0;
			bool bBeginUpperCase = false;
			int positionLastCharacterOfSequenceBeforeCompoundHead = surfaceFormToLower.find(compoundHead);
			if (positionLastCharacterOfSequenceBeforeCompoundHead < LgsString::npos)
			{
				sequenceBefore = sourceSurfaceForm.substr(0,positionLastCharacterOfSequenceBeforeCompoundHead);
				ctemp = sourceSurfaceForm[positionLastCharacterOfSequenceBeforeCompoundHead];
				bBeginUpperCase = CharUtil::isUpper(ctemp);
			}

			// if this compound has been found in a noun phrase and the head of the compound is an adjective
			// then make agreement of this ADJ to the gender of head noun of the NP
			if (genderNounPhraseHeadNoun > 0 && headElement_->primarySsu().wordClassCode() == LLanguage::ADJECTIVE)
			{
				sourceHeadInCanonicalForm = agreementADJtoN_GermanSource(sourceHeadInCanonicalForm,genderNounPhraseHeadNoun);
			}

			// reconstruct the compound with its head in canonical form
			// Check the Head is initially capped in source surface form
			if (bBeginUpperCase)
			{
				(*sourceHeadInCanonicalForm.begin()) =  CharUtil::upper((*sourceHeadInCanonicalForm.begin()));
			}
            
			canonicalForm = sequenceBefore + sourceHeadInCanonicalForm;

			// check the case of the compound: if the head is a N then initially cap the compound
			//                                 if the head is an ADJ then initially lowercase the compound
			if (headElement_->primarySsu().wordClassCode() == LLanguage::NOUN)
			{
				StringUtil::capitalize(canonicalForm);
			}
			else if (headElement_->primarySsu().wordClassCode() == LLanguage::ADJECTIVE)
			{
				StringUtil::toLowerFirstChar(canonicalForm);
			}
		}
	}

	return canonicalForm;
}

// --------------------------------------------------------------------------
// Compute from the first element of the compound word whether there are preceding
// spaces to the compound.
// --------------------------------------------------------------------------
LgsString GermanSourceCompoundWord::precedingSpaces()
{
	LgsString precedingSpaces = "";

	if (!elements_.empty())
	{
		int nSpace = (*elements_.begin())->precedingSpaces();
		LgsString strSpace(nSpace, ' ');
		precedingSpaces = strSpace;
	}

	return precedingSpaces;
}

// --------------------------------------------------------------------------
// Return the form of the given adjective that agrees with the noun of the phrase.
// --------------------------------------------------------------------------
LgsString GermanSourceCompoundWord::agreementADJtoN_GermanSource(LgsString adjectiveForm, int genderNounPhraseHeadNoun)
{
	LgsString adjectiveFormInAgreementWithNoun = "";

	// if the adjective ends with the character 'e'
	if (adjectiveForm.substr(adjectiveForm.length()-1,1) == "e")
	{
		// if the gender of the noun is masculine
		if (genderNounPhraseHeadNoun == 1)
		{
			// then append 'r' to the canonical form of the adjective
			adjectiveFormInAgreementWithNoun = adjectiveForm + "r";
		}
		// else if the gender of the noun is neuter
		else if (genderNounPhraseHeadNoun == 3)
		{
			// then append 's' to the canonical form of the adjective
			adjectiveFormInAgreementWithNoun = adjectiveForm + "s";
		}
	}
	else
	{
		// if the gender of the noun is masculine
		if (genderNounPhraseHeadNoun == 1)
		{
			// then append 'er' to the canonical form of the adjective
			adjectiveFormInAgreementWithNoun = adjectiveForm + "er";
		}
		// else if the gender of the noun is neuter
		else if (genderNounPhraseHeadNoun == 3)
		{
			// then append 'es' to the canonical form of the adjective
			adjectiveFormInAgreementWithNoun = adjectiveForm + "es";
		}
		// else the gender of the noun is feminine
		else
		{
			// then append 'e' to the canonical form of the adjective
			adjectiveFormInAgreementWithNoun = adjectiveForm + "e";
		}
	}

	return adjectiveFormInAgreementWithNoun;
}

