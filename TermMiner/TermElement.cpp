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
// TermElement.cpp: implementation of the TermElement class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/TermElement.h>
#include <logos_libs/linguistic/lsentence.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/linguistic/targetsentenceunit.h>

// --------------------------------------------------------------------------
TermElement::TermElement()
{
	resetAll(0,0,0,false);
}

// --------------------------------------------------------------------------
// Constructor used to find elements in target sequence order - more concerned with the transfer.
// --------------------------------------------------------------------------
TermElement::TermElement(int scon, bool thereAreMultipleCopiesOfTheScon, int opadr, LSentence* sentence, bool isHeadElement, TargetUnitVector& alreadySelectedTargetSentenceUnits)
{
	resetAll(scon,opadr,sentence,isHeadElement);
	findCorrespondingSentenceUnits(alreadySelectedTargetSentenceUnits,thereAreMultipleCopiesOfTheScon);
}

// --------------------------------------------------------------------------
// Constructor used to find elements in source sequence order - it requires less info because
// the transfer contains inserted units, etc. that the source does not.
// --------------------------------------------------------------------------
TermElement::TermElement(int scon, LSentence* sentence, bool isHeadElement)
{
	reset(scon,sentence,isHeadElement);
}
// --------------------------------------------------------------------------
void TermElement::reset(int scon, LSentence* sentence, bool isHeadElement)
{
	resetAll(scon,0,sentence,isHeadElement);
	TargetUnitVector alreadySelectedTargetSentenceUnits;
	findCorrespondingSentenceUnits(alreadySelectedTargetSentenceUnits,false);
}

// --------------------------------------------------------------------------
void TermElement::resetAll(int scon, int opadr, LSentence* sentence, bool isHeadElement)
{
	sourceSentenceUnit_ = 0;
	targetSentenceUnit_ = 0;
	headElement_ = isHeadElement;
	scon_ = scon;
	opadr_ = opadr;
	sentence_ = sentence;
	setCorrectly_ = false;
}

// --------------------------------------------------------------------------
void TermElement::findCorrespondingSentenceUnits(TargetUnitVector& alreadySelectedTargetSentenceUnits, bool thereAreMultipleCopiesOfTheScon)
{
	// set targetSentenceUnit_
	setCorrectly_ = false;

	// first, try a match based on the opadr value (the most specific) only if there are multiple
	// copies of the scon value in the SCONPO (see incident 3155)
	if (thereAreMultipleCopiesOfTheScon && opadr_ != 0)
	{
		setCorrectly_ = findTargetSentenceUnitAssociatedToOpadr(alreadySelectedTargetSentenceUnits);
	}

	// if no success with prior match, or if the opadr is not known, then try a match based on the
	// scon value (more general).
	if (!setCorrectly_)
	{
		setCorrectly_ = findTargetSentenceUnitAssociatedToScon(alreadySelectedTargetSentenceUnits);
	}

	// if a TSU has been found from any of the above matching approaches, then try to find its
	// corresponding SSU
	if (setCorrectly_)
	{
		// set sourceSentenceUnit_
		findSourceSentenceUnitAssociatedToTargetSentenceUnit();
	}
}

// --------------------------------------------------------------------------
TermElement::~TermElement()
{
}

// --------------------------------------------------------------------------
bool TermElement::isSetCorrectly() 
{
	bool result = false;

	if (headElement_)
	{
		if (   sourceSentenceUnit_ != 0 
			&& targetSentenceUnit_ != 0
			&& !targetSentenceUnit_->isFunctionalConstant())
		{
			result = true;
		}
	}
	else
	{
		result = setCorrectly_;
	}

	return result;
}

// --------------------------------------------------------------------------
// Try to find the associated target sentence unit without knowing the exact opadr value.
// Ie, look in the vector of scon pointers of each TSU in turn to match the desired scon value.
// This is a general approach since several TSUs may contain the same vector of scon values (in
// case of units merged by Generate).
// Set to 0 if not found.
// --------------------------------------------------------------------------
bool TermElement::findTargetSentenceUnitAssociatedToScon(TargetUnitVector& alreadySelectedTargetSentenceUnits)
{
	bool foundTSU = false;
	TargetUnitIterator targetUnit = sentence_->targetSentenceUnits().begin();
	while (!foundTSU && targetUnit!=sentence_->targetSentenceUnits().end())
	{
		if ((*targetUnit)->sconPointerFound(scon_) && isValidUnit(*targetUnit))
		{
			// this is the desired TSU
			foundTSU = true;
			targetSentenceUnit_ = *targetUnit;

			// bookkeeping
			alreadySelectedTargetSentenceUnits.push_back(targetSentenceUnit_);
		}
		else
		{
			// check next TSU in sequence
			targetUnit++; 
		}
	}

	return foundTSU;
}

// --------------------------------------------------------------------------
// Try to find the associated target sentence unit based on the exact opadr value.
// This is the most specific information, nad must be used first when it is known.
// Only one TSU will match on the exact opadr value.
// --------------------------------------------------------------------------
bool TermElement::findTargetSentenceUnitAssociatedToOpadr(TargetUnitVector& alreadySelectedTargetSentenceUnits)
{
	bool foundTSU = false;
	TargetUnitIterator targetUnit = sentence_->targetSentenceUnits().begin();
	while (!foundTSU && targetUnit!=sentence_->targetSentenceUnits().end())
	{
		if ((*targetUnit)->opadr() == opadr_ && isValidUnit(*targetUnit))
		{
			// this is the desired TSU
			foundTSU = true;
			targetSentenceUnit_ = *targetUnit;

			// bookkeeping
			alreadySelectedTargetSentenceUnits.push_back(targetSentenceUnit_);
		}
		else
		{
			// check next TSU in sequence
			targetUnit++; 
		}
	}

	return foundTSU;
}

// --------------------------------------------------------------------------
bool TermElement::isValidUnit(TargetSentenceUnit* targetUnit)
{
	bool isValid = true;

	// discard pointer to bos unit (functional constant -140)
	if (targetUnit->opadr() == -1)
	{
		isValid = false;
	}

	return isValid;
}

// --------------------------------------------------------------------------
// Return whether the given unit is a member of the given list.
// --------------------------------------------------------------------------
bool TermElement::memberTargetSentenceUnitList(TargetUnitVector& aList, TargetSentenceUnit* tsu)
{
	bool member = false;

	TargetUnitIterator targetUnit = aList.begin();
	while (!member && targetUnit!=aList.end())
	{
		if (*targetUnit == tsu)
		{
			member = true;
		}
		else
		{
			targetUnit++;
		}
	}

	return member;
}

// --------------------------------------------------------------------------
// Find the source sentence unit that is associated to the target sentence unit
// defined in targetSentenceUnit_. This is the one that corresponds to the opadr 
// of targetSentenceUnit_. 
// The opadr gives the position of the SSU in the vector of SSUs and takes into 
// account any inserted units (eg, switch68 in tran1).
// Set to 0 if not found.
// --------------------------------------------------------------------------
bool TermElement::findSourceSentenceUnitAssociatedToTargetSentenceUnit()
{
	bool foundSSU = false;
	if (targetSentenceUnit_ != 0)
	{
		if (targetSentenceUnit_->sourceSentenceUnit() != 0)
		{
			foundSSU = true;
			sourceSentenceUnit_ = targetSentenceUnit_->sourceSentenceUnit();

			// update the primarySSU in the source element to reflect RES selection
			// (that should be done after RES but is not done)
			sourceSentenceUnit_->setPrimarySsuPosition(targetSentenceUnit_->sourcePrimarySsuPosition());
		}
	}
	return foundSSU;
}

// --------------------------------------------------------------------------
// Return whether this element is a compound.
// --------------------------------------------------------------------------
bool TermElement::isGermanCompound()
{
	bool result = false;

	if (sourceSentenceUnit_ != 0)
	{
		if (sourceSentenceUnit_->compoundUnit())
		{
			result = true;
		}
	}

	return result;
}

// --------------------------------------------------------------------------
// Return whether this element is a compound and it is the head element of the compound.
// --------------------------------------------------------------------------
bool TermElement::isGermanCompoundAndHead()
{
	bool result = false;

	if (sourceSentenceUnit_ != 0)
	{
		if (sourceSentenceUnit_->compoundUnit() && sourceSentenceUnit_->headUnit())
		{
			result = true;
		}
	}

	return result;
}

// --------------------------------------------------------------------------
// Return whether this element is a compound but not the head element of the compound.
// --------------------------------------------------------------------------
bool TermElement::isGermanCompoundAndNotHead()
{
	bool result = false;

	if (sourceSentenceUnit_ != 0)
	{
		if (sourceSentenceUnit_->compoundUnit() && !sourceSentenceUnit_->headUnit())
		{
			result = true;
		}
	}

	return result;
}

// --------------------------------------------------------------------------
// Return whether this element is part of a compound and this part is an unfound word (no match
// in the dictionary).
// --------------------------------------------------------------------------
bool TermElement::isUnfoundGermanCompound()
{
	bool result = false;

	if (sourceSentenceUnit_ != 0)
	{
		if (sourceSentenceUnit_->compoundUnit() && sourceSentenceUnit_->isUnfoundWord())
		{
			result = true;
		}
	}

	return result;
}

// --------------------------------------------------------------------------
// Return whether this element is the first element of the compound decomposition.
// --------------------------------------------------------------------------
bool TermElement::isFirstElementOfGermanCompound()
{
	bool result = false;

	if (sourceSentenceUnit_ != 0)
	{
		if (sourceSentenceUnit_->compoundUnit())
		{
			LgsString wholeCompound = sourceSentenceUnit_->surfaceExpressionAsString();
			LgsString element = sourceSentenceUnit_->primarySsu().word();
			int positionOfElementInCompound = wholeCompound.find(element);
			if (positionOfElementInCompound == 0)
			{
				result = true;
			}
		}
	}

	return result;
}
