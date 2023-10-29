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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
// TermElement.h: interface for the TermElement class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _TermElement_h_
#define _TermElement_h_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class LSentence;
class SourceSentenceUnit;
class TargetSentenceUnit;
class TargetUnitVector;

// --------------------------------------------------------------------------
// Definition of a term element. The basic element of a term object. It groups together
// a source sentence unit and its corresponding target sentence unit.
// --------------------------------------------------------------------------
class TermElement : public Object
{
public:
	TermElement();
	TermElement(int,bool,int,LSentence*,bool,TargetUnitVector&);
	TermElement(int,LSentence*,bool);
	void reset(int,LSentence*,bool);
	virtual ~TermElement();
	bool hasSourceSetCorrectly();
	bool hasTargetSetCorrectly();
	SourceSentenceUnit* sourceSentenceUnit();
	TargetSentenceUnit* targetSentenceUnit();
	bool isHeadElement();						// whether this element is the head element
	bool isSetCorrectly();
	bool isGermanCompound();
	bool isUnfoundGermanCompound();
	bool isGermanCompoundAndHead();
	bool isGermanCompoundAndNotHead();
	bool isFirstElementOfGermanCompound();

private:
	int scon_;		// scon of this element from the target array (SCONPO) at the end of Tran1
	int opadr_;		// opadr of this element from the target array (OPADRO) at the end of Tran1
	LSentence* sentence_;
	SourceSentenceUnit* sourceSentenceUnit_;	// the SSU defining this element
	TargetSentenceUnit* targetSentenceUnit_;	// its associated TSU
	bool headElement_;							// whether this element is the head element
	bool setCorrectly_;							// whether this object is set properly (eg, both a TSU and SSU have been found)
	bool findTargetSentenceUnitAssociatedToScon(TargetUnitVector&);
	bool findTargetSentenceUnitAssociatedToOpadr(TargetUnitVector&);
	bool memberTargetSentenceUnitList(TargetUnitVector&,TargetSentenceUnit*);
	bool findSourceSentenceUnitAssociatedToTargetSentenceUnit();
	bool isValidUnit(TargetSentenceUnit*);
	void resetAll(int,int,LSentence*,bool);
	void findCorrespondingSentenceUnits(TargetUnitVector&,bool);
};

// --------------------------------------------------------------------------
// Definition of a vector of TermElement objects
// --------------------------------------------------------------------------
typedef LgsVector(TermElement) TermElementVector;
typedef TermElementVector::iterator TermElementIterator;

// --------------------------------------------------------------------------
inline bool TermElement::hasSourceSetCorrectly()
{
	return (sourceSentenceUnit_ != 0);
}

// --------------------------------------------------------------------------
inline bool TermElement::hasTargetSetCorrectly()
{
	return (targetSentenceUnit_ != 0);
}

// --------------------------------------------------------------------------
inline SourceSentenceUnit* TermElement::sourceSentenceUnit() 
{
	return sourceSentenceUnit_;
}

// --------------------------------------------------------------------------
inline TargetSentenceUnit* TermElement::targetSentenceUnit() 
{
	return targetSentenceUnit_;
}

// --------------------------------------------------------------------------
inline bool TermElement::isHeadElement() 
{
	return headElement_;
}

#endif // _TermElement_h_
