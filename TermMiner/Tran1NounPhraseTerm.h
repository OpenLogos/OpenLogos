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
// Tran1NounPhraseTerm.h: interface for the Tran1NounPhraseTerm class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _Tran1NounPhraseTerm_h_
#define _Tran1NounPhraseTerm_h_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <TermMiner/Term.h>

class TermSearchSentenceUnit;

// --------------------------------------------------------------------------
// Definition of a noun phrase term that has been generated during the Tran1 process.
// This defines all information that needs to be defined for unfound noun phrase terms.
//
// THIS CLASS SHOULD NOT BE USED BY ITSELF - RATHER ITS SUBCLASSES
// --------------------------------------------------------------------------
class Tran1NounPhraseTerm : public Term
{
public:
	Tran1NounPhraseTerm();
	Tran1NounPhraseTerm(TermSearchSentenceUnit*);
	virtual ~Tran1NounPhraseTerm();
	bool isSingleElement();

protected:
	bool isSingleElement_;					// whether this object is still a NP after cleaning it from front articles

	void resetNP(TermSearchSentenceUnit*);
	bool headIsNoun();
	bool isNounPhrase();
	void setAsNotANounPhrase();
	void doGeneralSettings();
};

#endif // _Tran1NounPhraseTerm_h_
