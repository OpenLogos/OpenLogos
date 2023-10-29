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
// Tran1GermanCompoundTerm.h: interface for the Tran1GermanCompoundTerm class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _Tran1GermanCompoundTerm_h_
#define _Tran1GermanCompoundTerm_h_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <TermMiner/Term.h>

class TermSearchSentenceUnit;
class GermanSourceCompoundWord;

// --------------------------------------------------------------------------
// Definition of a German compound term that has been generated during the Tran1 process
// and decomposed during the Lookup process.
// This defines all information that needs to be defined for unfound German compound terms.
// --------------------------------------------------------------------------
class Tran1GermanCompoundTerm : public Term
{
public:
	Tran1GermanCompoundTerm(TermSearchSentenceUnit*);
	virtual ~Tran1GermanCompoundTerm();

private:
	GermanSourceCompoundWord* sourceCompoundWord_;	// definition of the compound - in this case, the whole object is a single compound word
	void defineSourceCompoundWord();				// load all elements of this object as a compound word
	void computeSourceCanonicalForms();				// set various canonical forms of head, entire compound, etc.
	void setSourceGender();							// set sourceGender_
};

#endif // _Tran1GermanCompoundTerm_h_
