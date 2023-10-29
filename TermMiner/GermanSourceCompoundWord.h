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
// GermanSourceCompoundWord.h: interface for the GermanSourceCompoundWord class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _GermanSourceCompoundWord_h_
#define _GermanSourceCompoundWord_h_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class SourceSentenceUnit;

// --------------------------------------------------------------------------
// A source German compound is composed of several source sentence units. One of them
// may be the head. The head (if exists) can be a noun or an adjective.
// --------------------------------------------------------------------------
class GermanSourceCompoundWord : public Object
{
public:
	GermanSourceCompoundWord();
	virtual ~GermanSourceCompoundWord();
	void push_back(SourceSentenceUnit*);
	LgsString precedingSpaces();
	LgsString canonicalForm(LgsString,int);
	SourceSentenceUnit* headElement();
	bool hasElements();
	void reset();

private:
	LgsVector(SourceSentenceUnit*) elements_;				// all elements of the compound word
	SourceSentenceUnit* headElement_;					// pointer to the head element (if found)
	LgsString agreementADJtoN_GermanSource(LgsString,int);
};

// --------------------------------------------------------------------------
inline SourceSentenceUnit* GermanSourceCompoundWord::headElement()
{
	return headElement_;
}

// --------------------------------------------------------------------------
// Return whether this element has been defined (ie, contains some elements).
// --------------------------------------------------------------------------
inline bool GermanSourceCompoundWord::hasElements()
{
	return !elements_.empty();
}

// --------------------------------------------------------------------------
inline void GermanSourceCompoundWord::reset()
{
	elements_.clear();
}

#endif // _GermanSourceCompoundWord_h_
