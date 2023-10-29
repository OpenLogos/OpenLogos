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
// --------------------------------------------------------------------------
// File: ChangedName.h (interface)
// Purpose: maintain information about a name (first or last) that has been changed
// by proper name recognizer. This is to overcome the cache of lookup from one sentence
// to another and be able to recognize names defined or not in the dictionary (PNR changes
// the set/superset/subset/form of recognized names and that may affect names encountered
// in following sentences of the same document). The idea is to maintain the original
// information before PNR changes something and restore it before PNR takes place.
// --------------------------------------------------------------------------

#ifndef _ChangedName_h
#define _ChangedName_h

class SourceSentenceUnit;

// --------------------------------------------------------------------------
// Interface for class ChangedName
// --------------------------------------------------------------------------
class ChangedName 
{
public:
	ChangedName();
	ChangedName(SourceSentenceUnit*);
	virtual ~ChangedName();

	LgsString surfaceForm() {return surfaceForm_;}
	int superset() {return superset_;}
	int set() {return set_;}
	int subset() {return subset_;}
	int form() {return form_;}

private:
	LgsString surfaceForm_;		// identifier
	int superset_;				// original information before it was changed by PNR
	int set_;					// original information before it was changed by PNR
	int subset_;				// original information before it was changed by PNR
	int form_;					// original information before it was changed by PNR
};


// --------------------------------------------------------------------------
// Interface for class ChangedNameVector
// --------------------------------------------------------------------------
class ChangedNameVector : public LgsVector(ChangedName)
{
public:
	ChangedNameVector() {}
	virtual ~ChangedNameVector();
	void append(SourceSentenceUnit*);
	void restore(SourceSentenceUnit*);
};

typedef ChangedNameVector::iterator ChangedNameVectorIterator;

#endif
