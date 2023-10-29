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
// File: ChangedName.cpp (implementation)
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/PatternRecognition/ChangedName.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>

// --------------------------------------------------------------------------
// Construct a default object - necessary for STL
// --------------------------------------------------------------------------
ChangedName::ChangedName()
{
	surfaceForm_ = "";
	superset_ = 0;
	set_ = 0;
	subset_ = 0;
	form_ = 0;
}


// --------------------------------------------------------------------------
// Construct and set this object with original values of this SSU
// --------------------------------------------------------------------------
ChangedName::ChangedName(SourceSentenceUnit* sourceUnit)
{
	surfaceForm_ = sourceUnit->surfaceExpressionAsString();
	superset_ = sourceUnit->superSetID();
	set_ = sourceUnit->setID();
	subset_ = sourceUnit->subSetID();
	form_ = sourceUnit->formCode();
}


// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
ChangedName::~ChangedName()
{
}


// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
ChangedNameVector::~ChangedNameVector()
{
}


// --------------------------------------------------------------------------
// Append a name to the list - check for doublons
// --------------------------------------------------------------------------
void ChangedNameVector::append(SourceSentenceUnit* sourceUnit)
{
	ChangedNameVectorIterator name = begin();
	bool found = false;

	while (!found && name!=end())
	{
		if ((*name).surfaceForm() == sourceUnit->surfaceExpressionAsString())
		{
			found = true;
		}
		else
		{
			name++;
		}
	}

	if (!found)
	{
		ChangedName aName(sourceUnit);
		push_back(aName);
	}
}


// --------------------------------------------------------------------------
// Restore the given unit to its original saved values if it is a member of this
// list of names.
// --------------------------------------------------------------------------
void ChangedNameVector::restore(SourceSentenceUnit* sourceUnit)
{
	ChangedNameVectorIterator name = begin();
	bool found = false;

	while (!found && name!=end())
	{
		if ((*name).surfaceForm() == sourceUnit->surfaceExpressionAsString())
		{
			found = true;
			sourceUnit->setSuperSetID((*name).superset());
			sourceUnit->setSetID((*name).set());
			sourceUnit->setSubSetID((*name).subset());
			sourceUnit->setFormCode((*name).form());
		}
		else
		{
			name++;
		}
	}
}

