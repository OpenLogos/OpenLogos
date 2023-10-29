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
// File: CompanyCode.cpp (implementation)
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/companycode.h>
#include <logos_libs/translutility/translcommonobjects.h>
#include <logos_libs/gerdem/gcompanybuilder.h>
#include <logos_libs/gerdem/gfactory.h>

// --------------------------------------------------------------------------
// Default constructor
// --------------------------------------------------------------------------
CompanyCode::CompanyCode() :
	cc_("")
{
}


// --------------------------------------------------------------------------
// Construct with a given value for the company code
// --------------------------------------------------------------------------
CompanyCode::CompanyCode(const LgsString &aCompanyCodeValue) :
	cc_(aCompanyCodeValue)
{
}


// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
CompanyCode::~CompanyCode()
{
}


// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
CompanyCodeVector::~CompanyCodeVector()
{
}


// --------------------------------------------------------------------------
// Display content of this list of company codes. The codes reflect the order
// in which they have been set by the user.
// --------------------------------------------------------------------------
void CompanyCodeVector::content() const
{
	cout << endl << "COMPANY CODES:";
	for (ConstCompanyCodeIterator cc=begin(); cc!=end(); cc++)
	{
		cout << " " << (*cc).content();
	}
	cout << endl;
}


// --------------------------------------------------------------------------
// Return the content of this list as a vector of LgsString (each is a company code
// value). The codes reflect the order in which they have been set by the user.
// --------------------------------------------------------------------------
LgsVector(LgsString) CompanyCodeVector::toString()
{
	LgsVector(LgsString) ccList;

	for (CompanyCodeIterator cc=begin(); cc!=end(); cc++)
	{
		ccList.push_back((*cc).content());
	}

	return ccList;
}


// --------------------------------------------------------------------------
// Return the first element in the list of company codes. Return the default
// company code in case the list is empty.
// --------------------------------------------------------------------------
LgsString CompanyCodeVector::first()
{
	LgsString firstCompanyCodeInList = "LOG";		// default to LOG in case the list is empty

	if (size() > 0)			// get the first CC in the list if there is at least one
	{
		CompanyCodeIterator cc = begin();
		firstCompanyCodeInList = (*cc).content();
	}

	return firstCompanyCodeInList;
}


// --------------------------------------------------------------------------
// Read the database to set this object with the user-ordered list of all company 
// codes.
// --------------------------------------------------------------------------
void CompanyCodeVector::setWithAllCompanyCodes()
{
	// TO BE CHANGED without DCompany, etc.
	const CompanyBuilder& companyBuilder = TranslCommonObjects::GetCompanyBuilder();
	DCompanyVector* ccs = companyBuilder.Build(*(TranslCommonObjects::GetPersistDataFactory()));
	for (DCompanyVector::iterator cci=ccs->begin(); cci!=ccs->end(); cci++)
	{
		CompanyCode cc((*cci).CompanyCode());
		push_back(cc);
	}
	delete ccs;
}


// --------------------------------------------------------------------------
// Read the database to set this object with the user-ordered selected list of 
// company codes.
// --------------------------------------------------------------------------
void CompanyCodeVector::setWithUserSelectedCompanyCodes()
{
	LgsString s = LgsDBCommonObjects::GetJobControlArguments().CompanyCodes();
	LgsVector(LgsString) tokens;
	StringUtil::parseInto(s,tokens);

	for (LgsVector(LgsString)::iterator i=tokens.begin(); i!=tokens.end(); i++)
	{
		CompanyCode cc(*i);
		push_back(cc);
	}
}


// --------------------------------------------------------------------------
// Set this object with the difference between the two lists of company codes.
// This list = list1 - list2.
// Approah: for each cc in list1, if it is not a member of list2 then include 
// in this list.
// --------------------------------------------------------------------------
void CompanyCodeVector::setWithDifference(CompanyCodeVector* list1, CompanyCodeVector* list2)
{
	for (CompanyCodeIterator cc=list1->begin(); cc!=list1->end(); cc++)
	{
		if (!list2->member(*cc))
		{
			push_back(*cc);
		}
	}
}


// --------------------------------------------------------------------------
// Return whether the given company code object is a member of this list.
// --------------------------------------------------------------------------
bool CompanyCodeVector::member(const CompanyCode &aCompanyCode) const
{
	bool found = false;
	ConstCompanyCodeIterator cc=begin();

	while (!found && cc!=end())
	{
		if ((*cc).content() == aCompanyCode.content())
		{
			found = true;
		}
		else
		{
			cc++;
		}
	}

	return found;
}

