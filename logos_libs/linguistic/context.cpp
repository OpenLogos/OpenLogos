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
// File - context.cpp
//
// Class - Context (implementation)
// --------------------------------------------------------------------------

#include <logos_libs/linguistic/context.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <logos_libs/translutility/translcommonobjects.h>


// --------------------------------------------------------------------------
// Default constructor
// --------------------------------------------------------------------------
Context::Context() :
	allSMCs_(0),
	userSelectedSMCs_(0),
	otherSMCs_(0),
	allCompanyCodes_(0),
	userSelectedCompanyCodes_(0),
	otherCompanyCodes_(0)
{
	smcLogosDefault_ = new SubjectMatterCode("001");		// define the SMC for Logos default
	logosDefaultCompanyCode_ = new CompanyCode("LOG");			// and the default Logos company code
}


// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
Context::~Context()
{
	delete otherSMCs_;
	delete userSelectedSMCs_;
	delete allSMCs_;
	delete smcLogosDefault_;
	delete allCompanyCodes_;
	delete userSelectedCompanyCodes_;
	delete otherCompanyCodes_;
	delete logosDefaultCompanyCode_;
}


// --------------------------------------------------------------------------
// Create a context based on the selected company codes and the SMC subtrees.
// Set this context dependending on the type of dictionary search the user has
// selected; that is, whether to perform regular/extended dictionary search and 
// with/without Logos default.
// --------------------------------------------------------------------------
void Context::createContext()
{
	//  +----------------------------------------+
	// -| get the ordered lists of company codes |
	//  +----------------------------------------+
	// ordered list of all company codes in the database
	allCompanyCodes_ = new CompanyCodeVector();
	allCompanyCodes_->setWithAllCompanyCodes();

	// ordered list of all company codes selected by the user (subset of allCompanyCodes_)
	userSelectedCompanyCodes_ = new CompanyCodeVector();
	userSelectedCompanyCodes_->setWithUserSelectedCompanyCodes();

	//  +-----------------------------------------------+
	// -| get the ordered lists of subject matter codes |
	//  +-----------------------------------------------+
	// complete ordered tree of SMCs as ordered by user
	allSMCs_ = new SubjectMatterCodeTree();
	allSMCs_->setAsCompleteTree();

	// selection of ordered SMC subtrees selected by the user
	userSelectedSMCs_ = new SubjectMatterCodeTree();
	userSelectedSMCs_->setAsUserSelectedSubtrees(allSMCs_);

	// --- debug (begin)
	/*
	cout << endl << "*** ALL COMPANY CODES ***" << endl;
	allCompanyCodes_->content();
	cout << endl << "*** USER SELECTED COMPANY CODES ***" << endl;
	userSelectedCompanyCodes_->content();
	cout << endl << "*** TREE OF ALL SMCs ***" << endl;
	allSMCs_->display();
	cout << endl << "*** TREE OF USER SELECTED SMCs ***" << endl;
	userSelectedSMCs_->display();
	*/
	// --- debug (end)
}


// --------------------------------------------------------------------------
// This method returns a score of the the given contextual unit based on the 
// user selected search strategy. Each strategy searches the sets of company
// codes and subject matter codes differently, as described below.
// The objective is to find a match in the selected strutures of CCs and SMCs of
// the candidate contextual unit. The score reflects where in the structures a match
// has been found (no match => score = 0). The score depends on the number of 
// strutures searched, the matching CC node in the structure, and the matching SMC
// node in the struture. The higher the score, the better the value of the 
// candidate contextual unit according to the selected strutures.
// --------------------------------------------------------------------------
int Context::match(ContextualUnit& unit)
{
	int score = 0;

	// node number in the selected struture where the candidate CC or SMC has been found
	// (0 means not in the selected structure)
	int ccNodeNumber = 0;
	int smcNodeNumber = 0;

	// number of strutures searched to find a match
	int structureNumber = 0;

	switch (LgsDBCommonObjects::GetJobControlArguments().ExtendedSearch())
	{
	case 1: // regular search with Logos defaults option
			  regularSearch(ccNodeNumber,smcNodeNumber,unit);
			  structureNumber++;
			  if (ccNodeNumber==0 || smcNodeNumber==0)
			  {
				  // not found yet, continue search in next struture
				  defaultSearch(ccNodeNumber,smcNodeNumber,unit);
				  structureNumber++;
			  }
			  break;
	case 2: // extended search with Logos defaults option
			  regularSearch(ccNodeNumber,smcNodeNumber,unit);
			  structureNumber++;
			  if (ccNodeNumber==0 || smcNodeNumber==0)
			  {
				  // not found yet, continue search in next struture
				  extendedSearch(ccNodeNumber,smcNodeNumber,unit);
				  structureNumber++;
			  }
			  if (ccNodeNumber==0 || smcNodeNumber==0)
			  {
				  // not found yet, continue search in next struture
				  defaultSearch(ccNodeNumber,smcNodeNumber,unit);
				  structureNumber++;
			  }
			  break;
	case 4: // regular search without Logos defaults option
			  regularSearch(ccNodeNumber,smcNodeNumber,unit);
			  structureNumber++;
			  break;
	case 3: // extended search without Logos defaults option
			  regularSearch(ccNodeNumber,smcNodeNumber,unit);
			  structureNumber++;
			  if (ccNodeNumber==0 || smcNodeNumber==0)
			  {
				  // not found yet, continue search in next struture
				  extendedSearch(ccNodeNumber,smcNodeNumber,unit);
				  structureNumber++;
			  }
	}

	// compute score value based on search results. This is the value of the match in the ordered space
	// of all possible structures to search, and for each, all possible CCs, and for each all possible
	// SMCs. The result is a positive number between 0 and the value of searchSpace. The result is 0 
	// if there was no structure searched, and no CC node found and no SMC node found. The higher the
	// result value, the better the match.
	int searchSpace = 4000000;		// 3 basic searches => 1000000 values for each (for CCs and SMCs)
	int ccSpace = 1000000;			// up to 100 CCs to be defined => each can have 10000  SMC values
	int smcSpace = 10000;			// up to 10000 SMCs to be defined => each have 1 value
	int invCC = 0;
	int invSMC = 0;
	int invSearch = 0;
	if (ccNodeNumber>0 && smcNodeNumber>0 && structureNumber>0)
	{
		invSearch = searchSpace - structureNumber * ccSpace;
		invCC = ccSpace - ccNodeNumber * smcSpace;
		invSMC = smcSpace - smcNodeNumber;
	}
	score = invSearch + invCC + invSMC;

	// debug (begin)
	/*
	cout << "candidate unit: CC=" << unit.CompanyCode() << " SMC=" << unit.subjectMatterCode().content() << endl;
	cout << "match -> structureNumber=" << structureNumber << " ccNodeNumber=" << ccNodeNumber << " smcNodeNumber=" << smcNodeNumber << endl;
	cout << "final score: " << score << endl;
	*/
	// debug (end)

	return score;
}


// --------------------------------------------------------------------------
// Regular search consists in searching only the user-specified company codes
// and the use-specified subject matter codes.
// The approach is to search in the specified order each SMC for each CC.
// Return the node number of the match of the candidate CC and SMC in the searched
// struture.
// Note 1: A 0 value means that it has not been found in the struture.
// Note 2: the SMC struture is searched only if a CC has been matched.
// --------------------------------------------------------------------------
void Context::regularSearch(int& ccNodeNumber, int& smcNodeNumber, ContextualUnit& unit)
{
	//cout << "regular search" << endl;		// debug

	ccNodeNumber = 0;
	smcNodeNumber = 0;

	// search for the candidate company code in the selected structure
	bool foundCC = false;
	CompanyCodeIterator cc = userSelectedCompanyCodes_->begin();
	while (!foundCC && cc!=userSelectedCompanyCodes_->end())
	{
		ccNodeNumber++;
		if (unit.CompanyCode() == cc->content())
		{
			foundCC = true;
		}
		else
		{
			cc++;
		}
	}

	// search for the candidate subject matter code in the selected structure
	if (foundCC)
	{
		smcNodeNumber = userSelectedSMCs_->nodeNumber(unit.subjectMatterCode());
	}
}


// --------------------------------------------------------------------------
// Extended search consists in searching all company codes and all subject
// matter codes.
// The approach is to search in the specified order each SMC for each CC.
// Return the node number of the match of the candidate CC and SMC in the searched
// struture.
// Note 1: A 0 value means that it has not been found in the struture.
// Note 2: the SMC struture is searched only if a CC has been matched.
// --------------------------------------------------------------------------
void Context::extendedSearch(int& ccNodeNumber, int& smcNodeNumber, ContextualUnit& unit)
{
	//cout << "extended search" << endl;		// debug

	ccNodeNumber = 0;
	smcNodeNumber = 0;

	// search for the candidate company code in the selected structure
	bool foundCC = false;
	CompanyCodeIterator cc = allCompanyCodes_->begin();
	while (!foundCC && cc!=allCompanyCodes_->end())
	{
		ccNodeNumber++;
		if (unit.CompanyCode() == cc->content())
		{
			foundCC = true;
		}
		else
		{
			cc++;
		}
	}

	// search for the candidate subject matter code in the selected structure
	if (foundCC)
	{
		smcNodeNumber = allSMCs_->nodeNumber(unit.subjectMatterCode());
	}
}


// --------------------------------------------------------------------------
// Default search consists in searching only the general default subject
// matter code for the Logos default company code.
// Return the node number of the match of the candidate CC and SMC in the searched
// struture.
// Note 1: A 0 value means that it has not been found in the struture.
// Note 2: the SMC struture is searched only if a CC has been matched.
// --------------------------------------------------------------------------
void Context::defaultSearch(int& ccNodeNumber, int& smcNodeNumber, ContextualUnit& unit)
{
	//cout << "default search" << endl;		// debug

	ccNodeNumber = 0;
	smcNodeNumber = 0;

	// search for the candidate company code in the selected structure
	bool foundCC = false;
	if (unit.CompanyCode() == logosDefaultCompanyCode_->content())
	{
		ccNodeNumber = 1;		// node number where it has been found
		foundCC = true;
	}

	// search for the candidate subject matter code in the selected structure
	if (foundCC)
	{
		if (unit.subjectMatterCode() == *smcLogosDefault_)
		{
			smcNodeNumber = 1;			// node number where it has been found
		}
	}
}

