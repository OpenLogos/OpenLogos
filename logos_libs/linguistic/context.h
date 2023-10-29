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
// File - context.h
//
// Class - Context (interface)
// --------------------------------------------------------------------------

#ifndef _Context_h_
#define _Context_h_

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/contextualunit.h>
#include <logos_libs/SubjectMatterCodes/SubjectMatterCodeTree.h>
#include <logos_libs/linguistic/companycode.h>


// --------------------------------------------------------------------------
// Definition of class Context
// --------------------------------------------------------------------------
class Context : public Object
{
public:
   Context();
   virtual ~Context();

   void createContext();
   int match(ContextualUnit&);   // attempt a match with the CCs and SMCs of this context

   enum { InvalidValue = -1, UnmatchedValue = 0 };

	// getter methods
	CompanyCodeVector* allCompanyCodes() {return allCompanyCodes_;}
	CompanyCodeVector* userSelectedCompanyCodes() {return userSelectedCompanyCodes_;}
	CompanyCodeVector* otherCompanyCodes() {return otherCompanyCodes_;}
	CompanyCode* logosDefaultCompanyCode() {return logosDefaultCompanyCode_;}
	SubjectMatterCodeTree* allSMCs() {return allSMCs_;}
	SubjectMatterCodeTree* userSelectedSMCs() {return userSelectedSMCs_;}
	SubjectMatterCodeTree* otherSMCs() {return otherSMCs_;}
	SubjectMatterCode* smcLogosDefault() {return smcLogosDefault_;}

private:
   // company codes
	CompanyCodeVector* allCompanyCodes_;					// all company codes
	CompanyCodeVector* userSelectedCompanyCodes_;		// user selected company codes
	CompanyCodeVector* otherCompanyCodes_;					// allCompanyCodes_ - userSelectedCompanyCodes_
	CompanyCode* logosDefaultCompanyCode_;					// default Logos company code

   // subject matter codes
	SubjectMatterCodeTree* allSMCs_;							// whole tree of all user-ordered SMCs
	SubjectMatterCodeTree* userSelectedSMCs_;				// subtrees selected by the user
	SubjectMatterCodeTree* otherSMCs_;						// allSMCs_ - userSelectedSMCs_
   SubjectMatterCode* smcLogosDefault_;					// definition of the SMC for Logos default
   enum {DefaultSubjectMatter = 0};

	// basic dictionary search approaches (their combination define each search option)
	void regularSearch(int&,int&,ContextualUnit&);
	void extendedSearch(int&,int&,ContextualUnit&);
	void defaultSearch(int&,int&,ContextualUnit&);
};


#endif

