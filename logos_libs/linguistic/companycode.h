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
// File: CompanyCode.h (interface)
// Purpose: defines a company code.
// --------------------------------------------------------------------------

#ifndef _CompanyCode_h
#define _CompanyCode_h

// --------------------------------------------------------------------------
// Interface for class CompanyCode
// --------------------------------------------------------------------------
class CompanyCode : public Object
{
public:
	CompanyCode();							// default constructor
	CompanyCode(const LgsString&);			// construct this object given a company code value
	virtual ~CompanyCode();					// destructor
	void set(const LgsString &x) {cc_=x;}	// set the value of this company code
	LgsString content() const {return cc_;}		// get the value of this company code

private:
	LgsString cc_;								// the company code value
};


// --------------------------------------------------------------------------
// Definition of a vector of CompanyCode objects. Company codes are stored reflecting
// the order found in the database.
// --------------------------------------------------------------------------
class CompanyCodeVector : public LgsVector(CompanyCode)
{
public:
	CompanyCodeVector() {}					// default constructor
	virtual ~CompanyCodeVector();			// destructor

	// - setter methods
	// read database to set this object with all company codes
	void setWithAllCompanyCodes();
	// read database to set this object with user selected ccs
	void setWithUserSelectedCompanyCodes();
	// set with the diff between the two lists of CCs
	void setWithDifference(CompanyCodeVector*,CompanyCodeVector*);

	bool member(const CompanyCode &) const;				// whether the given CC is a member of this list
	void content() const;	     					// display content of this list of company codes
	LgsString first();						// return the first CC in the vector as a LgsString
	LgsVector(LgsString) toString();		// return all the CCs as a vector of strings
};

typedef LgsVector(CompanyCode)::iterator CompanyCodeIterator;
typedef LgsVector(CompanyCode)::const_iterator ConstCompanyCodeIterator;

#endif
