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
// File: SubjectMatterCode.cpp (implementation)
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/SubjectMatterCodes/SubjectMatterCode.h>
#include <logos_libs/utility/stringutil.h>

// --------------------------------------------------------------------------
// Default constructor
// To be removed when atomic+generic codes are not used anymore
// --------------------------------------------------------------------------
SubjectMatterCode::SubjectMatterCode()
{
	clear();
}

// --------------------------------------------------------------------------
// Construct this object given the specified SMC
// --------------------------------------------------------------------------
SubjectMatterCode::SubjectMatterCode(LgsString aSMC)
{
   set(aSMC);
}

void SubjectMatterCode::set(LgsString x)
{
   LgsString newSMC = x;
   StringUtil::rightTrim(newSMC);
   smc_ = newSMC;
}

// --------------------------------------------------------------------------
// Default destructor
// --------------------------------------------------------------------------
SubjectMatterCode::~SubjectMatterCode()
{
}

// --------------------------------------------------------------------------
// Copy operator
// --------------------------------------------------------------------------
const SubjectMatterCode& SubjectMatterCode::operator=(const SubjectMatterCode& anObject) 
{
   if (&anObject != this)
   {
      smc_ = anObject.smc_;
   }
   return *this;
}

// --------------------------------------------------------------------------
// Reset this object
// --------------------------------------------------------------------------
void SubjectMatterCode::clear()
{
	smc_="001";
}

// --------------------------------------------------------------------------
// Return the level of this SMC.
// Each level is a 3-digit sequence. For example, if smc is "aaabbbccc", then 
// thelevel is 3.
// --------------------------------------------------------------------------
int SubjectMatterCode::level()
{
	return ((int) smc_.size() / 3);
}

// --------------------------------------------------------------------------
// Return the SMC value at the specified level. For example, if smc is "aaabbbccc"
// and level is 2, it returns "aaabbb".
// --------------------------------------------------------------------------
LgsString SubjectMatterCode::contentAtLevel(int level)
{
	return (smc_.substr(0,3*level));
}

// append SMC information in the format LEVEL:DESCRIPTION:CODE
void SubjectMatterCode::appendSmcInfo(LgsString& smcTree)
{
    char smcString[100];
    sprintf(smcString, "%d:%s:%s;", level(), "", smc_.substr(smc_.size()-3).c_str());
    smcTree += smcString;
}

// --------------------------------------------------------------------------
// Return content as a 15-digit LgsString of characters
// --------------------------------------------------------------------------
LgsString SubjectMatterCode::formattedContent()
{
	LgsString s="               ";			// formatted space for 15 digits

	// copy SMC
	for (int i=0; i<smc_.length(); i++) 
	{
		s[i]=smc_[i];
	}

	return s;
}

// --------------------------------------------------------------------------
// Compare SMC objects
// --------------------------------------------------------------------------
bool SubjectMatterCode::operator==(const SubjectMatterCode& anObject) const
{
	return (smc_ == anObject.smc_);
}
bool SubjectMatterCode::operator>=(const SubjectMatterCode& anObject) const
{
	return (smc_ >= anObject.smc_);
}
bool SubjectMatterCode::operator>(const SubjectMatterCode& anObject) const
{
	return (smc_ > anObject.smc_);
}
bool SubjectMatterCode::operator<=(const SubjectMatterCode& anObject) const
{
	return (smc_ <= anObject.smc_);
}
bool SubjectMatterCode::operator<(const SubjectMatterCode& anObject) const
{
	return (smc_ < anObject.smc_);
}
bool SubjectMatterCode::operator!=(const SubjectMatterCode& anObject) const
{
	return (smc_ != anObject.smc_);
}

