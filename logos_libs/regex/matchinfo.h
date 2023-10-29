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
#ifndef __MatchInfo_h__
#define __MatchInfo_h__

//-------------------------------------------------------------------
// File - MatchInfo.h
//
// Class - EL_MatchInfo
//
// Description - a class used to store matching details
//               after a succesful regular expression match
//
//-------------------------------------------------------------------

#include "regularexpression.h"
#include <logos_include/lgsstring.h>

class MatchInfo
{
public:
   MatchInfo();

   // see comments for reset
   MatchInfo(const RegularExpression& pattern, const LgsString& text);
   virtual ~MatchInfo();

   // store match information for all groups, including group 0 - the whole pattern
   // if text is an empty LgsString the sub-strings will also be empty
   void reset(const RegularExpression& pattern, const LgsString& text = "");

   // no of groups
   int getGroups() const;

   void adjustMatchLength(int newLength, int group = 0);

   // get matching details for the given group
   RegularExpression::MatchPos getMatch(int group = 0) const;

   // get matched substring for the given group
   const LgsString& getSubString(int group = 0) const;

   // get matched substring for the given group,
   // allowing modifications to the strings, for later retrieval
   LgsString& getSubString(int group = 0);

private:
   int groups_;                                                       // no of sub-expressions
   RegularExpression::MatchPos match_[RegularExpression::MaxGroups];  // list of match positions
   LgsString subString_[RegularExpression::MaxGroups];                   // list of matching strings
};

inline MatchInfo::MatchInfo()
                 :groups_(0)
{
}

inline MatchInfo::MatchInfo(const RegularExpression& pattern, const LgsString& text)
{
   reset(pattern, text);
}

inline MatchInfo::~MatchInfo()
{
}

inline int MatchInfo::getGroups() const
{
   return groups_;
}

#endif

