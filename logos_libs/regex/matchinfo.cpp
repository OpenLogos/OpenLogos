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
//-------------------------------------------------------------------
// File - MatchInfo.cpp
//
// Class - EL_MatchInfo
//
//-------------------------------------------------------------------

#include "matchinfo.h"
#include <logos_include/lgsstring.h>
#include <assert.h>

RegularExpression::MatchPos MatchInfo::getMatch(int group) const
{
    assert(group < groups_);
    return match_[group];
}

void MatchInfo::adjustMatchLength(int newLength, int group)
{
   assert(group < groups_);
   match_[group].length = newLength;
}

const LgsString& MatchInfo::getSubString(int group) const
{
    assert(group < groups_);
    return subString_[group];
}

LgsString& MatchInfo::getSubString(int group)
{
    assert(group < groups_);
    return subString_[group];
}

void MatchInfo::reset(const RegularExpression& pattern, const LgsString& text)
{
    groups_ = pattern.groups();
    int textLength = text.length();
    for (int group = 0; group < groups_; group++)
    {
        match_[group] = pattern.match(group);
        int pos = match_[group].pos;
        int length = match_[group].length;
        if (textLength > 0 && length > 0 && pos >= 0){
			subString_[group] = LgsString();
			for(LgsString::const_iterator iter = text.begin() + pos; 
				iter != text.begin() + pos + length ; iter ++)
					subString_[group] += *iter ;

		}
        //   subString_[group] = LgsString(text.begin() + pos, text.begin() + pos + length);
        else
            subString_[group] = LgsString();
    }
    assert(match_[0].pos != -1);
}

