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
#ifndef __PatternReplaceConsequent_h__
#define __PatternReplaceConsequent_h__

//-------------------------------------------------------------------
// File - ReplaceConsequent.h
//
// Class - PM_ReplaceConsequent
//
// Description - consequent that does a replacement on the buffer.
//
//-------------------------------------------------------------------

#include <logos_libs/ruleengine/consequent.h>
#include <logos_libs/patternmatcher/variable.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace PatternMatcher {

class PM_ReplaceConsequent: public RE_Consequent<PM_Variable>
{
    DisableCopyAssign(PM_ReplaceConsequent);

public:
    PM_ReplaceConsequent(const LgsString& replacement);
    virtual void fire(PM_Variable& variable) const;

private:
    LgsString parseReplacement(PM_Variable& variable) const;

    LgsString replacement_;
};

//}

#endif



