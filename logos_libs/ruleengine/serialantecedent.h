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
#ifndef __RuleEngineSerialAntecedent_h__
#define __RuleEngineSerialAntecedent_h__

//----------------------------------------------------------------------------
// File - SerialAntecedent.h
//
// Class - RE_SerialAntecedent - abstract
//
// Description - consists of a nested list of antecedents. The evaluate method
//               will combine their evaluate results in some manner
//               at a child class level.
//
//----------------------------------------------------------------------------

#include <logos_libs/ruleengine/antecedent.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace RuleEngine {

template <class RuleVariable>
class RE_SerialAntecedent: public RE_Antecedent<RuleVariable>
{
    DisableCopyAssign(RE_SerialAntecedent);

public:
    DefaultConstructor(RE_SerialAntecedent)
    virtual ~RE_SerialAntecedent();

    void insert(RE_Antecedent<RuleVariable>*);

protected:
    list<RE_Antecedent<RuleVariable>*> list_;
    typedef typename list< RE_Antecedent<RuleVariable> * >::iterator Iterator;
};

//----------------------------------------------------------------------------
template <class RuleVariable>
RE_SerialAntecedent<RuleVariable>::~RE_SerialAntecedent()
{
    // iterate over container and delete elements
    for (Iterator iter_ = list_.begin(); iter_ != list_.end(); iter_++)
        delete *iter_;
}

template <class RuleVariable>
void RE_SerialAntecedent<RuleVariable>::insert(RE_Antecedent<RuleVariable>* a)
{
    list_.push_back(a);
}

//}

#endif



