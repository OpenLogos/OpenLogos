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
#ifndef __RuleEngineSerialConsequent_h__
#define __RuleEngineSerialConsequent_h__

//----------------------------------------------------------------------------
// File - SerialConsequent.h
//
// Class - RE_SerialConsequent
//
// Description - consists of a nested list of Consequents. The fire method
//               will call the fire method on each consequent in the list.
//
//----------------------------------------------------------------------------

#include <logos_libs/ruleengine/consequent.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace RuleEngine {

template <class RuleVariable>
class RE_SerialConsequent: public RE_Consequent<RuleVariable>
{
    DisableCopyAssign(RE_SerialConsequent);

public:
    DefaultConstructor(RE_SerialConsequent)
    virtual ~RE_SerialConsequent();

    void insert(RE_Consequent<RuleVariable>*);
    virtual void fire(RuleVariable&) const;

protected:
    list<RE_Consequent<RuleVariable>*> list_;
    typedef typename list<RE_Consequent<RuleVariable>*>::iterator Iterator;
    typedef typename list<RE_Consequent<RuleVariable>*>::const_iterator ConstIterator;
};

//----------------------------------------------------------------------------
template <class RuleVariable>
RE_SerialConsequent<RuleVariable>::~RE_SerialConsequent()
{
    // iterate over container and delete elements
    for (Iterator iter_ = list_.begin(); iter_ != list_.end(); iter_++)
        delete *iter_;
}

template <class RuleVariable>
void RE_SerialConsequent<RuleVariable>::insert(RE_Consequent<RuleVariable>* a)
{
    list_.push_back(a);

}

template <class RuleVariable>
void RE_SerialConsequent<RuleVariable>::fire(RuleVariable& variable) const
{
    // iterate over container and fire all elements
    for (ConstIterator iter_ = list_.begin(); iter_ != list_.end(); iter_++)
        (*iter_)->fire(variable);
}

//}

#endif




