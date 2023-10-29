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
#ifndef __RuleEngineSequentialRuleBase_h__
#define __RuleEngineSequentialRuleBase_h__

//-------------------------------------------------------------------
// File - SequentialRuleBase
//
// Class - RE_SequentialRuleBase
//
// Description - A RuleBase using a linked list as the container.
//
//-------------------------------------------------------------------

#include <logos_libs/ruleengine/rulebase.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace RuleEngine {

template <class RuleVariable>
class RE_SequentialRuleBase: public RE_RuleBase<RuleVariable>
{
   DisableCopyAssign(RE_SequentialRuleBase);

public:
   DefaultConstructor(RE_SequentialRuleBase)
   virtual ~RE_SequentialRuleBase();
   virtual void insertRule(RE_Rule<RuleVariable>*);
   virtual bool hasRules();

protected:
   virtual void reset();
   virtual RE_Rule<RuleVariable>* next(const RuleVariable&);
   virtual RE_Rule<RuleVariable>* next(void);

private:
   list<RE_Rule<RuleVariable>*> rules_;
   typename list<RE_Rule<RuleVariable>*>::iterator iter_;
};

//-------------------------------------------------------------------
template <class RuleVariable>
RE_SequentialRuleBase<RuleVariable>::~RE_SequentialRuleBase()
{
   // iterate over container and delete elements
   for (iter_ = rules_.begin(); iter_ != rules_.end(); iter_++)
   {
      delete *iter_;
   }
   rules_.erase(rules_.begin(), rules_.end());
}

template <class RuleVariable>
void RE_SequentialRuleBase<RuleVariable>::insertRule(RE_Rule<RuleVariable>* rule)
{
   rules_.push_back(rule);
}

template <class RuleVariable>
void RE_SequentialRuleBase<RuleVariable>::reset()
{
   iter_ = rules_.begin();
}

template <class RuleVariable>
RE_Rule<RuleVariable>* RE_SequentialRuleBase<RuleVariable>::next(const RuleVariable&)
{
   if (iter_ == rules_.end())
      return 0;
   return *iter_++;
}

template <class RuleVariable>
RE_Rule<RuleVariable>* RE_SequentialRuleBase<RuleVariable>::next()
{
   if (iter_ == rules_.end())
      return 0;
   return *iter_++;
}


template <class RuleVariable>
bool RE_SequentialRuleBase<RuleVariable>::hasRules()
{
   return !(rules_.empty());
}
//}

#endif



