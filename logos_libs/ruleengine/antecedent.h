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
#ifndef __RuleEngineAntecedent_h__
#define __RuleEngineAntecedent_h__

//----------------------------------------------------------------------------
// File - Antecedent.h
//
// Class - RE_Antecedent - abstract.
//
// Description - Objects of this class represent the "test" condition
//      for the Rule object. The primary purpose of subclasses of
//      this class is to provide meaningful implementations of the
//      "evaluate()" method. Other methods can be also implemented
//      that would allow the object to be initialized with data, etc.
//
//----------------------------------------------------------------------------

template <class RuleVariable> class RE_Rule;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace RuleEngine {

template <class RuleVariable>
class RE_Antecedent: public Object
{
   DisableCopyAssign(RE_Antecedent);

public:
   DefaultConstructor(RE_Antecedent)

	virtual ~RE_Antecedent();

   RE_Rule<RuleVariable>* rule_;

   // Returns true if the rule variable matches the appropriate conditions.
   // Note that in the future the Antecedent could include fuzzy logic.
   virtual bool evaluate(RuleVariable&) = 0;
   virtual bool isViableMatch(RuleVariable& variable);
};

template <class RuleVariable>
inline bool RE_Antecedent<RuleVariable>::isViableMatch(RuleVariable& variable)
{
   return true;
}

template <class RuleVariable>
RE_Antecedent<RuleVariable>::~RE_Antecedent() {}

//}

#endif



