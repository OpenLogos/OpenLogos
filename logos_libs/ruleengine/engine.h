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
#ifndef __RuleEngineEngine_h__
#define __RuleEngineEngine_h__

//----------------------------------------------------------------------------
// File - Engine.h
//
// Class - RE_RuleEngine - abstract
//
// Description - Holds a RuleBase, together with a method evaluate()
//               which will apply the rulebase to a variable.
//               evaluate() will call an appropriate method on the RuleBase.
//
//----------------------------------------------------------------------------

#include <logos_libs/ruleengine/rulebase.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace RuleEngine {

template <class RuleVariable>
class RE_Engine: public Object
{
   DisableCopyAssign(RE_Engine);

public:
   RE_Engine(RE_RuleBase<RuleVariable>* ruleBase);

   virtual ~RE_Engine();

   // evaluate the rule base on the variable - defined in the sub-class which evaluation flavor to call
   virtual bool evaluate(RuleVariable&) = 0;

   // indication that there are rules stored in the engine
   virtual bool hasRules();

   // Function to log statistics of rule matching
   virtual void logStatistics(ostream & logStream);

protected:
   RE_RuleBase<RuleVariable>* ruleBase_;
};

//----------------------------------------------------------------------------
template <class RuleVariable>
RE_Engine<RuleVariable>::RE_Engine(RE_RuleBase<RuleVariable>* ruleBase)
                        :ruleBase_(ruleBase)
{
}

//----------------------------------------------------------------------------
template <class RuleVariable>
RE_Engine<RuleVariable>::~RE_Engine()
{
   if (ruleBase_)
   {
      delete ruleBase_;
   }
}

//----------------------------------------------------------------------------
template <class RuleVariable>
bool RE_Engine<RuleVariable>::hasRules()
{
   return ruleBase_->hasRules();
}

//----------------------------------------------------------------------------
template <class RuleVariable>
void RE_Engine<RuleVariable>::logStatistics(ostream & logStream)
{
   ruleBase_->logStatistics(logStream);
}

//}

#endif



