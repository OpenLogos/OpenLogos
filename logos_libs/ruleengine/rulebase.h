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
#ifndef __RuleEngineRuleBase_h__
#define __RuleEngineRuleBase_h__

//----------------------------------------------------------------------------
// File - RuleBase.h
//
// Class - RE_RuleBase - abstract
//
// Description - a collection of rules, with various evaluate
//               methods to apply the rules to a variable.
//
//----------------------------------------------------------------------------

#include <logos_libs/ruleengine/rule.h>
//#include <logos_libs/linguistic/diagnostic.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace RuleEngine {

//----------------------------------------------------------------------------
// enumerations to describe options for the rule base

// how many times to execute a rule on a variable
enum RE_HorizontalOption { RE_horizontalFirst, RE_horizontalAll };

// how many times to scan the rulebase
enum RE_VerticalOption   { RE_verticalFirst, RE_verticalAll };

//----------------------------------------------------------------------------
template <class RuleVariable>
class RE_RuleBase: public Object
{
public:
   // traverse the collection of rules a given number of times,
   // and execute a given number of times over the variable
   bool evaluate(RE_HorizontalOption, RE_VerticalOption, RuleVariable&);

   // insert a rule in the container
   virtual void insertRule(RE_Rule<RuleVariable>*) = 0;

   // indicate if there are any rules in the rule base
   virtual bool hasRules() = 0;

   // Function to Log Rule Statistics
   virtual void logStatistics(ostream & logStream);

protected:
   // get the next matching rule for the variable
   // for some sub-classes the rule-variable will be ignored and the container will be traversed
   // for some sub-classes the rule-variable will contain a unique or non-unique key and this will be
   //     used to find the matching set of rules, or single matching rule
   virtual RE_Rule<RuleVariable>* next(const RuleVariable&) = 0;
   virtual RE_Rule<RuleVariable>* next(void) = 0;

   // reset the iterator over matching rules
   virtual void reset() = 0;
};

//----------------------------------------------------------------------------
template <class RuleVariable>
bool RE_RuleBase<RuleVariable>::evaluate(RE_HorizontalOption horizontalOption,
                                         RE_VerticalOption verticalOption,
                                         RuleVariable& variable)
{
   bool ruleFired = false;       // true iff any rule returned true on execute

   RE_Rule<RuleVariable>* rule;
   reset();

   while ((rule = next(variable)) != 0)
   {
      for (;;)
      {
         if (rule->evaluateAndFire(variable))
         {
            ruleFired = true;
            if (horizontalOption == RE_horizontalFirst)
               break;
         }
         else
            break;
      }
      if (verticalOption == RE_verticalFirst)
         break;
   }
   return ruleFired;
}

//----------------------------------------------------------------------------
template <class RuleVariable>
void RE_RuleBase<RuleVariable>::logStatistics(ostream & logStream)
{
   RE_Rule<RuleVariable>* rule;
   reset();
   logStream << "    MATCH_COUNT     SEARCH_COUNT  CUMULATIVE_TIME(ms)  AVERAGE_TIME/RULE(ms)  PREPROCESS_TIME(ms)  RULE_ID" << endl;

   unsigned long totalRuleTime = 0;
   unsigned long totalPreprocessTime = 0;
   while ((rule = next()) != 0)
   {
		rule->logStatistics(logStream);
      totalRuleTime += rule->cumulativeTime();
      totalPreprocessTime += rule->preprocessTime();
   }

   char lineBuffer[320];
   sprintf(lineBuffer, "TOTAL_TIMEs(ms) -%36d%44d", totalRuleTime, totalPreprocessTime);
   logStream << endl << lineBuffer << endl;
   unsigned long remainder = totalRuleTime % 1000;
   unsigned long seconds = (totalRuleTime / 1000) % 60;
   unsigned long minutes = (totalRuleTime / 60000) % 60;
   unsigned long hours = totalRuleTime / 3600000;
   sprintf(lineBuffer, "TOTAL_CUMULATIVE_TIME(h:m:s) -           %.2d:%.2d:%.2d.%.3d", hours, minutes, seconds, remainder);
   logStream << lineBuffer << endl;

   remainder = totalPreprocessTime % 1000;
   seconds = (totalPreprocessTime / 1000) % 60;
   minutes = (totalPreprocessTime / 60000) % 60;
   hours = totalPreprocessTime / 3600000;
   sprintf(lineBuffer, "TOTAL_PREPROCESS_TIME(h:m:s) -                                                       %.2d:%.2d:%.2d.%.3d", hours, minutes, seconds, remainder);
   logStream << lineBuffer << endl;
}

//}

#endif



