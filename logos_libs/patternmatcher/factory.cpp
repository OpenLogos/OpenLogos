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
//----------------------------------------------------------------------------
// File - Factory.cpp
//
// Class - PM_Factory
//
//----------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/ruleengine/serialconsequent.h>
#include <logos_libs/ruleengine/document.h>
#include <logos_libs/patternmatcher/factory.h>
#include <logos_libs/patternmatcher/antecedent.h>
#include <logos_libs/patternmatcher/protectconsequent.h>
#include <logos_libs/patternmatcher/synchconsequent.h>
#include <logos_libs/patternmatcher/replaceconsequent.h>
#include <logos_libs/patternmatcher/advanceconsequent.h>
#include <logos_libs/translutility/translcommonobjects.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace PatternMatcher {

PM_Factory::PM_Factory(const LgsString& description, SqlConnection* aConnection, bool source)
           :RE_DataBaseFactory<PM_Variable, PM_Engine, RE_SequentialRuleBase<PM_Variable> >(description, aConnection),
            source_(source)
{
}

void PM_Factory::initialize()
{
   pmQuery.Open(getDBConnection(), *(TranslCommonObjects::GetSourceLanguage()),
                *(TranslCommonObjects::GetTargetLanguage()),
				TranslCommonObjects::GetContext()->userSelectedCompanyCodes()->toString());

   pmQuery.executeQuery();
}

RE_Rule<PM_Variable>* PM_Factory::getNextRule()
{
   while (pmQuery.fetchNextRule())
   {
      RuleType ruleType = getRuleType();

      if (((ruleType == ReplaceTarget) && (!source_)) || ((ruleType != ReplaceTarget) && source_))
      {
         // Get the antecedent from the query object
         RE_Antecedent<PM_Variable>* antecedent = new PM_Antecedent(pmQuery.getAntecedent());

         // Get the consequent from the query object
         RE_Consequent<PM_Variable>* consequent = createConsequent(ruleType);

         // Get any documentation that goes with the rule
         RE_Document* document = new RE_Document(pmQuery.getRuleNumber(), pmQuery.getDescription());

         // Now build the rule and return it
         RE_Rule<PM_Variable>* rule = new RE_Rule<PM_Variable>(antecedent, consequent, document, "");

         return rule;
      }
   }
   pmQuery.Close();
   return 0;
}

RE_Consequent<PM_Variable>* PM_Factory::createConsequent(RuleType ruleType)
{
    RE_SerialConsequent<PM_Variable>* serialConsequent = new RE_SerialConsequent<PM_Variable>;

    if (ruleType == Protect)
    {
        serialConsequent->insert(new PM_ProtectConsequent);
    }
    else
    {
        serialConsequent->insert(new PM_ReplaceConsequent(pmQuery.getConsequent()));

        // for source words need to synch
        if (source_)
            serialConsequent->insert(new PM_SynchConsequent);

        if (ruleType == ReplaceSourceProtect)
            serialConsequent->insert(new PM_ProtectConsequent);
    }

    serialConsequent->insert(new PM_AdvanceConsequent);
    return serialConsequent;
}

PM_Factory::RuleType PM_Factory::getRuleType()
{
   switch (pmQuery.getRuleType())
   {
   case 1:
      return ReplaceSource;
   case 2:
      return Protect;
   case 3:
      return ReplaceSourceProtect;
   case 4:
      return ReplaceTarget;
   default:
      return UnknownType;
   }
}

//}

