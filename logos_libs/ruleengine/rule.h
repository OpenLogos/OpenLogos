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
#ifndef __RuleEngineRule_h__
#define __RuleEngineRule_h__

//----------------------------------------------------------------------------
// File - Rule.h
//
// Class - RE_Rule
//
// Description - it its simplest form, a Rule object works like this:
//
//       1. It has an antecedent component ("if" condition)
//       2. It has a consequent component ("action")
//       3. It receives a rule variable and if the rule variable is
//          true for antecedent component, it fires the consequent component.
//
//----------------------------------------------------------------------------

#include <logos_libs/ruleengine/document.h>
#include <logos_libs/ruleengine/antecedent.h>
#include <logos_libs/ruleengine/consequent.h>
#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <lgs_db_io/lgsdbcommonobjects.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace RuleEngine {

template <class RuleVariable>
class RE_Rule: public Object
{
   DisableCopyAssign(RE_Rule);

public:
   RE_Rule(RE_Antecedent<RuleVariable>* antecedent, RE_Consequent<RuleVariable>* consequent,
           RE_Document* document, const LgsString & ruleName);
   virtual ~RE_Rule();

   // tests the object's Antecedent component.
   bool evaluate(RuleVariable&);

   // fires the object's Consequent.
   void fire(RuleVariable&) const;

   // tests the object's Antecedent and if true, fires the object's Consequent.
   bool evaluateAndFire(RuleVariable&);

   const RE_Antecedent<RuleVariable>* getAntecedent();
   const RE_Consequent<RuleVariable>* getConsequent();
   const RE_Document* getDocument();
   void logStatistics(ostream & logStream);
   void incMatchCount(void);
   void incTotalCount(void);
   void decTotalCount(void);
   unsigned long cumulativeTime(void);
   unsigned long preprocessTime(void);

private:
   RE_Antecedent<RuleVariable>* antecedent_;
   RE_Consequent<RuleVariable>* consequent_;
   RE_Document* document_;
   LgsString ruleName_;
   unsigned long cumulativeTimer_;
   unsigned long preprocessTimer_;
   unsigned short matchCount_;
   unsigned long totalCount_;
};

//----------------------------------------------------------------------------
template <class RuleVariable>
RE_Rule<RuleVariable>::RE_Rule(RE_Antecedent<RuleVariable>* antecedent,
                               RE_Consequent<RuleVariable>* consequent,
                               RE_Document* document, const LgsString & ruleName)
                      :antecedent_(antecedent),
                       consequent_(consequent),
                       document_(document),
                       ruleName_(ruleName),
                       cumulativeTimer_(0),
                       preprocessTimer_(0),
                       matchCount_(0),
                       totalCount_(0)
{
    antecedent->rule_ = this;
    consequent->rule_ = this;
}

//----------------------------------------------------------------------------
template <class RuleVariable>
RE_Rule<RuleVariable>::~RE_Rule() {
	if(antecedent_)
		delete antecedent_;
	if(consequent_)
		delete consequent_;
	if(document_)
		delete document_;
}

//----------------------------------------------------------------------------
template <class RuleVariable>
bool RE_Rule<RuleVariable>::evaluate(RuleVariable& v)
{
  assert(antecedent_ != 0);
  bool retVal = false;
   
  if (LgsDBCommonObjects::GetJobControlArguments().TimeFlag())
    {
#ifdef TIME_RULE_APPLICATION
      HANDLE threadHandle_A = GetCurrentThread();
      FILETIME creationTime_A;
      FILETIME exitTime_A;
      FILETIME kernelTime_A;
      FILETIME userTime_A;
      __uint64 kernelTimeStart_A = 0;
      __uint64 userTimeStart_A = 0;
      __uint64 kernelMatchTime_A = 0;
      __uint64 userMatchTime_A = 0;
      __uint64 matchTime_A = 0;

      GetThreadTimes(threadHandle_A, &creationTime_A, &exitTime_A, &kernelTime_A, &userTime_A);
      ConvertTo64Bit(kernelTimeStart_A, kernelTime_A);
      ConvertTo64Bit(userTimeStart_A, userTime_A);
#endif
      bool attemptMatch = antecedent_->isViableMatch(v);
#ifdef TIME_RULE_APPLICATION
      GetThreadTimes(threadHandle_A, &creationTime_A, &exitTime_A, &kernelTime_A, &userTime_A);
      ConvertTo64Bit(kernelMatchTime_A, kernelTime_A);
      ConvertTo64Bit(userMatchTime_A, userTime_A);
      kernelMatchTime_A -= kernelTimeStart_A;
      userMatchTime_A -= userTimeStart_A;
      matchTime_A = (kernelMatchTime_A + userMatchTime_A) / 10000;

      preprocessTimer_ += matchTime_A;
#endif

      if (attemptMatch)
        {
#ifdef TIME_RULE_APPLICATION
          HANDLE threadHandle_B = GetCurrentThread();
          FILETIME creationTime_B;
          FILETIME exitTime_B;
          FILETIME kernelTime_B;
          FILETIME userTime_B;
          __uint64 kernelTimeStart_B = 0;
          __uint64 userTimeStart_B = 0;
          __uint64 kernelMatchTime_B = 0;
          __uint64 userMatchTime_B = 0;
          __uint64 matchTime_B = 0;

          GetThreadTimes(threadHandle_B, &creationTime_B, &exitTime_B, &kernelTime_B, &userTime_B);
          ConvertTo64Bit(kernelTimeStart_B, kernelTime_B);
          ConvertTo64Bit(userTimeStart_B, userTime_B);
#endif
          retVal = antecedent_->evaluate(v);
#ifdef TIME_RULE_APPLICATION
          GetThreadTimes(threadHandle_B, &creationTime_B, &exitTime_B, &kernelTime_B, &userTime_B);
          ConvertTo64Bit(kernelMatchTime_B, kernelTime_B);
          ConvertTo64Bit(userMatchTime_B, userTime_B);
          kernelMatchTime_B -= kernelTimeStart_B;
          userMatchTime_B -= userTimeStart_B;
          matchTime_B = (kernelMatchTime_B + userMatchTime_B) / 10000;

          cumulativeTimer_ += matchTime_B;
#endif
          totalCount_++;
        }
    }
  else if (antecedent_->isViableMatch(v))
    {
      retVal = antecedent_->evaluate(v);
    }
  return retVal;
}

template <class RuleVariable>
void RE_Rule<RuleVariable>::fire(RuleVariable& v) const
{
   assert(consequent_ != 0);
   consequent_->fire(v);
}

template <class RuleVariable>
bool RE_Rule<RuleVariable>::evaluateAndFire(RuleVariable& v)
{
   if (evaluate(v))
   {
      matchCount_++;
      fire(v);
      return true;
   }
   return false;
}

template <class RuleVariable>
const RE_Antecedent<RuleVariable>* RE_Rule<RuleVariable>::getAntecedent()
{
   return antecedent_;
}

template <class RuleVariable>
const RE_Consequent<RuleVariable>* RE_Rule<RuleVariable>::getConsequent()
{
   return consequent_;
}

template <class RuleVariable>
const RE_Document* RE_Rule<RuleVariable>::getDocument()
{
    return document_;
}

template <class RuleVariable>
void RE_Rule<RuleVariable>::incMatchCount()
{
    matchCount_++;
}

template <class RuleVariable>
void RE_Rule<RuleVariable>::incTotalCount()
{
    totalCount_++;
}


template <class RuleVariable>
void RE_Rule<RuleVariable>::decTotalCount()
{
    totalCount_--;
}


template <class RuleVariable>
void RE_Rule<RuleVariable>::logStatistics(ostream & logStream)
{
   char lineBuffer[320];
   sprintf(lineBuffer, "%15d  %15d  %19d  %21d  %19d  %s", matchCount_, totalCount_, cumulativeTimer_,
                                                           (totalCount_)? cumulativeTimer_/totalCount_:0,
                                                           preprocessTimer_, ruleName_.c_str());
   logStream << lineBuffer << endl;
}

template <class RuleVariable>
unsigned long RE_Rule<RuleVariable>::cumulativeTime(void)
{
   return cumulativeTimer_;
}

template <class RuleVariable>
unsigned long RE_Rule<RuleVariable>::preprocessTime(void)
{
   return preprocessTimer_;
}

//}

#endif



