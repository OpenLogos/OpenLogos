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
#ifndef __RuleEngineDataBaseFactory_h__
#define __RuleEngineDataBaseFactory_h__

//----------------------------------------------------------------------------
// File - DataBaseFactory.h
//
// Class - RE_DataBaseFactory - abstract
//
// Description - A factory class to create a rule-engine from a database.
//
//----------------------------------------------------------------------------

#include <logos_libs/ruleengine/rule.h>
#include <logos_libs/ruleengine/factory.h>
#include <logos_libs/utility/stringutil.h>
#include <logos_libs/regex/charutil.h>
#include <logos_libs/sql/sqlconnection.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
//namespace RuleEngine {

template <class RuleVariable_, class Engine_, class RuleBase_>
class RE_DataBaseFactory: public RE_Factory<RuleVariable_>
{
   DisableCopyAssign(RE_DataBaseFactory);

public:
   RE_DataBaseFactory(const LgsString& description, SqlConnection* dbConnection);

   // implementation provided for parent class hook - not to be overridden
   virtual RE_Engine<RuleVariable_>* createRuleEngine();

protected:
   Engine_* engine_;                        // the engine being built by the factory

   // hook methods - must be overridden

   // return next rule from the stream, 0 if at the end
   virtual RE_Rule<RuleVariable_>* getNextRule() = 0;

   // hook methods - may be overridden, but default implementations may be sufficient
   // the default implementation does nothing
   virtual void initialize();

   // display error, the default implementation is to display to cerr
//   virtual void displayError(const char* error, bool displayRule = true) const;

   RuleBase_* getRuleBase();
   SqlConnection* getDBConnection();

private:
    SqlConnection* theConnection_;        // sql database connection
    LgsString description_;                  // description of rule factory - for error reporting
    RuleBase_* ruleBase_;                 // rule-base for the engine to be created
};

//----------------------------------------------------------------------------
template <class RuleVariable_, class Engine_, class RuleBase_>
RE_DataBaseFactory<RuleVariable_, Engine_, RuleBase_>::RE_DataBaseFactory(const LgsString& description,
                                                                          SqlConnection* dbConnection)
                                                      :description_(description),
                                                       theConnection_(dbConnection)
{
}

template <class RuleVariable_, class Engine_, class RuleBase_>
RE_Engine<RuleVariable_>* RE_DataBaseFactory<RuleVariable_, Engine_, RuleBase_>::createRuleEngine()
{
   ruleBase_ = new RuleBase_;
   engine_ = new Engine_(ruleBase_);

   RE_Rule<RuleVariable_>* rule;
   initialize();
   while ((rule = getNextRule()) != 0)
      ruleBase_->insertRule(rule);

   return engine_;
}

template <class RuleVariable_, class Engine_, class RuleBase_>
void RE_DataBaseFactory<RuleVariable_, Engine_, RuleBase_>::initialize()
{
}

template <class RuleVariable_, class Engine_, class RuleBase_>
RuleBase_* RE_DataBaseFactory<RuleVariable_, Engine_, RuleBase_>::getRuleBase()
{
   return ruleBase_;
}

template <class RuleVariable_, class Engine_, class RuleBase_>
SqlConnection* RE_DataBaseFactory<RuleVariable_, Engine_, RuleBase_>::getDBConnection()
{
   return theConnection_;
}

//}

#endif



