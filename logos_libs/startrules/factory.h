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
#ifndef __StartFactory_h__
#define __StartFactory_h__

//----------------------------------------------------------------------------
// File - Factory.h
//
// Class - ST_Factory
//
// Description - A factory class to create a pattern matcher rule-engine.
//
//----------------------------------------------------------------------------

#include <logos_libs/ruleengine/streamfactory.h>
#include <logos_libs/ruleengine/sequentialrulebase.h>
#include <logos_libs/startrules/engine.h>
#include <logos_libs/startrules/variable.h>
#include <logos_libs/startrules/antecedent.h>
#include <logos_libs/startrules/consequent.h>
#include <logos_libs/startrules/action.h>
#include <logos_libs/startrules/startruleslocale.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Start {

class ST_Factory: public RE_StreamFactory<
                  ST_Variable, ST_Engine, RE_SequentialRuleBase<ST_Variable> >
{
   DisableCopyAssign(ST_Factory);

public:
    // constructor
    // input is the input stream.
    ST_Factory(const LgsString& description, istream* input, const ST_Locale* sourceLocale,
               const ST_Locale* targetLocale, char quote = '`');

protected:
   // hook method
   // return next rule from the stream, 0 if at the end
   virtual RE_Rule<ST_Variable>* getNextRule();

private:
   LgsMap(LgsString, LgsString) macros_;
   const ST_Locale* sourceLocale_;
   const ST_Locale* targetLocale_;
   char quote_;

   // parse the antecedent - can be on one or many lines
   ST_Antecedent* parseAntecedent(RegularExpression::Borders borders,
                                  ST_Antecedent::ST_Type antecedentType);

   // lookup the id and construct a suitable action
   RE_Consequent<class ST_Variable>* makeAction(const LgsString& id);

   // macro processing within the pattern
   void replaceMacros(LgsString& pattern);
};

//}

#endif



