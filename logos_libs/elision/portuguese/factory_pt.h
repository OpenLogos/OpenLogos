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
#ifndef __ElisionPortugueseFactory_h__
#define __ElisionPortugueseFactory_h__

//----------------------------------------------------------------------------
// File - Factory.h
//
// Class - EL_PT_Factory
//
// Description - A factory class to create a Portuguese elision rule-engine.
//
//----------------------------------------------------------------------------

#include <logos_libs/elision/factory.h>
#include <logos_libs/elision/portuguese/engine.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Elision { namespace Portuguese {

//----------------------------------------------------------------------------
class EL_PT_Factory: public EL_Factory<EL_PT_Engine>
{
    typedef EL_Factory<EL_PT_Engine> Parent;

    DisableCopyAssign(EL_PT_Factory);

public:
    EL_PT_Factory(istream* input);

protected:
    // read the special list of portuguese words into the Engine's noncompareList
    virtual void initialize();

private:
    // parse the antecedent - return 0 if a syntax error
    virtual RE_Antecedent<EL_Variable>* parseAntecedent();

    // parse the consequent - return 0 if a syntax error
    virtual RE_Consequent<EL_Variable>* parseConsequent();
};

//}}

#endif



