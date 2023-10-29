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
// Class - EL_FR_Factory
//
//----------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/elision/french/factory_fr.h>
#include <logos_libs/ruleengine/serialandantecedent.h>
#include <logos_libs/ruleengine/serialconsequent.h>
#include <logos_libs/elision/document.h>
#include <logos_libs/elision/french/listantecedent_fr.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Elision { namespace French {

EL_FR_Factory::EL_FR_Factory(istream* input)
    : EL_Factory<EL_FR_Engine>(LgsString("french"), LgsString("french elision"), input)
{
}

EL_FR_Factory::~EL_FR_Factory(){}

void EL_FR_Factory::initialize()
{
    Parent::initialize();

    // do a dynamic cast to get an engine of the right type
    EL_FR_Engine* engine = dynamic_cast<EL_FR_Engine*>(engine_);
    assert(engine != 0);

    // read special word lists
    readWordList(
        LgsString("start-non-compare>"),
        LgsString("end-non-compare>"),
        engine->getWordList(EL_FR_Engine::NonCompare));

    readWordList(
        LgsString("start-no-elision>"),
        LgsString("end-no-elision>"),
        engine->getWordList(EL_FR_Engine::NoElision));

    readWordList(
        LgsString("start-ce-list-1>"),
        LgsString("end-ce-list-1>"),
        engine->getWordList(EL_FR_Engine::CeList1));

    readWordList(
        LgsString("start-ce-list-2>"),
        LgsString("end-ce-list-2>"),
        engine->getWordList(EL_FR_Engine::CeList2));

    readWordList(
        LgsString("start-ce-list-3>"),
        LgsString("end-ce-list-3>"),
        engine->getWordList(EL_FR_Engine::CeList3));

    readWordList(
        LgsString("start-ce-list-4>"),
        LgsString("end-ce-list-4>"),
        engine->getWordList(EL_FR_Engine::CeList4));
}

RE_Antecedent<EL_Variable>* EL_FR_Factory::parseAntecedent()
{
    static LgsString comparitiveKey = "is_non_comparitive";
    static LgsString elisionableKey = "elisionable";
    static LgsString ceList1Key = "is_in_ce_list_1";
    static LgsString ceList2Key = "is_in_ce_list_2";
    static LgsString ceList3Key = "is_in_ce_list_3";
    static LgsString ceList4Key = "is_in_ce_list_4";

    // check for a common antecdent from the parent factory first
    RE_Antecedent<EL_Variable>* antecedent = Parent::parseAntecedent();
    if (antecedent)
        return antecedent;

    while (moreData())
    {
        // skip blank lines and comments
        if (isBlankLine() || isCommentLine())
        {
            readLine();
        }
        else if (search(comparitiveKey))
        {
            antecedent = new EL_FR_ListAntecedent(
                readGroup(), dynamic_cast<EL_FR_Engine*>(engine_), EL_FR_Engine::NonCompare);
            break;
        }
        else if (search(elisionableKey))
        {
            antecedent = new EL_FR_ListAntecedent(
                readGroup(), dynamic_cast<EL_FR_Engine*>(engine_), EL_FR_Engine::NoElision, false);
            break;
        }
        else if (search(ceList1Key))
        {
            antecedent = new EL_FR_ListAntecedent(
                readGroup(), dynamic_cast<EL_FR_Engine*>(engine_), EL_FR_Engine::CeList1);
            break;
        }
        else if (search(ceList2Key))
        {
            antecedent = new EL_FR_ListAntecedent(
                readGroup(), dynamic_cast<EL_FR_Engine*>(engine_), EL_FR_Engine::CeList2);
            break;
        }
        else if (search(ceList3Key))
        {
            antecedent = new EL_FR_ListAntecedent(
                readGroup(), dynamic_cast<EL_FR_Engine*>(engine_), EL_FR_Engine::CeList3);
            break;
        }
        else if (search(ceList4Key))
        {
            antecedent = new EL_FR_ListAntecedent(
                readGroup(), dynamic_cast<EL_FR_Engine*>(engine_), EL_FR_Engine::CeList4);
            break;
        }
        else
            break;
    }

    return antecedent;
}

//}}

