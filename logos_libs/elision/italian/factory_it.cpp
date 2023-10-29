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
// Class - EL_IT_Factory
//
//----------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/elision/italian/factory_it.h>
#include <logos_libs/ruleengine/serialandantecedent.h>
#include <logos_libs/ruleengine/serialconsequent.h>
#include <logos_libs/elision/document.h>
#include <logos_libs/elision/italian/listantecedent_it.h>
#include <logos_libs/elision/italian/startantecedent.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Elision { namespace Italian {

EL_IT_Factory::EL_IT_Factory(istream* input)
    : EL_Factory<EL_IT_Engine>(LgsString("italian"), LgsString("italian elision"), input)
{
}

void EL_IT_Factory::initialize()
{
    Parent::initialize();

    // do a dynamic cast to get an engine of the right type
    EL_IT_Engine* engine = dynamic_cast<EL_IT_Engine*>(engine_);
    assert(engine != 0);

    // read special word list
    readWordList(LgsString("start-non-compare>"), LgsString("end-non-compare>"), engine->getWordList());
}

RE_Antecedent<EL_Variable>* EL_IT_Factory::parseAntecedent()
{
    static LgsString comparitiveKey = "is_non_comparitive";
    static LgsString startsVowelKey = "starts_vowel";
    static LgsString startsConsSpecialKey = "starts_special";
    static LgsString startsConsNormalKey = "starts_normal";
    static LgsString startsVowelOrNormalKey = "starts_vowel_or_normal";
    static LgsString startsVowelOrSpecialKey = "starts_vowel_or_special";

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
            antecedent = new EL_IT_ListAntecedent(readGroup(), dynamic_cast<EL_IT_Engine*>(engine_));
            break;
        }
        // order of searches is important since some keys are prefixes of other
        else if (search(startsVowelOrNormalKey))
        {
            antecedent = new EL_IT_StartAntecedent(readGroup(), EL_IT_StartAntecedent::VowelOrNormal);
            break;
        }
        else if (search(startsVowelOrSpecialKey))
        {
            antecedent = new EL_IT_StartAntecedent(readGroup(), EL_IT_StartAntecedent::VowelOrSpecial);
            break;
        }
        else if (search(startsVowelKey))
        {
            antecedent = new EL_IT_StartAntecedent(readGroup(), EL_IT_StartAntecedent::Vowel);
            break;
        }
        else if (search(startsConsSpecialKey))
        {
            antecedent = new EL_IT_StartAntecedent(readGroup(), EL_IT_StartAntecedent::ConsSpecial);
            break;
        }
        else if (search(startsConsNormalKey))
        {
            antecedent = new EL_IT_StartAntecedent(readGroup(), EL_IT_StartAntecedent::ConsNormal);
            break;
        }
        else
            break;
    }

    return antecedent;
}

//}}

