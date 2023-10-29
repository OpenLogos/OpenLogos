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
// Class - EL_SP_Factory
//
//----------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/elision/spanish/factory_sp.h>
#include <logos_libs/ruleengine/serialandantecedent.h>
#include <logos_libs/ruleengine/serialconsequent.h>
#include <logos_libs/elision/replaceconsequent.h>
#include <logos_libs/elision/document.h>
#include <logos_libs/elision/spanish/vowelantecedent.h>
#include <logos_libs/elision/spanish/accentantecedent.h>
#include <logos_libs/elision/spanish/comparitiveantecedent.h>
#include <logos_libs/elision/spanish/accentconsequent.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Elision { namespace Spanish {

EL_SP_Factory::EL_SP_Factory(istream* input)
    : EL_Factory<EL_SP_Engine>(LgsString("spanish"), LgsString("spanish elision"), input)
{
}

void EL_SP_Factory::initialize()
{
    Parent::initialize();

    // do a dynamic cast to get an engine of the right type
    EL_SP_Engine* engine = dynamic_cast<EL_SP_Engine*>(engine_);
    assert(engine != 0);

    readWordList(LgsString("start-non-compare>"), LgsString("end-non-compare>"), engine->getWordList());
}

RE_Antecedent<EL_Variable>* EL_SP_Factory::parseAntecedent()
{
    static LgsString oneVowelKey = "has_one_vowel";
    static LgsString twoVowelsKey = "has_two_vowels";
    static LgsString moreVowelsKey = "has_more_than_one_vowel";
    static LgsString noAccentKey = "has_no_accent";
    static LgsString comparitiveKey = "is_non_comparitive";

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
        else if (search(oneVowelKey))
        {
            antecedent = new EL_SP_VowelAntecedent(readGroup(), EL_SP_VowelAntecedent::one);
            break;
        }
        else if (search(twoVowelsKey))
        {
            antecedent = new EL_SP_VowelAntecedent(readGroup(), EL_SP_VowelAntecedent::two);
            break;
        }
        else if (search(moreVowelsKey))
        {
            antecedent = new EL_SP_VowelAntecedent(readGroup(), EL_SP_VowelAntecedent::moreThanOne);
            break;
        }
        else if (search(noAccentKey))
        {
            antecedent = new EL_SP_AccentAntecedent(readGroup());
            break;
        }
        else if (search(comparitiveKey))
        {
            antecedent = new EL_SP_ComparitiveAntecedent(readGroup(), dynamic_cast<EL_SP_Engine*>(engine_));
            break;
        }
        else
            break;
    }

    return antecedent;
}

RE_Consequent<EL_Variable>* EL_SP_Factory::parseConsequent()
{
    static LgsString accentLastMaxKey = "accent_last_max_one";
    static LgsString accentSecondLastMaxKey = "accent_second_last_max_one";
    static LgsString accentLastKey = "accent_last";
    static LgsString accentSecondLastKey = "accent_second_last";

    // check for a common antecdent from the parent factory first
    RE_Consequent<EL_Variable>* consequent = Parent::parseConsequent();
    if (consequent)
        return consequent;

    while (moreData())
    {
        // skip blank lines and comments
        if (isBlankLine() || isCommentLine())
        {
            readLine();
        }
        // note - these 2 searches must be before the next 2 since the next 2
        //        are substrings of these 2
        else if (search(accentLastMaxKey))
        {
            consequent = new EL_SP_AccentConsequent(readGroup(), 1, true);
            break;
        }
        else if (search(accentSecondLastMaxKey))
        {
            consequent = new EL_SP_AccentConsequent(readGroup(), 2, true);
            break;
        }
        else if (search(accentLastKey))
        {
            consequent = new EL_SP_AccentConsequent(readGroup(), 1, false);
            break;
        }
        else if (search(accentSecondLastKey))
        {
            consequent = new EL_SP_AccentConsequent(readGroup(), 2, false);
            break;
        }
        else
            break;
    }

    return consequent;
}

//}}
