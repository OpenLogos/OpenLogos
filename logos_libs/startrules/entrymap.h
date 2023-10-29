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
#ifndef __StartEntryMap_h__
#define __StartEntryMap_h__

//----------------------------------------------------------------------------
// File - EntryMap.h
//
// Class - ST_EntryMap - utility class - static methods only
//
// Description - A mapping between TokenType and Dictionary Entry
//               outputs the appropriate dictionary entry for a token-type.
//
//----------------------------------------------------------------------------

#include <logos_libs/linguistic/lookuptokentype.h>
#include <logos_libs/linguistic/ldictionaryentry.h>
#include <logos_libs/startrules/startruleslocale.h>
#include <logos_libs/linguistic/lsemantosyntacticunit.h>

class ST_EntryMap
{
public:
    static void Initialize();
    static void Cleanup();
    static LDictionaryEntry* getEntryForToken(LookupTokenType::Type tokenType, LLanguage::ID language);

private:
    struct DictionaryEntrySubset
    {
        LookupTokenType::Type tokenType_;
        LLanguage::ID language_;
        struct
        {
            int wordClass_;
            int superset_;
            int set_;
            int subset_;
            int form_;
            int overflow2b_;
            int overflow3b_;
            int gender_;
        } ssu[3];
    };

    static DictionaryEntrySubset entrySubset_[];
    static LgsMap(int, LDictionaryEntry*) dictionaryEntry_;

    // make a single key out of a pair of values
    static int makeInt(LookupTokenType::Type tokenType, LLanguage::ID language);

    static void insert(const DictionaryEntrySubset* entrySubset);
};

inline int ST_EntryMap::makeInt(LookupTokenType::Type tokenType, LLanguage::ID language)
{
    return int(language) * 1000 + int(tokenType);
}

inline LDictionaryEntry* ST_EntryMap::getEntryForToken(
    LookupTokenType::Type tokenType, LLanguage::ID language)
{
    LgsMap(int, LDictionaryEntry*)::iterator iter =
        dictionaryEntry_.find(makeInt(tokenType, language));
    if (iter == dictionaryEntry_.end())
        return 0;
    return iter->second;
}

#endif



