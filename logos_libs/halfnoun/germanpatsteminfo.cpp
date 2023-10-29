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
//---------------------------------------------------------------------
// File - GermanPatStemInfo.cpp
//
// class - GermanPatStemInfo
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>

#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/halfnoun/germanpatsteminfo.h>
#include <logos_libs/halfnoun/timer.h>
#include <logos_libs/halfnoun/germanwordclass.h>

GermanPatStemInfo* GermanPatStemInfo::thePatStemInfo_s;
LgsSet(GermanPatStem) GermanPatStemInfo::patStemList_s;

void GermanPatStemInfo::initialize(SqlConnection* connect)
{
    readPatStems(connect);

    thePatStemInfo_s = new GermanPatStemInfo();
}

GermanPatStemInfo& GermanPatStemInfo::singleton()
{
    assert(thePatStemInfo_s != 0);

    return *thePatStemInfo_s;
}

void GermanPatStemInfo::readPatStems(SqlConnection* connect)
{
    Timer timer(LgsString("readPatStems"));

    LgsString statementText =
        "select pat_number, source_stem_number "
        "from german_pat_stem ";

    SqlStatement* statement = connect->CreateStatement();
    assert(statement != 0);

    statement->AddToCommandString(statementText);
    statement->Parse();

    SqlColumn* colPat   = statement->BindOutputColumn(1, SqlColumn::Integer);
    SqlColumn* colStem  = statement->BindOutputColumn(2, SqlColumn::Integer);

    statement->Execute();

    for (;;)
    {
        if (!statement->Fetch())
            break;

        int pat = colPat->AsInteger();
        int stem = colStem->AsInteger();
        patStemList_s.insert(GermanPatStem(pat, stem));
    }
}

bool GermanPatStemInfo::isNonHeadPatStem(GermanWordClass::PartOfSpeech partOfSpeech, int pat, int stem) const
{
    // don't need to check pat/stem for null pat or stem,
    // or for stored half nouns and aritmates,
    // or for canonical form adjectives and nouns
    if ((pat == 0 || stem == 0) && partOfSpeech == GermanWordClass::verb)
    {
         return false;
    }
    if (pat == 0 || stem == 0 || partOfSpeech == GermanWordClass::halfnoun ||
        partOfSpeech == GermanWordClass::arith14 || partOfSpeech == GermanWordClass::arith16 ||
       ((partOfSpeech == GermanWordClass::noun || partOfSpeech == GermanWordClass::adj) && stem == 1))
    {
        return true;
    }

    // lookup pat/stem in cached table
    LgsSet(GermanPatStem)::const_iterator iter = patStemList_s.find(
        GermanPatStem(pat, stem));
    return (iter != patStemList_s.end());
}


