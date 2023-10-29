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
// File - GermanSuffixInfo.cpp
//
// class - GermanSuffixInfo
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>

#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/halfnoun/germansuffixinfo.h>
#include <logos_libs/halfnoun/timer.h>
#include <logos_libs/halfnoun/logfile.h>

GermanSuffixInfo* GermanSuffixInfo::theSuffixInfo_s;
LgsMap(GermanPatStem, GermanSuffixFlag) GermanSuffixInfo::mapPatStem_s;

void GermanSuffixInfo::initialize(SqlConnection* connect)
{
    readSuffixesForPatStem(connect);
    theSuffixInfo_s = new GermanSuffixInfo();
}

GermanSuffixInfo& GermanSuffixInfo::singleton()
{
    assert(theSuffixInfo_s != 0);

    return *theSuffixInfo_s;
}

void GermanSuffixInfo::readSuffixesForPatStem(SqlConnection* connect)
{
    Timer timer(LgsString("readSuffixesForPatStem"));

    LgsString statementText =
        "select pat_number, stem_number, ending "
        "from german_ending ";

    SqlStatement* statement = connect->CreateStatement();
    assert(statement != 0);

    statement->AddToCommandString(statementText);
    statement->Parse();

    SqlColumn* colPat   = statement->BindOutputColumn(1, SqlColumn::Integer);
    SqlColumn* colStem  = statement->BindOutputColumn(2, SqlColumn::Integer);
    SqlColumn* colEnding  = statement->BindOutputColumn(3, SqlColumn::StringType);

    statement->Execute();

    for (;;)
    {
        if (!statement->Fetch())
            break;

        // create patStem key object
        int pat = colPat->AsInteger();
        int stem = colStem->AsInteger();
        GermanPatStem key(pat, stem);

        // search for key
        LgsMap(GermanPatStem, GermanSuffixFlag)::iterator iter = mapPatStem_s.find(key);

        if (iter == mapPatStem_s.end())
        {
            // not found - so insert
            pair<LgsMap(GermanPatStem, GermanSuffixFlag)::iterator, bool> rc =
                mapPatStem_s.insert(pair<GermanPatStem, GermanSuffixFlag>(key, GermanSuffixFlag()));
            assert(rc.second);
            iter = rc.first;
        }


        // update suffix flag
        LgsString ending = colEnding->AsString();
        if (ending != LgsString("'"))
        {
            try
            {
                SuffixFlagValue index = GermanSuffixFlag::indexFromSuffix(ending);
                iter->second.set(index);
            }
            catch(const LgsString&)
            {
                // log error
                LogFile::singleton().printDateTime()
                    << "ReadSuffixesForPatStem: invalid suffix in derived form table";
                LogFile::singleton().skipDateTime()
                    << "Pat/stem=" << pat << "/" << stem
                    << ", suffix=" << ending;
                LogFile::singleton().skipDateTime()
                    << "Suffix ignored "
                    << endl;
            }
        }
    }
}

GermanSuffixFlag GermanSuffixInfo::getSuffixesByPatStem(int pat, int stem) const
{
    if (pat != 0 && stem != 0)
    {
        LgsMap(GermanPatStem, GermanSuffixFlag)::const_iterator iter = mapPatStem_s.find(
            GermanPatStem(pat, stem));
        if (iter != mapPatStem_s.end())
            return iter->second;
        else
        {
            // no such pat/stem
            // log error
            LogFile::singleton().printDateTime()
                << "Update: no suffix data for pat/stem=" << pat << "/" << stem
                << ". Suffixes ignored " << endl;
        }
    }

    // no such pat/stem or null pat or stem
    GermanSuffixFlag flag;
    flag.set(suf_none);
    return flag;
}


