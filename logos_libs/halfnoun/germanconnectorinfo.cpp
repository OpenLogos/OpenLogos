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
// File - GermanConnectorInfo.cpp
//
// class - GermanConnectorInfo
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>

#include <logos_libs/utility/iniparser.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/halfnoun/germanconnectorinfo.h>
#include <logos_libs/halfnoun/germanutil.h>
#include <logos_libs/halfnoun/timer.h>
#include <logos_libs/halfnoun/logfile.h>
#include <logos_libs/halfnoun/germanbufferlength.h>

GermanConnectorInfo* GermanConnectorInfo::theConnectorInfo_s;
//char** GermanConnectorInfo::sSuffix_s;
//char** GermanConnectorInfo::sSuffixException_s;
//char** GermanConnectorInfo::noSuffix_s;
//char** GermanConnectorInfo::noSuffixException_s;

const short GermanConnectorInfo::SUFFIX_INDEX = 0;
const short GermanConnectorInfo::PAT_INDEX = 1;
const short GermanConnectorInfo::STEM_INDEX = 2;
const short GermanConnectorInfo::CONNECTOR_START_INDEX = 3;

GermanConnectorFlag GermanConnectorInfo::nounDefaultConn;
GermanConnectorFlag GermanConnectorInfo::verbDefaultConn;
GermanConnectorFlag GermanConnectorInfo::adjDefaultConn;

GermanSuffixSet GermanConnectorInfo::m_suffixConnectorList;
LgsMap(GermanWordPatStem,   GermanConnectorFlag) GermanConnectorInfo::mapMorphology_s;
LgsMap(GermanPatStemLetter, GermanConnectorFlag) GermanConnectorInfo::mapPatStem_s;

void GermanConnectorInfo::initialize(const LgsString& iniFileName, SqlConnection* connect)
{
   const char* name = iniFileName.c_str();

	readDefaultConnector(name, "default_conn", "noun_conn", nounDefaultConn);
	readDefaultConnector(name, "default_conn", "verb_conn", verbDefaultConn);
	readDefaultConnector(name, "default_conn", "adj_conn", adjDefaultConn);

   // read connectors for all suffixes having a prticular pat/stem value

   LgsString *suffixConnectorList = 0;
   readIni(name, "connectors", suffixConnectorList);
   if (suffixConnectorList)
   {
      while (!suffixConnectorList->empty())
      {
         LgsVector(LgsString) suffixParameters;
         StringUtil::parseInto(*suffixConnectorList, suffixParameters, ','); 
         int patValue = atoi(suffixParameters[PAT_INDEX].c_str());
         int stemValue = atoi(suffixParameters[STEM_INDEX].c_str());
		 if (suffixParameters[SUFFIX_INDEX] == "t" && patValue == 443)
		 {
			int iiii = 1;
		 }
         GermanConnectorFlag connFlag;
         int noSuffixParameters = suffixParameters.size();
         short i = CONNECTOR_START_INDEX;
         for (int connectorWeight = noSuffixParameters-CONNECTOR_START_INDEX; i < noSuffixParameters; i++, connectorWeight--)
         {
            connFlag.set(connFlag.indexFromConnector(suffixParameters[i]), connectorWeight);      
         }
         GermanSuffixPatStemConn suffixConn(suffixParameters[SUFFIX_INDEX], 
                               patValue, stemValue, connFlag);
 
         m_suffixConnectorList.insert(suffixConn);
         suffixConnectorList++;
      }
   }

   // readIni(name, "connector_s_exceptions", sSuffixException_s);
   // readIni(name, "connector_none", noSuffix_s);
   // readIni(name, "connector_none_exceptions", noSuffixException_s);

    readConnectorsForMorphology(connect);
    readConnectorsForPatStem(connect);

    theConnectorInfo_s = new GermanConnectorInfo();
}

void GermanConnectorInfo::readDefaultConnector(const char * fileName, 
											   const char * section, 
											   const char * wordClass, 
											   GermanConnectorFlag & connFlag)
{
    IniParser parser;
    parser.open(fileName, section);
	const Argument & arg = parser.GetArgument(wordClass);
	if (!arg.IsNull())
	{
		LgsVector(LgsString) arrayValues;
		StringUtil::parseInto(arg.Value(), arrayValues, ',');
      short connectorWeight = arrayValues.size();
		LgsVector(LgsString)::iterator endIter = arrayValues.end();
		for (LgsVector(LgsString)::iterator j = arrayValues.begin();
			 j != endIter;
			 j++, connectorWeight--)
		{
			connFlag.set(connFlag.indexFromConnector(*j), connectorWeight);
		}
	}
}

GermanConnectorInfo& GermanConnectorInfo::singleton()
{
    assert(theConnectorInfo_s != 0);

    return *theConnectorInfo_s;
}

void GermanConnectorInfo::readIni(const char* iniFileName, char* section, LgsString *& endingList)
{
    IniParser parser;
    parser.open(iniFileName, section);
    int count = parser.AsInteger("count");
    assert(count >= 0);

    // don't log allocation, since this memory does not need to be freed
    // it is used till the end of the program
    endingList = new LgsString [count + 1];
    LgsString* current = endingList;
    for (int i = 0; i < count; i++)
    {
        char key[4];
        sprintf(key, "%02d", i + 1);
        *current++ = parser.Value(key);
    }
    *current = "";
}

void GermanConnectorInfo::readConnectorsForMorphology(SqlConnection* connect)
{
    Timer timer(LgsString("readConnectorsForMorphology"));

    LgsString statementText =
        "select "
        "company_code, word, pat_number, source_stem_number, "
        "no_connector, s_connector, es_connector, er_connector, "
        "en_connector, n_connector, nen_connector, e_connector "
        "from german_morphology_conn";

    SqlStatement* statement = connect->CreateStatement();
    assert(statement != 0);

    statement->AddToCommandString(statementText);
    statement->Parse();

    SqlColumn* colCompany   = statement->BindOutputColumn(1,  SqlColumn::StringType);
    SqlColumn* colWord      = statement->BindOutputColumn(2,  SqlColumn::StringType);
    SqlColumn* colPat       = statement->BindOutputColumn(3,  SqlColumn::Integer);
    SqlColumn* colStem      = statement->BindOutputColumn(4,  SqlColumn::Integer);
    SqlColumn* col_no_Conn  = statement->BindOutputColumn(5,  SqlColumn::StringType);
    SqlColumn* col_s_Conn   = statement->BindOutputColumn(6,  SqlColumn::StringType);
    SqlColumn* col_es_Conn  = statement->BindOutputColumn(7,  SqlColumn::StringType);
    SqlColumn* col_er_Conn  = statement->BindOutputColumn(8,  SqlColumn::StringType);
    SqlColumn* col_en_Conn  = statement->BindOutputColumn(9,  SqlColumn::StringType);
    SqlColumn* col_n_Conn   = statement->BindOutputColumn(10, SqlColumn::StringType);
    SqlColumn* col_nen_Conn = statement->BindOutputColumn(11, SqlColumn::StringType);
    SqlColumn* col_e_Conn   = statement->BindOutputColumn(12, SqlColumn::StringType);

    statement->Execute();

    for (;;)
    {
        if (!statement->Fetch())
            break;

        char buffer[BUFFER_LENGTH];
        GermanUtil::normalize(colWord->AsCharArray(), buffer);

        GermanWordPatStem key(colCompany->AsString(), buffer,
            colPat->AsInteger(), colStem->AsInteger());

        GermanConnectorFlag flag;
        FLD_VALUE_TYPE connectorWeight = 0;
        if (connectorWeight = col_no_Conn->AsIntegerFromString())
                flag.set(conn_none, connectorWeight);
        if (connectorWeight = col_s_Conn->AsIntegerFromString())
                flag.set(conn_s, connectorWeight);
        if (connectorWeight = col_es_Conn->AsIntegerFromString())
                flag.set(conn_es, connectorWeight);
        if (connectorWeight = col_er_Conn->AsIntegerFromString())
                flag.set(conn_er, connectorWeight);
        if (connectorWeight = col_en_Conn->AsIntegerFromString())
                flag.set(conn_en, connectorWeight);
        if (connectorWeight = col_n_Conn->AsIntegerFromString())
                flag.set(conn_n, connectorWeight);
        if (connectorWeight = col_nen_Conn->AsIntegerFromString())
                flag.set(conn_nen, connectorWeight);
        if (connectorWeight = col_e_Conn->AsIntegerFromString())
                flag.set(conn_e, connectorWeight);

        // insert record into map
        mapMorphology_s[key] = flag;
    }
}

void GermanConnectorInfo::readConnectorsForPatStem(SqlConnection* connect)
{
    Timer timer(LgsString("readConnectorsForPatStem"));

    LgsString statementText =
        "select "
        "pat_number, source_stem_number, letters, "
        "no_connector, s_connector, es_connector, er_connector, "
        "en_connector, n_connector, nen_connector, e_connector "
        "from german_pat_stem_conn ";

    SqlStatement* statement = connect->CreateStatement();
    assert(statement != 0);

    statement->AddToCommandString(statementText);
    statement->Parse();

    SqlColumn* colPat       = statement->BindOutputColumn(1,  SqlColumn::Integer);
    SqlColumn* colStem      = statement->BindOutputColumn(2,  SqlColumn::Integer);
    SqlColumn* colLetters   = statement->BindOutputColumn(3,  SqlColumn::StringType);
    SqlColumn* col_no_Conn  = statement->BindOutputColumn(4,  SqlColumn::StringType);
    SqlColumn* col_s_Conn   = statement->BindOutputColumn(5,  SqlColumn::StringType);
    SqlColumn* col_es_Conn  = statement->BindOutputColumn(6,  SqlColumn::StringType);
    SqlColumn* col_er_Conn  = statement->BindOutputColumn(7,  SqlColumn::StringType);
    SqlColumn* col_en_Conn  = statement->BindOutputColumn(8,  SqlColumn::StringType);
    SqlColumn* col_n_Conn   = statement->BindOutputColumn(9,  SqlColumn::StringType);
    SqlColumn* col_nen_Conn = statement->BindOutputColumn(10,  SqlColumn::StringType);
    SqlColumn* col_e_Conn   = statement->BindOutputColumn(11, SqlColumn::StringType);

    statement->Execute();

    for (;;)
    {
        if (!statement->Fetch())
            break;

        GermanConnectorFlag flag;
        FLD_VALUE_TYPE connectorWeight = 0;

        if (connectorWeight = col_no_Conn->AsIntegerFromString())
            flag.set(conn_none, connectorWeight);
        if (connectorWeight = col_s_Conn->AsIntegerFromString())
            flag.set(conn_s, connectorWeight);
        if (connectorWeight = col_es_Conn->AsIntegerFromString())
            flag.set(conn_es, connectorWeight);
        if (connectorWeight = col_er_Conn->AsIntegerFromString())
            flag.set(conn_er, connectorWeight);
        if (connectorWeight = col_en_Conn->AsIntegerFromString())
            flag.set(conn_en, connectorWeight);
        if (connectorWeight = col_n_Conn->AsIntegerFromString())
            flag.set(conn_n, connectorWeight);
        if (connectorWeight = col_nen_Conn->AsIntegerFromString())
            flag.set(conn_nen, connectorWeight);
        if (connectorWeight = col_e_Conn->AsIntegerFromString())
            flag.set(conn_e, connectorWeight);

        int pat = colPat->AsInteger();
        int stem = colStem->AsInteger();
        LgsString letters = colLetters->AsString();
        if (letters.length() == 0)
        {
            insertPatStem(pat, stem, char(0), flag);
        }
        else
        {
            for (int i = 0; i < letters.length(); i++)
                insertPatStem(pat, stem, letters[i], flag);
        }
    }
}

void GermanConnectorInfo::insertPatStem(int pat, int stem, char letter, GermanConnectorFlag flag)
{
    // search for key
    GermanPatStemLetter key(pat, stem, letter);
    LgsMap(GermanPatStemLetter, GermanConnectorFlag)::iterator iter = mapPatStem_s.find(key);

    if (iter == mapPatStem_s.end())
    {
        // not found - so insert
        pair<LgsMap(GermanPatStemLetter, GermanConnectorFlag)::iterator, bool> rc =
            mapPatStem_s.insert(pair<GermanPatStemLetter, GermanConnectorFlag>(key, flag));
        assert(rc.second);
    }
    else
    {
        // log error
        LogFile::singleton().printDateTime()
            << "ReadConnectorsForPatStem: duplicate pat/stem/last_letter="
            << pat << "/" << stem << "/" << char(letter);
        LogFile::singleton().skipDateTime()
            << "Pat/stem/last_letter ignored" << endl;
    }
}

bool GermanConnectorInfo::listApplies(char** endingListStart, const LgsString& word)
{
    for (char** ending = endingListStart; *ending != 0; ending++)
        if (GermanUtil::isEnding(*ending, word))
            return true;
    return false;
}

GermanConnectorFlag GermanConnectorInfo::getConnectorsByMorphology(
    const LgsString& company, const LgsString& word, int pat, int stem) const
{
    GermanWordPatStem key(company, word, pat, stem);
    LgsMap(GermanWordPatStem, GermanConnectorFlag)::iterator iter = mapMorphology_s.find(key);
    if (iter == mapMorphology_s.end())
        return GermanConnectorFlag();
    return iter->second;
}


GermanSuffixConstIterator GermanConnectorInfo::longestSuffix(GermanSuffixPatStemConn & suffixKey, GermanSuffixConstIterator endIter) const
{
    // find insertion point in sorted vector
    GermanSuffixConstIterator begin = m_suffixConnectorList.begin();

    // invoke special constructor for entry - does not allocate memory, word is already normalized
    pair<GermanSuffixConstIterator,GermanSuffixConstIterator> suffixRange = equal_range(begin, endIter, suffixKey);

    if (suffixRange.first != suffixRange.second)
        return suffixRange.first;

    if (suffixRange.first == begin)
        return m_suffixConnectorList.end();

    // look at previous entry and determine common suffix length with word
    GermanSuffixConstIterator suffixIter = suffixRange.first;
    suffixIter--;

    if ((*suffixIter).pat() != suffixKey.pat() ||
		(*suffixIter).stem() != suffixKey.stem() ||
		!suffixKey.setCommonSuffix((*suffixIter).suffix()))
    {
       return m_suffixConnectorList.end();
    }

    return GermanConnectorInfo::longestSuffix(suffixKey, suffixRange.first);
}

GermanConnectorFlag GermanConnectorInfo::getConnectorsFromIni(
    const LgsString& word, int pat, int stem) const
{
   GermanSuffixPatStemConn suffixKey(word, pat, stem);
   GermanSuffixConstIterator maxMatch = longestSuffix(suffixKey, m_suffixConnectorList.end());
   if (maxMatch == m_suffixConnectorList.end())
   {
      return GermanConnectorFlag();
   }

   return (*maxMatch).connectorFlag();
}


GermanConnectorFlag GermanConnectorInfo::getConnectorsByPatStem(
    const LgsString& word,int pat, int stem, char lastLetter, ConnectorResolution& res) const
{
    LgsMap(GermanPatStemLetter, GermanConnectorFlag)::iterator iter = mapPatStem_s.find(
        GermanPatStemLetter(pat, stem, lastLetter));
    if (iter == mapPatStem_s.end())
    {
        // not found on individual letter - search default for pat stem
        iter = mapPatStem_s.find(GermanPatStemLetter(pat, stem, char(0)));
        if (iter == mapPatStem_s.end())
        {
            return GermanConnectorFlag();
        }

        res = connPatStemDefault;
    }
    else
    {
        res = connPatStemLastLetter;
    }

    return iter->second;
}

GermanConnectorFlag GermanConnectorInfo::getDefaultConnector(
    const LgsString& word, GermanWordClass wordClass) const
{
    GermanConnectorFlag flag;

    switch (wordClass.getPartOfSpeech())
    {
    case GermanWordClass::noun:
/*
        // used to be same as AdjNonHeadForm
        flag.setWord(word, conn_none);
        break;
*/
		  return nounDefaultConn;
        break;
    case GermanWordClass::adj:
		  return adjDefaultConn;
        break;

    case GermanWordClass::verb:
		  return verbDefaultConn;
        break;

    default:
        assert(("invalid value", 0));
        break;
    }
	return flag;
}

GermanConnectorFlag GermanConnectorInfo::getConnectors(
    const LgsString& word, GermanWordClass wordClass,
    const LgsString& company, int pat, int stem, char lastLetter, ConnectorResolution& res) const
{
    assert(
        wordClass.getPartOfSpeech() == GermanWordClass::noun ||
        wordClass.getPartOfSpeech() == GermanWordClass::adj ||
        wordClass.getPartOfSpeech() == GermanWordClass::verb);

    GermanConnectorFlag flag;

    // check for null pat or stem
    if (pat == 0 || stem == 0)
    {
        flag.set(conn_none, 1);
        res = connNullPatStem;
    }
    else
    {
        // not a stored halfnoun
        // check morphology level
        flag = getConnectorsByMorphology(company, word, pat, stem);
        if (!flag.blank())
        {
            res = connMorphology;
        }
        else
        {
            // morphology level does not apply
            // check word level - ini file - s connector
           /*
            if (needs_s_Connector(word))
            {
                flag.setWord(word, conn_none);
                flag.setWord(word, conn_s);
                res = connIni_s;
            }
            // word level - ini file - s connector does not apply
            // check word level - ini file - <no> connector
            else if (takesNoConnectors(word))
            {
                flag.setWord(word, conn_none);
                res = connIni_none;
            }
            */
            flag = getConnectorsFromIni(word, pat, stem);
            if (!flag.blank())
            {
               res = connIni;
            }
            else
            {
                // word level does not apply
                // check pat/stem level
                flag = getConnectorsByPatStem(word, pat, stem, lastLetter, res);
                if (flag.blank())
                {
                    // pat/stem level does not apply
                    // apply default level
                    flag = getDefaultConnector(word, wordClass);
                    res = connDefault;
                }
            }
        }
    }

    flag.removeSuffix(word);
    assert(res != connNone);
    return flag;
}

