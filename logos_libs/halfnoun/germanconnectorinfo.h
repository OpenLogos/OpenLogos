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
#ifndef __GermanConnectorInfo_h__
#define __GermanConnectorInfo_h__

//---------------------------------------------------------------------
// File - GermanConnectorInfo.h
//
// class - GermanConnectorInfo
//
// Description - class used to hold German connector information
//
//---------------------------------------------------------------------

#include <logos_libs/sql/sqlconnection.h>
#include <logos_libs/halfnoun/germanpatstem.h>
#include <logos_libs/halfnoun/germanflag.h>
#include <logos_libs/halfnoun/germanwordclass.h>
#include <logos_libs/halfnoun/germanconnectorresolution.h>

typedef LgsSet(GermanSuffixPatStemConn) GermanSuffixSet;
typedef GermanSuffixSet::iterator GermanSuffixIterator;
typedef GermanSuffixSet::const_iterator GermanSuffixConstIterator;

class GermanConnectorInfo
{
public:
    // read details from ini file, and database.
    // also construct the unique instance of this class
    static void initialize(const LgsString& iniFileName, SqlConnection* connect);

    // singleton - get unique instance of this class
    static GermanConnectorInfo& singleton();

    // get connector info given all the input
    // lastLetter is the last letter of the un-normalized word - in lower case
    GermanConnectorFlag getConnectors(const LgsString& word, GermanWordClass wordClass,
        const LgsString& company, int pat, int stem, char lastLetter, ConnectorResolution& res) const;

private:
   static const short SUFFIX_INDEX;
   static const short PAT_INDEX;
   static const short STEM_INDEX;
   static const short CONNECTOR_START_INDEX;

   static GermanConnectorInfo* theConnectorInfo_s;
   //static char** sSuffix_s;
   //static char** sSuffixException_s;
   //static char** noSuffix_s;
   //static char** noSuffixException_s;
   static GermanSuffixSet m_suffixConnectorList;
   static LgsMap(GermanWordPatStem, GermanConnectorFlag) mapMorphology_s;
   static LgsMap(GermanPatStemLetter, GermanConnectorFlag) mapPatStem_s;
	static GermanConnectorFlag nounDefaultConn;
	static GermanConnectorFlag verbDefaultConn;
	static GermanConnectorFlag adjDefaultConn;

    // read lists for a section from iniFile into array
    static void readIni(const char* iniFileName, char* section, LgsString*& endingList);

    // read data base info
    static void readConnectorsForMorphology(SqlConnection* connect);
    static void readConnectorsForPatStem(SqlConnection* connect);
    static void readDefaultConnector(const char * fileName, 
									 const char * section, 
									 const char * wordClass, 
									 GermanConnectorFlag & connFlag);
    static void insertPatStem(int pat, int stem, char letter, GermanConnectorFlag flag);

    // does the ending list apply to the buffer
    static bool listApplies(char** endingListStart, const LgsString& word);

    DefaultConstructor(GermanConnectorInfo)

    // word level methods -------------------------------

    // morphology level methods -------------------------
    GermanConnectorFlag getConnectorsByMorphology(
        const LgsString& company, const LgsString& word, int pat, int stem) const;

    // pat/stem level methods ---------------------------
    GermanConnectorFlag getConnectorsByPatStem(
        const LgsString& word, int pat, int stem, char lastLetter, ConnectorResolution& res) const;
    GermanConnectorFlag getConnectorsFromIni(const LgsString& word, int pat, int stem) const;	
    // default level methods ----------------------------
    GermanConnectorFlag getDefaultConnector(const LgsString& word, GermanWordClass wordClass) const;
    GermanSuffixConstIterator longestSuffix(GermanSuffixPatStemConn & suffixKey, GermanSuffixConstIterator endIter) const;
};


#endif



