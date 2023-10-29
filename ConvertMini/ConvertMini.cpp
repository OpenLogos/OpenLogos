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
#ifdef _MSC_VER
#include "stdafx.h"
#else
#define TRY try
#define CATCH(__Ty, __arg) catch( ... ) 
#define END_CATCH
#define TCHAR char
#define FormatMessage( __a, __b, __c, __d, __temp, __tempsize, __f) strerror_r(errno, __temp, __tempsize)
#define CopyMemory memcpy
#define VirtualAlloc(__a, __bufLen, __c, __d) malloc( __bufLen )
#define VirtualFree( __ptr, __b, __c ) free( __ptr )
#define LPVOID void *
#include <sstream>
#include <errno.h>
#endif
#include <logos_libs/GrammarRules/LgsGrammarRules.h>
#include "Resource.h"

static void Usage(const char* pname)
{
cout
<< "\n"
<< "Usage: " << pname << " -i <input-file>\n"
<< "               [-o <output-file>]\n"
<< "               [-3 <tab30-file>] [-4 <tab40-file>] [-5 <tab50-file>]\n"
<< "               -p <pass> [-s <src-lang>] [-t <trg-lang>]\n"
<< "               [-m <mini>]\n"
<< "  where:\n"
<< "   <input-file>  - input mini file\n"
<< "   <output-file> - output rules file in new format\n"
<< "   <tab30-file>  - output table30 file\n"
<< "   <tab40-file>  - output table40 file\n"
<< "   <tab50-file>  - output table50 file\n"
<< "   <pass>        - one of: res1, res2, res22\n"
<< "                           parse, parse1, parse2, parse3, parse4\n"
<< "                           tran, tran1, tran2, tran3, tran4\n"
<< "   <src-lang>    - source language, one of: german, english, french,\n"
<< "                     spanish, italian, portuguese (default: NA)\n"
<< "   <trg-lang>    - target language\n"
<< "   <mini>        - yes/no, default: yes\n"
<< "At least -i and -p options should be specified.\n"
<< "When one of the options -o, -3, -4, -5 is not specified,\n"
<< "the default is the input file name with the file extension\n"
<< "replaced by one of the following extensions\n"
<< ".lgs, .30, .40, .50 correspondingly.\n"
;
}

CLgsTableSet* LoadTableSet(const char* file)
{
	CLgsTableSet* result = NULL;

	TRY {
		CFile in( file, CFile::modeRead );
		CArchive arIn(&in, CArchive::load);
		result = new CLgsTableSet();
		result -> Serialize(arIn);
		arIn.Close();
		in.Close();
	}
	CATCH( CFileException, e )
	{
		if (result) delete result, result = NULL;
		return NULL;
	}
	END_CATCH

	return result;
}

#ifdef _MSC_VER
static bool StoreOb( CObject* o, string& fileName )
#else
// in the linux version, the Serialize functions are non-virtual
template<class __TYPE> static bool StoreOb( __TYPE* o, string& fileName )
#endif
{
	TRY {
		CFile out( fileName.c_str(), CFile::modeCreate | CFile::modeWrite);
		CArchive arOut(&out, CArchive::store);
		o -> Serialize(arOut);
		arOut.Close();
		out.Close();
	}
	CATCH( CFileException, e )
	{
		TCHAR temp[1024];
		FormatMessage( FORMAT_MESSAGE_FROM_SYSTEM,
			NULL, e->m_lOsError, 0, temp, sizeof(temp), NULL);
		cout << temp << endl;
		return false;
	}
	END_CATCH

	return true;
}


// _fix_me_
// This is stolen from logos_libs/lgs_tran_rule_io/util.cpp
static bool Res2Ob(LPCTSTR resType, LPCTSTR resName, CLgsSortKeyTemplate* ob)
{
#ifdef _MSC_VER
	CMemFile f;
	CArchive ar(&f, CArchive::load);
	HRSRC hRsrc = FindResourceEx(NULL, resType, resName,
					MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL));

	if ( !hRsrc )
		return false;

	DWORD resSize = SizeofResource(NULL, hRsrc);
	if ( resSize <= 0 )
		return false;

	HGLOBAL hMem = LoadResource(NULL, hRsrc);
	if( !hMem )
		return false;

	LPVOID buf = LockResource(hMem);
	if ( !buf )
		return false;

	f.Write(buf, resSize);
	f.Seek(0, CFile::begin);
	ob -> Serialize(ar);
	ar.Close();
#else
        // for unix, we can simply read the resource from a file, provided that
        // we gave it the right name
        ostringstream fn;
        fn << resType << (int) resName;
        try {
          CFile f(fn.str().c_str(), CFile::modeRead);
          CArchive ar(&f, CArchive::load);
          f.Seek(0, CFile::begin);
          ob -> Serialize(ar);
          ar.Close();
        }
        catch (string s) {
          cerr << s << endl;
          return false;
        }
#endif
	return true;
}


static bool ReadMini(ifstream& in, char* buf, int bufLen, DWORD* bytesRead)
{
	static char lineBuf[1024];
	static int lineLen = 0;
	int bytesInBuf = 0;

	if ( lineLen > 0 )
	{
		CopyMemory(buf + bytesInBuf, lineBuf, lineLen);
		bytesInBuf += lineLen;
		buf[bytesInBuf++] = 0xa;
		lineLen = 0;
	}

	while ( !in.eof() )
	{
		in.getline(lineBuf, sizeof(lineBuf));

		lineLen = strlen(lineBuf);
		if ( !lineLen ) continue;
		if ( lineLen > 1 && '/' == lineBuf[0] && '*' == lineBuf[1] ) continue;
		if ( bytesInBuf + lineLen + 1 <= bufLen )
		{
			CopyMemory(buf + bytesInBuf, lineBuf, lineLen);
			bytesInBuf += lineLen;
			buf[bytesInBuf++] = 0xa;
			lineLen = 0;
		}
		else
			break;
	}

	*bytesRead = bytesInBuf;
	return true;
}

#define ADDITEM()\
{\
	if ( '*' == ruleBuf[0] )\
	{\
		switch ( ruleBuf[2] )\
		{\
		case '3':\
			table = new CLgsTable(table30Id++);\
			if ( table -> parse(ruleBuf) )\
				t30 -> Add(table);\
			else \
				cerr << ruleBuf; \
			break;\
		case '4':\
			table = new CLgsTable(table40Id++);\
			if ( table -> parse(ruleBuf) )\
				t40 -> Add(table);\
			else \
				cerr << ruleBuf; \
			break;\
		case '5':\
			table = new CLgsTable(table50Id++);\
			if ( table -> parse(ruleBuf) )\
				t50 -> Add(table);\
			else \
				cerr << ruleBuf; \
			break;\
		default:\
			VERIFY(false);\
		}\
	}\
	else\
	{\
		rule = new CLgsRule(ruleId++);\
		if ( rule -> parse(ruleBuf, isTranRule) )\
			rs -> Add(rule);\
		else \
			cerr << ruleBuf; \
	}\
}\

int main(int argc, char** argv)
{
	int result = 1;
	string inputFile;
	string outputFile;
	string tab30File;
	string tab40File;
	string tab50File;
	string passStr;
	string srcStr = "NA";
	string trgStr = "NA";
	string miniStr = "yes";

        int i;
	for ( i = strlen(argv[0])-1;
			i >= 0 && '\\' != argv[0][i] && '/' != argv[0][i]; i--);
	char* pname = argv[0] + i + 1;

	for ( i = 1; i < argc; i++ )
	{
		if ( '-' == argv[i][0] )
		{
			if ( 'i' == argv[i][1] )
				inputFile =
					argv[i][2]? argv[i]+2 : (i+1<argc ? (i++, argv[i]) : "" );
			else if ( 'o' == argv[i][1] )
				outputFile =
					argv[i][2]? argv[i]+2 : (i+1<argc ? (i++, argv[i]) : "" );
			else if ( '3' == argv[i][1] )
				tab30File =
					argv[i][2]? argv[i]+2 : (i+1<argc ? (i++, argv[i]) : "" );
			else if ( '4' == argv[i][1] )
				tab40File =
					argv[i][2]? argv[i]+2 : (i+1<argc ? (i++, argv[i]) : "" );
			else if ( '5' == argv[i][1] )
				tab50File =
					argv[i][2]? argv[i]+2 : (i+1<argc ? (i++, argv[i]) : "" );
			else if ( 'p' == argv[i][1] )
				passStr =
					argv[i][2]? argv[i]+2 : (i+1<argc ? (i++, argv[i]) : "" );
			else if ( 's' == argv[i][1] )
				srcStr =
					argv[i][2]? argv[i]+2 : (i+1<argc ? (i++, argv[i]) : "" );
			else if ( 't' == argv[i][1] )
				trgStr =
					argv[i][2]? argv[i]+2 : (i+1<argc ? (i++, argv[i]) : "" );
			else if ( 'm' == argv[i][1] )
				miniStr =
					argv[i][2]? argv[i]+2 : (i+1<argc ? (i++, argv[i]) : "" );
		}
	}

	if ( !inputFile.length() || !srcStr.length() || !trgStr.length() )
	{
		Usage(pname);
		return result;
	}

	if ( srcStr.compare("NA") &&
		 srcStr.compare("german") &&
		 srcStr.compare("english") &&
		 srcStr.compare("french") &&
		 srcStr.compare("spanish") &&
		 srcStr.compare("italian") &&
		 srcStr.compare("portuguese") )
	{
		Usage(pname);
		return result;
	}

	if ( trgStr.compare("NA") &&
		 trgStr.compare("german") &&
		 trgStr.compare("english") &&
		 trgStr.compare("french") &&
		 trgStr.compare("spanish") &&
		 trgStr.compare("italian") &&
		 trgStr.compare("portuguese") )
	{
		Usage(pname);
		return result;
	}

	// capitalize the first letter of the lang string
	srcStr[0] = toupper(srcStr[0]);
	trgStr[0] = toupper(trgStr[0]);

	int dotPos = inputFile.rfind('.');
	string base = string::npos == dotPos ? inputFile : inputFile.substr(0, dotPos);

	if ( !passStr.length() )
	{
		Usage(pname);
		return result;
	}

	if ( passStr.compare("res1") &&
		 passStr.compare("res2") &&
		 passStr.compare("res22") &&
		 passStr.compare("parse") &&
		 passStr.compare("parse1") &&
		 passStr.compare("parse2") &&
		 passStr.compare("parse3") &&
		 passStr.compare("parse4") &&
		 passStr.compare("tran") &&
		 passStr.compare("tran1") &&
		 passStr.compare("tran2") &&
		 passStr.compare("tran3") &&
		 passStr.compare("tran4") )
	{
		Usage(pname);
		return result;
	}

	if ( !outputFile.length() ) outputFile = base + "." + passStr + ".lgr";
	if ( !tab30File.length() ) tab30File = base + ".30";
	if ( !tab40File.length() ) tab40File = base + ".40";
	if ( !tab50File.length() ) tab50File = base + ".50";

	bool isTranRule = 't' == passStr[0] || 'p' == passStr[0];
	bool isResRule = 'r' == passStr[0];
	int resNo = !isResRule ? 0 :
				5 == passStr.length() ? 22 : (passStr[3] - '0');

	LPTSTR buf = NULL;
	int bufLen = 0x100000;
	DWORD bytesRead;
	int ruleBufSize = 0x10000;
	char* ruleBuf = new char[ruleBufSize];
	int ruleId = 0;
	int table30Id = 0;
	int table40Id = 0;
	int table50Id = 0;
	int ix = 0;

	CLgsRuleSet* rs = new CLgsRuleSet(
						passStr.c_str(), srcStr.c_str(), trgStr.c_str());
	CLgsTableSet* t30 = new CLgsTableSet();

	// for 40 and 50 tables if the file exists,
	// use it's contents as a starting point
	CLgsTableSet* t40 = LoadTableSet(tab40File.c_str());
	if ( t40 )
	{
		table40Id = t40 -> NumberOfTables();
	}
	else
	{
		t40 = new CLgsTableSet();
	}

	CLgsTableSet* t50 = LoadTableSet(tab50File.c_str());
	if ( t50 )
	{
		table50Id = t50 -> NumberOfTables();
	}
	else
	{
		t50 = new CLgsTableSet();
	}

	CLgsRule* rule = NULL;
	CLgsTable* table = NULL;

	rs -> MainMiniCode('y' == miniStr[0] ? "mini" : "main" );

	ifstream in(inputFile.c_str());
	if ( !in.good() )
	{
		cout << "File not found: " << inputFile << endl;
		goto cleanup;
	}

	buf = (LPTSTR)VirtualAlloc(NULL, bufLen, MEM_COMMIT, PAGE_READWRITE);
	if ( !buf ) goto cleanup;

	while ( ReadMini(in, buf, bufLen, &bytesRead) )
	{
		if ( 0 == bytesRead )
		{
			if ( ix > 0 )
			{
				ruleBuf[ix] = 0;
				ADDITEM();
			}
			break;
		}

		for ( int i = 0; i < bytesRead; i++ )
		{
			ruleBuf[ix] = buf[i];
			VERIFY( ix < ruleBufSize -1 );

			if ( ( ix > 5 && 
					 ' ' == ruleBuf[ix] &&
					 '0' <= ruleBuf[ix-1] && '9' >= ruleBuf[ix-1] &&
					 '0' <= ruleBuf[ix-2] && '9' >= ruleBuf[ix-2] &&
					 '\xa' == ruleBuf[ix-3] &&
					 '9' == ruleBuf[ix-4] &&
					 '9' == ruleBuf[ix-5] &&
					 '9' == ruleBuf[ix-6] ) ||
				 ( ix > 6 &&
					 ' ' == ruleBuf[ix] &&
					 '0' <= ruleBuf[ix-1] && '9' >= ruleBuf[ix-1] &&
					 '0' <= ruleBuf[ix-2] && '9' >= ruleBuf[ix-2] &&
					 '\xa' == ruleBuf[ix-3] &&
					 '\xd' == ruleBuf[ix-4] &&
					 '9' == ruleBuf[ix-5] &&
					 '9' == ruleBuf[ix-6] &&
					 '9' == ruleBuf[ix-7] )
				)
			{
				char sch = ruleBuf[ix-2];
				ruleBuf[ix-2] = 0;

				ADDITEM();

				ruleBuf[0] = sch;
				ruleBuf[1] = ruleBuf[ix-1];
				ruleBuf[2] = ruleBuf[ix];
				ix = 3;
			}
			else if ( ( ix > 7 && // table 30/40/50 entry
					 ' ' == ruleBuf[ix] &&
					 ruleBuf[ix-1] == ruleBuf[ix-1] &&
					 '3' <= ruleBuf[ix-2] && '5' >= ruleBuf[ix-2] &&
					 ' ' == ruleBuf[ix-3] &&
					 '*' == ruleBuf[ix-4] &&
					 '\xa' == ruleBuf[ix-5] &&
					 '9' == ruleBuf[ix-6] &&
					 '9' == ruleBuf[ix-7] &&
					 '9' == ruleBuf[ix-8] ) ||
				 ( ix > 8 &&
					 ' ' == ruleBuf[ix] &&
					 ruleBuf[ix-1] == ruleBuf[ix-1] &&
					 '3' <= ruleBuf[ix-2] && '5' >= ruleBuf[ix-2] &&
					 ' ' == ruleBuf[ix-3] &&
					 '*' == ruleBuf[ix-4] &&
					 '\xa' == ruleBuf[ix-5] &&
					 '\xd' == ruleBuf[ix-6] &&
					 '9' == ruleBuf[ix-7] &&
					 '9' == ruleBuf[ix-8] &&
					 '9' == ruleBuf[ix-9] ) )
			{
				int tableType = ruleBuf[ix-2];
				char sch = ruleBuf[ix-4];
				ruleBuf[ix-4] = 0;
				ADDITEM();
				ruleBuf[0] = sch;
				ruleBuf[1] = ruleBuf[ix-3];
				ruleBuf[2] = ruleBuf[ix-2];
				ruleBuf[3] = ruleBuf[ix-1];
				ruleBuf[4] = ruleBuf[ix];
				ix = 5;
			}
			else
				ix++;
		}
	}

	if ( bytesRead )
	{
		TCHAR temp[1024];
		FormatMessage( FORMAT_MESSAGE_FROM_SYSTEM,
			NULL, GetLastError(), 0, temp, sizeof(temp), NULL);
		cout << temp << endl;
		goto cleanup;
	}

	VirtualFree( (LPVOID)buf, 0, MEM_RELEASE); buf = NULL;
	in.close();

	result = 0;

	// sort the rule set if it is not empty,
	// use proper sort key template
	if ( rs -> NumberOfRules() > 0 )
	{
		int sktId = 0;

                // _fix_me_ Sorting via resource files does not yet work with
                // linux
		if ( isResRule )
			sktId = 22 == resNo? IDR_R22_RULES_DEFAULT:IDR_RES1_RES2_RULES_DEFAULT;
		else
			sktId = IDR_SP_RULES_DEFAULT;

		CLgsSortKeyTemplate* pTmpl = new CLgsSortKeyTemplate();
		if ( Res2Ob("SORTKEYTEMPLATE", (LPCTSTR)sktId, pTmpl) )
		{
			//cout << "Sorting rules using default sort key template...";
			rs -> Sort(pTmpl, true);
			//cout << "done" << endl;
		}
		else
		{
			// print out warning msg
			cout << "ERROR: failed to load sort key template" << endl;
		}
	}

	// save generated rules and tables
	if ( rs -> NumberOfRules() > 0 )
		result = StoreOb(rs, outputFile) ? result : 1;
	else
		DeleteFile(outputFile.c_str());

	if ( t30 -> NumberOfTables() > 0 )
		result = StoreOb(t30, tab30File) ? result : 1;
	else
		DeleteFile(tab30File.c_str());

	if ( t40 -> NumberOfTables() > 0 )
		result = StoreOb(t40, tab40File) ? result : 1;

	if ( t50 -> NumberOfTables() > 0 )
		result = StoreOb(t50, tab50File) ? result : 1;

cleanup:
	if ( rs ) delete rs;
	if ( t30 ) delete t30;
	if ( t40 ) delete t40;
	if ( t50 ) delete t50;
	if ( ruleBuf ) delete ruleBuf;
	if ( buf ) VirtualFree( buf, 0, MEM_RELEASE);
	if ( in.good() ) in.close();
	return result;
}
