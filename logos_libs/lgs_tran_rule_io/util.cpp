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
#include <sstream>
#endif
#include <logos_libs/GrammarRules/LgsGrammarRules.h>
#include "util.h"
#include "resource.h"

void OutputMsg(const char* tag, const char* msg)
{
	cerr << tag << ": " << msg << endl;
}

bool Res2Ob(LPCTSTR resType, LPCTSTR resName, CLgsSortKeyTemplate* ob)
{
#ifdef _MSC_VER
	CMemFile f;
	CArchive ar(&f, CArchive::load);
	HMODULE hMod = GetModuleHandle("lgs_tran_rule_io");
	HRSRC hRsrc = FindResourceEx(hMod, resType, resName,
					MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL));

	if ( !hRsrc )
		return false;

	DWORD resSize = SizeofResource(hMod, hRsrc);
	if ( resSize <= 0 )
		return false;

	HGLOBAL hMem = LoadResource(hMod, hRsrc);
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

void SortRuleSet(CLgsRuleSet* pset, int type)
{
	if ( 0 == pset -> NumberOfRules() )
		return;

	int sktId = 0;

	switch (type)
	{
	case RST_RES1:
	case RST_RES2:
		sktId = IDR_RES1_RES2_RULES_DEFAULT;
		break;
	case RST_RES22:
		sktId = IDR_R22_RULES_DEFAULT;
		break;
	case RST_TRAN:
	case RST_PARSE:
	case RST_PARSETRAN:
		sktId = IDR_SP_RULES_DEFAULT;
		break;
	}

	CLgsSortKeyTemplate* pTmpl = new CLgsSortKeyTemplate();
	if ( Res2Ob("SORTKEYTEMPLATE", (LPCTSTR)sktId, pTmpl) )
	{
		pset -> Sort(pTmpl, true);
	}
	else
	{
		OutputMsg("Sort", "failed to sort mini rule set");
	}
	delete pTmpl;
}
