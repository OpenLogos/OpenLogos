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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
// Checker.h: interface for the CChecker class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_CHECKER_H__604A66B1_7BC3_11D1_B50A_0020AF7669B3__INCLUDED_)
#define AFX_CHECKER_H__604A66B1_7BC3_11D1_B50A_0020AF7669B3__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

struct CheckerMsg {char* str; int id;};

#ifdef _MSC_VER
typedef CTypedPtrList<CPtrList, CheckerMsg*> MyTypedPointerList;
#else
#include <list> 
typedef list<CheckerMsg *> MyTypedPointerList;
#endif

#include "logos_libs/multithreadlib/lgscritsect.h"

struct CChecker
{
	CChecker() {}
	virtual ~CChecker() {}
	virtual void ConfigInterProcessCommunications(
		MyTypedPointerList* msgQueue,
		// CRITICAL_SECTION* pCsec,
                LgsCriticalSection *pCsec,
                bool* pAbortFlag) = 0;
	virtual void SetMiniTabs(
		CLgsTableSet* pT30, CLgsTableSet* pT40, CLgsTableSet* pT50) = 0;
	virtual void SetMainTabs(
		CLgsTableSet* pT30, CLgsTableSet* pT40, CLgsTableSet* pT50) = 0;
	virtual bool Check(CLgsRuleSet* ruleSet, CLgsRule* rule) = 0;
};

#endif // !defined(AFX_CHECKER_H__604A66B1_7BC3_11D1_B50A_0020AF7669B3__INCLUDED_)
