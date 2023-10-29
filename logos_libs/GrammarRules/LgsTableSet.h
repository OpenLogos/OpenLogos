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
// LgsTableSet.h: interface for the CLgsTableSet class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_LGSTABLESET_H__5E1D41C1_46F5_11D1_B50A_0020AF7669B3__INCLUDED_)
#define AFX_LGSTABLESET_H__5E1D41C1_46F5_11D1_B50A_0020AF7669B3__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "logos_include/lgscontainers.h"

class AFX_EXT_CLASS CLgsTableSet : public CObject  
{
public:
	int GetItemIndex(CLgsTable* src);
	CLgsTableSet();
	CLgsTableSet(LPCTSTR pass, LPCTSTR srcL, LPCTSTR tgtL);
	virtual ~CLgsTableSet();
    LPCTSTR Source();
    LPCTSTR Target();
    LPCTSTR Pass();
    LPCTSTR Description();
    LPCTSTR MainMiniCode();
    void MainMiniCode(LPCTSTR code);
    void Description(LPCTSTR description);
    void Add(CLgsTable* tab, int insertAfter = -1);
    bool Delete(CLgsTable* tab);
    int NumberOfTables();
	void Serialize(CArchive& ar);
	CLgsTable* GetItem(int i);
	CLgsTable* GetTable(int tabNo);
	bool IsIn(CLgsTable* tab);
	CLgsTable* Replace(CLgsTable* tab, int insertAfter = -1);

	void SetStripped(bool stripped);
	bool IsModified();

private:
	bool m_bModified;
	bool m_bIndexNeedsRebuild;
	int m_maxTabNo;
	void BuildIndex();
	LgsVector(CLgsTable*) m_table;
    CLgsLanguage* m_langSource;
    CLgsLanguage* m_langTarget;
    CLgsPass* m_pass;
	CLgsTable** m_index;
	LgsString m_mainMiniCode;
    LgsString m_description;
	static unsigned int m_magicVal;

	bool m_bStripped;
	DECLARE_SERIAL(CLgsTableSet);
};

#endif // !defined(AFX_LGSTABLESET_H__5E1D41C1_46F5_11D1_B50A_0020AF7669B3__INCLUDED_)
