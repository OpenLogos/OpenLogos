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
// LgsVtrStatement.h: interface for the CLgsVtrStatement class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_LGSVTRSTATEMENT_H__2CF4A207_4584_11D1_B50A_0020AF7669B3__INCLUDED_)
#define AFX_LGSVTRSTATEMENT_H__2CF4A207_4584_11D1_B50A_0020AF7669B3__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef _MSC_VER
#include "fake_mfc.h"
#endif

class AFX_EXT_CLASS CLgsVtrStatement : public CObject  
{
public:
	bool IsEqualTo(CLgsVtrStatement* r);
	CLgsVtrStatement();
	CLgsVtrStatement(short* s, int num);
	virtual ~CLgsVtrStatement();
	int GetNumbers(short** pNums);
	//void AppendNumbers(short* pNums, int num);
	void Description(LPCTSTR d);
	LPCTSTR Description();
	//void AppendDescription(LPCTSTR d);
	void Serialize(CArchive& ar);
	static bool parseVtrNumbers(
		short* nums, int numNums, CLgsVtrStatement*** pVtrs, short* pNumVtrs);
	static const int MinSwVal;
	static const int MaxSwVal;

private:
	//LgsString m_description;
	int m_num;
	short* m_statement;

	DECLARE_SERIAL(CLgsVtrStatement);
};

#endif // !defined(AFX_LGSVTRSTATEMENT_H__2CF4A207_4584_11D1_B50A_0020AF7669B3__INCLUDED_)
