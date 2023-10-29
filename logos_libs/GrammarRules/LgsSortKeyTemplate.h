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
// LgsSortKeyTemplate.h: interface for the CLgsSortKeyTemplate class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_LGSSORTKEYTEMPLATE_H__0F244CB2_56B2_11D1_B50A_0020AF7669B3__INCLUDED_)
#define AFX_LGSSORTKEYTEMPLATE_H__0F244CB2_56B2_11D1_B50A_0020AF7669B3__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef _MSC_VER
#include "fake_mfc.h"
#endif

#include "logos_include/lgsstring.h"
#include "logos_include/lgscontainers.h"

// match item types
#define MIT_SPELEMEMT	1
#define MIT_LEVEL		2
#define MIT_TAGSET		3

// sp element field selection flags
#define SPF_WC	(1<<0)
#define SPF_TY	(1<<1)
#define SPF_FC	(1<<2)

class AFX_EXT_CLASS CLgsSortKeyTemplate : public CObject  
{
#ifdef _MSC_VER       
        friend class CSortKeyTemplateDlg;
#endif
	friend class CLgsRule;
public:
	CLgsSortKeyTemplate();
	CLgsSortKeyTemplate(LPCTSTR name);
	virtual ~CLgsSortKeyTemplate();
	LPCTSTR toString();
	bool IsOKForBuildingWCTYIndex(bool wcOnly);
	struct MatchItem
	{
		MatchItem() { type = no = flags = start = num = 0; };
		int type;	// spelem/level/tagset
		int no;		// element number within a rule
		int flags;	// sp element field selection flags
					//   make sense only for spelem match items
		int start;	// starting tagset number index,
					//   makes sense only for tagset match items
		int num;	// number of consecutive tagset numbers
					//   makes sense only for tagset match items
	};

	void CopyFrom(CLgsSortKeyTemplate* from);
#ifdef _MSC_VER
	void EditTemplate(bool readOnly);
#endif
	void Serialize(CArchive& ar);

private:
	void Clear();
	LgsString m_name;		// template name
	LgsVector(MatchItem*) m_matchItems;

	DECLARE_SERIAL(CLgsSortKeyTemplate);
};

#endif // !defined(AFX_LGSSORTKEYTEMPLATE_H__0F244CB2_56B2_11D1_B50A_0020AF7669B3__INCLUDED_)
