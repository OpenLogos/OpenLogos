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
// LgsSortKeyTemplate.cpp: implementation of the CLgsSortKeyTemplate class.
//
//////////////////////////////////////////////////////////////////////

#ifdef _MSC_VER
#include <logos_libs/GrammarRules/stdafx.h>
#endif
#include <logos_libs/GrammarRules/LgsSortKeyTemplate.h>

#include "resource.h"
// #include "SortKeyTemplateDlg.h"

IMPLEMENT_SERIAL(CLgsSortKeyTemplate,CObject,1);

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CLgsSortKeyTemplate::CLgsSortKeyTemplate()
{
}

CLgsSortKeyTemplate::CLgsSortKeyTemplate(LPCTSTR name)
{
	if (!name)
      throw("");
	if (name)
      m_name = name;
}

CLgsSortKeyTemplate::~CLgsSortKeyTemplate()
{
	Clear();
}

LPCTSTR CLgsSortKeyTemplate::toString()
{
	return m_name.c_str();
}

void CLgsSortKeyTemplate::Serialize(CArchive& ar)
{
	int i;
	int numItems;
	unsigned int _ll;

	if (ar.IsStoring())
	{
		ar << m_name.length();
		if (m_name.length() > 0)
			ar.Write(m_name.c_str(), m_name.length());

		numItems = m_matchItems.size();
		ar << numItems;
		for (LgsVector(MatchItem*)::iterator i = m_matchItems.begin();
				i != m_matchItems.end(); i++)
		{
			ar << (*i)->type;
			ar << (*i)->no;
			ar << (*i)->flags;
			ar << (*i)->start;
			ar << (*i)->num;
		}
	}
	else
	{
		m_name.erase();
		ar >> _ll;
		if (_ll > 0)
		{
			char* b = new char[_ll+1];
			if (_ll != ar.Read(b, _ll))
				throw("");
			b[_ll] = 0;
			m_name = b;
			delete[] b;
		}

		Clear();
		ar >> numItems;
		for (i = 0; i < numItems; i++)
		{
			MatchItem* mit = new MatchItem();
			ar >> mit->type;
			ar >> mit->no;
			ar >> mit->flags;
			ar >> mit->start;
			ar >> mit->num;
			m_matchItems.push_back(mit);
		}
	}
}

void CLgsSortKeyTemplate::Clear()
{
	for (LgsVector(MatchItem*)::iterator i = m_matchItems.begin();
			i != m_matchItems.end(); i++)
		delete (*i);
	m_matchItems.erase(m_matchItems.begin(), m_matchItems.end());
}

#ifdef _MSC_VER
void CLgsSortKeyTemplate::EditTemplate(bool readOnly)
{
	LgsString title = "Sort Key Template: ";
	title += m_name.c_str();
	CSortKeyTemplateDlg dlg(title.c_str(), readOnly, this);
	dlg.DoModal();
}
#endif

void CLgsSortKeyTemplate::CopyFrom(CLgsSortKeyTemplate* from)
{
	Clear();
	for (LgsVector(MatchItem*)::iterator i = from->m_matchItems.begin();
			i != from->m_matchItems.end(); i++)
	{
		MatchItem* mit = new MatchItem();
		mit->type		= (*i)->type;
		mit->no		= (*i)->no;
		mit->flags	= (*i)->flags;
		mit->start	= (*i)->start;
		mit->num		= (*i)->num;
		m_matchItems.push_back(mit);
	}
	m_name = from->m_name;
}

bool CLgsSortKeyTemplate::IsOKForBuildingWCTYIndex(bool wcOnly)
{
	return m_matchItems.size() > 0 &&
		 (*m_matchItems.begin())->flags & SPF_WC &&
		 (wcOnly || (*m_matchItems.begin())->flags & SPF_TY);
}
