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
// LgsTableSet.cpp: implementation of the CLgsTableSet class.
//
//////////////////////////////////////////////////////////////////////

#ifdef _MSC_VER
#include <logos_libs/GrammarRules/stdafx.h>
#endif
#include <logos_libs/GrammarRules/LgsPass.h>
#include <logos_libs/GrammarRules/LgsLanguage.h>
#include <logos_libs/GrammarRules/LgsVtrStatement.h>
#include <logos_libs/GrammarRules/ExtRec.h>
#include <logos_libs/GrammarRules/LgsTable.h>
#include <logos_libs/GrammarRules/LgsTableSet.h>

IMPLEMENT_SERIAL(CLgsTableSet,CObject,1);

unsigned int CLgsTableSet::m_magicVal =0xeeee5248;
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CLgsTableSet::CLgsTableSet()
{
	m_bModified = false;
	m_bIndexNeedsRebuild = true;
	m_bStripped = false; // load all stuff by default
	m_maxTabNo = -1000000;
	m_index = NULL;
    m_langSource = NULL;
    m_langTarget = NULL;
    m_pass = NULL;
    m_description = "";
}

CLgsTableSet::CLgsTableSet(LPCTSTR pass, LPCTSTR srcL, LPCTSTR tgtL)
{
	m_bModified = false;
	m_bIndexNeedsRebuild = true;
	m_bStripped = false; // load all stuff by default
    m_langSource = new CLgsLanguage();
	m_langSource->parse(srcL);

    m_langTarget = new CLgsLanguage();
	m_langTarget->parse(tgtL);

    m_pass = new CLgsPass();
	m_pass->parse(pass);

    m_description = "";
}

CLgsTableSet::~CLgsTableSet()
{
	if (m_langSource)
      delete m_langSource;
	if (m_langTarget)
      delete m_langTarget;
	if (m_pass)
      delete m_pass;

	if (m_index)
		delete[] m_index;

	for (LgsVector(CLgsTable*)::iterator i = m_table.begin(); i != m_table.end(); i++)
		delete (*i);
}

void CLgsTableSet::Serialize(CArchive& ar)
{
	bool isThere;
	int xxx;
	unsigned long _mm;
	unsigned int _ll;

	if (ar.IsStoring())
	{
		ar << m_magicVal;

		isThere = NULL != m_langSource;
		ar << isThere;
		if (isThere) m_langSource->Serialize(ar);

		isThere = NULL != m_langTarget;
		ar << isThere;
		if (isThere) m_langTarget->Serialize(ar);

		isThere = NULL != m_pass;
		ar << isThere;
		if (isThere) m_pass->Serialize(ar);

		ar << m_description.length();
		if (m_description.length() > 0)
			ar.Write(m_description.c_str(), m_description.length());

		ar << m_mainMiniCode.length();
		if (m_mainMiniCode.length() > 0)
			ar.Write(m_mainMiniCode.c_str(), m_mainMiniCode.length());

		ar << m_table.size();
		for (LgsVector(CLgsTable*)::iterator i = m_table.begin();
				i != m_table.end(); i++)
			(*i)->Serialize(ar);
	}
	else
	{
		ar >> _mm;
		if (_mm != m_magicVal)
			return;

		ar >> xxx; isThere = 0 != xxx;
		if (isThere)
		{
			if (!m_langSource) m_langSource = new CLgsLanguage();
			m_langSource->Serialize(ar);
		}
		else
		{
			if (m_langSource)
			{
				delete m_langSource;
				m_langSource = NULL;
			}
		}

		ar >> xxx; isThere = 0 != xxx;
		if (isThere)
		{
			if (!m_langTarget) m_langTarget = new CLgsLanguage();
			m_langTarget->Serialize(ar);
		}
		else
		{
			if (m_langTarget)
			{
				delete m_langTarget;
				m_langTarget = NULL;
			}
		}

		ar >> xxx; isThere = 0 != xxx;
		if (isThere)
		{
			if (!m_pass) m_pass = new CLgsPass();
			m_pass->Serialize(ar);
		}
		else
		{
			if (m_pass)
			{
				delete m_pass;
				m_pass = NULL;
			}
		}

		m_description.erase();
		ar >> _ll;
		if (_ll > 0)
		{
			char* b = new char[_ll+1];
			if (_ll != ar.Read(b, _ll))
				throw("");
			b[_ll] = 0;
			m_description = b;
			delete[] b;
		}

		m_mainMiniCode.erase();
		ar >> _ll;
		if (_ll > 0)
		{
			char* b = new char[_ll+1];
			if (_ll != ar.Read(b, _ll))
				throw("");
			b[_ll] = 0;
			m_mainMiniCode = b;
			delete[] b;
		}

		for (LgsVector(CLgsTable*)::iterator i = m_table.begin(); i != m_table.end(); i++)
			delete (*i);
		m_table.erase(m_table.begin(), m_table.end());
		int num;
		ar >> num;
		for (int j = 0; j < num; j++)
		{
			CLgsTable* tab = new CLgsTable();
			tab->SetStripped(m_bStripped);
			tab->Serialize(ar);
			m_table.push_back(tab);
		}

		m_bModified = false;
	}
}

LPCTSTR CLgsTableSet::Source()
{
	LPCTSTR result = NULL;
	if (m_langSource) result = m_langSource->toString();
	return result;
}

LPCTSTR CLgsTableSet::Target()
{
	LPCTSTR result = NULL;
	if (m_langTarget) result = m_langTarget->toString();
	return result;
}

LPCTSTR CLgsTableSet::Pass()
{
	LPCTSTR result = NULL;
	if (m_pass) result = m_pass->toString();
	return result;
}

LPCTSTR CLgsTableSet::Description()
{
	return m_description.c_str();
}

void CLgsTableSet::Description(LPCTSTR description)
{
	m_bModified = true;
	m_description = description;
}

void CLgsTableSet::Add(CLgsTable* tab, int insertAfter)
{
	if (-1 == insertAfter || insertAfter < m_table.size() -1)
		m_table.push_back(tab);
	else
		m_table.insert(m_table.begin() + insertAfter + 1, tab);
	m_bIndexNeedsRebuild = true;
	m_bModified = true;
}

bool CLgsTableSet::Delete(CLgsTable* tab)
{
	bool result = false;
	for (LgsVector(CLgsTable*)::iterator i = m_table.begin();
			i != m_table.end(); i++)
	{
		if (tab == (*i))
		{
			// don't remove the table, just clear the entry
			m_table.erase(i);
			result = true;
			m_bIndexNeedsRebuild = true;
			m_bModified = true;
			break;
		}
	}

	return result;
}

int CLgsTableSet::NumberOfTables()
{
	return m_table.size();
}

LPCTSTR CLgsTableSet::MainMiniCode()
{
	return m_mainMiniCode.c_str();
}

void CLgsTableSet::MainMiniCode(LPCTSTR code)
{
	m_mainMiniCode = code;
	m_bModified = true;
}

CLgsTable* CLgsTableSet::GetItem(int i)
{
	CLgsTable* result = NULL;
	if (i >= 0 && i < m_table.size()) result = m_table[i];
	return result;
}

CLgsTable* CLgsTableSet::GetTable(int tabNo)
{
	if (0 == m_table.size())
		return NULL;

	if (!m_index || m_bIndexNeedsRebuild)
		BuildIndex();

	if (tabNo > m_maxTabNo || tabNo < 0)
		return NULL;

	return m_index[tabNo];
}

void CLgsTableSet::BuildIndex()
{
	m_maxTabNo = -1000000;
        LgsVector(CLgsTable*)::iterator i;
	for (i = m_table.begin();
			i != m_table.end(); i++)
	{
		if (m_maxTabNo < (*i)->TableNo())
			m_maxTabNo = (*i)->TableNo();
	}

	if (m_index)
	{
		delete[] m_index;
		m_index = NULL;
	}

	VERIFY(m_maxTabNo >= 0);
	m_index = new CLgsTable*[m_maxTabNo + 1];
	memset(m_index, 0, sizeof(CLgsTable*) * (m_maxTabNo + 1));

	for (i = m_table.begin(); i != m_table.end(); i++)
	{
		int tabNo = (*i)->TableNo();
		VERIFY(tabNo <= m_maxTabNo);
		m_index[tabNo] = *i;
	}

	m_bIndexNeedsRebuild = false;
}

void CLgsTableSet::SetStripped(bool stripped)
{
	m_bStripped = stripped;
}

bool CLgsTableSet::IsIn(CLgsTable* tab)
{
	for (LgsVector(CLgsTable*)::iterator i = m_table.begin();
			i != m_table.end(); i++)
	{
		if ((*i) == tab)
			return true;
	}
	return false;
}

int CLgsTableSet::GetItemIndex(CLgsTable* src)
{
	VERIFY(src);

	for (LgsVector(CLgsTable*)::iterator i =  m_table.begin(); i != m_table.end(); i++)
	{
		if (src == *i)
			return i - m_table.begin();
	}

	return -1;
}

CLgsTable* CLgsTableSet::Replace(CLgsTable* tab, int insertAfter)
{
	// return:
	//		NULL	- table has been added to the set
	//		other	- arg replaces a table with matching tabNo,
	//					the return value is the old table pointer
	CLgsTable* result = NULL;

	for (LgsVector(CLgsTable*)::iterator i =  m_table.begin();
			i != m_table.end(); i++)
	{
		if (tab->TableNo() == (*i)->TableNo())
		{
			result = *i;
			*i = tab;
			m_bIndexNeedsRebuild = true;
			m_bModified = true;
			break;
		}
	}

	if (NULL == result)
	{
		Add(tab, insertAfter);
	}

	return result;
}

bool CLgsTableSet::IsModified()
{
	return m_bModified;
}
