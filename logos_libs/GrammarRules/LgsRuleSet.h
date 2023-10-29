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
// LgsRuleSet.h: interface for the CLgsRuleSet class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_LGSRULESET_H__2CF4A204_4584_11D1_B50A_0020AF7669B3__INCLUDED_)
#define AFX_LGSRULESET_H__2CF4A204_4584_11D1_B50A_0020AF7669B3__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

class CLgsRule;
class CLgsSortKeyTemplate;
class CLgsLanguage;
class CLgsPass;

class AFX_EXT_CLASS CLgsRuleSet : public CObject  
{
public:
	void AddRule(CLgsRule *rule, int  insertAfter = -1);
	int GetItemIndex(CLgsRule* src);
	CLgsRule* IsIn(CLgsRule* src);
	int GetNextId();
    LPCTSTR MainMiniCode();
    void MainMiniCode(LPCTSTR code);
	void Sort(CLgsSortKeyTemplate* skTmp, bool ascending = true);
	void DeleteAll();
    CLgsRuleSet();
    CLgsRuleSet (LPCTSTR pass, LPCTSTR srcL, LPCTSTR tgtL);
    CLgsRuleSet (LPCTSTR file);
	virtual ~CLgsRuleSet();
    LPCTSTR Source();
    bool Source(LPCTSTR srcLang);
    LPCTSTR Target();
    bool Target(LPCTSTR trgLang);
    LPCTSTR Pass();
    bool Pass(LPCTSTR passStr);
    LPCTSTR Description();
    void Description(LPCTSTR description);
    void Add(CLgsRule* rule, int insertAfter = -1);
    bool Delete(CLgsRule* rule);
    CLgsRule* Replace(CLgsRule* rule, int insertAfter = -1);
	bool QueryBounds(int* wcMin, int* wcMax, int* tyMin, int* tyMax);
	bool BuildIndex( int** startIndex, int** countIndex,
		int** startNegIndex, int** countNegIndex, bool wcOnly);
	CLgsRule* GetItem(int i);
    int NumberOfRules();
	void Serialize(CArchive& ar);
   void SaveAsText(LPCTSTR fileName);
	bool IsSorted();
	CLgsSortKeyTemplate* GetSortKeyTemplate();
	LPCTSTR GetSortOrder();
	struct CCmpElem {
		bool m_ascending;
		CLgsSortKeyTemplate* m_skTmp;
		CLgsRule* m_rule;
	};

private:
    LgsString m_mainMiniCode;
	LgsVector(CLgsRule*) m_rule;
    CLgsLanguage* m_langSource;
    CLgsLanguage* m_langTarget;
    CLgsPass* m_pass;
    LgsString m_description;
	static const unsigned long m_magicVal;
	bool m_sorted;
	LgsString m_sortOrder;
	CLgsSortKeyTemplate* m_skTmp;

	DECLARE_SERIAL(CLgsRuleSet);
};

#endif // !defined(AFX_LGSRULESET_H__2CF4A204_4584_11D1_B50A_0020AF7669B3__INCLUDED_)
