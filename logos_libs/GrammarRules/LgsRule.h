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
// LgsRule.h: interface for the CLgsRule class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_LGSRULE_H__2CF4A203_4584_11D1_B50A_0020AF7669B3__INCLUDED_)
#define AFX_LGSRULE_H__2CF4A203_4584_11D1_B50A_0020AF7669B3__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

class AFX_EXT_CLASS CLgsRule : public CObject  
{
private:
	int CalcSpec(int specad);
public:
	int Compare(CLgsRule* r, CLgsSortKeyTemplate* t);
	bool IsEqualTo(CLgsRule* r);
	CLgsRule();
	CLgsRule(CLgsRule* rule);
	CLgsRule(int id);
	virtual ~CLgsRule();
    int id();
    void id(int i);
    int Level();
    void Level(int l);
    int SpecificityAdjustment();
    void SpecificityAdjustment(int specadj);
    int CallingRuleLevel();
    void CallingRuleLevel(int l);
    int Specificity();
    LPCTSTR User();
    void User(LPCTSTR user);
    CTime* Created();
    void Created(CTime& date);
    CTime* LastModified();
    void LastModified(CTime& date);
    LPCTSTR Notes();
    void Notes(LPCTSTR notes);
    LPCTSTR Description();
    void Description(LPCTSTR desc);
    bool parse(LPCTSTR str, bool isTranRule);
    LPTSTR toString();
    LPTSTR CommentDisplayString(bool rtf);
    LPTSTR SpsDisplayString(bool rtf);
    LPTSTR TagsetsDisplayString(bool rtf, bool cr);
    LPTSTR VtrsDisplayString(bool rtf, bool cr);
    int SpStatements(CLgsSpElement*** sps);
    void SpStatements(CLgsSpElement** sps, int num);
    int VtrStatements(CLgsVtrStatement*** vtrs);
    void VtrStatements(CLgsVtrStatement** vtrs, int num);
	void Serialize(CArchive& ar);

private:
    short m_id;
    short m_level;
    short m_callingRuleLevel; // valid only for tran rules
    LgsString m_description;
	short m_numSps;
    CLgsSpElement** m_spElement;
	short m_numVtrs;
    CLgsVtrStatement** m_vtrStatement;
	CExtensionRec* m_ext;

	DECLARE_SERIAL(CLgsRule);
};

#endif // !defined(AFX_LGSRULE_H__2CF4A203_4584_11D1_B50A_0020AF7669B3__INCLUDED_)
