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
// LgsTable.h: interface for the CLgsTable class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_LGSTABLE_H__CDD55120_4656_11D1_B50A_0020AF7669B3__INCLUDED_)
#define AFX_LGSTABLE_H__CDD55120_4656_11D1_B50A_0020AF7669B3__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

class AFX_EXT_CLASS CLgsTable : public CObject  
{
public:
	CLgsTable();
	CLgsTable(int id);
	CLgsTable(CLgsTable* table);
	virtual ~CLgsTable();
    int id();
    void id(int i);
    int Level();
    void Level(int l);
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
    bool parse(LPCTSTR str);
    LPTSTR toString();
    int VtrStatements(CLgsVtrStatement*** vtrs);
    void VtrStatements(CLgsVtrStatement** vtrs, int num);
	void Serialize(CArchive& ar);
	int TableNo();
	void TableNo(int no);

	void SetStripped(bool stripped);
	void GetHints(unsigned short** hints, LgsString*** comments);
	void SetHints(unsigned short* hints, LgsString** comments);

private:
    short m_id;
    short m_level;
    short m_tableNo;
    LgsString m_description;
	short m_numVtrs;
    CLgsVtrStatement** m_vtrStatement;
	CExtensionRec* m_ext;

	// the low order byte of m_hints[i] minus one is an index to m_comments[],
	// or 0xff if the line does not have preceeding comment
	// the high order byte is the number of vtr numbers in this line
	// Comment lines after the last vtr line are dropped
	unsigned short* m_hints;
	LgsString** m_comments;	// vtr comments

	// exists only at run-time
	bool m_bStripped;
	DECLARE_SERIAL(CLgsTable);
};

#endif // !defined(AFX_LGSTABLE_H__CDD55120_4656_11D1_B50A_0020AF7669B3__INCLUDED_)
