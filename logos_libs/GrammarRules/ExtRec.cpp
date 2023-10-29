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
// ExtRec.cpp: implementation of the CExtensionRec class.
//
//////////////////////////////////////////////////////////////////////

#ifdef _MSC_VER
#include <logos_libs/GrammarRules/stdafx.h>
#endif
#include <logos_libs/GrammarRules/ExtRec.h>

// CExtensionRec stuff
IMPLEMENT_SERIAL(CExtensionRec,CObject,1);

CExtensionRec::CExtensionRec()
{
    m_created = CTime::GetCurrentTime();
    m_lastModified = m_created;
    m_user = "";
    m_notes = "";
}

CExtensionRec::~CExtensionRec()
{
}

void CExtensionRec::Serialize(CArchive& ar)
{
	unsigned int _ll;

	if ( ar.IsStoring() )
	{
		ar << m_user.length();
		if ( m_user.length() > 0)
			ar.Write(m_user.c_str(), m_user.length());

		ar << m_created;
		ar << m_lastModified;

		ar << m_notes.length();
		if ( m_notes.length() > 0)
			ar.Write(m_notes.c_str(), m_notes.length());

	}
	else
	{
		m_user.erase();
		ar >> _ll;
		if ( _ll > 0 )
		{
			char* b = new char[_ll+1];
			if ( _ll != ar.Read(b, _ll) )
				throw("");
			b[_ll] = 0;
			m_user = b;
			delete[] b;
		}

		ar >> m_created;
		ar >> m_lastModified;

		m_notes.erase();
		ar >> _ll;
		if ( _ll > 0 )
		{
			char* b = new char[_ll+1];
			if ( _ll != ar.Read(b, _ll) )
				throw("");
			b[_ll] = 0;
			m_notes = b;
			delete[] b;
		}
	}
}

