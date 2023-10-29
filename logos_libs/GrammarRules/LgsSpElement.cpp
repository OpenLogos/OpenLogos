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
// LgsSpElement.cpp: implementation of the CLgsSpElement class.
//
//////////////////////////////////////////////////////////////////////

#ifdef _MSC_VER
#include <logos_libs/GrammarRules/stdafx.h>
#endif
#include <logos_libs/GrammarRules/LgsTypeStatement.h>
#include <logos_libs/GrammarRules/LgsSpElement.h>

IMPLEMENT_SERIAL(CLgsSpElement,CObject,1);

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CLgsSpElement::CLgsSpElement()
{
    //m_description = "";
    m_wordClass = 0;
    m_formCode = 0;
    m_pTypeStatement = NULL;
}

CLgsSpElement::CLgsSpElement(int wc, int fc, short* s, int num, char* hints)
{
    //m_description = "";
    m_wordClass = wc;
    m_formCode = fc;
    m_pTypeStatement = new CLgsTypeStatement(s, num, hints);
}

CLgsSpElement::~CLgsSpElement()
{
	if (m_pTypeStatement)
      delete m_pTypeStatement;
}

void CLgsSpElement::Serialize(CArchive& ar)
{
	bool isThere;
	int xxx;
	//unsigned int _ll;

	if (ar.IsStoring())
	{
		//ar << m_description.length();
		//if (m_description.length() > 0)
		//	ar.Write(m_description.c_str(), m_description.length());

		ar << m_formCode;
		ar << m_wordClass;
		isThere = NULL != m_pTypeStatement;
		ar << (int)isThere;
		if (isThere)
			m_pTypeStatement->Serialize(ar);
	}
	else
	{
		/*m_description.erase();
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
		*/

		ar >> m_formCode;
		ar >> m_wordClass;
		ar >> xxx;
		isThere = 0 != xxx;
		if (isThere)
		{
			if (m_pTypeStatement)
            delete m_pTypeStatement;
			m_pTypeStatement = new CLgsTypeStatement();
			m_pTypeStatement->Serialize(ar);
		}
		else
		{
			if (m_pTypeStatement)
			{
				delete m_pTypeStatement;
				m_pTypeStatement = NULL;
			}
		}
	}
}

int CLgsSpElement::WordClass() { return m_wordClass; }
int CLgsSpElement::FormCode() { return m_formCode; }
CLgsTypeStatement* CLgsSpElement::TypeStatement() { return m_pTypeStatement; }

bool CLgsSpElement::IsEqualTo(CLgsSpElement* r)
{
	if (m_wordClass != r->m_wordClass)
		return false;

	if (m_formCode != r->m_formCode)
		return false;

	if (!m_pTypeStatement->IsEqualTo(r->m_pTypeStatement))
		return false;

	return true;
}
