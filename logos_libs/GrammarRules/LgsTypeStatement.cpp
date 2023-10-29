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
// LgsTypeStatement.cpp: implementation of the CLgsTypeStatement class.
//
//////////////////////////////////////////////////////////////////////

#ifdef _MSC_VER
#include <logos_libs/GrammarRules/stdafx.h>
#endif
#include <logos_libs/GrammarRules/LgsTypeStatement.h>

IMPLEMENT_SERIAL(CLgsTypeStatement,CObject,1);

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CLgsTypeStatement::CLgsTypeStatement()
{
	//m_description = "";
	m_num = 0;
	m_statement = NULL;
	m_hints = NULL;
}

CLgsTypeStatement::CLgsTypeStatement(short* s, int num, char* hints)
{
	//m_description = "";
	if ( !(s && num > 0) )
		throw("");
	m_num = num;
	m_statement = s;
	m_hints = NULL;
	if ( hints )
	{
		m_hints = new char[1+strlen(hints)];
		strcpy(m_hints, hints);
	}
}

CLgsTypeStatement::~CLgsTypeStatement()
{
	if ( m_statement )
      delete[] m_statement;
	if ( m_hints )
      delete[] m_hints;
}

void CLgsTypeStatement::Serialize(CArchive& ar)
{
	int i;
//	unsigned int _ll;

	if ( ar.IsStoring() )
	{
		//// description
		//ar << m_description.length();
		//if ( m_description.length() > 0)
		//	ar.Write(m_description.c_str(), m_description.length());

		//if ( m_num > 1 )
		//{
		//	// get rid of insignificant zeros at the end of a tag set line
		//	while ( m_num > 1 && 0 == m_statement[m_num-1] )
		//		m_num--;
		//}

		// hints
		i = 0;
		if ( m_hints ) i = strlen(m_hints);
		ar << i;
		if ( i > 0 )
			ar.Write(m_hints, i);

		// numbers
		ar << m_num;
		if ( m_num > 0 )
		{
			for ( i = 0; i < m_num; i++ )
				ar << m_statement[i];
		}
	}
	else
	{
		/*
		// description
		m_description.erase();
		ar >> _ll;
		if ( _ll > 0 )
		{
			char* b = new char[_ll+1];
			if ( _ll != ar.Read(b, _ll) )
				throw("");
			b[_ll] = 0;
			m_description = b;
			delete[] b;
		}
		*/

		// hints
		ar >> i;
		if ( m_hints )
			delete[] m_hints, m_hints = NULL;
		if ( i > 0 )
		{
			m_hints = new char[1+i];
			ar.Read(m_hints, i);
			m_hints[i] = 0;
		}

		// numbers
		ar >> m_num;
		if ( m_num > 0 )
		{
			if (m_statement)
            delete[] m_statement;
			m_statement = new short[m_num];
			for ( i = 0; i < m_num; i++ )
				ar >> m_statement[i];
		}
		else
		{
			if (m_statement)
            delete[] m_statement;
			m_statement = NULL;
		}
	}
}

int CLgsTypeStatement::GetNumbers(short** pNums)
{
	*pNums = m_statement;
	return m_num;
}

char* CLgsTypeStatement::GetHints()
{
	return m_hints;
}

bool CLgsTypeStatement::IsEqualTo(CLgsTypeStatement* r)
{
	if ( m_num != r -> m_num )
		return false;

	for ( int i = 0; i < m_num; i++ )
	{
		if ( m_statement[i] != r -> m_statement[i] )
			return false;
	}

	return true;
}
