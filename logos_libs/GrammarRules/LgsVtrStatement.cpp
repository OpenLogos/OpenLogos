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
// LgsVtrStatement.cpp: implementation of the CLgsVtrStatement class.
//
//////////////////////////////////////////////////////////////////////

#ifdef _MSC_VER
#include <logos_libs/GrammarRules/stdafx.h>
#endif
#include <logos_libs/GrammarRules/LgsVtrStatement.h>

IMPLEMENT_SERIAL(CLgsVtrStatement,CObject,1);

const int CLgsVtrStatement::MinSwVal = -68;
const int CLgsVtrStatement::MaxSwVal = -11;

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CLgsVtrStatement::CLgsVtrStatement()
{
	//m_description = "";
	m_num = 0;
	m_statement = NULL;
}

CLgsVtrStatement::CLgsVtrStatement(short* s, int num)
{
	//m_description = "";
	if ( s && 0 == num ) throw("");
	if ( !s && num > 0) throw("");
	m_num = num;
	m_statement = s;
}

CLgsVtrStatement::~CLgsVtrStatement()
{
	if (m_statement)
      delete[] m_statement;
}

void CLgsVtrStatement::Serialize(CArchive& ar)
{
	int i;
//	unsigned int _ll;

	if ( ar.IsStoring() )
	{
		//ar << m_description.length();
		//if ( m_description.length() > 0)
		//	ar.Write(m_description.c_str(), m_description.length());

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

int CLgsVtrStatement::GetNumbers(short** pNums)
{
	*pNums = m_statement;
	return m_num;
}

LPCTSTR CLgsVtrStatement::Description()
{
	return NULL;
	//return m_description.c_str();
}

void CLgsVtrStatement::Description(LPCTSTR d)
{
	//if ( d ) m_description = d;
	//else m_description = "";
}

// compat func: linear vtrs
bool CLgsVtrStatement::parseVtrNumbers(
		short* nums, int numNums, CLgsVtrStatement*** pVtrs, short* pNumVtrs)
{
	if ( !pVtrs || !pNumVtrs)
		return false;
	int numVtrs = 1;
    CLgsVtrStatement** allvtrs = new CLgsVtrStatement* [numVtrs];
    allvtrs[0] = new CLgsVtrStatement(nums, numNums);

	*pVtrs = allvtrs;
	*pNumVtrs = numVtrs;
	return true;
}

bool CLgsVtrStatement::IsEqualTo(CLgsVtrStatement* r)
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
