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
// LgsLanguage.cpp: implementation of the CLgsLanguage class.
//
//////////////////////////////////////////////////////////////////////

#ifdef _MSC_VER
#include <logos_libs/GrammarRules/stdafx.h>
#endif
#include <logos_libs/GrammarRules/LgsLanguage.h>

IMPLEMENT_SERIAL(CLgsLanguage,CObject,1);

// list of supported languages (language names)
const LPCTSTR CLgsLanguage::mc_langName[] =
	{ "NA",
	  "German",
	  "English",
	  "French",
	  "Spanish",
	  "Italian",
	  "Portugese" };
#define NumLangs (sizeof(mc_langName)/sizeof(mc_langName[0]))

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
CLgsLanguage::CLgsLanguage() { m_code = 0; }
CLgsLanguage::CLgsLanguage(CLgsLanguage& lang) { m_code = lang.m_code; }
CLgsLanguage::~CLgsLanguage() {}

void CLgsLanguage::Serialize(CArchive& ar)
{
	if ( ar.IsStoring() )
	{
		ar << m_code;
	}
	else
	{
		ar >> m_code;
	}
}

int CLgsLanguage::toInt() { return m_code; }

LPCTSTR CLgsLanguage::toString()
{
	return mc_langName[m_code];
}

bool
CLgsLanguage::parse(LPCTSTR langName)
{
    for ( int i = 0; i < NumLangs; i++ )
    {
        if ( !lstrcmp(langName, mc_langName[i]) )
        {
            m_code = i;
            break;
        }
    }

	if ( m_code >= 0 && m_code < NumLangs )
		return true;
	else
	{
		m_code = 0;
		return false;
	}
}
