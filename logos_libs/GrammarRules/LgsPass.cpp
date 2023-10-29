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
// LgsPass.cpp: implementation of the CLgsPass class.
//
//////////////////////////////////////////////////////////////////////

#ifdef _MSC_VER
#include <logos_libs/GrammarRules/stdafx.h>
#endif
#include <logos_libs/GrammarRules/LgsPass.h>

IMPLEMENT_SERIAL(CLgsPass,CObject,1);

const LPCTSTR CLgsPass::mc_passName[] =
{
	"invalid",
	"res1",
	"res2",
	"res22",
	"parse",
	"parse1",
	"parse2",
	"parse3",
	"parse4",
	"tran",
	"tran1",
	"tran2",
	"tran3",
	"tran4"
};

#define NumPasses (sizeof(mc_passName)/sizeof(mc_passName[0]))

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CLgsPass::CLgsPass() { m_code = 0; }
CLgsPass::CLgsPass(CLgsPass& pass) { m_code = pass.m_code; }
CLgsPass::~CLgsPass() {}

void CLgsPass::Serialize(CArchive& ar)
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

int CLgsPass::toInt() { return m_code; }

LPCTSTR CLgsPass::toString()
{
	return mc_passName[m_code];
}

bool CLgsPass::parse(LPCTSTR passName)
{
    for ( int i = 0; i < NumPasses; i++ )
    {
        if ( !lstrcmp(passName, mc_passName[i]) )
        {
            m_code = i;
            break;
        }
    }

	if ( m_code >= 0 && m_code < NumPasses )
		return true;
	else
	{
		m_code = 0;
		return false;
	}
}
