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
#ifdef _MSC_VER
#include "stdafx.h"
#else
#define TRY try
#define CATCH( __a, __b) catch( ... )
#define END_CATCH
#endif
#include <logos_libs/GrammarRules/LgsGrammarRules.h>
#include "util.h"
#include "errno.h"

// utility functions used internally by the dll

CLgsRuleSet* LoadRuleSet(const char* file)
{
  CLgsRuleSet* result = NULL;

  TRY {
    CFile in( file, CFile::modeRead );
    CArchive arIn(&in, CArchive::load);
    result = new CLgsRuleSet();
    result -> Serialize(arIn);
    arIn.Close();
    in.Close();
  }
  CATCH( CFileException, e )
    {
      if (result)
        delete result, result = NULL;
      char s[1024];
#ifdef _MSC_VER
      FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NULL, e -> m_lOsError,
                    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), 
                    s, sizeof(s), NULL );
#else
      strerror_r(errno, s, 1023); s[1023]='\0';
#endif
      OutputMsg(file, s);
      return NULL;
    }
  END_CATCH

    return result;
}

CLgsTableSet* LoadTableSet(const char* file, bool verbose)
{
	CLgsTableSet* result = NULL;

	TRY {
		CFile in( file, CFile::modeRead );
		CArchive arIn(&in, CArchive::load);
		result = new CLgsTableSet();
		result -> SetStripped(true);
		result -> Serialize(arIn);
		arIn.Close();
		in.Close();
	}
	CATCH( CFileException, e )
	{
		if (result) delete result, result = NULL;
		if ( verbose )
		{
			char s[1024];
#ifdef _MSC_VER
			FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NULL, e -> m_lOsError,
				MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), s, sizeof(s), NULL );
#else
                        strerror_r(errno, s, 1023); s[1023]='\0';;
#endif
			OutputMsg(file, s);
		}
		return NULL;
	}
	END_CATCH

	return result;
}
