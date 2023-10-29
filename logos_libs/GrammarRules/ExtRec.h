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
// ExtRec.h: interface for the CExtensionRec class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(_EXT_REC_H)
#define _EXT_REC_H

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef _MSC_VER
#include "fake_mfc.h"

#include <cassert>
#define VERIFY assert
#include <cstdio>
#define TRACE(__fmt, __msg) fprintf(stderr, __fmt, __msg)
#endif

#include "logos_include/lgsstring.h"

class CExtensionRec : public CObject
{
public:
	CExtensionRec();
	virtual ~CExtensionRec();
	void Serialize(CArchive& ar);
    LgsString m_user;
    LgsString m_notes;
    CTime m_created;
    CTime m_lastModified;
	DECLARE_SERIAL(CExtensionRec);
};

#if ! defined(HAVE_ITOA) && ! defined(_MSC_VER)
inline char * itoa(int i, char *buffer, int base) {
  int digit, pos = 0;
  bool neg = false;
  if (i < 0) { i = -i ; neg = true; }
  if (i == 0) { buffer[0] = '0' ; pos = 1; }
  else 
    while (i > 0) {
      digit = i % base;
      buffer[pos] = (digit < 10) ? ('0' + digit) : ('A' + (digit - 10));
       i = i / base ; 
       pos ++ ;
    }
  if (neg) { buffer[pos++] = '-'; }
  buffer[pos] = '\0';
  char c;
  for(int j = pos - 1, k = 0; j > k; j--, k++) {
    c = buffer[j];
    buffer[j] = buffer[k];
    buffer[k] = c;
  }
  return buffer;
}
#define _itoa itoa
#endif


#endif // !defined(_EXT_REC_H)
