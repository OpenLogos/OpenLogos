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
// LgsLex.h: interface for the CLgsLex class.
//
//////////////////////////////////////////////////////////////////////
#ifndef __lgslex_h__
#define __lgslex_h__

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include <logos_libs/lgssgml/lgstag.h>
#include <logos_libs/lgssgml/lgsbuffer.h>

#define None ((LexType)-1)
#define MAX_TAG_LENGTH 256


class CLgsLex  
{
public:
	enum LexType
   {
		Error, EndOfFile, Data,
		UserProtChar, LgsExtBegin, LgsExtEnd,
		LgsIntwBegin, LgsIntwEnd,
		LgsIntlBegin, LgsIntlEnd,
      LgsIntldBegin, LgsIntldEnd,
		LgsIntrBegin, LgsIntrEnd,
      LgsIntrdBegin, LgsIntrdEnd,
		LgsTitleBegin, LgsTitleEnd,
		LgsHeaderBegin, LgsHeaderEnd,
		LgsFooterBegin, LgsFooterEnd, LgsPlusMinus
   };

   CLgsLex();
	virtual ~CLgsLex();
	CLgsLex::LexType GetTagType();
	bool Init(const LgsString& fileName, int bufSize, char userProtChar);
	LexType NextItem(char* buf, int bufLen, int& dataLen);

private:
   bool m_bPlusMinus;
   bool m_bExternalData;
   bool m_initialized;	// ob is init-es flag
   char m_userProtChar;// user protection character
   bool m_bLastChar; //Last char was a special char if true
   int m_nLastCharLen;

   char m_readAheadBuf[MAX_TAG_LENGTH];	// read ahead buffer
   int m_bytesReadAhead;					// #of bytes that have been read ahead
   char m_raStart;			// character which started reading ahead
   LexType m_readAheadItem;// type of the item in the read ahead buffer
					            // it will be return on next call to NextItem()
   CLgsTag* m_pLgsTag;
   CLgsBuffer* m_pLgsBuffer;
};

#endif

