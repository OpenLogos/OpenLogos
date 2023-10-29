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
//////////////////////////////////////////////////////////////////////
// CLgsSent.h: interface for the CLgsSent class.
//////////////////////////////////////////////////////////////////////

#ifndef __lgssent_h__
#define __lgssent_h__
 
#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include <logos_libs/lgssgml/lgsword.h>
#include <logos_libs/lgssgml/lgssentence.h>

class CLgsSent  
{
public:
	CLgsSent();
	virtual ~CLgsSent();

   bool GetLastWord(CLgsWord& word);
	bool AppendWord(CLgsWord& word);
   bool GetWord(int Id, CLgsWord& lgsWord);
   
   bool IsSentTran();
   int GetSentId();
   int GetPSIndexBySeqNum(int seqNum);
   void GetFormatSentence(const char* formatSent);

private:
   int m_nSentId; //Sentence number
   bool m_bTran; //Flag to indicate that the sentence is Translatable
   int m_nSentSpaceCount; //Count of spaces before the sentence begining
   LgsString m_sSentSpace; //string containing spaces before the sentence
   PSV m_vProtectedString; //Vector of protected strings
   WV m_vLgsWord; //Vector of words which constitute the sentence
};

inline bool CLgsSent::IsSentTran()
{
   return m_bTran;
}

inline int CLgsSent::GetSentId()
{
   return m_nSentId;
}

#endif
