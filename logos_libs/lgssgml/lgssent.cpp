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
//////////////////////////////////////////////////////////////////////
//
// LgsSent.cpp: implementation of the CLgsSent class.
//
//////////////////////////////////////////////////////////////////////
#include <logos_include/logoscommon.h>
#include <logos_libs/lgssgml/lgsutil.h>
#include <logos_libs/lgssgml/lgssent.h>

// -------------------------------------------------------------------
CLgsSent::CLgsSent()
{
   m_bTran = false; //Not a translatable sentence.
   m_nSentId = -1;  //Invalid Sent Id
   m_nSentSpaceCount = -1; //Invalid Number of Spaces 
}
// -------------------------------------------------------------------
CLgsSent::~CLgsSent()
{
   m_vLgsWord.clear();
   m_vProtectedString.clear();
}
// -------------------------------------------------------------------
void CLgsSent::GetFormatSentence(const char* formatSent)
{
   m_vLgsWord.clear();
   m_vProtectedString.clear();

   // pointer to sentence info
   const char *info = formatSent;

   // needs tran
	int tran = 0;
	memcpy((char *)&tran, info, sizeof(int));
   info += sizeof(int);
	m_bTran = (tran != 0);

   // sentence id
	if (m_bTran)
   {
	   memcpy((char *)&m_nSentId, info, sizeof(int));
      info += sizeof(int);
   }

	// number of spaces
   memcpy((char *)&m_nSentSpaceCount, info, sizeof(int));
   info += sizeof(int);

	// space string
	if (m_nSentSpaceCount > 0)
	{
		char* buf = new char[m_nSentSpaceCount + 1];
		CLgsUtil::Assert(NULL != buf, "Memory Allocation Failure");
	   memcpy(buf, info, m_nSentSpaceCount);
      info += m_nSentSpaceCount;
		buf[m_nSentSpaceCount] = 0;
		m_sSentSpace = buf;
		delete[] buf;
	}

	// protected strings
	int numPs = 0;
	memcpy((char *)&numPs, info, sizeof(int));
   info += sizeof(int);
        int i;
	for (i = 0; i < numPs; i++)
	{
		CLgsProtectedString ps;
		info = ps.Load(info);
		m_vProtectedString.push_back(ps);
	}

   // lgs words
	int numWords = 0;
	memcpy((char *)&numWords, info, sizeof(int));
   info += sizeof(int);
	for (i = 0; i < numWords; i++)
	{
		CLgsWord w;
		info = w.Load(info);
		m_vLgsWord.push_back(w);
	}
}
// -------------------------------------------------------------------
bool CLgsSent::GetWord(int Id, CLgsWord& lgsWord)
{
   // Description: Gets a word in the sentence by its Id. Returns TRUE on getting
   // the word from the sentence with the Id passed. Otherwise, returns FALSE.
   for (WVIT wvIt = m_vLgsWord.begin(); wvIt != m_vLgsWord.end(); wvIt++)
   {
      if (wvIt->Id() == Id)
      {
         //Check whether the string was user protected
         //also do a bound check of the vector m_vProtectedString
         int numProtStr = m_vProtectedString.size();
         if (wvIt->IsProtected())
         {
            int seqnum = wvIt->SequenceNumber();
            int nPSIndex = GetPSIndexBySeqNum(seqnum);
            if (nPSIndex != -1)
            {
               lgsWord = m_vProtectedString[nPSIndex].m_string.c_str();
            }
            else
            {
               lgsWord = *wvIt;
            }
         }
         else
         {
            lgsWord = *wvIt;
         }
         return true;
      }
   }
   return false;
}
// -------------------------------------------------------------------
bool CLgsSent::AppendWord(CLgsWord& word)
{
   // Appends a new word to the sentence
   if (!word.IsValid()) //sanity check
   {
      return false;
   }
   else
   {
      m_vLgsWord.push_back(word);
   }
   return true;
}
// -------------------------------------------------------------------
bool CLgsSent::GetLastWord(CLgsWord& word)
{
   // Gets the last word in a sentence.
   if (m_vLgsWord.empty())
   {
      return false;
   }
   else
   {
      word = m_vLgsWord.back();
   }
   return true;
}
// -------------------------------------------------------------------
int CLgsSent::GetPSIndexBySeqNum(int seqNum)
{
	// given a sequence number, this function returns protected string index in
   // the m_psStr array, or -1 if there is no such protected string
	for (int i = 0; i < m_vProtectedString.size(); i++)
	{
		if (seqNum == m_vProtectedString[i].m_seqNum)
      {
			return i;
      }
	}
	return -1;
}

