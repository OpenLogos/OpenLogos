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
/////////////////////////////////////////////////////////////
//
// LgsSentence.cpp: implementation of the CLgsSentence class.
//
/////////////////////////////////////////////////////////////
#include <logos_include/logoscommon.h>
#include <logos_libs/regex/charutil.h>
#include <logos_libs/lgssgml/lgsutil.h>
#include <logos_libs/lgssgml/lgsalign.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <logos_libs/lgssgml/lgssentence.h>
#include <configdatafileinterface/configdatainterfacemain.h>
bool CLgsSentence::m_bSingleQuoteOpen = false;
bool CLgsSentence::m_bDoubleQuoteOpen = false;

// -------------------------------------------------------------------
int CLgsProtectedString::calcMsgSize()
{
   int msgSize = 0;
   msgSize += sizeof(int);                      // type
   msgSize += sizeof(int);                      // seqNo
   msgSize += sizeof(int);                      // subtype type
   msgSize += sizeof(int);                      // subtype seqNo
   msgSize += sizeof(int);                      // string length
   msgSize += m_string.length() * sizeof(char); // # chars in string
   msgSize += sizeof(int);                      // lb length
   msgSize += m_lb.length() * sizeof(char);     // # chars in lb
   msgSize += sizeof(int);                      // rb length
   msgSize += m_rb.length() * sizeof(char);     // # chars in rb
   return msgSize;
}
// -------------------------------------------------------------------
char* CLgsProtectedString::Store(char* f)
{
	int dataTemp;
   char* info = f;

	// type
	memcpy(info, (const char*)&m_type, sizeof(int));
   info += sizeof(int);

	// seqNo
	memcpy(info, (const char*)&m_seqNum, sizeof(int));
   info += sizeof(int);

   //subtype
	memcpy(info, (const char*)&(m_subtype.m_nIntType), sizeof(int));
   info += sizeof(int);

	memcpy(info, (const char*)&(m_subtype.m_nIntNum), sizeof(int));
   info += sizeof(int);

	// string
	dataTemp = m_string.length();
	memcpy(info, (const char*)&dataTemp, sizeof(int));
   info += sizeof(int);
	if (dataTemp > 0)
   {
	   memcpy(info, m_string.c_str(), dataTemp);
      info += dataTemp;
   }

	// lb
	dataTemp = m_lb.length();
	memcpy(info, (const char*)&dataTemp, sizeof(int));
   info += sizeof(int);
	if (dataTemp > 0)
   {
	   memcpy(info, m_lb.c_str(), dataTemp);
      info += dataTemp;
   }

	// rb
	dataTemp = m_rb.length();
	memcpy(info, (const char*)&dataTemp, sizeof(int));
   info += sizeof(int);
	if (dataTemp > 0)
   {
	   memcpy(info, m_rb.c_str(), dataTemp);
      info += dataTemp;
   }
   return info;
}
// -------------------------------------------------------------------
const char* CLgsProtectedString::Load(const char* f)
{
	int dataTemp;
   const char* info = f;

	// type
   memcpy((char *)&m_type, info, sizeof(int));
   info += sizeof(int);

	// seqNo
   memcpy((char *)&m_seqNum, info, sizeof(int));
   info += sizeof(int);

   // subtype
   memcpy((char *)&(m_subtype.m_nIntType), info, sizeof(int));
   info += sizeof(int);

   memcpy((char *)&(m_subtype.m_nIntNum), info, sizeof(int));
   info += sizeof(int);

	// LgsString
   memcpy((char *)&dataTemp, info, sizeof(int));
   info += sizeof(int);
	if (dataTemp > 0)
	{
		char* buf = new char[dataTemp + 1];
		CLgsUtil::Assert(NULL != buf, "out of memory");
      memcpy(buf, info, dataTemp);
      info += dataTemp;
		buf[dataTemp] = 0;
		m_string = buf;
		delete[] buf;
	}

	// lb
   memcpy((char *)&dataTemp, info, sizeof(int));
   info += sizeof(int);
	if (dataTemp > 0)
	{
		char* buf = new char[dataTemp + 1];
		CLgsUtil::Assert(NULL != buf, "out of memory");
      memcpy(buf, info, dataTemp);
      info += dataTemp;
		buf[dataTemp] = 0;
		m_lb = buf;
		delete[] buf;
	}

	// rb
   memcpy((char *)&dataTemp, info, sizeof(int));
   info += sizeof(int);
	if (dataTemp > 0)
	{
		char* buf = new char[dataTemp + 1];
		CLgsUtil::Assert(NULL != buf, "out of memory");
      memcpy(buf, info, dataTemp);
      info += dataTemp;
		buf[dataTemp] = 0;
		m_rb = buf;
		delete[] buf;
	}
   return info;
}
// -------------------------------------------------------------------
void CLgsProtectedString::SetSubType(LgsString& lb, const char* markType) 
{
   int nBeginPaired = lb.find("Paired=");
   int nBeginOpen = lb.find("-O");
   int nBeginClose = lb.find("-C");
   int nEnd = lb.find(">");
   if (nBeginPaired != NPOS)
   {
      if (markType[0] == 'r' || markType[0] == 'n') 
         m_subtype.m_nIntType = PST_OPEN;
      else if (markType[0] == 'l' || markType[0] == 'm') 
         m_subtype.m_nIntType = PST_CLOSE;
      int nBegin = nBeginPaired + strlen("Paired=");
      m_subtype.m_nIntNum = ::atoi(lb.substr(nBegin, nEnd-nBegin).c_str());
   }
   else if (nBeginOpen != NPOS &&
            (markType[0] == 'r' || markType[0] == 'n'|| markType[0] == 'l' || markType[0] == 'm'))
   {
      m_subtype.m_nIntType = PST_OPEN;
      int nBegin = nBeginOpen + strlen("-O");
      m_subtype.m_nIntNum = ::atoi(lb.substr(nBegin, nEnd-nBegin).c_str());
   }
   else if (nBeginClose != NPOS &&
            (markType[0] == 'r' || markType[0] == 'n'|| markType[0] == 'l' || markType[0] == 'm'))
   {
      m_subtype.m_nIntType = PST_CLOSE;
      int nBegin = nBeginClose + strlen("-C");
      m_subtype.m_nIntNum = ::atoi(lb.substr(nBegin, nEnd-nBegin).c_str());
   }
   else
   {
      m_subtype.m_nIntType = PST_NONE;
      m_subtype.m_nIntNum = 0;
   }

}
// -------------------------------------------------------------------
int PairEntry::calcMsgSize()
{
   int msgSize = 0;
   msgSize += sizeof(unsigned int); // pairFlag
   msgSize += sizeof(int);          // intrSeqNum
   msgSize += sizeof(int);          // intlSeqNum
   return msgSize;
}
// -------------------------------------------------------------------
char* PairEntry::Store(char* f)
{
   char* info = f;
	memcpy(info, (const char*)&nPairFlag, sizeof(unsigned int));
   info += sizeof(unsigned int);
	memcpy(info, (const char*)&nIntrSeqNum, sizeof(int));
   info += sizeof(int);
	memcpy(info, (const char*)&nIntlSeqNum, sizeof(int));
   info += sizeof(int);

   return info;
}
// -------------------------------------------------------------------
const char* PairEntry::Load(const char* f)
{
   const char* info = f;
	memcpy((char *)&nPairFlag, info, sizeof(unsigned int));
   info += sizeof(unsigned int);
	memcpy((char *)&nIntrSeqNum, info, sizeof(int));
   info += sizeof(int);
	memcpy((char *)&nIntlSeqNum, info, sizeof(int));
   info += sizeof(int);

   return info;
}
// -------------------------------------------------------------------
CLgsProtSubType CLgsProtectedString::GetSubType() 
{
   return m_subtype;
}
// -------------------------------------------------------------------
CLgsSentence::CLgsSentence()
{
   m_bEOS = false;
	m_pTranIO = NULL;
   m_fmt = NULL;
   m_align = NULL;
	m_numTrailingSpaces = 0;
	m_flags = 0;
	m_nSentId = 1;
   m_bNeedsTran = true;
   m_nSentFlags = 0;
   m_bAlign = false;
   m_totalTransSent = 0;
   m_translationMode = (LgsDBCommonObjects::GetJobControlArguments().RunMode() == JobControlArguments::translationMode);
}
// -------------------------------------------------------------------
CLgsSentence::~CLgsSentence()
{
   if (m_fmt)
   {
      delete m_fmt;
   }
	if (m_pTranIO)
   {
      delete m_pTranIO;
   }
   if (m_align)
   {
      delete m_align;
   }
}
// -------------------------------------------------------------------
int CLgsSentence::calcMsgSize(int start, int count, bool needsTran)
{
   int i;
   int msgSize = 0;

   msgSize += sizeof(int);                                        // needs tran
   if (needsTran)
   {
      msgSize += sizeof(int);                                     // sent ID
   }

   msgSize += sizeof(int);                                        // num spaces
   msgSize += m_lgsWords[start].TotalSpaces() * sizeof(char);     // space string
   msgSize += sizeof(int);                                        // num protected strings

	// go through all words in the sentence, looking for protected words,
	// or words with attributes assigned, and add totals to message size.
	for (i = start; i < start + count; i++)
	{
      if (m_lgsWords[i].GetTranslate())
      {
		   INTV v;
		   INTV vNumSps;
		   STRV vSps;

		   // if word is protected
		   if (m_lgsWords[i].IsProtected())
		   {
			   int psIx = GetPSIndexBySeqNum(m_lgsWords[i].SequenceNumber());
			   if (psIx != -1)
            {
				   msgSize += m_psStr[psIx].calcMsgSize();
            }
		   }

		   // count intl/intr tags associated with the word if any
		   if (m_lgsWords[i].Attributes(v, vSps) > 0)
		   {
			   for (int k = 0; k < v.size(); k++)
			   {
				   int psIx = GetPSIndexBySeqNum(v[k]);
				   if (psIx != -1)
               {
					   msgSize += m_psStr[psIx].calcMsgSize();
               }
			   }
		   }
      }
	}

   for (PairEntryIterator pit = m_vPairTable.begin(); pit != m_vPairTable.end(); pit++)
   {
      int psIx = GetPSIndexBySeqNum(pit->nIntrSeqNum);
      if (psIx != -1)
      {
         msgSize += m_psStr[psIx].calcMsgSize();
      }
      psIx = GetPSIndexBySeqNum(pit->nIntlSeqNum);
      if (psIx != -1)
      {
         msgSize += m_psStr[psIx].calcMsgSize();
      }
   }

   msgSize += sizeof(int);                      // num words

	for (i = start; i < start + count; i++)      // words themself
   {
      if (m_lgsWords[i].GetTranslate())
      {
         msgSize += m_lgsWords[i].calcMsgSize();
      }
   }

   msgSize += sizeof(int);                      // num pairs

   for (i = 0; i < m_vPairTable.size(); i++)    // pairs themself
   {
      msgSize += m_vPairTable[i].calcMsgSize();
   }
   return msgSize;
}
// -------------------------------------------------------------------
void CLgsSentence::print_sentence(int start, int count, bool needsTran, int sentId)
{
   int i;
   if (m_translationMode)
   {
	   int senNumSps = 0;
	   LgsString senSpStr;
      m_vPairTable.clear();

      if (needsTran)
      {
         PairTags(start, count);
         ApplyDupTags(start, count);
      }
	   if (count > 0)
      {
		   senNumSps = m_lgsWords[start].SpaceInfo(senSpStr);
      }

      int msgSize = calcMsgSize(start, count, needsTran);
      char* msgBuf = new char[msgSize];
      CLgsUtil::Assert(NULL != msgBuf, "out of memory");
      char* info = msgBuf;

	   // needs tran
	   int requiresTran = (int)(needsTran ? true : false);
	   memcpy(info, (const char*)&requiresTran, sizeof(int));
      info += sizeof(int);
	   if (requiresTran)
      {
	      memcpy(info, (const char*)&sentId, sizeof(int));
         info += sizeof(int);
      }

	   // sentence's numSps
	   memcpy(info, (const char*)&senNumSps, sizeof(int));
      info += sizeof(int);

	   // sen's spStr
	   if (senNumSps > 0)
      {
	      memcpy(info, senSpStr.c_str(), senSpStr.length());
         info += senSpStr.length();
      }

	   // count # of protected strings, and print it
	   int numPs = 0;
	   for (i = start; i < start + count; i++)
	   {
         if (m_lgsWords[i].GetTranslate())
         {
		      INTV v;
		      STRV vSps;

		      // print intw/user protected sequences if applicable
		      if (m_lgsWords[i].IsProtected() && (GetPSIndexBySeqNum(m_lgsWords[i].SequenceNumber()) != -1))
            {
               numPs++;
            }

		      // print intl/intr tags associated with the word if any
		      if (m_lgsWords[i].Attributes(v, vSps) > 0)
		      {
			      for (int k = 0; k < v.size(); k++)
               {
				      if (GetPSIndexBySeqNum(v[k]) != -1)
                  {
                     numPs++;
                  }
               }
		      }
         }
	   }
   
      numPs += m_vPairTable.size() * 2;

	   memcpy(info, (const char*)&numPs, sizeof(int));
      info += sizeof(int);

	   // go through all words in the sentence, looking for protected words,
	   // or words with attributes assigned, printing out protected strings
	   for (i = start; i < start + count; i++)
	   {
         if (m_lgsWords[i].GetTranslate())
         {
		      INTV v;
		      INTV vNumSps;
		      STRV vSps;

		      // print intw/user protected sequences if applicable
		      if (m_lgsWords[i].IsProtected())
		      {
			      int psIx = GetPSIndexBySeqNum(m_lgsWords[i].SequenceNumber());
			      if (psIx != -1)
               {
				      info = m_psStr[psIx].Store(info);
                  m_psStr[psIx].m_bMarkedForDeletion = true;
               }
		      }

		      // print intl/intr tags associated with the word if any
		      if (m_lgsWords[i].Attributes(v, vSps) > 0)
		      {
			      for (int k = 0; k < v.size(); k++)
			      {
				      int psIx = GetPSIndexBySeqNum(v[k]);
				      if (psIx != -1)
                  {
					      info = m_psStr[psIx].Store(info);
                     m_psStr[psIx].m_bMarkedForDeletion = true;
                  }
			      }
		      }
         }
	   }

      for (PairEntryIterator pit = m_vPairTable.begin(); pit != m_vPairTable.end(); pit++)
      {
         int psIx = GetPSIndexBySeqNum(pit->nIntrSeqNum);
         if (psIx != -1)
         {
            info = m_psStr[psIx].Store(info);
            m_psStr[psIx].m_bMarkedForDeletion = true;
         }
         psIx = GetPSIndexBySeqNum(pit->nIntlSeqNum);
         if (psIx != -1)
         {
            info = m_psStr[psIx].Store(info);
            m_psStr[psIx].m_bMarkedForDeletion = true;
         }
      }

	   // print #of words to be printed out, and the words themselves
	   int numWords = 0;
	   for (i = start; i < start + count; i++)
      {
         if (m_lgsWords[i].GetTranslate())
         {
			   numWords++;
         }
      }
	   memcpy(info, (const char*)&numWords, sizeof(int));
      info += sizeof(int);

	   for (i = start; i < start + count; i++)
      {
         if (m_lgsWords[i].GetTranslate())
         {
            info = m_lgsWords[i].Store(info);
         }
      }

      int numPairs = m_vPairTable.size();
	   memcpy(info, (const char*)&numPairs, sizeof(int));
      info += sizeof(int);

      for (i = 0; i < numPairs; i++)
      {
         info = m_vPairTable[i].Store(info);
      }
   
      // Send the message
      m_fmt->sendMsg((unsigned int)CLgsFmtPipe::SENT, msgBuf, msgSize);
      m_totalTransSent++;

      // If the sentence is to be aligned
      if (m_bAlign && needsTran)
      {
         m_align->GetFormatSentence(msgBuf, sentId);
      }
      delete[] msgBuf;
   }
   else
   {
	   // go through all words in the sentence, looking for protected words,
	   // or words with attributes assigned, printing out protected strings
	   for (i = start; i < start + count; i++)
	   {
         if (m_lgsWords[i].GetTranslate())
         {
		      INTV v;
		      INTV vNumSps;
		      STRV vSps;

		      // print intw/user protected sequences if applicable
		      if (m_lgsWords[i].IsProtected())
		      {
			      int psIx = GetPSIndexBySeqNum(m_lgsWords[i].SequenceNumber());
			      if (psIx != -1)
               {
                  m_psStr[psIx].m_bMarkedForDeletion = true;
               }
		      }

		      // print intl/intr tags associated with the word if any
		      if (m_lgsWords[i].Attributes(v, vSps) > 0)
		      {
			      for (int k = 0; k < v.size(); k++)
			      {
				      int psIx = GetPSIndexBySeqNum(v[k]);
				      if (psIx != -1)
                  {
                     m_psStr[psIx].m_bMarkedForDeletion = true;
                  }
			      }
		      }
         }
	   }

      for (PairEntryIterator pit = m_vPairTable.begin(); pit != m_vPairTable.end(); pit++)
      {
         int psIx = GetPSIndexBySeqNum(pit->nIntrSeqNum);
         if (psIx != -1)
         {
            m_psStr[psIx].m_bMarkedForDeletion = true;
         }
         psIx = GetPSIndexBySeqNum(pit->nIntlSeqNum);
         if (psIx != -1)
         {
            m_psStr[psIx].m_bMarkedForDeletion = true;
         }
      }
   }

   //Delete all the Protected Strings that have been stored to disk
   LgsVector(CLgsProtectedString) tmpPsStr;
   int psSize = m_psStr.size();
   for (i = 0; i < psSize;  i++)
   {
      CLgsProtectedString ps = m_psStr[i];
      if (!ps.m_bMarkedForDeletion)
      {
         tmpPsStr.push_back(ps);
      }
   }
   m_psStr.clear();
   m_psStr = tmpPsStr; 
}
// -------------------------------------------------------------------
inline static void SetFlags(unsigned int& flags, CLgsWord& word)
{
	if (flags & FL_BOLD)
		word.SetFlag(CLgsWord::bold);
	else
		word.ClearFlag(CLgsWord::bold);

	if (flags & FL_ITALIC)
		word.SetFlag(CLgsWord::italic);
	else
		word.ClearFlag(CLgsWord::italic);

	if (flags & FL_UNDERLINE)
		word.SetFlag(CLgsWord::underline);
	else
		word.ClearFlag(CLgsWord::underline);

  	if (flags & FL_DBLUNDERLINE)
		word.SetFlag(CLgsWord::dblunderline);
	else
		word.ClearFlag(CLgsWord::dblunderline);

   if (flags & FL_DBLQUOTE)
		word.SetFlag(CLgsWord::dblquote);
	else
		word.ClearFlag(CLgsWord::dblquote);

   if (flags & FL_SQUAREBRK)
		word.SetFlag(CLgsWord::squarebrk);
	else
		word.ClearFlag(CLgsWord::squarebrk);

   if (flags & FL_ANGLEBRK)
		word.SetFlag(CLgsWord::anglebrk);
	else
		word.ClearFlag(CLgsWord::anglebrk);

   if (flags & FL_PARENTHES)
		word.SetFlag(CLgsWord::parenthes);
	else
		word.ClearFlag(CLgsWord::parenthes);

   if (flags & FL_CURLBRACE)
		word.SetFlag(CLgsWord::curlbrace);
	else
		word.ClearFlag(CLgsWord::curlbrace);

   if (flags & FL_SGLQUOTE)
		word.SetFlag(CLgsWord::sglquote);
	else
		word.ClearFlag(CLgsWord::sglquote);

}
// -------------------------------------------------------------------
void CLgsSentence::AddProtectedWord(int seqNum, const char* markType, LgsString& pwStr,
                                    LgsString lb, LgsString rb)
{
	char buf[1024];
	CLgsProtectedString ps;

	ps.m_seqNum = seqNum;
	switch(markType[0])
   {
      case 'w':
         ps.m_type = PST_INTW;
         break;
      case 'l':
         ps.m_type = PST_INTL;
         ps.SetSubType(lb, markType);
         break;
      case 'r':
         ps.m_type = PST_INTR;
         ps.SetSubType(lb, markType);
         break;
      case 'm':
         ps.m_type = PST_INTLD;
         ps.SetSubType(lb, markType);
         break;
      case 'n':
         ps.m_type = PST_INTRD;
         ps.SetSubType(lb, markType);
         break;
      default:
         ps.m_type = PST_USER;
         break;
   }
	ps.m_string = pwStr;
	ps.m_lb = lb;
	ps.m_rb = rb;
   ps.m_bMarkedForDeletion = false;
	m_psStr.push_back(ps);

	LgsString m = "<";
	m += markType;
	m += ",";
   m += CLgsUtil::itoa(seqNum);
	m += ">";
	strcpy(buf, m.c_str());
	MoreData(buf, m.length());
}
// -------------------------------------------------------------------
void CLgsSentence::MoreData(char * buf, int bufLen)
{
	// one byte has been reservered for line termination character by the caller
	buf[bufLen] = 0;
	LgsString phrase = buf;

   //Convert entities to chars before translating
	m_entity.replaceEntity(phrase);

	STRV tokens;
	LgsVector(unsigned char) flags;
	m_scn.Scan(phrase, tokens, flags);
	int numTokens = tokens.size();

	bool f = false;
	for (int i = 0; i < flags.size(); i++)
	{
		if (SCF_SPACES != flags[i])
		{
			f = true;
			break;
		}
	}

	if (!f)
	{
		if (bufLen > 0)
		{
			m_numTrailingSpaces += bufLen;
			m_trailingSpaces += buf;
		}
		return;
	}

	for (int ix = 0; ix < numTokens;)
	{
		LgsString res = tokens[ix];
		int ixIncr = 1;
		
		// sequences like <something> should be one word, except...
		if (SCF_PUNCTUATION == flags[ix] && !tokens[ix].compare("<") &&
		    ix < numTokens-1 && strchr("lrwsmndDIBUibu/", tokens[ix+1].c_str()[0]))
		{
			LgsString s = res;

			for (int i = 1; i < 10; i++)
			{
				if (ix + i >= numTokens)
               break;
				if (SCF_SPACES == flags[ix+i])
               break;
				s += tokens[ix+i];
				if (SCF_PUNCTUATION == flags[ix+i] && !tokens[ix+i].compare(">"))
				{
					ixIncr = i+1;
					res = s;
					break;
				}
			}
		}// sequences like <something> should be one word, except...

		//Expressions like '95 should be kept together.
		if (ixIncr == 1 && ix < numTokens-1 && !tokens[ix].compare("\'") &&
		    SCF_PUNCTUATION == flags[ix] && CharUtil::isNumeric(tokens[ix+1][0]) &&
          !m_bSingleQuoteOpen)
		{
			LgsString s = res;
			for (int i = 1; i < 10; i++)
			{
				if (ix + i >= numTokens)
				{
					res = s;
					break;
				}
				if (SCF_SPACES == flags[ix+i])
				{
					res = s;
					break;
				}
				s += tokens[ix+i];
				ixIncr = i+1;
			}
		}
		else
		{
			if (ixIncr == 1 && ix < numTokens  && !tokens[ix].compare("\'"))
			{
				// A starting quote can not be preceded by a period.
				if (m_bSingleQuoteOpen)
               m_bSingleQuoteOpen = !m_bSingleQuoteOpen;
				else if (!((ix >0)  && (!tokens[ix-1].compare(".")))) 
				   m_bSingleQuoteOpen = !m_bSingleQuoteOpen;
			}
		}//Expressions like '95 should be kept together.

		LgsString res2 = tokens[ix];

		if (ixIncr == 1 && ix < numTokens  && !tokens[ix].compare("\"") && !((ix > 0) && (ix+1 < numTokens) &&  !tokens[ix-1].compare("(") && !tokens[ix+1].compare(")")))
		{
			// A starting quote can not be preceded by a period.
			if (m_bDoubleQuoteOpen)
            m_bDoubleQuoteOpen = !m_bDoubleQuoteOpen;
			else if (!((ix > 0) && (!tokens[ix-1].compare(".")))) 
			   m_bDoubleQuoteOpen = !m_bDoubleQuoteOpen;
		}

		//Contractions like "Prospects'", "Can't", "I've" should be held together.
		if (ixIncr == 1 && ix+1 <= numTokens-1 && !tokens[ix+1].compare("\'") &&
          SCF_PUNCTUATION == flags[ix+1] && SCF_SPACES != flags[ix])
		{
			if (!m_bSingleQuoteOpen) 
			{
				if (CharUtil::isAlphabetic(tokens[ix][tokens[ix].length()-1]))
				{
					res = tokens[ix] + tokens[ix+1];
					ixIncr = 2;
					if (ix + 2 <= numTokens - 1 && CharUtil::isAlphabetic(tokens[ix+2][0]))
					{
						res = res + tokens[ix+2];
						ixIncr = 3;
					}
				}
			}
			else if ((ix+2 < numTokens)  && (CharUtil::isAlphabetic(tokens[ix][tokens[ix].length()-1])) &&
				      (CharUtil::isAlphabetic(tokens[ix+2][0])) && (SCF_SPACES != flags[ix+2]))
			{
				res = tokens[ix] + tokens[ix+1] +tokens[ix+2];
				ixIncr = 3;
			}
         // A starting quote can not be preceded by a period.
			else if (!((ix >0)  && (!tokens[ix].compare("."))))
				m_bSingleQuoteOpen = true;
		  
		}//Contractions like "Prospects'", "Can't", "I've" should be held together.

		//Contractions like 45" should be held together.
		if (ixIncr == 1 && ix+1 <= numTokens-1 && !tokens[ix+1].compare("\"") &&
          SCF_PUNCTUATION == flags[ix+1] && SCF_SPACES != flags[ix])
		{
			if (!m_bDoubleQuoteOpen)
			{
				if (CharUtil::isNumeric(tokens[ix][tokens[ix].length()-1]))
				{
					res = tokens[ix] + tokens[ix+1];
					ixIncr = 2;
				}
			}
			else
			m_bDoubleQuoteOpen = true;
		}

		// Contractions like 45' should be held together.
		if (ixIncr == 1 && ix+1 <= numTokens-1 && !tokens[ix+1].compare("\'") &&
          SCF_PUNCTUATION == flags[ix+1] && SCF_SPACES != flags[ix])
		{
			if (!m_bSingleQuoteOpen)
			{
				if (CharUtil::isNumeric(tokens[ix][tokens[ix].length()-1]))
				{
					 res = tokens[ix] + tokens[ix+1];
					ixIncr = 2;
					//Contraction like 1980's should be held together
					if (ix+ixIncr < numTokens && SCF_GENERIC_WORD == flags[ix+ixIncr])
					{
						res += tokens[ix+ixIncr];
						ixIncr++;
					}
				}
			}
			else
			{
			  m_bSingleQuoteOpen = true;
			}
		}

		// Expressions like 10% should be kept together
		if (ixIncr == 1 && ix+1 < numTokens-1 && !tokens[ix+1].compare("%") &&
          SCF_PUNCTUATION == flags[ix+1] && SCF_SPACES != flags[ix])
		{
			if (CharUtil::isNumeric(tokens[ix][tokens[ix].length()-1]))
			{
				 res = tokens[ix] + tokens[ix+1];
				 ixIncr = 2;
			}
		}

		// Expressions like $1000 should be kept together
		if (ixIncr == 1 && ix+1 < numTokens-1 && !tokens[ix].compare("$") &&
          SCF_PUNCTUATION == flags[ix] && SCF_SPACES != flags[ix+1])
		{
			if (CharUtil::isNumeric(tokens[ix+1][0]))
			{
				 res = tokens[ix] + tokens[ix+1];
				 ixIncr = 2;
			}
		}
		if ((SCF_GENERIC_WORD == flags[ix]) && (tokens[ix].length() > 1) &&
			 CharUtil::isNumeric(tokens[ix][0]) && (ixIncr == 1))
		{
			LgsString restofstring = tokens[ix];
			LgsString strTemp =  "";
			LgsString spString = "";
			if (ix > 0 && flags[ix-1] == SCF_SPACES)
				spString = tokens[ix-1];
                        int i;
			for (i = 0; i < tokens[ix].length() && strchr("0123456789.,-", tokens[ix][i]); i++)
            strTemp += tokens[ix][i];
			restofstring = restofstring.substr(i);
			m_ss.push_back(spString.length());
         m_sps.push_back(spString);
         m_ts.push_back(strTemp);
         m_flg.push_back(SCF_LITERAL == flags[ix]);
			spString.erase();
			if (restofstring.length())
			{
				m_ss.push_back(spString.length());
				m_sps.push_back(spString);
				m_ts.push_back(restofstring);
				m_flg.push_back(SCF_LITERAL == flags[ix]);
			}
			ix += 1;
			continue;
		}

		if (SCF_SPACES != flags[ix])
		{
			int numSpaces;
			LgsString spString;

			if (0 == ix || SCF_SPACES != flags[ix-1])
			{
				numSpaces = 0;
				spString = "";
			}
			else
			{
				numSpaces = tokens[ix-1].length();
				spString = tokens[ix-1];
			}

			if (0 == ix && m_numTrailingSpaces > 0)
			{
				numSpaces += m_numTrailingSpaces;
				spString += m_trailingSpaces;
			}

         m_ss.push_back(numSpaces);
         m_sps.push_back(spString);
         m_ts.push_back(res);
         m_flg.push_back(SCF_LITERAL == flags[ix]);
		}
		ix += ixIncr;
	}

	if (SCF_SPACES == flags[numTokens-1])
	{
		m_numTrailingSpaces = tokens[numTokens-1].length();
		m_trailingSpaces = tokens[numTokens-1];
	}
	else
	{
		m_numTrailingSpaces = 0;
		m_trailingSpaces.erase();
	}

	// if there are no trailing spaces, we probably have an incomplete word
	// at the end of m_ts array. The probability of the following hit
	// depends on word length. E.g. if the average word length is 10,
	// the probability is 9/10.
	if (0 != m_numTrailingSpaces)
		WriteSentences(false);
}
// -------------------------------------------------------------------
static inline bool HasBadChars(LgsString& s, bool bCompat)
{
	for (int i = 0; i < s.length(); i++)
	{
		if (i > 0 && '%' == s[i] && CharUtil::isNumeric(s[i-1]))
			return false;
		if (i < s.length() - 1 && '$' == s[i] && CharUtil::isNumeric(s[i+1]))
			return false;
		if (bCompat)
		{
			if (strchr("$%&#*@^_~\x99\xa9\xae\x1a\xb7", s[i])) // The \xb7 is added to protect the middot on 2/12/99 by Anand Bosco.
				return true;                                    
		}
		else
		{
			if (strchr("\x99\xa9\xae\x1a", s[i])) // The char '_' is removed from list on 11/11/99. Ref Ticket 1555.
			   return true;                              
		}
	}

	return false;
}
// -------------------------------------------------------------------
void CLgsSentence::WriteSentences(bool all)
{
   
	int numTokens = m_ts.size();

	if (numTokens > 0)
	{
		// build array of lgs-words, and assign attrbutes to words
		for (int i = 0; i < numTokens; i++)
		{
			CLgsWord w;
         LgsString token = m_ts[i];
         LgsString tokenSpace = m_sps[i];
         LgsString prvToken = "";
         if (i > 0)
            prvToken = m_ts[i-1];
         LgsString nextToken = "";
         if ((i + 1) < numTokens)
            nextToken = m_ts[i+1];
         bool bQuoteInParens = false;
         if (!token.compare("\"") && !prvToken.compare("(") && !nextToken.compare(")"))
            bQuoteInParens = true;


			if (!token.compare("<B>") || !token.compare("<b>"))
			{
				m_flags |= FL_BOLD;
				m_detachedSpaces += tokenSpace;
			}
			else if (!token.compare("</B>") || !token.compare("</b>"))
			{
				m_flags &= ~FL_BOLD;
				m_detachedSpaces += tokenSpace;
			}
			else if (!token.compare("<I>") || !token.compare("<i>"))
			{
				m_flags |= FL_ITALIC;
				m_detachedSpaces += tokenSpace;
			}
			else if (!token.compare("</I>") || !token.compare("</i>"))
			{
				m_flags &= ~FL_ITALIC;
				m_detachedSpaces += tokenSpace;
			}
			else if (!token.compare("<U>") || !token.compare("<u>"))
			{
				m_flags |= FL_UNDERLINE;
				m_detachedSpaces += tokenSpace;
			}
			else if (!token.compare("</U>") || !token.compare("</u>"))
			{
				m_flags &= ~FL_UNDERLINE;
				m_detachedSpaces += tokenSpace;
			}
  			else if (!token.compare("<D>") || !token.compare("<d>"))
			{
				m_flags |= FL_DBLUNDERLINE;
				m_detachedSpaces += tokenSpace;
			}
			else if (!token.compare("</D>") || !token.compare("</d>"))
			{
				m_flags &= ~FL_DBLUNDERLINE;
				m_detachedSpaces += tokenSpace;
			}  // A starting quote can not be preceded by a period.
         else if ((!token.compare("\"") && !bQuoteInParens) && ((m_flags & FL_DBLQUOTE) || (!(!prvToken.compare(".") && tokenSpace.empty()))))
			 // the && !m_compact is added to prevent the setting of
         {// double quotes flag. The double quote is treated as regular word.
            m_flags ^= FL_DBLQUOTE;
            m_detachedSpaces += tokenSpace;
         }
         else if (!token.compare("'")&& ((m_flags & FL_SGLQUOTE) || (!(!prvToken.compare(".") && tokenSpace.empty())))) 
         {
            // A starting quote can not be preceded by a period.
            m_flags ^= FL_SGLQUOTE;
            m_detachedSpaces += tokenSpace;
         }
         else if (token.length() > 3 && token[0] == '<' &&
                 (token[1] == 'l' || token[1] == 'm') && token[2] == ',')
			{
				// L-protected sequence, associate it with the prev word
				int num = m_lgsWords.size();
            LgsVector(int) attrs;
            LgsVector(LgsString) attrSps;
            bool assigned = false;
            if (num > 0)
            {
               m_lgsWords[num-1].Attributes(attrs, attrSps);
               if (attrs.size() > 0)
               {
                  int seq = attrs[attrs.size()-1];
                  int index = GetPSIndexBySeqNum(seq);
                  if (index != -1 && token[1] == 'l')
                     assigned = m_psStr[index].m_type != PST_INTL;
                  else if (index != -1 && token[1] == 'm')
                     assigned = m_psStr[index].m_type != PST_INTLD;
               }
               else
                  assigned = true;
            }
         
            if (assigned)
            {
               m_detachedSpaces += tokenSpace;
					m_lgsWords[num-1].AddAttribute(atoi(token.c_str()+3), tokenSpace);
            }
				else
				{
               m_psNum.push_back(atoi(token.c_str()+3));

				   //The space information has to be associated with the next word.
				   m_detachedSpaces += tokenSpace;
				   tokenSpace = "";
				   m_ss[i] = 0;
				   m_psSps.push_back(tokenSpace);
            } 
			}
			else if (token.length() > 3 && token[0] == '<' &&
					   (token[1] == 'r' || token[1] == 'n') && token[2] == ',')
			{
			   // R-protected sequence. To associate it with the next word
			   // save it in m_psNum array, the association will happen shortly
			   m_psNum.push_back(atoi(token.c_str()+3));

			   //The space information has to be associated with the next word.
			   m_detachedSpaces += tokenSpace;
			   tokenSpace = "";
			   m_ss[i] = 0;
			   m_psSps.push_back(tokenSpace);
			}
			else
			{
				if (token.length() > 3 && token[0] == '<' && token[1] == 'w' && token[2] == ',')
				{
					// W-protected sequence
					w = "";
					w.SetProtected(true);
					w.SetSequenceNumber(atoi(token.c_str()+3));
				}
				else if (token.length() > 3 && token[0] == '<' && token[1] == 's' && token[2] == ',')
				{
					// U-protected sequence
					w = "";
					w.SetProtected(true);
					w.SetSequenceNumber(atoi(token.c_str()+3));
				}
				else
				{
					// regular word
					w = token.c_str();
					w.SetLiteral(m_flg[i]);
					w.SetProtected(HasBadChars(token, false));
					w.SetSequenceNumber(0); // protected str is the word itself
				}

				// space info
				w.SetSpaceInfo(m_ss[i] + m_detachedSpaces.length(),
					tokenSpace + m_detachedSpaces);
				m_detachedSpaces.erase();

				// flags
				SetFlags(m_flags, w);

				// attrs
				w.SetAttributes(m_psNum, m_psSps);
				m_psNum.erase(m_psNum.begin(), m_psNum.end());
				m_psSps.erase(m_psSps.begin(), m_psSps.end());

            // add the word to m_lgsWords array
            m_lgsWords.push_back(w);
			}
		}

      // delete content of m_ts, m_ss, m_flg, and m_sps arrays
		m_ts.erase(m_ts.begin(), m_ts.end());
		m_ss.erase(m_ss.begin(), m_ss.end());
		m_sps.erase(m_sps.begin(), m_sps.end());
		m_flg.erase(m_flg.begin(), m_flg.end());
	}

	int senStart, curIx;

   for (curIx = 0; curIx < m_lgsWords.size(); curIx++)
   {
      // The && !m_compact is added to prevent the setting of double quotes
      // double quotes. The double quotes are treated as regular word.
      // So we need not move the tags associated with quotes in 7.8.
      if ((m_lgsWords[curIx].toString() == LgsString("\"")) ||
          (m_lgsWords[curIx].toString() == LgsString("\'")) )
      {
         LgsVector(int) attrs;
         LgsVector(LgsString) attrSps;
         m_lgsWords[curIx].Attributes(attrs, attrSps);
         for (int Idx = 0; Idx < attrs.size(); Idx++)
         {
            int seq = attrs[Idx];
            int index = GetPSIndexBySeqNum(seq);
            int num = m_psStr.size();
            if (index != - 1 && PST_INTL == m_psStr[index].m_type && curIx - 1 > 0)
               m_lgsWords[curIx-1].AddAttribute(attrs[Idx], attrSps[Idx]);
            else if (index != - 1 && PST_INTR == m_psStr[index].m_type && curIx + 1 < m_lgsWords.size()) 
               m_lgsWords[curIx+1].AddAttribute(attrs[Idx], attrSps[Idx]);
         }
         m_lgsWords[curIx].RemoveAttrs();
      }
   }

	// extract sentences from the lgs word list array starting from the beginning
	// of the array, and write them to ltx, and fmt file
	for (senStart = 0, curIx = 0; curIx < m_lgsWords.size(); curIx++)
	{
		bool eos = IsEOS(senStart, curIx);
		if ((all && (curIx == m_lgsWords.size() -1)) || eos)
		{
         //Clear the double and single quote flags
			if (eos && (curIx == m_lgsWords.size() - 1))
			{
				m_flags &= ~FL_SGLQUOTE;
				m_flags &= ~FL_DBLQUOTE;
			}
			for (int id = 1, j = senStart; j <= curIx; j++, id++)
   			m_lgsWords[j].Id(id);

			// write sentence
			bool needsTran = NeedsTran(senStart, curIx - senStart + 1);

			if (needsTran)
			{
				m_pTranIO->Store(m_nSentId, m_lgsWords, senStart, curIx - senStart + 1);
            if (m_bAlign)
            {
               m_align->GetTransSourceWords(m_lgsWords, senStart, curIx - senStart + 1);
            }
			}

  			print_sentence(senStart, curIx - senStart + 1, needsTran, m_nSentId);
         if (needsTran && m_bAlign)
         {
            m_align->AlignSentence();
         }

			m_nSentId++;
			senStart = curIx + 1;
		}
	}

	if (senStart > 0)
   {
		m_lgsWords.erase(m_lgsWords.begin(), m_lgsWords.begin() + senStart);
   }
}
// -------------------------------------------------------------------
bool CLgsSentence::Init(int bufSize)
{
   m_entity.AddEntityPair("\x22", "quot");

	// if abbrs/months LgsString is longer than 1022 chars, it will be truncated to 1022 chars
	char buf[1024];

   // format pipe is only needed for translation
   if (m_translationMode)
   {
      m_fmt = new CLgsFmtPipe(true);
      if (!m_fmt)
	   {
		   cerr << "Cannot open format pipe in CLgsSentence" << "\n";
		   return false;
	   }
   }

	// this statement throws exception if the file cannot be opened
	m_pTranIO = new LgsTranIO(false);

   // Aligned output initialzation
   LgsString alignedFileName = LgsDBCommonObjects::GetJobControlArguments().AlignedFile();
   if (!alignedFileName.empty())
   {
      m_bAlign = true;
      m_align = new CLgsAlign(true, bufSize);
   }

   // read in months/nevereos files here
   char pMonthsFile[MAX_FILEPATH_LEN];

   GetConfigData("sourcedata", "monthsfile", pMonthsFile, MAX_FILEPATH_LEN);
  	ifstream in(pMonthsFile); 
	if (in.good())
	{
		while (!in.eof())
		{
			memset(buf, ' ', sizeof(buf));
			in.getline(buf, sizeof(buf));
         buf[in.gcount()] = 0;
         int length = strlen(buf);
         if ((length - 1 > 0) && (buf[length-1] == 0xd))
            buf[length-1] = 0;
			if (!buf[0])
            continue;
			m_monthsList.push_back(LgsString(buf));
		}
	}
   else
   {
      char pMessage[1024];
      sprintf(pMessage, "Error opening MONTHSFILE %s", pMonthsFile);
      CLgsUtil::Assert(false, pMessage);
   }

   char pNeverEosFile[MAX_FILEPATH_LEN];

   GetConfigData("sourcedata", "nevereosfile", pNeverEosFile, MAX_FILEPATH_LEN);
   ifstream in1(pNeverEosFile);
	if (in1.good())
	{
		while (!in1.eof())
		{
			memset(buf, ' ', sizeof(buf));
			in1.getline(buf, sizeof(buf));
         buf[in1.gcount()] = 0;
         int length = strlen(buf);
         if ((length - 1 > 0) && (buf[length-1] == 0xd))
            buf[length-1] = 0;
			if (!buf[0])
            continue;
			m_abbrList.push_back(LgsString(buf));
		}
      in1.close();
	}
   else
   {
      char pMessage[1024];
      sprintf(pMessage, "Error opening NEVEREOSFILE %s", pNeverEosFile);
      CLgsUtil::Assert(false, pMessage);
   }

   /*Read the oridinal file */
   char pOrdinalFile[MAX_FILEPATH_LEN];

   GetConfigData("sourcedata", "ordinalfile", pOrdinalFile, MAX_FILEPATH_LEN);
   ifstream in2(pOrdinalFile);
	if (in2.good())
	{
		while (!in2.eof())
		{
			memset(buf, ' ', sizeof(buf));
			in2.getline(buf, sizeof(buf));
			buf[in2.gcount()] = 0;
			int length = strlen(buf);
			if ((length - 1 > 0) && (buf[length-1] == 0xd))
			   buf[length-1] = 0;
			if (!buf[0])
            continue;
			m_ordinalList.push_back(LgsString(buf));
		}
      in2.close();
	}
   else
   {
      char  pMessage[1024];
      sprintf(pMessage,"Error opening ORDINALFILE  %s", pOrdinalFile);
      CLgsUtil::Assert(false, pMessage);
   }

	// init lit scanner
	STRV lits;


	int i;
	for (i = 0; i < m_abbrList.size(); i++)
		lits.push_back(m_abbrList[i]);
	for (i = 0; i < m_monthsList.size(); i++)
		lits.push_back(m_monthsList[i]);
	for (i = 0; i < m_ordinalList.size(); i++)
		lits.push_back(m_ordinalList[i]);

	lits.push_back("(");
	lits.push_back(")");
	lits.push_back("[");
	lits.push_back("]");
	lits.push_back("{");
	lits.push_back("}");
	lits.push_back("<");
	lits.push_back(">");
	lits.push_back("/");
	lits.push_back("+");
	lits.push_back("=");
	lits.push_back("\""); 
	lits.push_back("\\");
	lits.push_back("'");
	lits.push_back(";");
   lits.push_back("-");
	lits.push_back("?");
	lits.push_back("!");
	lits.push_back(","); // to break the words seperated by comma.
		
//   lits.push_back(",");
   lits.push_back(".");
   lits.push_back(":");
//   lits.push_back("?");
//   lits.push_back("!");
//   lits.push_back("#");
//   lits.push_back("@");
//   lits.push_back("%");
	lits.push_back("^");
//   lits.push_back("&");
//   lits.push_back("_");
//   lits.push_back("$");
	lits.push_back("\\");
	lits.push_back("~");
	lits.push_back("\x96"); //emdash  Added to break the words seperated by emdash.
	lits.push_back("\x97"); //endash  Added to break the words seperated by endash.

	if (!m_scn.Init(10, lits))
	{
		cout << "failed to init lit-scanner\n";
		return false;
	}
	return true;
}
// -------------------------------------------------------------------
void CLgsSentence::Flush()
{
	WriteSentences(true);
	FlushUnacclaimedIntRs();
}
// -------------------------------------------------------------------
bool CLgsSentence::IsMonth(LgsString& s)
{
	int num = m_monthsList.size();
	return CLgsUtil::IsIn(s, m_monthsList, 0, num);
}
// -------------------------------------------------------------------
bool CLgsSentence::IsAbbreviation(LgsString& s)
{
	int num = m_abbrList.size();
   return CLgsUtil::IsIn(s, m_abbrList, 0, num);
}
// -------------------------------------------------------------------
bool CLgsSentence::IsOrdinal(LgsString & s)
{
	int num = m_ordinalList.size();

	//convert the LgsString into uppercase
	int n = s.size();
	LgsString tempStr;
	const char *pszstr = s.c_str();
	for (int i = 0; i < n; i++)
	{
		tempStr += CharUtil::upper(pszstr[i]);
	}
	return CLgsUtil::IsIn(tempStr, m_ordinalList, 0, num);
}
// -------------------------------------------------------------------
bool CLgsSentence::IsEOS(int start, int cur)
{
   if (m_bEOS)
   {
      m_bEOS = false;
      return true;
   }
	
   LgsString curWord = m_lgsWords[cur].toString();

   // The \xb7 is added to treat the middot as eos.
   // The \x96 and \x97 (emdash and endash) is added because the sgml filter does not protect it.
   LgsString eosChars = ".?!:\xb7\x96\x97";

   // The following condition is added to treat the emdash as EOS.
   if ((curWord.compare("\x97") == 0) ||(curWord.compare("\x96") == 0))
   {
      return true;
   }

   bool notLast = cur < m_lgsWords.size() - 1;
   if (notLast)
	{
		LgsString s;
		int numSps = m_lgsWords[cur+1].SpaceInfo(s);
		int numNls = 0;
		for (LgsString::iterator i =  s.begin(); i != s.end(); i++)
		{
			if (0xa == (*i))
				numNls++;
		}

		if (numNls > 1)
      {
		   return true;
      }
	}
   
   //Find if there are eosChars
   int nPos = curWord.find_last_of(eosChars, NPOS);

   if (nPos != NPOS)
   {
      LgsString MidSentence;
      LgsString spacestring;
      MidSentence = curWord;
      //Get all the characters following the EOSchar. Here an arbitrary value of
      //five is chosen. The for loop appends atleast four words after the EOSchar.
      for (int idx = cur + 1; (idx < m_lgsWords.size()) && ((idx - cur) < 5); idx++)
      {
         m_lgsWords[idx].SpaceEOSInfo(spacestring);
         MidSentence += spacestring + m_lgsWords[idx].toString();
      }
      //Iterate to the point where you have the EOS char
      LgsString::iterator siter = MidSentence.begin() + nPos;
      //Get the EOS char
      char curChar = *siter;
      char nextChar = 0;
      char nextnextChar = 0;
      LgsString fstStr = m_lgsWords[start].toString();
      char fstChar = fstStr.length() ? fstChar = fstStr[0] : 0;

      //Get the next char after the EOS char 
      if ((++siter != MidSentence.end()) && !nextChar)
         nextChar = *siter;

      //Check for valid iterator value.
      if ((siter == MidSentence.end()) || (nextChar == 0))
         return false;

      //Get the character after nextchar. The character got should not be
      //one of the following characters {}[]()<>"'0x20 and 0xa(restrictions apply)
      while (++siter != MidSentence.end()) 
      {
         LgsString oddstring;
         if (nextChar != 0xa)
         {
            oddstring = "\"'{[(<>)]} \n"; 
         }
         else
         {
            oddstring = "\"'{[(<>)]} "; 
         }
         if (!nextnextChar || strchr(oddstring.c_str(), nextnextChar))
         {
            nextnextChar = *siter;
         }
         else
         {
            break;
         }
      }

      // Now that we have got all the characters to check EOS condition
      // Go ahead and test the conditions
      // Two or more new line char result in EOS
      if ((nextChar == '\xa') && (nextnextChar == '\xa'))
         return true;

      if ((nextChar == 0) && (nextnextChar == 0))
         return true;
     

       //Algorithm for checking EOS condition
      //if current char is '.?!:'
      //then
      //    if next char is alpha or numeric
      //       NOT EOS
      //    endif
      //    if next char is start of LGS-EXT block
      //       EOS
      //    endif
      //    if previous word is in months or nevereos list
      //       NOT EOS
      //    endif
      //    if next char is space
      //    then
      //       if next+1 char is a lower case alpha
      //       then NOT EOS
      //       else EOS
      //       endif
      //    endif
      //    if next char is a ]}>) or quote or dquote
      //    then
      //       if first character of Sentence is a matching brace or quote
      //       then EOS
      //       endif
      //    endif
      //    if next char is a {[(< or quote or dquote
      //    then 
      //       if first character following braces is Upper case alpha
      //       then EOS
      //       endif
      //    endif
      //    if previous word is a number and preceding word is in ordinal list
      //       NOT EOS
      //    endif
      //endif

      int nMaxWords = m_lgsWords.size();

      if (IsMonth(curWord) || IsAbbreviation(curWord))
         return false; //NOT EOS

      //The current word is not the first word and there are more words.	
      if ((curWord[curWord.length()-1] == '.') && (cur > 0) && (cur < nMaxWords-1))
      {
			LgsString Number;
			Number= m_lgsWords[cur-1].toString();
         // the word does not contain any non numaric
			if ((Number.size() > 0) && (Number.find_first_not_of("0123456789") == NPOS))
         {
            LgsString nextWord = m_lgsWords[cur+1].toString();	 
            if (IsMonth(nextWord))
               return false; // Not EOS.
         }
      }

      // Refer to Oridinal file.
      if ((curWord[curWord.length()-1] =='.') &&(cur >1))
      {
			LgsString Number;
			Number= m_lgsWords[cur-1].toString();
			
         // the word does not contain any non numaric
         if ((Number.size() > 0) && (Number.find_first_not_of("0123456789") == NPOS))
         {
            // Refer to Oridinal file.
            LgsString previousWord = m_lgsWords[cur-2].toString();
            if (IsOrdinal(previousWord))
               return false;//NOT EOS
         }
      }
		
      if (strchr("}]>)'\"", nextChar))
      {
         if ((nextChar == '}' && fstChar == '{') ||
             (nextChar == ']' && fstChar == '[') ||
             (nextChar == '>' && fstChar == '<') ||
             (nextChar == ')' && fstChar == '(') ||
             (nextChar == 0x27 && fstChar == 0x27) ||
             (nextChar == '\"' && fstChar == '\"') ||
             (nextChar == '}' && fstChar != '{' && CharUtil::isUpper(nextnextChar)) ||
             (nextChar == ']' && fstChar != '[' && CharUtil::isUpper(nextnextChar)) ||
             (nextChar == '>' && fstChar != '<' && CharUtil::isUpper(nextnextChar)) ||
             (nextChar == ')' && fstChar != '(' && CharUtil::isUpper(nextnextChar)) ||
             (nextChar == '\"' && fstChar != '\"' && CharUtil::isUpper(nextnextChar)) ||
             (nextChar == '\x27' && fstChar != '\x27' && CharUtil::isUpper(nextnextChar)))

         {
            m_bEOS = true;
            return false;
         }
      }
      if (strchr("{[<('\"", nextChar))
      {
         if (CharUtil::isUpper(nextnextChar))
            return true;
      }
      if ((nextChar == 0x20) || (nextChar == 0xa))
         if (!CharUtil::isLower(nextnextChar))
            return true;
     // The eoschar  followed by a space and a protected word is  EOS.
      if ((nextChar == ' ') && (m_lgsWords[cur+1].IsProtected()))
        return true;

   }      

   return false;
}
// -------------------------------------------------------------------
bool CLgsSentence::NeedsTran(int start, int count)
{
	// a sentence doesn't need translation iff
	// - it contains less than 2 alpha chars, or
	// - #of vowels in the sentence is 0, or
	// - all the words in the sentence have char that is <= '1'
   if (count == 2 && m_lgsWords[start].toString() == LgsString("+") &&
       m_lgsWords[start+1].toString() == LgsString("-"))
   {
       m_bNeedsTran = !m_bNeedsTran;
       return false;
   }

   if (!m_bNeedsTran)
      return false;
   int i = start, dntNum = 0, vowelNum = 0, alphaNum = 0, Numcount = 0;
	for (; i < start + count; i++)
	{
		LgsString w = m_lgsWords[i].toString();

		if (m_lgsWords[i].IsProtected())
		{
			dntNum++;
			continue;
		}

		for (LgsString::iterator iter = w.begin(); iter != w.end(); iter++)
		{
			if (CharUtil::isVowel(*iter))
				vowelNum++;
			if (CharUtil::isAlphabetic(*iter))
				alphaNum++;
         if (CharUtil::isNumeric(*iter))
            Numcount++;
 		}
	}
	return ((alphaNum >= 2) || (vowelNum > 0) || (Numcount > 0));
}
// -------------------------------------------------------------------
int CLgsSentence::GetPSIndexBySeqNum(int seqNum)
{
	// given a sequence number, this function returns protected LgsString index in
   // the m_psStr array, or -1 if there is no such protected LgsString
	for (int i = 0; i < m_psStr.size(); i++)
	{
		if (seqNum == m_psStr[i].m_seqNum)
			return i;
	}
	return -1;
}
// -------------------------------------------------------------------
void CLgsSentence::WriteExtHeader(const char* buf, int len)
{
   FlushUnacclaimedIntRs();
   if (m_translationMode)
   {
	   m_fmt->write(buf, len);
   }
}
// -------------------------------------------------------------------
void CLgsSentence::SendEndOfInput()
{
   m_pTranIO->SendEndOfDocument();

   if (m_translationMode)
   {
      m_fmt->sendEndOfDocument();
   }
}
// -------------------------------------------------------------------
void CLgsSentence::WriteString(const char* s, int len)
{
   if (m_translationMode)
   {
	   m_fmt->write(s, len);
   }
}
// -------------------------------------------------------------------
void CLgsSentence::FlushUnacclaimedIntRs()
{
	int num = m_psNum.size();
	if (num == 0)
   {
		WriteTrailSpaces();
		return;
	}

	int i, bufSize = 0;
	for (i = 0; i < num; i++)
	{
		int psIx = GetPSIndexBySeqNum(m_psNum[i]);
		bufSize += m_psSps[i].length();
		bufSize += m_psStr[psIx].m_string.length() + 100;
	}

	char* buf = new char[bufSize];
	buf[0] = 0;

	for (i = 0; i < num; i++)
	{
		int psIx = GetPSIndexBySeqNum(m_psNum[i]);
		strcat(buf, m_psStr[psIx].m_lb.c_str());
		strcat(buf, m_psStr[psIx].m_string.c_str());
		strcat(buf, m_psStr[psIx].m_rb.c_str());
	}

   if (m_detachedSpaces.length()) 
   {
      m_detachedSpaces += buf;
      strcpy(buf, m_detachedSpaces.c_str());
      m_detachedSpaces.erase(m_detachedSpaces.begin(), m_detachedSpaces.end());
   }

   if (m_translationMode)
   {
	   m_fmt->write(buf, strlen(buf));
   }
	delete[] buf;

	m_psNum.erase(m_psNum.begin(), m_psNum.end());
	m_psSps.erase(m_psSps.begin(), m_psSps.end());

	WriteTrailSpaces();
}
// -------------------------------------------------------------------
void CLgsSentence::WriteTrailSpaces()
{
	//Raw data
	if (m_lgsWords.empty() && m_numTrailingSpaces > 0)
	{
      if (m_translationMode)
      {
		   // write raw data block
		   m_fmt->write(m_trailingSpaces.c_str(), m_numTrailingSpaces);
      }

		// discard trailing spaces
		m_numTrailingSpaces = 0;
		m_trailingSpaces.erase();
	}
}
// -------------------------------------------------------------------
void CLgsSentence::PairTags(int start, int count)
{
#if 0 //def _UNIX_STL_ This is the default in the macro
   LgsSet(TagTableEntry, less<TagTableEntry>) TagTable;
   LgsSet(TagTableEntry, less<TagTableEntry>)::iterator tabIt;
   pair<LgsSet(TagTableEntry, less<TagTableEntry>)::iterator, bool> rtp;
#else
   LgsSet(TagTableEntry) TagTable;
   LgsSet(TagTableEntry)::iterator tabIt;
   pair<LgsSet(TagTableEntry)::iterator, bool> rtp;
#endif
   LgsVector(int) intv;
   LgsVector(int)::iterator vit;
   LgsVector(LgsString) strv;
   LgsVector(LgsString)::iterator sit;
   int nIndex = 0;
   m_vPairTable.clear();

   //Initialize the table with source Word Vector
   for (nIndex = start; nIndex < start+count; nIndex++)
   {
      LgsVector(int) intv;
      LgsVector(LgsString) strv;
      int nSrcWordId = m_lgsWords[nIndex].Id();
      if (intv.size() > 0)
      {
         for (vit = intv.begin(); vit != intv.end(); vit++)
         {
            int psvIdx = GetPSIndexBySeqNo(m_psStr, *vit);
            if (psvIdx != -1 && nSrcWordId != -1 && (m_psStr[psvIdx].GetSubType().GetType() == PST_OPEN ||
                m_psStr[psvIdx].GetSubType().GetType() == PST_CLOSE))
            {
               int tagId = m_psStr[psvIdx].GetSubType().GetSeqNum();
               TagTableEntry tte;
               tte.nTagId = tagId;
               tte.InsSrcSeqNum(*vit);
               tte.InsSrcWordId(nSrcWordId);
               rtp = TagTable.insert(tte);
               if (!rtp.second)
               {
                  const_cast<struct TagTableEntry&>(*rtp.first).InsSrcSeqNum(*vit);
                  const_cast<struct TagTableEntry&>(*rtp.first).InsSrcWordId(nSrcWordId);
               }
            }
         }
      }
   }
   int shiftcount = 12;
   for (tabIt = TagTable.begin(); tabIt != TagTable.end(); tabIt++)
   {
        TagTableEntry tte;
        tte = const_cast<struct TagTableEntry&>(*tabIt);
        //A pair exists
        if (tte.vSrcSeqNum.size() == 2)
        {
           PairEntry pe;
           pe.nPairFlag = (1 << shiftcount);
           shiftcount++;
           pe.nIntrSeqNum = tte.vSrcSeqNum[0];
           pe.nIntlSeqNum = tte.vSrcSeqNum[1];
           int nBeginWord = tte.vSrcWordId[0];
           int nEndWord = tte.vSrcWordId[1];
           int nBeginSeqNum = tte.vSrcSeqNum[0];
           int nEndSeqNum = tte.vSrcSeqNum[1];
           //If the pair does not belong to any word skip it
           if (nBeginWord != -1 && nEndWord != -1)
              m_vPairTable.push_back(pe);
           else
              continue;
           //Set the flag for all the words between the tags.
           for (int index = nBeginWord; index <= nEndWord; index++)
           {
              m_lgsWords[index-1].SetFlag(pe.nPairFlag);
           }
           //Remove the tag from the word attributes
           //Remove the OPENING Tag of the pair from the word
           m_lgsWords[nBeginWord-1].Attributes(intv, strv);
           for (vit = intv.begin(), sit = strv.begin(); vit != intv.end(); vit++, sit++)
           {
              if (*vit == nBeginSeqNum)
              {
                 intv.erase(vit);
                 strv.erase(sit);
                 break;
              }
           }
           m_lgsWords[nBeginWord-1].SetAttributes(intv,strv); 
           //Remove the CLOSING tag of the pair from the word attributes
           m_lgsWords[nEndWord-1].Attributes(intv, strv);
           for (vit = intv.begin(), sit = strv.begin(); vit != intv.end(); vit++, sit++)
           {
              if (*vit == nEndSeqNum)
              {
                 intv.erase(vit);
                 strv.erase(sit);
                 break;
              }
           }
           m_lgsWords[nEndWord-1].SetAttributes(intv,strv);  
        }
   }
}
// -------------------------------------------------------------------
void CLgsSentence::ApplyDupTags(int start, int count)
{
   // Apply the Duplicate tags to all words between INTRD and INTLD
//   LgsStackC(DupTagsEntry, LgsVector) DupTagsTable;
   LgsStack(DupTagsEntry) DupTagsTable;
   LgsVector(int) attrs, newattrs;
   LgsVector(int)::iterator vit;
   LgsVector(LgsString) attrSps, newattrSps;
   LgsVector(LgsString)::iterator sit;
   CLgsWord word;
   LgsString space = "";
   for (int index = start; index < start+count; index++)
   {
      word = m_lgsWords[index];
      attrs.clear();
      attrSps.clear();
      if (0 < word.Attributes(attrs, attrSps))
      {
         newattrs.clear();
         newattrSps.clear();
         m_lgsWords[index].SetAttributes(newattrs, newattrSps);
         for (vit = attrs.begin(), sit = attrSps.begin(); vit != attrs.end(); vit++, sit++) 
         {
            int psIdx = GetPSIndexBySeqNo(m_psStr, *vit);
            if (psIdx != -1)
            {
               if (m_psStr[psIdx].m_type == PST_INTRD)
               { 
                  DupTagsEntry dpe;
                  dpe.nRSeqNo = *vit;
                  dpe.vWordId.insert(word.Id());
                  DupTagsTable.push(dpe);
               }
               else if (m_psStr[psIdx].m_type == PST_INTLD && !DupTagsTable.empty())
               {
                  DupTagsEntry dpe, ndpe;
                  dpe = DupTagsTable.top();
                  dpe.vWordId.insert(word.Id());
                  DupTagsTable.pop();
                  bool bNotLast = false;
                  if (!DupTagsTable.empty())
                  {
                     ndpe = DupTagsTable.top();
                     DupTagsTable.pop();
                     bNotLast = true;
                  }
   #if 0 // def _UNIX_STL_ this is the default in the macro
                  for (LgsSet(int, less<int>)::iterator It = dpe.vWordId.begin(); It != dpe.vWordId.end(); It++)
   #else 
                  for (LgsSet(int)::iterator It = dpe.vWordId.begin(); It != dpe.vWordId.end(); It++)
   #endif
                  {
                     int wordIndex = GetWordIndexByWordId(m_lgsWords, *It, start);
                     if (wordIndex != -1)
                     {
                        m_lgsWords[wordIndex].AddAttribute(dpe.nRSeqNo, space);
                        m_lgsWords[wordIndex].AddAttribute(*vit, space);
                        if (bNotLast)
                           ndpe.vWordId.insert(*It);
                     }
                  }   
                  if (bNotLast)
                     DupTagsTable.push(ndpe);
               }
               else 
               {
                  //Bug: INTLD & INTRD are lost when there is a INTL Tag associated
                  m_lgsWords[index].Attributes(newattrs, newattrSps);
                  newattrs.push_back(*vit);
                  newattrSps.push_back(*sit);
                  m_lgsWords[index].SetAttributes(newattrs, newattrSps);
               }
            }
         }
      }
      else
      {
         if (!DupTagsTable.empty())
         {
            DupTagsEntry dpe;
            dpe = DupTagsTable.top();
            DupTagsTable.pop();
            dpe.vWordId.insert(word.Id());
            DupTagsTable.push(dpe);
         }
      }
   }

   while (!DupTagsTable.empty())
   {
      DupTagsEntry dpe, ndpe;
      dpe = DupTagsTable.top();
      DupTagsTable.pop();
      bool bNotLast = false;
      if (!DupTagsTable.empty())
      {
         ndpe = DupTagsTable.top();
         DupTagsTable.pop();
         bNotLast = true;
      }
#if 0 // def _UNIX_STL_ this is the default, as in the macro
      for (LgsSet(int, less<int>)::iterator It = dpe.vWordId.begin(); It != dpe.vWordId.end(); It++)
#else 
      for (LgsSet(int)::iterator It = dpe.vWordId.begin(); It != dpe.vWordId.end(); It++)
#endif
      {
         int wordIndex = GetWordIndexByWordId(m_lgsWords, *It);
         if (wordIndex != -1)
         {
            m_lgsWords[wordIndex].AddAttribute(dpe.nRSeqNo, space);
            if (bNotLast)
               ndpe.vWordId.insert(*It);
         }
      }   
      if (bNotLast)
         DupTagsTable.push(ndpe);
   }
}


