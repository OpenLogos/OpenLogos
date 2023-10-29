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
// LgsLex.cpp: implementation of the CLgsLex class.
//
//////////////////////////////////////////////////////////////////////
#include <logos_include/logoscommon.h>
#include <logos_libs/lgssgml/lgsutil.h>
#include <logos_libs/lgssgml/lgstag.h>
#include <logos_libs/lgssgml/lgsbuffer.h>
#include <logos_libs/lgssgml/lgslex.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <configdatafileinterface/configdatainterfacemain.h>
// -------------------------------------------------------------------
CLgsLex::CLgsLex()
{
	m_bPlusMinus = false;
   m_bExternalData = false;
	m_initialized = false;
	m_bLastChar = false;
	m_userProtChar = 0;
	m_bytesReadAhead = 0;
	m_readAheadItem = None;
	m_raStart = 0;
   m_nLastCharLen = 0;
   m_pLgsTag = NULL;
   char TagDefFileName[MAX_FILEPATH_LEN];

   GetConfigData("filter", "lgssgml2ltx_tagtbl", TagDefFileName, MAX_FILEPATH_LEN);
   m_pLgsTag = new CLgsTag(TagDefFileName);
   m_pLgsBuffer = NULL;
}
// -------------------------------------------------------------------
CLgsLex::~CLgsLex()
{
	m_initialized = false;
	delete m_pLgsTag;
   delete m_pLgsBuffer;
}
// -------------------------------------------------------------------
bool CLgsLex::Init(const LgsString& fileName, int bufSize, char userProtChar)
{
	bool result = false;

   m_userProtChar = userProtChar;
   m_pLgsBuffer = new CLgsBuffer(fileName, bufSize);

   if (!m_pLgsBuffer)
      return false;
   
   result = m_initialized = true;
	return result;
}
// -------------------------------------------------------------------
CLgsLex::LexType CLgsLex::NextItem(char* buf, int bufLen, int& dataLen)
{
	bool readingAhead = false;
	bool flushReadAheadBuffer = false;
	dataLen = 0;
	int bytesInBuffer = 0;

   if (Data == m_readAheadItem)
	{
		memcpy(buf, m_readAheadBuf, m_bytesReadAhead);
		bytesInBuffer = m_bytesReadAhead;
		m_readAheadItem = None;
		m_bytesReadAhead = 0;
	}
	else if (LgsPlusMinus == m_readAheadItem)
	{
      memcpy(buf, m_readAheadBuf, m_bytesReadAhead);
      dataLen = m_bytesReadAhead;
		m_readAheadItem = None;
		m_bytesReadAhead = 0;
		return LgsPlusMinus;
	}
	else if (UserProtChar == m_readAheadItem)
	{
		buf[0] = m_userProtChar;
		dataLen = 1;
		m_readAheadItem = None;
		m_bytesReadAhead = 0;
		return UserProtChar;
	}
	else if (None != m_readAheadItem)
	{
		if (m_bytesReadAhead > 0)
			memcpy(buf, m_readAheadBuf, m_bytesReadAhead);
		LexType result = m_readAheadItem;
		dataLen = m_bytesReadAhead;
		m_readAheadItem = None;
		m_bytesReadAhead = 0;
		return result;
	}

	for (; bytesInBuffer < bufLen;)
	{
		char c;
		if (!(m_pLgsBuffer->GetNextChar(&c)))
		{
			if (0 == bytesInBuffer && 0 == m_bytesReadAhead)
			{
				if (m_bPlusMinus)
				{
					m_readAheadItem = EndOfFile;
					strcpy(buf, "\r\n+-\r\n");
					dataLen = strlen("\r\n+-\r\n");
					m_bytesReadAhead = 0;
					m_bPlusMinus = false;
					return LgsPlusMinus;
				}
				else
					return EndOfFile;
			}

			if (m_bPlusMinus)
			{
				m_readAheadItem = LgsPlusMinus;
				m_bPlusMinus = false;
			}
			else
				m_readAheadItem = EndOfFile;

			if (m_bytesReadAhead > 0 &&
				 bytesInBuffer + m_bytesReadAhead < bufLen)
			{
				memcpy(buf+bytesInBuffer, m_readAheadBuf, m_bytesReadAhead);
				bytesInBuffer += m_bytesReadAhead;
				m_bytesReadAhead = 0;
				readingAhead = false;
			}
			break;
		}

		if (m_userProtChar && c == m_userProtChar)
		{
			if (0 == bytesInBuffer && 0 == m_bytesReadAhead)
			{
				buf[0] = m_userProtChar;
				dataLen = 1;
				return UserProtChar;
			}
			m_readAheadItem = UserProtChar;
			if (m_bytesReadAhead > 0 &&
				 bytesInBuffer + m_bytesReadAhead < bufLen)
			{
				memcpy(buf+bytesInBuffer, m_readAheadBuf, m_bytesReadAhead);
				bytesInBuffer += m_bytesReadAhead;
				m_bytesReadAhead = 0;
				readingAhead = false;
			}
			break;
		}

		if (('<' == c || '+' == c) && !readingAhead)
		{
			m_raStart = c;
			readingAhead = true;
		}
		else if (readingAhead)
		{
			// under certain conditions current read ahead should be stopped,
			// and a new one should be started
			if (('<' == m_raStart && '+' == c) || ('+' == m_raStart && '<' == c) ||
             ('<' == m_raStart && '<' == c))
			{
				// flush read ahead buffer
				if (bytesInBuffer + m_bytesReadAhead < bufLen)
				{
					memcpy(buf+bytesInBuffer, m_readAheadBuf, m_bytesReadAhead);
					bytesInBuffer += m_bytesReadAhead;
					m_bytesReadAhead = 0;
					m_raStart = c;
					readingAhead = true;
				}
				else
				{
					m_pLgsBuffer->UnGetLastChar();
					m_readAheadItem = Data;
					break;
				}
			}
		}
		
		if (readingAhead)
		{
			m_readAheadBuf[m_bytesReadAhead++] = c;

			if (('>' == c) && ('<' == m_raStart) && !m_bPlusMinus)
			{
            //terminate the LgsString
            m_readAheadBuf[m_bytesReadAhead] = 0;
				LexType iType = GetTagType();
				if (iType != Error)
				{
					if (0 == bytesInBuffer)
					{
						if (iType == LgsExtBegin || iType == LgsExtEnd)
                  {
                     m_bExternalData = iType == LgsExtBegin;
                     m_readAheadBuf[m_bytesReadAhead] = 0;
                     LgsString TagString = m_readAheadBuf;
                     if (m_pLgsBuffer->LookAhead() == '\r' && m_pLgsBuffer->LookAhead() == '\n')
                     {
                        int i = 2;
                        char rch = 0;
                        while (i > 0 && m_pLgsBuffer->GetNextChar(&rch))
                        {
                           TagString += rch;
                           i--;
                        }
                     }
                     memcpy(m_readAheadBuf, TagString.c_str(), TagString.length());
                     m_bytesReadAhead = TagString.length();
                  }
                  if (!m_bExternalData || (iType == LgsExtBegin))
                  {
                     memcpy(buf, m_readAheadBuf, m_bytesReadAhead);
						   dataLen = m_bytesReadAhead;
                     m_bytesReadAhead = 0;
						   return iType;
                  }
                  else
                  {
                     m_readAheadBuf[m_bytesReadAhead] = 0;
                     buf[bytesInBuffer] = 0;
                     strcat(buf, m_readAheadBuf);
                     dataLen = m_bytesReadAhead;
                     m_bytesReadAhead = 0;
                  }
					}
					else
               {
						m_readAheadItem = iType;
                  //This part of the code deals with the CR/LF characters
                  //associated with the Tag <LGS-EXT>
                  if (iType == LgsExtBegin || iType == LgsExtEnd) 
                  {
                     m_bExternalData = iType == LgsExtBegin;
                     LgsString TagString;
                     if ((bytesInBuffer >= 2) && (buf[bytesInBuffer-2] == '\r') &&
                         buf[bytesInBuffer-1] == '\n')
                     {                        
                        TagString = buf[bytesInBuffer-2];
                        TagString += buf[bytesInBuffer-1];
                        bytesInBuffer -= 2; //Reduce the length of the buf
                     }
                     TagString += m_readAheadBuf;
                     if ((m_pLgsBuffer->LookAhead() == '\r') && (m_pLgsBuffer->LookAhead() == '\n'))
                     {
                        int i = 2;
                        char rch = 0;
                        while ((i > 0) && m_pLgsBuffer->GetNextChar(&rch))
                        {
                           TagString += rch;
                           i--;
                        }
                     }
                     if (bytesInBuffer == 0)
                     {
                         memcpy(buf, TagString.c_str(), TagString.length());
                         dataLen = TagString.length();
                         m_bytesReadAhead = 0;
                         m_readAheadItem = None;
                         return iType;
                      }
                      else
                      {
                         memcpy(m_readAheadBuf, TagString.c_str(), TagString.length());
                         m_bytesReadAhead = TagString.length();
                      }
                  }
                  if (m_bExternalData && (iType != LgsExtBegin))
                  {
                        if (bytesInBuffer == 0)
                        {
                           memcpy(buf, m_readAheadBuf, m_bytesReadAhead);
                           bytesInBuffer = m_bytesReadAhead;
                        }
                        else
                        {
                          m_readAheadBuf[m_bytesReadAhead] = 0;
                          buf[bytesInBuffer] = 0;
                          strcat(buf, m_readAheadBuf);
                          bytesInBuffer += m_bytesReadAhead;
                        }
                        m_bytesReadAhead = 0;
                        m_readAheadItem = None;
                  }
                  break;
                }
				}
				else
					flushReadAheadBuffer = true;
			}
			else if ('-' == c && '+' == m_raStart)
			{
				if (2 == m_bytesReadAhead)
				{
               //Modified to Handle all possible combinations
               //for Block Protection.
               char penultChar = 0;
               char ultChar = 0;
               char nextChar = 0;
               char nextnextChar = 0;
               bool CRLF1found = false;
               bool CRLF2found = false;
               int count1 = 0;
               int count2 = 0;
               //Get the last two characters before +-
               if (bytesInBuffer >= 2)
               {
                  penultChar = buf[bytesInBuffer-2];
                  ultChar = buf[bytesInBuffer-1];
               }
               else if (bytesInBuffer == 1)
               {
                  ultChar = buf[bytesInBuffer-1];
               }
               //Get the first two characters after +-
               nextChar = m_pLgsBuffer->LookAhead();   
               nextnextChar = m_pLgsBuffer->LookAhead();
               
               //Determine whether a CR/LF precedes +-
               //To Handle CR/LF in Dos/Windows
               if ((penultChar == '\r') && (ultChar == '\n'))
               {
                  count1 = 2;
                  CRLF1found = true;
               }
               //To Handle CR/LF in Unix
               else if ((penultChar != '\r') && (ultChar == '\n'))
               {
                  count1 = 1;
                  CRLF1found = true;
               }
               //To Handle +- at Begining of file
               else if (bytesInBuffer == 0) 
               {
                  count1 = 0;
                  CRLF1found = true;
               }

               // Determine whether a CR/LF follows +-
               //To Handle CR/LF in Dos/Windows
               if ((nextChar == '\r') && (nextnextChar == '\n')) 
               {
                  count2 = 2;
                  CRLF2found = true;
               }
               //To Handle CR/LF in Unix
               else if (nextChar == '\n')
               {
                  count2 = 1;
                  CRLF2found = true;
               }
               //To Handle +- at end of file
               else if (nextChar == 0)
               {
                  count2 = 0;
                  CRLF2found = true; 
               }

               m_readAheadBuf[m_bytesReadAhead] = 0;
               LgsString PlusMinus;
               if (CRLF1found && CRLF2found)
               {
                  char uch = 0;
                  m_bPlusMinus = !m_bPlusMinus;
                  int remBytes = bytesInBuffer-count1;
                  while (count1 > 0)
                  {
                     PlusMinus += buf[bytesInBuffer-count1];
                     count1--;
                  }
                  bytesInBuffer = remBytes;
                  PlusMinus += m_readAheadBuf;
                  while (count2 > 0 && m_pLgsBuffer->GetNextChar(&uch)) 
                  {
                     PlusMinus += uch;
                     count2--;
                  }

                  if (bytesInBuffer - count1 > 0)
                  {
                     memcpy(m_readAheadBuf, PlusMinus.c_str(), PlusMinus.length());
                     m_bytesReadAhead = PlusMinus.length();
                     m_readAheadItem = LgsPlusMinus;
                  }
                  else 
                  {
                     memcpy(buf, PlusMinus.c_str(), PlusMinus.length());
                     dataLen = PlusMinus.length();
                     m_bytesReadAhead = 0;
                     m_readAheadItem = None;
                     return LgsPlusMinus;
                  }
               }
               else
               {
                  buf[bytesInBuffer] = 0;
                  m_readAheadBuf[m_bytesReadAhead] = 0;
                  strcat(buf, m_readAheadBuf);
                  bytesInBuffer += strlen(m_readAheadBuf);
                  m_bytesReadAhead = 0;
               }
               break;
				}
				else
					flushReadAheadBuffer = true;
			}

			if ((MAX_TAG_LENGTH == m_bytesReadAhead) || flushReadAheadBuffer)
			{
				flushReadAheadBuffer = false;
				if (bytesInBuffer + m_bytesReadAhead < bufLen)
				{
					memcpy(buf+bytesInBuffer, m_readAheadBuf, m_bytesReadAhead);
					bytesInBuffer += m_bytesReadAhead;
					m_bytesReadAhead = 0;
					readingAhead = false;
				}
				else
				{
					m_readAheadItem = Data;
					break;
				}
			}
		}
		else
			buf[bytesInBuffer++] = c;
	}

	dataLen = bytesInBuffer;
	return Data;
}
// -------------------------------------------------------------------
CLgsLex::LexType CLgsLex::GetTagType()
{
	int tagtype = m_pLgsTag->GetTagClass(m_readAheadBuf);
	if (tagtype != CLgsTag::Invalid)
	{
		if (!strncmp("</", m_readAheadBuf,2))
		{
			if (tagtype & CLgsTag::SectionProtect)
				return LgsExtEnd;
			if (tagtype & CLgsTag::SectionNoTran)
				return Data;
			if (tagtype & CLgsTag::SectionTran)
				return Data;
			if (tagtype & CLgsTag::MidSentTran)
				return Data;
			if (tagtype & CLgsTag::MidSentNoTran)
				return LgsIntwEnd;
			if (tagtype & CLgsTag::MidSentHidden && tagtype & CLgsTag::AttachLeft)
         {
            if (tagtype & CLgsTag::Duplicate)
				   return LgsIntldEnd;
            else
               return LgsIntlEnd;
         }
			if (tagtype & CLgsTag::MidSentHidden && tagtype & CLgsTag::AttachRight)
         {
            if (tagtype & CLgsTag::Duplicate)
               return LgsIntrdEnd;  
            else
               return LgsIntrEnd;
         }
			if (tagtype & CLgsTag::PointSection)
				return LgsHeaderEnd;
			if (tagtype & CLgsTag::Point)
				return LgsTitleEnd;
		}
		else if (m_readAheadBuf[0] == '<' && m_readAheadBuf[1] != '/')
		{
			if (tagtype & CLgsTag::SectionProtect)
				return LgsExtBegin;
			if (tagtype & CLgsTag::SectionNoTran)
				return Data;
			if (tagtype & CLgsTag::SectionTran)
				return Data;
			if (tagtype & CLgsTag::MidSentTran)
				return Data;
			if (tagtype & CLgsTag::MidSentNoTran)
				return LgsIntwBegin;
         //Adding a new tag type INTLD. This tag should be duplicated in the target
         if (tagtype & CLgsTag::MidSentHidden && tagtype & CLgsTag::AttachLeft)
         {
            if (tagtype  & CLgsTag::Duplicate)
				   return LgsIntldBegin;
            else
               return LgsIntlBegin;
         }
         //Adding a new tag type INTRD. This tag should be duplicated in the target
  			if (tagtype & CLgsTag::MidSentHidden && tagtype & CLgsTag::AttachRight)
         {
            if (tagtype & CLgsTag::Duplicate)
				   return LgsIntrdBegin;
            else
               return LgsIntrBegin;
         }
			if (tagtype & CLgsTag::PointSection)
				return LgsHeaderBegin;
			if (tagtype & CLgsTag::Point)
				return LgsTitleBegin;
		}
		else
			return Error;
	}
	return Error;
}
