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
// LgsSplitter.cpp: implementation of the CLgsSplitter class.
//
//////////////////////////////////////////////////////////////////////
#include <logos_include/logoscommon.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <logos_libs/lgssgml/lgssplitter.h>
#include <configdatafileinterface/configdatainterfacemain.h>
// -------------------------------------------------------------------
CLgsSplitter::CLgsSplitter()
{
	m_userProtChar = 0;
	m_bufSize = 0x100000;
	m_buf = NULL;
}
// -------------------------------------------------------------------
CLgsSplitter::~CLgsSplitter()
{
	if (m_buf)
   {
      delete[] m_buf;
   }
}
// -------------------------------------------------------------------
bool CLgsSplitter::Init()
{
   char inputFile[MAX_FILEPATH_LEN];

   GetConfigData("tempfile", "transl_input", inputFile, MAX_FILEPATH_LEN);
	m_userProtChar = LgsDBCommonObjects::GetJobControlArguments().ProtectionCharacter().c_str()[0];

	if (!m_lex.Init(inputFile, m_bufSize, m_userProtChar))
	{
		cout << "Input file not found: " << inputFile << "\n";
		return false;
	}

	if (!m_sen.Init(m_bufSize))
	{
		cerr << "m_sen.Init() failed\n";
		return false;
	}

	// alloc buffer for output files
	m_buf = new char[m_bufSize + 1];

	if (!m_buf)
	{
		cerr << "Out of memory\n";
		return false;
	}

	return true;
}

// mode flags
#define FLG_USER_PROTECTED_DATA  (1<<0)
#define FLG_PROTECTED_DATA       (1<<1)
#define FLG_PLUS_DATA            (1<<2)

// -------------------------------------------------------------------
bool CLgsSplitter::Run()
{
	LgsString pwStr;
	LgsString lb;
	LgsString rb;
	int seqNum = 1;
	int dataLen;
	unsigned int mode = 0;
	OutputType cur = tran;
	CLgsLex::LexType iType;
	int plusNo = 0;
   bool bJobCancelled = false;

	while (!bJobCancelled && (CLgsLex::EndOfFile != (iType = m_lex.NextItem(m_buf, m_bufSize, dataLen))))
	{
		char* markType;
		switch (iType)
		{
		case CLgsLex::LgsTitleBegin:
		case CLgsLex::LgsHeaderBegin:
		case CLgsLex::LgsFooterBegin:
		case CLgsLex::LgsTitleEnd:
		case CLgsLex::LgsHeaderEnd:
		case CLgsLex::LgsFooterEnd:
			if (mode & FLG_USER_PROTECTED_DATA)
			{
				pwStr += m_buf;
			}
			else if (mode & FLG_PLUS_DATA)
			{
				m_sen.WriteString(m_buf, dataLen);
			}
			else
			{
				m_sen.Flush();
				m_sen.WriteString(m_buf, dataLen);
			}
			break;

		case CLgsLex::LgsPlusMinus:
			if (0 == plusNo%2)
			{
				// opening pm-bracket
				if (mode & FLG_USER_PROTECTED_DATA)
				{
					rb = "";
					cur = tran;
					m_sen.AddProtectedWord(seqNum, "s", pwStr, lb, rb);
					pwStr.erase();
					lb.erase();
					rb.erase();
					seqNum++;
					mode &= ~FLG_USER_PROTECTED_DATA;
				}
				m_sen.Flush();
				cur = form;
				m_sen.WriteExtHeader(m_buf, dataLen);
				mode |= FLG_PLUS_DATA;
			}
			else
			{
				// closing pm-bracket
				m_sen.WriteString(m_buf, dataLen);
				cur = tran;
				mode &= ~FLG_PLUS_DATA;
			}
			plusNo++;
			break;

		case CLgsLex::LgsExtBegin:
			if (mode & FLG_USER_PROTECTED_DATA)
			{
				// end of user protected sequence in FMT file
				rb = "";
				cur = tran;
				m_sen.AddProtectedWord(seqNum, "s", pwStr, lb, rb);
				pwStr.erase();
				lb.erase();
				rb.erase();
				seqNum++;
				mode &= ~FLG_USER_PROTECTED_DATA;
			}
			if (mode & FLG_PLUS_DATA)
			{
				m_sen.WriteString(m_buf, dataLen);
			}
			else
			{
				m_sen.Flush();
				cur = form;
				m_sen.WriteExtHeader(m_buf, dataLen);
			}
			break;

		case CLgsLex::LgsExtEnd:
			if (mode & FLG_PLUS_DATA)
			{
				m_sen.WriteString(m_buf, dataLen);
			}
			else
			{
				m_sen.WriteString(m_buf, dataLen);
				cur = tran;
			}
			break;

		case CLgsLex::LgsIntwBegin:
		case CLgsLex::LgsIntlBegin:
		case CLgsLex::LgsIntrBegin:
		case CLgsLex::LgsIntldBegin:
		case CLgsLex::LgsIntrdBegin:
			if (mode & FLG_USER_PROTECTED_DATA)
			{
				m_buf[dataLen] = 0;
				pwStr += m_buf;
			}
			else if (mode & FLG_PLUS_DATA)
			{
				m_sen.WriteString(m_buf, dataLen);
			}
			else
			{
				mode |= FLG_PROTECTED_DATA;
				cur = form;
            switch (iType)
            {
               case CLgsLex::LgsIntwBegin : 
                              markType = "w";
                              break;
               case CLgsLex::LgsIntlBegin :
                              markType = "l";
                              break;
               case CLgsLex::LgsIntrBegin :
                              markType = "r";
                              break;
               case CLgsLex::LgsIntldBegin :
                              markType = "m"; //INTLD
                              break;
               case CLgsLex::LgsIntrdBegin :
                              markType = "n"; //INTRD
                              break;
            }
				m_buf[dataLen] = 0;
				lb = m_buf;
			}
			break;

		case CLgsLex::LgsIntwEnd:
		case CLgsLex::LgsIntlEnd:
		case CLgsLex::LgsIntrEnd:
  		case CLgsLex::LgsIntldEnd:
		case CLgsLex::LgsIntrdEnd:
			if (mode & FLG_USER_PROTECTED_DATA)
			{
				m_buf[dataLen] = 0;
				pwStr += m_buf;
			}
			else if (mode & FLG_PLUS_DATA)
			{
				m_sen.WriteString(m_buf, dataLen);
			}
			else
			{
				cur = tran;
				m_buf[dataLen] = 0;
				switch (iType)
				{
				   case CLgsLex::LgsIntwEnd : 
								  markType = "w";
								  break;
				   case CLgsLex::LgsIntlEnd :
								  markType = "l";
								  break;
				   case CLgsLex::LgsIntrEnd :
								  markType = "r";
								  break;
				   case CLgsLex::LgsIntldEnd :
								  markType = "m"; //INTLD
								  break;
				   case CLgsLex::LgsIntrdEnd :
								  markType = "n"; //INTRD
								  break;
				}
				rb = m_buf;
				m_sen.AddProtectedWord(seqNum, markType, pwStr, lb, rb);
				pwStr.erase();
				lb.erase();
				rb.erase();
				seqNum++;
				mode &= ~FLG_PROTECTED_DATA;
			}
			break;

		case CLgsLex::UserProtChar:
			if (mode & FLG_PLUS_DATA)
			{
				m_sen.WriteString(m_buf, dataLen);
			}
			else
			{
				if (mode & FLG_PROTECTED_DATA)
				{
					// (writing to format file...)
					// user protection char in protected data, - treat it as data
					pwStr += m_userProtChar;
				}
				else if (cur == form)
				{
					if (mode & FLG_USER_PROTECTED_DATA)
					{
						// end of user protected sequence in FMT file
						m_buf[dataLen] = 0;
						rb = m_buf;
						cur = tran;
						m_sen.AddProtectedWord(seqNum, "s", pwStr, lb, rb);
						pwStr.erase();
						lb.erase();
						rb.erase();
						seqNum++;
						mode &= ~FLG_USER_PROTECTED_DATA;
					}
					else // !(mode & FLG_USER_PROTECTED_DATA)
					{
						// (writing to format file...)
						// user protection char in format data, - treat it as data
						m_sen.WriteString(&m_userProtChar, 1);
					}
				}
				else // cur == tran
				{
					m_buf[dataLen] = 0;
					lb = m_buf;
					cur = form;
					mode |= FLG_USER_PROTECTED_DATA;
				}
			}
			break;

		case CLgsLex::Data:
			if (mode & FLG_PLUS_DATA)
			{
				m_sen.WriteString(m_buf, dataLen);
			}
			else
			{
				if (cur == form)
				{
					m_buf[dataLen] = 0;
					if (mode & FLG_USER_PROTECTED_DATA ||  mode & FLG_PROTECTED_DATA)
						pwStr += m_buf;
					else
						m_sen.WriteString(m_buf, dataLen);
				}
				else
					m_sen.MoreData(m_buf, dataLen);
			}
			break;

		case CLgsLex::Error:
			cerr << "CLgsLex::Error lexem found in input stream\n";
			return false;
		}

		// Just in case the job has been cancelled, we need to stop the tranl process as well.
		if(m_sen.SentencesSentToTranslation()%20==0) {
			if (LgsDBCommonObjects::GetJobControlArguments().Status() 
				!= JobControlArguments::StatusRUNNING) {
					bJobCancelled = true;
			}
		}
	}

	if (mode & FLG_USER_PROTECTED_DATA)
	{
		rb = "";
		cur = tran;
		m_sen.AddProtectedWord(seqNum, "s", pwStr, lb, rb);
		pwStr.erase();
		lb.erase();
		rb.erase();
		seqNum++;
		mode &= ~FLG_USER_PROTECTED_DATA;
	}
	// flush what's left
	m_sen.Flush();
   m_sen.SendEndOfInput();

   LgsDBCommonObjects::GetJobControlArguments().SentInputCount(m_sen.SentencesSentToTranslation());
 
	return true;
}
