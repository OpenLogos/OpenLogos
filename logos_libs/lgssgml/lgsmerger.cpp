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
// LgsMerger.cpp: implementation of the CLgsMerger class.
//
//////////////////////////////////////////////////////////////////////
#include <logos_include/logoscommon.h>
#include <logos_libs/lgssgml/lgsutil.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <logos_libs/lgssgml/lgsmerger.h>
#include <configdatafileinterface/configdatainterfacemain.h>

// -------------------------------------------------------------------
CLgsMerger::CLgsMerger()
{
	m_end = false;
	m_repl = false;
   m_convert = false;
	m_preserve = true;
	m_tran = false;
   m_droptag = false;
	m_nSentId = 0;
	m_flags = 0;
	m_pTranIO = NULL;
   m_fmt = NULL;
   m_align = NULL;
	m_bufSize = 0x100000;
	m_buf = NULL;
   m_ws.clear();
   m_bAlign = false;
   m_totalTransSent = 0;
}
// -------------------------------------------------------------------
CLgsMerger::~CLgsMerger()
{
   if (m_fmt)
   {
      delete m_fmt;
   }
	if (m_buf)
   {
      delete[] m_buf;
   }
	if (m_pTranIO)
   {
      delete m_pTranIO;
   }
	if (m_out)
   {
      delete m_out;
   }
   if (m_align)
   {
      delete m_align;
   }
}
// -------------------------------------------------------------------
bool CLgsMerger::Init()
{
   if (LgsDBCommonObjects::GetJobControlArguments().InputFormat() == "lgssgml")
   {
      m_convert = false;
      m_droptag = false;
   }
   else
   {
      m_convert = true;
      m_droptag = true;
   }
	m_repl = true;
	m_preserve = true;
	m_end = false;

   char outputFile[MAX_FILEPATH_LEN];

   GetConfigData("tempfile", "transl_output", outputFile, MAX_FILEPATH_LEN);
   m_fmt = new CLgsFmtPipe(false);
   if (!m_fmt)
	{
		cerr << "Cannot open format pipe in CLgsSentence" << "\n";
		return false;
	}

	m_out = new CLgsTarget(outputFile);
	if (!m_out)
	{
		cerr << "Failed to open the output file " << outputFile << "\n";
		return false;
	}

	// alloc buf
	m_buf = new char[m_bufSize + 1];
	if (!m_buf)
	{
		cerr << "Out of memory\n";
		return false;
	}

	// create ltx ob (throws exception if the file cannot be found)
	m_pTranIO = new LgsTranIO(true);
	if (!m_pTranIO)
	{
		cerr << "Failed to create an LgsTranIO object\n";
		return false;
	}

   // Aligned output initialzation
   LgsString alignedFileName = LgsDBCommonObjects::GetJobControlArguments().AlignedFile();
   if (!alignedFileName.empty())
   {
      m_bAlign = true;
      m_align = new CLgsAlign(false, m_bufSize);
   }

	return true;
}
// -------------------------------------------------------------------
static inline void SwitchOffFlags(unsigned int& flags, CLgsTarget* f, CLgsWord& w, LgsVector(CLgsProtectedString)& psv, LgsVector(PairEntry)& pev)
{
	f->SetAdjustFlag();
	if ((flags&FL_SGLQUOTE) && !w.IsSglQuoted())
	{
		flags ^= FL_SGLQUOTE;
		*f << "'";
	}
	if ((flags&FL_DBLQUOTE) && !w.IsDblQuoted())
	{
		flags ^= FL_DBLQUOTE;
		*f << "\"";
	}
   f->ClearAdjustFlag();
   if ((flags&FL_CURLBRACE) && !w.IsCurlybraced())
   {
      flags ^= FL_CURLBRACE;
      *f << "}";
   }
   if ((flags&FL_ANGLEBRK) && !w.IsAnglebrk())
   {
      flags ^= FL_ANGLEBRK;
      *f << ">";
   }
   if ((flags&FL_SQUAREBRK) && !w.IsSquarebrk())
   {
      flags ^= FL_SQUAREBRK;
      *f << "]";
   }
   if ((flags&FL_PARENTHES) && !w.IsParenthes())
   {
      flags ^= FL_PARENTHES;
      *f << ")";
   }
   if ((flags&FL_DBLUNDERLINE) && !w.IsDblUnderline())
	{
		flags &= ~FL_DBLUNDERLINE;
		*f << "</D>";
	}
   if ((flags&FL_UNDERLINE) && !w.IsUnderline())
	{
		flags &= ~FL_UNDERLINE;
		*f << "</U>";
	}
   if ((flags&FL_ITALIC) && !w.IsItalic())
	{
		flags &= ~FL_ITALIC;
		*f << "</I>";
	}
  	if ((flags&FL_BOLD) && !w.IsBold())
	{
		flags &= ~FL_BOLD;
		*f << "</B>";
	}
   //Print paired INTL/INTR  tags
   for (LgsVector(PairEntry)::iterator pit = pev.begin(); pit != pev.end(); pit++)
   {
      if ((flags & pit->nPairFlag) && !(pit->nPairFlag & w.GetPairFlag()))
      {
         flags ^= pit->nPairFlag;
         int idx = GetPSIndexBySeqNo(psv, pit->nIntlSeqNum);
         if (idx != -1)
         {
            f->write(psv[idx].m_lb.c_str(), psv[idx].m_lb.length());
            f->write(psv[idx].m_string.c_str(), psv[idx].m_string.length());
            f->write(psv[idx].m_rb.c_str(), psv[idx].m_rb.length());
         }
      }
   }
}
// -------------------------------------------------------------------
static inline void SwitchOnFlags(unsigned int& flags, CLgsTarget* f, CLgsWord& w, LgsVector(CLgsProtectedString)& psv, LgsVector(PairEntry)& pev)
{
   //Print paired INTL/INTR  tags
   for (LgsVector(PairEntry)::iterator pit = pev.begin(); pit != pev.end(); pit++)
   {
      if (!(flags & pit->nPairFlag) && (pit->nPairFlag & w.GetPairFlag()))
      {
         flags |= pit->nPairFlag;
         int idx = GetPSIndexBySeqNo(psv, pit->nIntrSeqNum);
         if (idx != -1)
         {
            f->write(psv[idx].m_lb.c_str(), psv[idx].m_lb.length());
            f->write(psv[idx].m_string.c_str(), psv[idx].m_string.length());
            f->write(psv[idx].m_rb.c_str(), psv[idx].m_rb.length());
         }
      }
   }
	if (!(flags&FL_BOLD) && w.IsBold())
	{
		flags |= FL_BOLD;
		*f << "<B>";
	}
	if (!(flags&FL_ITALIC) && w.IsItalic())
	{
		flags |= FL_ITALIC;
		*f << "<I>";
	}
	if (!(flags&FL_UNDERLINE) && w.IsUnderline())
	{
		flags |= FL_UNDERLINE;
		*f << "<U>";
	}
  	if (!(flags&FL_DBLUNDERLINE) && w.IsDblUnderline())
	{
		flags |= FL_DBLUNDERLINE;
		*f << "<D>";
	}
   if (!(flags&FL_CURLBRACE) && w.IsCurlybraced())
   {
      flags ^= FL_CURLBRACE;
      *f << "{";
   }
   if (!(flags&FL_ANGLEBRK) && w.IsAnglebrk())
   {
      flags ^= FL_ANGLEBRK;
      *f << "<";
   }
   if (!(flags&FL_SQUAREBRK) && w.IsSquarebrk())
   {
      flags ^= FL_SQUAREBRK;
      *f << "[";
   }
   if (!(flags&FL_PARENTHES) && w.IsParenthes())
   {
      flags ^= FL_PARENTHES;
      *f << "(";
   }
   if (!(flags&FL_DBLQUOTE) && w.IsDblQuoted())
   {
      flags ^= FL_DBLQUOTE;
      *f << "\"";
   }
   if (!(flags&FL_SGLQUOTE) && w.IsSglQuoted())
   {
      flags ^= FL_SGLQUOTE;
      *f << "'";
   }
}
// -------------------------------------------------------------------
bool CLgsMerger::Run()
{
   int len;
   bool done = false;
	while (!done)
	{
		try
		{
		   unsigned int msgType = m_fmt->receiveMsg(m_buf, len);

         switch(msgType)
         {
         case CLgsFmtPipe::SENT:
			   // sentence blk
			   ReadSentence();
            break;

         case CLgsFmtPipe::RAWDATA:
			   m_flags = 0;
            m_out->write(m_buf, len);
            break;

         case CLgsFmtPipe::TRANSEND:
			   m_flags = 0;
            done = true;
			   break;

         default:
            done = true;
            break;
         }

		   if ((m_totalTransSent % 20) == 0)
		   {
			   // Write out to the database when every 20th sentence has completed.
			   LgsDBCommonObjects::GetJobControlArguments().SentCurrentComplete(m_totalTransSent);
		   }
		}

		catch(...)
		{
			int k = 1;
		}
	}
	LgsDBCommonObjects::GetJobControlArguments().SentCurrentComplete(m_totalTransSent);
	
	return true;
}
// -------------------------------------------------------------------
int CLgsMerger::ReadSentence()
{
	int  result = 0;
	int numWords = 0;
   static int lSentId = 0;
	bool preserve = m_preserve;
	LgsVector(CLgsProtectedString) protectedStrings;
	LgsVector(CLgsWord) lgsWords;
   LgsVector(PairEntry) pairTable;
	LoadSentence(lgsWords, protectedStrings, pairTable);
	if (m_tran)
	{
		// get words form translation (ie from m_pTranIO),
		// and print'em out assigning attrs properly
		//
		// We have words from tran here, and we possibly have words read from
		// the format file (remember that we saved all the words in the format file).
		// Word ID is the key element in the following algorithm.
		// It might well be the case that we have two or more
		// words with the same ID coming from tran, and that would mean
		// that the single word has been translated to a pair, or more words.
		// If attributes have been assigned to the original word,
		// they should be assigned to all resulting words.
		// Words are written to the output file in the order
		// they come from translation.
		//
		// Sequence numbers have meaning only for protected words.
		// They start at 1 and go up.
		// Sequence number value 0 has special meaning. When we see this value
		// on a word coming from translation, it means that the word has been
		// protected by the translation process. In this case we get LgsString
		// value of the word from the format file. Again word matching is done
		// based on word id.
      if (lSentId < m_nSentId)
      {
         m_ws.clear();
		   bool result = m_pTranIO->Load(m_ws, lSentId);
         if (result)
         {
            m_totalTransSent++;
         }
         if (m_bAlign && result)
         {
            m_align->GetTransTargetWords(m_ws);
            m_align->AlignSentence();
         }
         m_nTranWid = 0;
      }
		if (lSentId > m_nSentId || lSentId == 0)
		{
			cerr << endl << "MT has dropped sentence " << m_nSentId << ". Restoring from format..." << endl;
			result = PrintFromFormat(lgsWords, protectedStrings, pairTable);
			return result;
		}

		int numTranWords = m_ws.size();
		
		if (!m_preserve)
      {
			preserve = !CheckTags(lgsWords, m_ws, protectedStrings);
      }

		if (preserve)
		{
			SetDropFlag(lgsWords,m_ws);
			PrintDroppedINTR(lgsWords, protectedStrings);
         CLgsWord word;
         while (GetNextWord(word))
			{
				PrintWord(lgsWords,protectedStrings, pairTable, word);
				result++;
			}
         //Print out the missing flags
         SwitchOffFlags(m_flags, m_out, word, protectedStrings, pairTable);
		}
		else
		{
			InitWordVector(lgsWords, m_ws);
			numWords = lgsWords.size();
			for (int index = 0; index < numWords; index++)
			{
				PrintWord(lgsWords[index], protectedStrings, pairTable);
				result++;
			}
		}
	} //endif (tran)
	else
   {
		result = PrintFromFormat(lgsWords, protectedStrings, pairTable);
   }
	
	if (preserve)
   {
		PrintDroppedINTL(lgsWords, protectedStrings);
   }

	return result;
}
// -------------------------------------------------------------------
void CLgsMerger::WritePS(CLgsProtectedString& ps)
{
   if ((ps.m_type != PST_USER) &&
       (((ps.m_rb.length() == 0) && (ps.m_lb.length() > 0)) || (ps.m_type == PST_INTW)))
   {
      m_out->write(ps.m_lb.c_str(), ps.m_lb.length());
   }
	m_out->write(ps.m_string.c_str(), ps.m_string.length());
   if ((ps.m_type != PST_USER) &&
       (((ps.m_lb.length() == 0) && (ps.m_rb.length() > 0)) || (ps.m_type == PST_INTW)))
   {
      m_out->write(ps.m_rb.c_str(), ps.m_rb.length());
   }
}
// -------------------------------------------------------------------
void CLgsMerger::WriteIntLR(CLgsProtectedString& ps, LgsString& sps, bool psps, bool nl)
{
	m_buf[0] = 0;
	strcat(m_buf, ps.m_lb.c_str());
	strcat(m_buf, ps.m_string.c_str());
	strcat(m_buf, ps.m_rb.c_str());
	m_out->write(m_buf, strlen(m_buf));
}
// -------------------------------------------------------------------
//All the words are in format file, print them out
int CLgsMerger::PrintFromFormat(LgsVector(CLgsWord)& wv, LgsVector(CLgsProtectedString)& psv, LgsVector(PairEntry)& pev)
{
	int words = 0;
	int numWords = wv.size();
   if ((numWords == 2) && (wv[0].toString() == LgsString("+")) &&
       (wv[1].toString() == LgsString("-")))
   {
      return 0;
   }
	for (int index = 0; index < numWords; index++)
	{
		SwitchOffFlags(m_flags, m_out, wv[index], psv, pev);
		m_out->SetFlush();

		// print out spaces
		LgsString spsStr;
		int nSp = wv[index].SpaceInfo(spsStr);
		if (nSp > 0)
      {
			m_out->write(spsStr.c_str(), nSp);
      }

		SwitchOnFlags(m_flags, m_out, wv[index], psv, pev);

		// print out R-tags if any
		LgsVector(int) attrs;
		LgsVector(LgsString) attrsSps;
		wv[index].Attributes(attrs, attrsSps);
		int j;
                for (j = 0; j < attrs.size(); j++)
		{
			int psIx = GetPSIndexBySeqNo(psv, attrs[j]);
			if ((PST_INTR == psv[psIx].m_type) || (PST_INTRD == psv[psIx].m_type))
         {
				WriteIntLR(psv[psIx], attrsSps[j]);
         }
		}

		// print out the word
		int sno = wv[index].SequenceNumber();
		if (wv[index].IsProtected() && sno > 0)
      {
         CLgsProtectedString ps = psv[GetPSIndexBySeqNo(psv, sno)];
         //if (m_convert && ps.m_type == PST_USER)
         //   m_entity.replaceExtChars(ps.m_string);
			WritePS(ps);
      }
		else
		{
         LgsString norStr = wv[index].toString();
         if (m_convert)
         {
            m_entity.replaceExtChars(norStr);
         }
			m_out->write(norStr.c_str(), norStr.length());
		}
		words++;
		// print out L-tags if any
		for (j = 0; j < attrs.size(); j++)
		{
			int psIx = GetPSIndexBySeqNo(psv, attrs[j]);
			if ((PST_INTL == psv[psIx].m_type) || (PST_INTLD == psv[psIx].m_type))
         {
				WriteIntLR(psv[psIx], attrsSps[j]);
         }
		}
	}
	return words;
}
// -------------------------------------------------------------------
void CLgsMerger::PrintDroppedINTL(LgsVector(CLgsWord)& wv, LgsVector(CLgsProtectedString)& psv)
{
   m_out->SetAdjustFlag();
	int numWords = wv.size();
	if (m_dropped && !m_droptag)
	{
		for (int i = 0; i < numWords; i++)
		{
			if (!wv[i].IsTouched())
			{
				// print out L-tags if any
				LgsVector(int) attrs;
				LgsVector(LgsString) attrsSps;
				wv[i].Attributes(attrs, attrsSps);

				for (int j = 0; j < attrs.size(); j++)
				{
					int psIx = GetPSIndexBySeqNo(psv, attrs[j]);
					if (PST_INTL == psv[psIx].m_type || PST_INTLD == psv[psIx].m_type)
					WriteIntLR(psv[psIx],
					attrsSps[j], false, true);
				}
			}
		}
	}
   m_out->ClearAdjustFlag();
}
// -------------------------------------------------------------------
void CLgsMerger::PrintDroppedINTR(LgsVector(CLgsWord)& wv, LgsVector(CLgsProtectedString)& psv)
{
	//Print the R-Tags that were dropped 
	if (m_dropped && !m_droptag)
	{
		int numWords = wv.size();
		for (int i = 0; i < numWords; i++)
		{
			if (!wv[i].IsTouched())
			{
				// print out R-tags if any
				LgsVector(int) attrs;
				LgsVector(LgsString) attrsSps;
				wv[i].Attributes(attrs, attrsSps);
				for (int j = 0; j < attrs.size(); j++)
				{
					int psIx = GetPSIndexBySeqNo(psv, attrs[j]);
					if ((PST_INTR == psv[psIx].m_type) || (PST_INTRD == psv[psIx].m_type))
               {
						WriteIntLR(psv[psIx], attrsSps[j], false, true);
               }
				}
			}
		}
	}
}
// -------------------------------------------------------------------
void CLgsMerger::SetDropFlag(LgsVector(CLgsWord)& swv, LgsVector(CLgsWord)& twv)
{
	m_dropped = false;
	int numTranWords = twv.size();
	int numWords = swv.size();
	int i;
	for (i = 0; i < numTranWords; i++)
	{
		int ix = GetWordIndexByWordId(swv, twv[i].Id());
		if (ix != -1)
      {
			swv[ix].SetFlag(CLgsWord::touched);
      }
	}
	// check if some words have been dropped by tran
	for (i = 0; i < numWords; i++)
   {
		if (!swv[i].IsTouched())
      {
         m_dropped = true;
      }
   }
}
// -------------------------------------------------------------------
void CLgsMerger::PrintWord(LgsVector(CLgsWord)& swv, LgsVector(CLgsProtectedString)& psv, LgsVector(PairEntry)& pev, CLgsWord& word)
{
   int ix = GetWordIndexByWordId(swv, word.Id());
   SwitchOffFlags(m_flags, m_out, word, psv, pev);
   m_out->SetFlush();
   LgsVector(int) attrs;
   LgsVector(LgsString) attrsSps;
   if (-1 != ix)
   {
      // print out R-tags if any
      swv[ix].Attributes(attrs, attrsSps);
      for (int j = 0; j < attrs.size(); j++)
      {
         int psIx = GetPSIndexBySeqNo(psv, attrs[j]);
         if (psIx >= 0 && PST_INTR == psv[psIx].m_type)
            WriteIntLR(psv[psIx], attrsSps[j]);
      }
      if (swv[ix].IsDblUnderline())
         word.SetFlag(CLgsWord::dblunderline);
      word.SetFlag(swv[ix].GetPairFlag());
   }


   // print out the word
   int seqNum;
   bool translWordSingle = false;
   LgsString tranString = word.toString();
   if (tranString == "^p" || tranString == "^p ")
      translWordSingle = true;
   if (word.IsProtected() && -1 != ix)
   {
      ix = GetWordIndexByWordId(swv, word.Id());

      if (swv[ix].IsProtected() && (seqNum = swv[ix].SequenceNumber()) > 0)
      {
         CLgsProtectedString ps = psv[GetPSIndexBySeqNo(psv, seqNum)];
         LgsString originalStr;
         if (ps.m_type == PST_INTW)
            originalStr = ps.m_lb + ps.m_string + ps.m_rb;
         else
            originalStr = ps.m_string;
         word.SetTranString(tranString,originalStr);
         if (m_convert && ps.m_type != PST_INTW  && !translWordSingle)
            m_entity.replaceExtChars(tranString);
      }
      else
      {
         LgsString originalStr = swv[ix].toString();
         word.SetTranString(tranString,originalStr);
         if (m_convert)
            m_entity.replaceExtChars(tranString);
      }
   }
   else
   {
      if (m_convert)
         m_entity.replaceExtChars(tranString);
   }

   //Replicate INTRD/INTLD tags on each word in translated LgsString
   LgsVector(LgsString) tranWords;
   LgsString space = " ";
   LgsString trailingSpace;
   if (!translWordSingle)
   {
      CLgsUtil::BreakDown(space, tranString, tranWords, false);
      for (LgsString::reverse_iterator It = tranString.rbegin();  It != tranString.rend() && *It == 0x20; It++)
         trailingSpace += *It;
   }
   else
   {
      tranWords.push_back(tranString);
   }
   for (int tranIt = 0; tranIt < tranWords.size(); tranIt++)
   {
      int item;
      for (item = 0; item < attrs.size(); item++)
      {
         int psIx = GetPSIndexBySeqNo(psv, attrs[item]);
         if (psIx >= 0 && PST_INTRD == psv[psIx].m_type)
            WriteIntLR(psv[psIx], attrsSps[item]);
      }

      if (tranIt == 0)
      {
         if (FirstWordInTransSent(word))
            PrintSpace(swv[0]);
         SwitchOnFlags(m_flags, m_out, word, psv, pev);
      }

      m_out->write(tranWords[tranIt].c_str(), tranWords[tranIt].length());

      if (tranIt+1 == tranWords.size())
      {
         m_out->write(trailingSpace.c_str(), trailingSpace.length());
         CLgsWord nextWord;
         if (GetNextWord(nextWord))
         {
            nextWord.SetFlag(swv[ix].GetPairFlag());
            UnGetLastWord();
         }
         SwitchOffFlags(m_flags, m_out, nextWord, psv, pev);
      }
      for (item = 0; item < attrs.size(); item++)
      {
         int psIx = GetPSIndexBySeqNo(psv, attrs[item]);
         if (psIx >= 0 && PST_INTLD == psv[psIx].m_type)
            WriteIntLR(psv[psIx], attrsSps[item]);
      }
   }

   // print out L-tags if any
   if (-1 != ix)
   {
      for (int j = 0; j < attrs.size(); j++)
      {
         int psIx = GetPSIndexBySeqNo(psv, attrs[j]);
         if (psIx >= 0 && PST_INTL == psv[psIx].m_type)
            WriteIntLR(psv[psIx], attrsSps[j]);
      }
      // remove atributes from the word read from the format file
      if (m_repl)
         RemoveAttrs(swv[ix], psv);
   }
}
// -------------------------------------------------------------------
void CLgsMerger::InitWordVector(LgsVector(CLgsWord)& swv, LgsVector(CLgsWord)& twv)
{
	int numWords = swv.size();
	int numTranWords= twv.size();
        int p, i;
	for (p = 0, i = 0; i < numWords && p < numTranWords; i++)
	{
		int ix = GetWordIndexByWordId(twv, swv[i].Id());
		if (-1 != ix)
		{
			swv[i].SetFlag(CLgsWord::touched);
			twv[p].SetFlag(CLgsWord::touched);
			swv[i].SetProtected(twv[p].IsProtected());
			if (twv[p].IsProtected())
			{
			   ix = GetWordIndexByWordId(swv, twv[p].Id());
			   LgsString tranString = twv[p].toString();
			   LgsString originalStr;
			   if (ix != -1)
				   originalStr = swv[ix].toString();
			   swv[i].SetTranString(tranString,originalStr);
				swv[i].SetSequenceNumber(swv[ix].SequenceNumber());
			}
			else
			{
			   LgsString tranString = twv[p].toString();
			   swv[i].SetTranString(tranString);
			}
			p++;
		}

	}
	//If more number of words were received from Translation,
	//push rest of the words back of swv
	while (p < numTranWords) 
	{
		if (twv[p].IsProtected())
		{
         int ix = GetWordIndexByWordId(swv, twv[p].Id());
         LgsString tranString = twv[p].toString();
         LgsString originalStr;
         if (ix != -1)
            originalStr = swv[ix].toString();
         twv[p].SetTranString(tranString,originalStr);
		}
		else
		{
		   LgsString tranString = twv[p].toString();
		   twv[p].SetTranString(tranString);   
		}
		twv[p].SetFlag(CLgsWord::touched);
		swv.push_back(twv[p]);
		p++;
	}	
	numWords = swv.size();
}
// -------------------------------------------------------------------
void CLgsMerger::PrintWord(CLgsWord& word, LgsVector(CLgsProtectedString)& psv, LgsVector(PairEntry)& pev)
{

	if (!word.IsTouched())
	{
		// print out R/L-tags if any
		LgsVector(int) attrs;
		LgsVector(LgsString) attrsSps;
		word.Attributes(attrs, attrsSps);
		for (int j = 0; j < attrs.size(); j++)
		{
			int psIx = GetPSIndexBySeqNo(psv, attrs[j]);
			if (PST_INTR == psv[psIx].m_type || PST_INTRD == psv[psIx].m_type)
				WriteIntLR(psv[psIx],attrsSps[j], false, true);
			if (PST_INTL == psv[psIx].m_type || PST_INTLD == psv[psIx].m_type)
			{
				WriteIntLR(psv[psIx], attrsSps[j]);
				// remove atributes from the word read from the format file
				if (m_repl)
               RemoveAttrs(word, psv);
			}
		}
	}
	else // The word has been translated
	{
		SwitchOffFlags(m_flags, m_out, word, psv, pev);
		m_out->SetFlush();
		LgsVector(int) attrs;
		LgsVector(LgsString) attrsSps;

		// print out R-tags if any
		word.Attributes(attrs, attrsSps);
		for (int j = 0; j < attrs.size(); j++)
		{
			int psIx = GetPSIndexBySeqNo(psv, attrs[j]);
			if (PST_INTR == psv[psIx].m_type || PST_INTRD == psv[psIx].m_type)
			   WriteIntLR(psv[psIx], attrsSps[j]);
		}
		SwitchOnFlags(m_flags, m_out, word, psv, pev);

		// print out the word
		int seqNum;

		if (word.IsProtected())
		{
			if (word.IsProtected() && 
				(seqNum = word.SequenceNumber()) > 0)
			{
				WritePS(psv[GetPSIndexBySeqNo(psv, seqNum)]);
			}
			else
			{
				//replace the extended characters with Entities
				LgsString tranString = word.toTranString();
				if (m_convert)
					m_entity.replaceExtChars(tranString);
				m_out->write(tranString.c_str(),	tranString.length());
			}
		}
		else
		{		
			LgsString tranString = word.toTranString();
			if (m_convert)
				m_entity.replaceExtChars(tranString);
			char* p = (char*) tranString.c_str();
			if (' ' == p[0])
				p++;
			int len = strlen(p);
			if (len > 0)
				m_out->write(p, len);
		}

		// print out L-tags if any
		for (int k = 0; k < attrs.size(); k++)
		{
			int psIx = GetPSIndexBySeqNo(psv, attrs[k]);
			if (PST_INTL == psv[psIx].m_type || PST_INTLD == psv[psIx].m_type)
			   WriteIntLR(psv[psIx], attrsSps[k]);
		}

		// remove atributes from the word read from the format file
		if (m_repl)
         RemoveAttrs(word, psv);
	}
}
// -------------------------------------------------------------------
void CLgsMerger::LoadSentence(LgsVector(CLgsWord)& wv, LgsVector(CLgsProtectedString)& psv, LgsVector(PairEntry)& pev)
{
	//Purge the array of words
	wv.clear();
	psv.clear();
   pev.clear();

   // pointer to sentence info
   const char *info = m_buf;

	// needs tran
	int tran = 0;
	memcpy((char *)&tran, info, sizeof(int));
   info += sizeof(int);
	m_tran = (tran != 0);

   // sentence id
	if (m_tran)
   {
	   memcpy((char *)&m_nSentId, info, sizeof(int));
      info += sizeof(int);
   }

	// numSps
	int senNumSps = 0;
	memcpy((char *)&senNumSps, info, sizeof(int));
   info += sizeof(int);

	// sps
	if (senNumSps > 0)
	{
		char* buf = new char[senNumSps + 1];
		CLgsUtil::Assert(NULL != buf, "Memory Allocation Failure");
	   memcpy(buf, info, senNumSps);
      info += senNumSps;
		buf[senNumSps] = 0;
		m_senSpStr = buf;
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
		psv.push_back(ps);
	}

	// lgs words
	int numWords = 0;
	memcpy((char *)&numWords, info, sizeof(int));
        info += sizeof(int);
	for (i = 0; i < numWords; i++)
	{
		CLgsWord w;
		info = w.Load(info);
		wv.push_back(w);
	}

   int numPairs = 0;
	memcpy((char *)&numPairs, info, sizeof(int));
   info += sizeof(int);
   for (i = 0; i < numPairs; i++)
	{
		PairEntry pe;
		info = pe.Load(info);
		pev.push_back(pe);
	}

   if (m_tran && m_bAlign)
   {
      m_align->GetFormatSentence(m_buf, m_nSentId);
   }
}
// -------------------------------------------------------------------
bool CLgsMerger::CheckTags(LgsVector(CLgsWord)& swv, LgsVector(CLgsWord)& twv, LgsVector(CLgsProtectedString)& psv)
{
	//Check for the sequence of Tags in translated sentence are in proper order
	LgsVector(int) src_idx;
   LgsVector(int) dst_idx;

	//Get the Indexes of the Protected Strings for each word in the source sentence
	src_idx.clear();
	dst_idx.clear();
	for (int sindex = 0; sindex < swv.size(); sindex++)
	{
		LgsVector(int) intv;
		LgsVector(LgsString) strv;
		if (swv[sindex].Attributes(intv, strv) > 0) 
		{
			for (LgsVector(int)::iterator vit = intv.begin(); vit != intv.end(); vit++)
			{
				int idx = GetPSIndexBySeqNo(psv, *vit);
				if (idx != -1)
					src_idx.push_back(idx);
			}
		}

	}	
	//Get the Indexes of the Protected Strings for each word in the target sentence
	for (int tindex = 0; tindex < twv.size(); tindex++)
	{
		LgsVector(int) intv;
		LgsVector(LgsString) strv;
		int id = GetWordIndexByWordId(swv,twv[tindex].Id());
		if (id != -1 && swv[id].Attributes(intv, strv) > 0) 
		{
			for (LgsVector(int)::iterator vit = intv.begin(); vit != intv.end(); vit++)
			{
				int idx = GetPSIndexBySeqNo(psv, *vit);
				if (idx != -1)
					dst_idx.push_back(idx);
			}
		}
	}	
	//Now the real test to see if the tags need to be preserved
	if (src_idx.size() == dst_idx.size())
	{
		bool altered = false;
		LgsVector(int)::iterator sit, dit;
		sit = src_idx.begin();
		dit = dst_idx.begin();
		while (sit != src_idx.end() && dit != dst_idx.end())
		{
			if (*sit++ != *dit++)
				altered = true;
		}

		return altered;
	}
	else
		return true;
}
// -------------------------------------------------------------------
void CLgsMerger::PrintSpace(CLgsWord& lgsWord)
{
	// print out spaces
	LgsString spsStr;
	int nSp = 0;
	nSp = lgsWord.SpaceInfo(spsStr);
	if (nSp > 0)
   {
		m_out->write(spsStr.c_str(), nSp);
   }
}
// -------------------------------------------------------------------
void CLgsMerger::RemoveAttrs(CLgsWord & word, LgsVector(CLgsProtectedString)& psv)
{
   LgsVector(int) attrs;
   LgsVector(LgsString) attrsSps;
   int count = word.Attributes(attrs, attrsSps);
   if (count == 0)
   {
      return;
   }
   else
   {
      LgsVector(int)::iterator vit = attrs.begin();
      LgsVector(LgsString)::iterator sit = attrsSps.begin();
      int psCount = psv.size();
      while (vit != attrs.end())
      {
         int psIx = GetPSIndexBySeqNo(psv, *vit);
         if ((psIx != -1) && (psv[psIx].m_type != PST_INTLD) && (psv[psIx].m_type != PST_INTRD))
#if defined(_MSC_VER)
         {
            vit = attrs.erase(vit);
            sit = attrsSps.erase(sit);
         }
         else
         {
            vit++;
            sit++;
         }
#else
         //ObjectSpace has implemented LgsVector::erase() differently.
         //It does not return an iterator on deletion.
         {
            attrs.erase(vit);
            attrsSps.erase(sit);
         }
         if (vit == attrs.end())
         {
            break;
         }
         vit++;
         sit++;
#endif //_MSC_VER
      }
      word.SetAttributes(attrs, attrsSps);
   }
}
// -------------------------------------------------------------------
bool CLgsMerger::GetNextWord(CLgsWord& word)
{
   if (m_nTranWid < m_ws.size())
   {
      word = m_ws[m_nTranWid++];
      return true;
   }
   else
   {
      word.ClearFlag(word.GetFlag());
      return false;
   }
}
// -------------------------------------------------------------------
bool CLgsMerger::UnGetLastWord()
{
   if (m_nTranWid - 1 >= 0)
   {
      m_nTranWid--;
      return true;
   }
   else
   {
      return false;
   }
}
// -------------------------------------------------------------------
bool CLgsMerger::FirstWordInTransSent(CLgsWord& word)
{
   if ((m_ws[0].Id() == word.Id()) && (m_ws[0].toString() == word.toString()))
   {
      return true;
   }
   return false;
}
