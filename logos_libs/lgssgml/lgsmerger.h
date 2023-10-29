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
// LgsMerger.h: interface for the CLgsMerger class.
//
//////////////////////////////////////////////////////////////////////
#ifndef __lgsmerger_h__
#define __lgsmerger_h__

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include <transl/translthrman.h>
#include <logos_libs/multithreadlib/comminterface.h>
#include <logos_libs/lgssgml/lgsword.h>
#include <logos_libs/lgssgml/lgsfmtpipe.h>
#include <logos_libs/lgssgml/lgstarget.h>
#include <logos_libs/lgssgml/lgsentity.h>
#include <logos_libs/lgssgml/lgstranio.h>
#include <logos_libs/lgssgml/lgssentence.h>
#include <logos_libs/lgssgml/lgsalign.h>

// flags associated with a word
#define FL_BOLD		(1<<0)
#define FL_ITALIC	   (1<<1)
#define FL_UNDERLINE (1<<2)
#define FL_DBLQUOTE  (1<<3)
#define FL_CURLBRACE (1<<4)
#define FL_PARENTHES (1<<5)
#define FL_SQUAREBRK (1<<6)
#define FL_ANGLEBRK  (1<<7)
#define FL_SGLQUOTE  (1<<8)

class CLgsMerger  
{
public:
   CLgsMerger();
   virtual ~CLgsMerger();
   bool UnGetLastWord();
   bool GetNextWord(CLgsWord& tranWord);
   void RemoveAttrs(CLgsWord& word, LgsVector(CLgsProtectedString) & psv);
   bool Init();
   bool Run();

protected:
   void SetDropFlag(LgsVector(CLgsWord)& swv, LgsVector(CLgsWord)& dwv);
   void PrintDroppedINTR(LgsVector(CLgsWord)& wv, LgsVector(CLgsProtectedString)& psv);
   void PrintDroppedINTL(LgsVector(CLgsWord)& wv, LgsVector(CLgsProtectedString)& psv);
   int  PrintFromFormat(LgsVector(CLgsWord)& wv, LgsVector(CLgsProtectedString)& psv, LgsVector(PairEntry)& pev);
   void LoadSentence(LgsVector(CLgsWord)& wv, LgsVector(CLgsProtectedString)& psv, LgsVector(PairEntry)& pev);
   void PrintSpace(CLgsWord& lgsWord);
   //New_Algorithm Methods
   void InitWordVector(LgsVector(CLgsWord)& swv, LgsVector(CLgsWord)& twv);
   void PrintWord(CLgsWord & word, LgsVector(CLgsProtectedString)& psv, LgsVector(PairEntry)& pev);
   void PrintWord(LgsVector(CLgsWord)& swv, LgsVector(CLgsProtectedString)& psv, LgsVector(PairEntry)& pev , CLgsWord& word);
   bool CheckTags(LgsVector(CLgsWord)& swv, LgsVector(CLgsWord)& twv, LgsVector(CLgsProtectedString)& psv);
   bool FirstWordInTransSent(CLgsWord& word);

private:
   void WritePS(CLgsProtectedString& ps);
   void WriteIntLR(CLgsProtectedString& ps, LgsString& sps, bool psps = true, bool nl = false);
   int ReadSentence();

   bool m_bAlign;
   bool m_droptag;
   bool m_dropped;
   bool m_repl;
   bool m_end;
   bool m_convert;
   bool m_preserve;
   bool m_tran;
   int m_nTranWid;
   int m_bufSize;
   int m_nSentId;
   int m_totalTransSent;
   unsigned int m_flags;
   char* m_buf;
   LgsString m_senSpStr;
   CLgsFmtPipe* m_fmt;
   CLgsTarget* m_out;
   LgsTranIO* m_pTranIO;
   CLgsEntity m_entity;
   LgsVector(CLgsWord) m_ws;
   CLgsAlign* m_align;
};

#endif
