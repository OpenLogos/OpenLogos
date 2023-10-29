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
// LgsSentence.h: interface for the CLgsSentence class.
//
//////////////////////////////////////////////////////////////////////

#ifndef __lgssentence_h__
#define __lgssentence_h__

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include <transl/translthrman.h>
#include <logos_libs/multithreadlib/comminterface.h>
#include <logos_libs/lgssgml/lgsword.h>
#include <logos_libs/lgssgml/lgsentity.h>
#include <logos_libs/lgssgml/lgsfmtpipe.h>
#include <logos_libs/lgssgml/litscan.h>
#include <logos_libs/lgssgml/lgsentity.h>
#include <logos_libs/lgssgml/lgstranio.h>

class CLgsAlign;

// flags associated with a word
#define FL_BOLD		(1<<0)
#define FL_ITALIC		(1<<1)
#define FL_UNDERLINE	(1<<2)
#define FL_DBLQUOTE  (1<<3)
#define FL_CURLBRACE (1<<4)
#define FL_PARENTHES (1<<5)
#define FL_SQUAREBRK (1<<6)
#define FL_ANGLEBRK  (1<<7)
#define FL_SGLQUOTE  (1<<8)
#define FL_DBLUNDERLINE (1<<9)

// protected LgsString types
#define PST_INTR	(1<<0)
#define PST_INTRD	(1<<1)
#define PST_INTL	(1<<2)
#define PST_INTLD	(1<<3)
#define PST_INTW	(1<<4)
#define PST_USER	(1<<5)
#define PST_NONE  (1<<6)
#define PST_OPEN  (1<<7)
#define PST_CLOSE (1<<8)

struct CLgsProtSubType
{
   int m_nIntType;
   int m_nIntNum;
   int GetType();
   int GetSeqNum();
};

struct CLgsProtectedString
{
	LgsString m_lb;
	LgsString m_rb;
	int m_seqNum; // for internal use only
	int m_type;
	LgsString m_string;
   CLgsProtSubType m_subtype;
	char* Store(char* f);
	const char* Load(const char* f);
   int calcMsgSize();
   void SetSubType(LgsString& lb, const char* markType);
   CLgsProtSubType GetSubType();
   bool m_bMarkedForDeletion;
};

struct TagTableEntry
{
   int nTagId;
   LgsVector(int) vSrcSeqNum;
   LgsVector(int) vSrcWordId;
   bool operator<(const TagTableEntry& rhs) const;
   void InsSrcSeqNum(int seqNum);
   void InsSrcWordId(int Id);
};

struct PairEntry
{
   unsigned int nPairFlag;
   int nIntrSeqNum;
   int nIntlSeqNum;
   char* Store(char* f);
   const char* Load(const char* f);
   int calcMsgSize();
};

struct DupTagsEntry
{
   int nRSeqNo;
   LgsSet(int) vWordId;
   DupTagsEntry();
   ~DupTagsEntry();
};


typedef LgsVector(bool) BV;
typedef LgsVector(bool)::iterator BVIT;
typedef LgsVector(int) INTV;
typedef LgsVector(int)::iterator IVIT;
typedef LgsVector(LgsString) STRV;
typedef LgsVector(LgsString)::iterator SVIT;
typedef LgsVector(CLgsWord) WV;
typedef LgsVector(CLgsWord)::iterator WVIT;
typedef LgsVector(CLgsProtectedString) PSV;
typedef LgsVector(CLgsProtectedString)::iterator PSVIT;
typedef LgsVector(PairEntry) PairEntryVector;
typedef LgsVector(PairEntry)::iterator PairEntryIterator;

class CLgsSentence  
{
public:
	CLgsSentence();
	virtual ~CLgsSentence();

   bool IsOrdinal(LgsString &s);
	void ApplyDupTags(int start, int count);
	void PairTags(int start, int count);

	void WriteExtHeader(const char* buf, int len);
	void WriteString(const char* s, int len);
   void SendEndOfInput();
	void MoreData(char* buf, int bufLen);
	void Flush();
	bool Init(int bufSize);
   int SentencesSentToTranslation();
	void AddProtectedWord(int seqNum, const char* markType, LgsString& pwStr, LgsString lb, LgsString rb);
   int CurrentSentenceID();

private:
	void FlushUnacclaimedIntRs();
	bool IsAbbreviation(LgsString& s);
	bool IsMonth(LgsString& s);
	bool NeedsTran(int start, int count);
	bool IsEOS(int start, int cur);
	int GetPSIndexBySeqNum(int seqNum);
	void print_sentence(int start, int count, bool needsTran, int sentId);
   int calcMsgSize(int start, int count, bool needsTran);
	void WriteTrailSpaces();
	void WriteSentences(bool all);

	static bool m_bDoubleQuoteOpen;
	static bool m_bSingleQuoteOpen;
   int m_totalTransSent;
   bool m_bEOS;
   bool m_bAlign;
   bool m_bNeedsTran;
   bool m_translationMode;
	CLitScan m_scn;
	LgsTranIO* m_pTranIO;
	CLgsFmtPipe* m_fmt;
	unsigned int m_flags;
   unsigned int m_nSentFlags;
	STRV m_ts;	   // tokens
	BV m_flg;	   // isLiteral flags
	int m_numTrailingSpaces;
	LgsString m_trailingSpaces;
	LgsString m_detachedSpaces;
	INTV m_ss;		// m_ss[i] is the #of spaces before m_ts[i]
	STRV m_sps;	   // m_sps[i] is the spaces string for m_ss[i]
	INTV m_psNum;	// attr seq numbers to associate with a word
	STRV m_psSps;	// attr spaces strings
	PSV m_psStr;   // array of protected strings
   PairEntryVector m_vPairTable;
	CLgsEntity m_entity;
   CLgsAlign* m_align;
	int m_nSentId;
	WV m_lgsWords;
	STRV m_monthsList;
	STRV m_abbrList;
	STRV m_ordinalList;
};
//-------------------------------------------------------------------
inline int CLgsProtSubType::GetType()
{
   return m_nIntType;
}
//-------------------------------------------------------------------
inline int CLgsProtSubType::GetSeqNum()
{
   return m_nIntNum;
}
//-------------------------------------------------------------------
inline bool TagTableEntry::operator<(const TagTableEntry& rhs) const
{
   return (this->nTagId < rhs.nTagId);
}
//-------------------------------------------------------------------
inline void TagTableEntry::InsSrcSeqNum(int seqNum)
{
   vSrcSeqNum.push_back(seqNum);
}
//-------------------------------------------------------------------
inline void TagTableEntry::InsSrcWordId(int Id)
{
   vSrcWordId.push_back(Id);
}
//-------------------------------------------------------------------
inline DupTagsEntry::DupTagsEntry()
{
   nRSeqNo = -1; vWordId.clear();
}
//-------------------------------------------------------------------
inline DupTagsEntry::~DupTagsEntry()
{
   vWordId.clear();
} 
//-------------------------------------------------------------------
inline int CLgsSentence::SentencesSentToTranslation()
{
   return m_totalTransSent;
}
//-------------------------------------------------------------------
inline int CLgsSentence::CurrentSentenceID()
{
   return m_nSentId;
}
//-------------------------------------------------------------------
static inline int GetPSIndexBySeqNo(LgsVector(CLgsProtectedString)& a, int seqNo)
{
	for (int i = 0; i < a.size(); i++)
	{
		if (seqNo == a[i].m_seqNum)
      {
			return i;
      }
	}
	return -1;
}
//-------------------------------------------------------------------
static inline int GetWordIndexByWordId(LgsVector(CLgsWord)& a, int id, int start = 0)
{
	for (int i = start; i < a.size(); i++)
	{
		if (id == a[i].Id())
      {
		   return i;
      }
	}
	return -1;
}

#endif

