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
// LgsWord.h: interface for the CLgsWord class.
//
//////////////////////////////////////////////////////////////////////

#ifndef __lgsword_h__
#define __lgsword_h__

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

typedef LgsVector(int) INTV;
typedef LgsVector(int)::iterator INTVIT;
typedef LgsVector(LgsString) STRV;
typedef LgsVector(LgsString)::iterator STRVIT;

class CLgsWord
{
public:
	typedef enum 
   {
		bold = 0x01, italic = 0x02, underline = 0x04, dblunderline = 0x08,
      squarebrk = 0x10, curlbrace = 0x20, anglebrk = 0x40, parenthes = 0x80,
      dblquote = 0x100, sglquote = 0x200, touched = 0x400
   } WordFlag;

   CLgsWord();
   virtual ~CLgsWord();

   CLgsWord& operator=(const char* s);
   CLgsWord& operator=(const LgsString& s);
	const LgsString& toString();
	char* Store(char* f);
	const char* Load(const char* f);
   int calcMsgSize();
   int SpaceEOSInfo(LgsString& spcLgsString);

	void SetSequenceNumber(int n);
	int SequenceNumber();

	void SetSpaceInfo(int numSpaces, const LgsString& spacesLgsString);
	int SpaceInfo(LgsString& spacesLgsString);
   int TotalSpaces();

	void AddAttribute(int n, LgsString& spacesLgsString);
	void SetAttributes(const LgsVector(int)& attrs, const LgsVector(LgsString) & attrsSps);
	int Attributes(LgsVector(int)& attrs, LgsVector(LgsString)& attrsSps);
	void RemoveAttrs();
   int TotalAttributes();

	void SetFlag(WordFlag f);
  	void SetFlag(unsigned int f);
	void ClearFlag(unsigned int f);
   void ClearAllFlags();
   unsigned int GetFlag();

	int Id();
   void Id(int id);
   
   void SetTranString(LgsString&);
   void SetTranString(LgsString&, LgsString&);
   const LgsString& toTranString();

   CLgsWord(const CLgsWord &); //Copy Constructor
   CLgsWord& operator=(const CLgsWord &);   
	unsigned int GetPairFlag();
	bool GetTranslate();
	void SetTranslate(bool tran);
	bool IsValid();
	bool IsBold();
	bool IsItalic();
	bool IsUnderline();
	bool IsDblUnderline();
	bool IsSquarebrk();
	bool IsCurlybraced();
	bool IsAnglebrk();
	bool IsParenthes();
	bool IsDblQuoted();
	bool IsSglQuoted();
	bool IsTouched();
	bool IsProtected();
	bool IsLiteral();
	void SetBold();
	void SetItalic();
	void SetUnderline();
	void SetDblUnderline();
	void SetSquarebrk();
	void SetCurlybraced();
	void SetAnglebrk();
	void SetParenthes();
	void SetDblQuoted();
	void SetSglQuoted();
	void SetTouched();
   void SetLiteral(bool p);
	void SetProtected(bool p);
	bool AttrSpcs();

private:
	bool m_bValid;
	bool m_bTran;
	int m_id;			// unique word identifier
   LgsString m_str;		// LgsString value of the word
   LgsString m_tranStr; // LgsString value of the word (After translation)
  	int m_numSps;		// #of spaces preceding the word (sizeof m_spStr)
	LgsString m_spStr;	// spaces LgsString which comes before the word
	unsigned int m_flags; // bold, italic, underline flags
	bool m_protected;	// this word is protected, m_str should be empty
	bool m_isLiteral; // listed either in $MONTHSFILE, or $NEVEREOSFILE file
	int m_seqNum;		// this word sequence number if this is a protected word
	INTV m_attrs;     // tags sequence numbers in the format file, can be empty
	STRV m_attrsSps;  // spaces LgsString
};

inline int CLgsWord::TotalSpaces()
{
   return m_numSps;
}

inline int CLgsWord::TotalAttributes()
{
   return m_attrs.size();
}

#endif
