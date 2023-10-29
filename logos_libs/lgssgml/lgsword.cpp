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
// LgsWord.cpp: implementation of the CLgsWord class.
//
//////////////////////////////////////////////////////////////////////
#include <logos_include/logoscommon.h>
#include <logos_libs/lgssgml/lgsutil.h>
#include <logos_libs/lgssgml/lgsword.h>

// -------------------------------------------------------------------
CLgsWord::CLgsWord()
{
   m_bValid = false;
   m_isLiteral = false;
	m_numSps = 0;
	m_protected = false;
	m_flags = 0;
	m_seqNum = -1; // void
	m_id = -1;	// void
   m_bTran = true;
}
// -------------------------------------------------------------------
CLgsWord::~CLgsWord()
{
}
// -------------------------------------------------------------------
CLgsWord::CLgsWord(const CLgsWord& rhs) //Copy Constructor
{
	m_id = rhs.m_id;			
   m_str = rhs.m_str;
   m_tranStr = rhs.m_tranStr; 
  	m_numSps = rhs.m_numSps;
	m_spStr = rhs.m_spStr;	
	m_flags = rhs.m_flags; 
	m_protected = rhs.m_protected;
	m_isLiteral = rhs.m_isLiteral; 
	m_seqNum = rhs.m_seqNum;
   SetAttributes(rhs.m_attrs, rhs.m_attrsSps);
   m_bValid = rhs.m_bValid;
   m_bTran = rhs.m_bTran;
}
// -------------------------------------------------------------------
CLgsWord& CLgsWord::operator=(const CLgsWord& rhs) 
{
   m_id = rhs.m_id;			
   m_str = rhs.m_str;
   m_tranStr = rhs.m_tranStr; 
  	m_numSps = rhs.m_numSps;
	m_spStr = rhs.m_spStr;	
	m_flags = rhs.m_flags; 
	m_protected = rhs.m_protected;
	m_isLiteral = rhs.m_isLiteral; 
	m_seqNum = rhs.m_seqNum;
   SetAttributes(rhs.m_attrs, rhs.m_attrsSps);
   m_bValid = rhs.m_bValid;
   m_bTran = rhs.m_bTran;
   return *this;
}
// -------------------------------------------------------------------
int CLgsWord::Id()
{
	return m_id;
}
// -------------------------------------------------------------------
void CLgsWord::Id(int id)
{
	m_id = id;
}
// -------------------------------------------------------------------
CLgsWord& CLgsWord::operator=(const char* s)
{
	m_str = s;
	return *this;
}
// -------------------------------------------------------------------
CLgsWord& CLgsWord::operator=(const LgsString& s)
{
   m_str = s;
   return *this;
}
// -------------------------------------------------------------------
void CLgsWord::SetTranString(LgsString& tranString)
{
   m_tranStr = tranString;
}
// -------------------------------------------------------------------
void CLgsWord::SetTranString(LgsString& tranString, LgsString& OrigStr)
{
   LgsString ProtectedWord = "^p";
   int pos = tranString.find(ProtectedWord);
   if (pos == -1)
      tranString = OrigStr;
   else
   {
      LgsString tempStr = tranString.substr(0, pos) 
                       + OrigStr 
                       + tranString.substr(pos + ProtectedWord.length(),
                       tranString.length() - (pos + ProtectedWord.length()));
      tranString = tempStr;
   }

   m_tranStr = tranString;
}
// -------------------------------------------------------------------
const LgsString& CLgsWord::toTranString()
{
   return m_tranStr;
}
// -------------------------------------------------------------------
const LgsString& CLgsWord::toString()
{
	return m_str;
}
// -------------------------------------------------------------------
void CLgsWord::SetProtected(bool p)
{
	m_protected = p;
}
// -------------------------------------------------------------------
bool CLgsWord::IsProtected()
{
	return m_protected;
}
// -------------------------------------------------------------------
void CLgsWord::SetSequenceNumber(int n)
{
	m_seqNum = n;
}
// -------------------------------------------------------------------
int CLgsWord::SequenceNumber()
{
	return m_seqNum;
}
// -------------------------------------------------------------------
void CLgsWord::AddAttribute(int n, LgsString& spacesString)
{
	m_attrs.push_back(n);
	m_attrsSps.push_back(spacesString);
}
// -------------------------------------------------------------------
void CLgsWord::SetAttributes(const LgsVector(int)& attrs, const LgsVector(LgsString)& attrsSps)
{
	m_attrs.erase(m_attrs.begin(), m_attrs.end());
	m_attrs = attrs;
	m_attrsSps.erase(m_attrsSps.begin(), m_attrsSps.end());
	m_attrsSps = attrsSps;
}
// -------------------------------------------------------------------
int CLgsWord::Attributes(LgsVector(int)& attrs, LgsVector(LgsString)& attrsSps)
{
	attrs = m_attrs;
	attrsSps = m_attrsSps;
	return m_attrs.size();
}
// -------------------------------------------------------------------
bool CLgsWord::AttrSpcs()
{
	return m_attrsSps.size();
}
// -------------------------------------------------------------------
void CLgsWord::SetFlag(WordFlag f)
{
	m_flags |= f;
}
// -------------------------------------------------------------------
void CLgsWord::ClearFlag(unsigned int f)
{
	m_flags &= ~f;
}
// -------------------------------------------------------------------
void CLgsWord::ClearAllFlags()
{
   m_flags = 0;
}
// -------------------------------------------------------------------
void CLgsWord::SetSpaceInfo(int numSpaces, const LgsString& spacesString)
{
	m_numSps = numSpaces;
	m_spStr = spacesString;
}
// -------------------------------------------------------------------
int CLgsWord::SpaceInfo(LgsString& spacesString)
{
	spacesString = m_spStr;
	return m_numSps;
}
// -------------------------------------------------------------------
int CLgsWord::calcMsgSize()
{
   int msgSize = 0;
   msgSize += sizeof(int);                   // id
   msgSize += sizeof(unsigned int);          // flags
   msgSize += sizeof(bool);                  // protected
   if (IsProtected())
   {
      msgSize += sizeof(int);                // seq #
   }
   msgSize += sizeof(int);                   // # attrs
   msgSize += m_attrs.size() * sizeof(int);  // attrs
   for (int i = 0; i < m_attrs.size(); i++)  // attrs spaces
   {
      msgSize += sizeof(int);                // # attrs spaces
      msgSize += m_attrsSps[i].length() * sizeof(char);
   }

   msgSize += sizeof(int);                   // number of spaces
   msgSize += m_numSps * sizeof(char);       // spaces
   msgSize += sizeof(int);                   // word length
   msgSize += m_str.length() * sizeof(char); // word

   return msgSize;
}
// -------------------------------------------------------------------
char* CLgsWord::Store(char* f)
{
   char* info = f;
	int dataTemp;

	// id
   memcpy(info, (const char*)&m_id, sizeof(int));
   info += sizeof(int);

	// flags
   memcpy(info, (const char*)&m_flags, sizeof(unsigned int));
   info += sizeof(unsigned int);

	// protected
   memcpy(info, (const char*)&m_protected, sizeof(bool));
   info += sizeof(bool);

	// seq# only if protected
	if (IsProtected())
   {
      memcpy(info, (const char*)&m_seqNum, sizeof(int));
      info += sizeof(int);
   }

	// attrs, attrsNumSps, attrsSps
	dataTemp = m_attrs.size();
   memcpy(info, (const char*)&dataTemp, sizeof(int));
   info += sizeof(int);
   int i;
	for (i = 0; i < dataTemp; i++)
	{
      memcpy(info, (const char*)&(m_attrs[i]), sizeof(int));
      info += sizeof(int);
	}
	for (i = 0; i < dataTemp; i++)
	{
      int len = m_attrsSps[i].length();
      memcpy(info, (const char*)&len, sizeof(int));
      info += sizeof(int);
      memcpy(info, (const char*)m_attrsSps[i].c_str(), len);
      info += len;
	}

	// numSps, and Sps str if not empty
   memcpy(info, (const char*)&m_numSps, sizeof(int));
   info += sizeof(int);
	if (m_numSps > 0)
   {
      memcpy(info, (const char*)m_spStr.c_str(), m_numSps);
      info += m_numSps;
	}

	// LgsString
	dataTemp = m_str.length();
   memcpy(info, (const char*)&dataTemp, sizeof(int));
   info += sizeof(int);
	if (dataTemp > 0)
   {
      memcpy(info, (const char*)m_str.c_str(), dataTemp);
      info += dataTemp;
	}
   return info;
}
// -------------------------------------------------------------------
const char* CLgsWord::Load(const char* f)
{
   const char* info = f;
	int dataTemp;

	// id
   memcpy((char*)&m_id, info, sizeof(int));
   info += sizeof(int);

	// flags
   memcpy((char*)&m_flags, info, sizeof(unsigned int));
   info += sizeof(unsigned int);

	// protected
   memcpy((char*)&m_protected, info, sizeof(bool));
   info += sizeof(bool);

	// seq# only if protected
	if (m_protected)
   {
      memcpy((char*)&m_seqNum, info, sizeof(int));
      info += sizeof(int);
   }

	// attrs
	m_attrs.erase(m_attrs.begin(), m_attrs.end());
	m_attrsSps.erase(m_attrsSps.begin(), m_attrsSps.end());
   memcpy((char*)&dataTemp, info, sizeof(int));
   info += sizeof(int);
   int i;
	for (i = 0; i < dataTemp; i++)
	{
		int jj;
      memcpy((char*)&jj, info, sizeof(int));
      info += sizeof(int);
		m_attrs.push_back(jj);
	}
	for (i = 0; i < dataTemp; i++)
	{
		int len;
      memcpy((char*)&len, info, sizeof(int));
      info += sizeof(int);
		char* buf = new char[len + 1];
		CLgsUtil::Assert(NULL != buf, "out of memory");
      memcpy(buf, info, len);
      info += len;
		buf[len] = 0;
		LgsString attrsSpsStr = buf;
		m_attrsSps.push_back(attrsSpsStr);
		delete[] buf;
	}

	// space LgsString
	m_spStr.erase();
   memcpy((char*)&m_numSps, info, sizeof(int));
   info += sizeof(int);
	if (m_numSps > 0)
	{
		char* buf = new char[m_numSps + 1];
		CLgsUtil::Assert(NULL != buf, "out of memory");
      memcpy(buf, info, m_numSps);
      info += m_numSps;
		buf[m_numSps] = 0;
		m_spStr = buf;
		delete[] buf;
	}

	// LgsString
	m_str.erase();
   memcpy((char*)&dataTemp, info, sizeof(int));
   info += sizeof(int);
	if (dataTemp > 0)
	{
		char* buf = new char[dataTemp + 1];
		CLgsUtil::Assert(NULL != buf, "out of memory");
      memcpy(buf, info, dataTemp);
      info += dataTemp;
		buf[dataTemp] = 0;
		m_str = buf;
		delete[] buf;
	}

   return info;
}
// -------------------------------------------------------------------
void CLgsWord::RemoveAttrs()
{
	m_attrs.erase(m_attrs.begin(), m_attrs.end());	
	m_attrsSps.erase(m_attrsSps.begin(), m_attrsSps.end());	
}
// -------------------------------------------------------------------
void CLgsWord::SetLiteral(bool p)
{
   m_isLiteral = p;
}
// -------------------------------------------------------------------
bool CLgsWord::IsLiteral()
{
   return m_isLiteral;
}
// -------------------------------------------------------------------
int CLgsWord::SpaceEOSInfo(LgsString & spcString)
{
   int numSpcs = SpaceInfo(spcString);
   if (numSpcs == 0)
      return 0;
   LgsString SpaceString;
   LgsString::iterator vindex = spcString.begin();
   char prevChar = 0;
   //Convert multiple spaces to a single Space
   while (vindex != spcString.end()) 
   {
      if (*vindex != prevChar) 
      {
         prevChar = *vindex;
         SpaceString += *vindex;
      }
      vindex++;
   }
   spcString = SpaceString;
   SpaceString.erase(SpaceString.begin(), SpaceString.end());
   vindex = spcString.begin();
   //Converting the combination CR\LF to 0xa
   while (vindex != spcString.end())
   {
      if ((*vindex == 0xd) && ((vindex + 1) != spcString.end()) && (*(vindex + 1) == 0xa))
      {
         SpaceString += 0xa;
         vindex++;
      }
      else
         SpaceString += *vindex;
      
      vindex++;
   }

   spcString = SpaceString;
   return spcString.length();
}
// -------------------------------------------------------------------
bool CLgsWord::IsValid()
{
   return m_bValid;
}
// -------------------------------------------------------------------
void CLgsWord::SetTranslate(bool tran)
{
   m_bTran = tran;
}
// -------------------------------------------------------------------
bool CLgsWord::GetTranslate()
{
   return m_bTran;
}
// -------------------------------------------------------------------
unsigned int CLgsWord::GetFlag()
{
   return m_flags;
}
// -------------------------------------------------------------------
void CLgsWord::SetFlag(unsigned int f)
{
   m_flags |= f;
}
// -------------------------------------------------------------------
unsigned int CLgsWord::GetPairFlag()
{
   return m_flags & 0xFFFFF000;
}
// -------------------------------------------------------------------
bool CLgsWord::IsBold()
{
	return 0 != (m_flags & bold);
}
// -------------------------------------------------------------------
bool CLgsWord::IsItalic()
{
	return 0 != (m_flags & italic);
}
// -------------------------------------------------------------------
bool CLgsWord::IsUnderline()
{
	return 0 != (m_flags & underline);
}
// -------------------------------------------------------------------
bool CLgsWord::IsDblUnderline()
{
   return 0 != (m_flags & dblunderline);
}
// -------------------------------------------------------------------
bool CLgsWord::IsSquarebrk()
{
   return 0 != (m_flags & squarebrk);
}
// -------------------------------------------------------------------
bool CLgsWord::IsCurlybraced()
{
   return 0 != (m_flags & curlbrace);
}
// -------------------------------------------------------------------
bool CLgsWord::IsAnglebrk()
{
   return 0 != (m_flags & anglebrk);
}
// -------------------------------------------------------------------
bool CLgsWord::IsParenthes()
{
   return 0 != (m_flags & parenthes);
}
// -------------------------------------------------------------------
bool CLgsWord::IsDblQuoted()
{
   return 0 != (m_flags & dblquote);
}
// -------------------------------------------------------------------
bool CLgsWord::IsSglQuoted()
{
   return 0 != (m_flags & sglquote);

}
// -------------------------------------------------------------------
bool CLgsWord::IsTouched()
{
	return 0 != (m_flags & touched);
}
// -------------------------------------------------------------------
void CLgsWord::SetBold()
{
   m_flags |= bold;
}
// -------------------------------------------------------------------
void CLgsWord::SetItalic()
{
   m_flags |= italic;
}
// -------------------------------------------------------------------
void CLgsWord::SetUnderline()
{
   m_flags |= underline;
}
// -------------------------------------------------------------------
void CLgsWord::SetDblUnderline()
{
   m_flags |= dblunderline;
}
// -------------------------------------------------------------------
void CLgsWord::SetSquarebrk()
{
   m_flags |= squarebrk;
}
// -------------------------------------------------------------------
void CLgsWord::SetCurlybraced()
{
   m_flags |= curlbrace;
}
// -------------------------------------------------------------------
void CLgsWord::SetAnglebrk()
{
   m_flags |= anglebrk;
}
// -------------------------------------------------------------------
void CLgsWord::SetParenthes()
{
   m_flags |= parenthes;
}
// -------------------------------------------------------------------
void CLgsWord::SetDblQuoted()
{
   m_flags |= dblquote;
}
// -------------------------------------------------------------------
void CLgsWord::SetSglQuoted()
{
   m_flags |= sglquote;
}
// -------------------------------------------------------------------
void CLgsWord::SetTouched()
{
   m_flags |= touched;
}
