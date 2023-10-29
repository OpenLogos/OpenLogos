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
// LgsTag.cpp: implementation of the CLgsTag class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>

#include <logos_libs/lgssgml/lgsutil.h>
#include <logos_libs/lgssgml/lgstag.h>

// -------------------------------------------------------------------
CLgsTag::CLgsTag(const LgsString& Tagfilename)
{
	m_defaultClass = SectionTran;
	m_defaultAttr = AttachLeft ^ NoDuplicate;
	
	CLgsUtil::Assert((!Tagfilename.empty()), LgsString("Invalid Tag file name"));
	ifstream TagFile(Tagfilename.c_str());
	CLgsUtil::Assert(TagFile.good(), LgsString("Unable to open Tag file: ") + Tagfilename);
	LgsString TagName, Class, Attribute;
	while (!TagFile.eof())
	{
		LgsString TagRecord = GetRecord(TagFile);
		if (!TagRecord.empty())
		{
			//Break the record into TagName, Class and Attribute
			//The fields are either separated by space or tab character
			const char * recBuffer = TagRecord.c_str();
			//Get TagName
			char* token = strtok((char*) recBuffer, " \t");
			if (token != NULL)
				TagName = token;
			if (TagName == LgsString("LOGOS_DEFAULT"))
			{
				token = strtok(NULL, " \t");
				if (token != NULL)
					Class = token;
				token = strtok(NULL, " \t");
				if (token != NULL)
					Attribute = token;
				if (Class == LgsString("TagClass")) 
					m_defaultClass = LookUp(LgsString("Class"),Attribute);
				if (Class == LgsString("AttachAttribute")) 
					m_defaultAttr = LookUp(LgsString("Attribute"),Attribute);
				if (Class == LgsString("DuplicateAttribute")) 
					m_defaultAttr ^= LookUp(LgsString("Attribute"),Attribute);
			}
			else
			{
				Tag tagVar;
				tagVar.Name = TagName;
				token = strtok(NULL, " \t");
				if (token != NULL)
					Class = token;
				int clas = LookUp(LgsString("Class"), Class);
				if (clas == Invalid)
					throw("Invalid class type");
				else
					tagVar.Class = clas;

				while (NULL != (token = strtok(NULL, " \t")))
				{
					int attr = 0;
					if ((attr = LookUp(LgsString("Attribute"), LgsString(token))) != Invalid)
						tagVar.Attr ^= attr;
				}
				m_tagvector.push_back(tagVar);
			}
		}
	}
	TagFile.close();
}
// -------------------------------------------------------------------
CLgsTag::~CLgsTag()
{
	m_tagvector.clear();
}
// -------------------------------------------------------------------
LgsString CLgsTag::GetRecord(ifstream & TagFile)
{
	if (!TagFile.good())
		throw("Unable to read Tag file");

	char* tagbuf = 0;
	if (!TagFile.eof())
	{
		tagbuf = new char[TagRecordLen];
		CLgsUtil::Assert((tagbuf != NULL) , "Memory Allocation failure");
		TagFile.getline(tagbuf, TagRecordLen-1);
		int buflen = TagFile.gcount();
		tagbuf[buflen] = 0;
      int length = strlen(tagbuf);
      if (length - 1 > 0 && tagbuf[length-1] == 0xd)
         tagbuf[length-1] = 0;
		LgsString TagRecord;
		int index = 0;
		//Copy until either end of buffer or upto comment.
		while (index < buflen && tagbuf[index] != ';')
			TagRecord += tagbuf[index++];
		delete[] tagbuf;
		//Remove Leading & Trailing spaces
		CLgsUtil::TrimSpace(TagRecord);
		return TagRecord;
	}
	else
		return LgsString();
}
// -------------------------------------------------------------------
int CLgsTag::LookUp(const LgsString& type, const LgsString & rStr)
{
	if (type.empty() || rStr.empty())
		return Invalid;
	if (type == LgsString("Class"))
	{
		if (!CLgsUtil::StrCmpCase(rStr, LgsString("SectionTran")))
			return SectionTran;
		if (!CLgsUtil::StrCmpCase(rStr, LgsString("SectionNoTran")))
			return SectionNoTran;
		if (!CLgsUtil::StrCmpCase(rStr, LgsString("MidSentTran")))
			return MidSentTran;
		if (!CLgsUtil::StrCmpCase(rStr, LgsString("MidSentNoTran")))
			return MidSentNoTran;
		if (!CLgsUtil::StrCmpCase(rStr, LgsString("MidSentHidden")))
			return MidSentHidden;
		if (!CLgsUtil::StrCmpCase(rStr, LgsString("PointSection")))
			return PointSection;
		if (!CLgsUtil::StrCmpCase(rStr, LgsString("Point")))
			return Point;
		if (!CLgsUtil::StrCmpCase(rStr, LgsString("SectionProtect")))
			return SectionProtect;
		return Invalid;
	}
	if (type == LgsString("Attribute"))
	{
		if (!CLgsUtil::StrCmpCase(rStr, LgsString("AttachLeft")))
			return AttachLeft;
		if (!CLgsUtil::StrCmpCase(rStr, LgsString("AttachRight")))
			return AttachRight;
		if (!CLgsUtil::StrCmpCase(rStr, LgsString("Duplicate")))
			return Duplicate;
		if (!CLgsUtil::StrCmpCase(rStr, LgsString("NoDuplicate")))
			return NoDuplicate;
		return Invalid;
	}
	return Invalid;
}
// -------------------------------------------------------------------
int CLgsTag::GetTagClass(LgsString & rStr)
{
	if (rStr.empty())
		return Invalid;
	TAGV::iterator VIter = m_tagvector.begin();

	while (VIter != m_tagvector.end())
	{
		if (!CLgsUtil::StrCmpCase(VIter->Name,rStr))
			return VIter->Class;
		VIter++;
	}

	return Invalid;

}
// -------------------------------------------------------------------
int CLgsTag::GetTagAttr(LgsString & rStr)
{
	if (rStr.empty())
		return Invalid;
	TAGV::iterator VIter = m_tagvector.begin();

	while (VIter != m_tagvector.end())
	{
		if (CLgsUtil::StrCmpCase(VIter->Name,rStr))
			return VIter->Attr;
		VIter++;
	}

	return m_defaultAttr;

}
// -------------------------------------------------------------------
int CLgsTag::GetTagClass(const char * str)
{
	LgsString TagStr = str;
	if (TagStr.empty())
		return Invalid;
   TAGV::iterator tagIter = m_tagvector.begin();
   int nPos = 0;
	while (tagIter != m_tagvector.end())
	{
		if (NPOS != (nPos = TagStr.find(tagIter->Name)))
      {
         char ch = TagStr.at(nPos+tagIter->Name.length());
         if (ch == ' ' || ch == '>')
			   return tagIter->Class ^ tagIter->Attr;
      }
		tagIter++;
	}
	return Invalid;
}
