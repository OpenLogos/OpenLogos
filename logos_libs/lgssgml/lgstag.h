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
// LgsTag.h: interface for the CLgsTag class.
//
//////////////////////////////////////////////////////////////////////

#ifndef __lgstag_h__
#define __lgstag_h__

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

typedef struct TagRecord
{
	LgsString Name;    //The Name defined in SGML.tags file
	int Class;      //The Class it belongs to
	int Attr;       //The Attribute of the class
	TagRecord();
} Tag;

typedef LgsVector(Tag) TAGV;

class CLgsTag  
{
public:
	enum TagType
   {
      AttachRight = 0x01, AttachLeft = 0x02, Duplicate = 0x04, 
      NoDuplicate = 0x08, SectionTran = 0x10, SectionNoTran = 0x20, 
      MidSentTran = 0x40,	MidSentNoTran = 0x80, MidSentHidden = 0x100,
      PointSection = 0x200, Point = 0x400, Invalid = 0x800, SectionProtect = 0x1000
   };

   enum
   {
      TagRecordLen = 512
   };

	CLgsTag(const LgsString& Tagfilename);
	virtual ~CLgsTag();

   int GetTagClass(LgsString& rStr);
	int GetTagAttr(LgsString & rStr);
	int LookUp(const LgsString& type, const LgsString & rStr);
	LgsString GetRecord(ifstream& file);
	int GetTagClass(const char * str);

private:
	TAGV m_tagvector;
	int m_defaultClass;
	int m_defaultAttr;
	int m_defaultDuplAttr;
};
//-------------------------------------------------------------------
inline TagRecord::TagRecord()
{
   Class = 0;
   Attr = 0;
} 
#endif


