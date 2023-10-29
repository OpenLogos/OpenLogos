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
// LgsTarget.cpp: implementation of the CLgsTarget class.
//
//////////////////////////////////////////////////////////////////////
#include <logos_include/logoscommon.h>
#include <logos_libs/lgssgml/lgsutil.h>
#include <logos_libs/lgssgml/lgstarget.h>

// -------------------------------------------------------------------
CLgsTarget::CLgsTarget(const LgsString& filename, bool text)
{
	CLgsUtil::Assert(!filename.empty(), "Output filename not specfied");
	m_bText = text;
   //Open the target file for writing
#ifdef _MSC_VER
	if (!text)
		m_fsOutfile = new ofstream(filename.c_str(), ios::out | ios::binary);
	else
		m_fsOutfile = new ofstream(filename.c_str(), ios::out);
#else //UNIX_STL
	m_fsOutfile = new ofstream(filename.c_str(), ios::out);
#endif //_MSC_VER

   //Throw an exception on error!!
   if (!m_fsOutfile->good())
   {
      throw LgsString("Could not open the file ") + filename + LgsString("for writing");
   }
   m_bFlush = false;
   m_strBuffer.erase(m_strBuffer.begin(), m_strBuffer.end());
}
// -------------------------------------------------------------------
CLgsTarget::~CLgsTarget()
{
   if (m_fsOutfile)
   {
	   if (m_strBuffer.length() > 0)
      {
		   m_fsOutfile->write(m_strBuffer.c_str(), m_strBuffer.length());
      }
	   m_fsOutfile->close();
      delete m_fsOutfile;
   }
}

// -------------------------------------------------------------------
void CLgsTarget::write(const char * buf, int buflength)
{
	if (m_bFlush)
	{
		m_fsOutfile->write(m_strBuffer.c_str(), m_strBuffer.length());
		m_strBuffer.erase(m_strBuffer.begin(), m_strBuffer.end());
		m_strBuffer = LgsString(buf, buflength);
		m_bFlush = false;
	}
	else
	{
		if (m_bAdjustLastSpace)
		{
			if ((m_strBuffer.length() > 0) && (m_strBuffer.at(m_strBuffer.length() - 1)  == ' '))
				m_strBuffer.insert(m_strBuffer.length() -1 , LgsString(buf, buflength) ) ;
			else
				m_strBuffer += LgsString(buf, buflength);
		}
		else
		{
			m_strBuffer += LgsString(buf, buflength);
		}
	}
}
// -------------------------------------------------------------------
CLgsTarget& CLgsTarget::operator <<(const char * buf)
{
	if (m_bText)
		(*m_fsOutfile) << buf;
	else
		write(buf, strlen(buf));
	return *this;
}
// -------------------------------------------------------------------
CLgsTarget& CLgsTarget::operator <<(const LgsString & str)
{
	if (m_bText)
		(*m_fsOutfile) << str;
	else
		write(str.c_str(), str.length());
	return *this;
}
// -------------------------------------------------------------------
CLgsTarget & CLgsTarget::operator <<(int num)
{
	if (m_bText)
		(*m_fsOutfile) << num;
	else
		write((char*) & num, sizeof(int));
	return * this;
}
// -------------------------------------------------------------------
CLgsTarget& CLgsTarget::operator <<(char ch)
{
	if (m_bText)
		(*m_fsOutfile) << ch;
	else
		write((const char*) &ch, sizeof(char));
	return * this;
}
