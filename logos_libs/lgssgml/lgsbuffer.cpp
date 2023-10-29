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
// LgsBuffer.cpp: implementation of the CLgsBuffer class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>

#include <logos_libs/lgssgml/lgsutil.h>
#include <logos_libs/lgssgml/lgsbuffer.h>

// -------------------------------------------------------------------
CLgsBuffer::CLgsBuffer(const LgsString& filename, int bufsize)
{
   m_inBuf = NULL;
  
   CLgsUtil::Assert(bufsize > 0, "Invalid Buffer size");
   m_bufSize = bufsize;

#if defined(_MSC_VER)
   m_in = new ifstream(filename.c_str(), ios::in | ios::binary);
#else
   m_in = new ifstream(filename.c_str(), ios::in);
#endif
   LgsString Message = LgsString("Could not open the file ") + filename.c_str(); 
   CLgsUtil::Assert(m_in->good(), Message);

   m_inBuf = new char[m_bufSize];
   CLgsUtil::Assert(m_inBuf != NULL, "Memory allocation failed");
   m_bytesInBuf = 0;
   m_ix = 0;
   m_nIndex = 0;
}
// -------------------------------------------------------------------
CLgsBuffer::~CLgsBuffer()
{
   if (m_in)
   {
      m_in->close();
      delete m_in;
   }
   if (m_inBuf)
   {
      delete[] m_inBuf;
   }
}
// -------------------------------------------------------------------
bool CLgsBuffer::GetNextChar(char *c)
{
   bool result = false;

   if (m_ix == m_bytesInBuf)
   {
	   m_in->read(m_inBuf, m_bufSize);
	   m_bytesInBuf = m_in->gcount();
	   m_ix = 0;
      m_nIndex = 0 ;
	   if (0 == m_bytesInBuf)
      {
         return false;
      }
   }
   unsigned char uch = m_inBuf[m_ix++];
   //Swap 0x91 & 0x92 with 0x27(single quotes)
   if ((uch == 0x91) || (uch == 0x92))
      *c = 0x27 ;
   //Swap 0x93 & 0x94 with 0x22(double quotes)
   else if ((uch == 0x93) || (uch == 0x94))
      *c = 0x22;
   else
      *c = uch;
   return true;
}
// -------------------------------------------------------------------
void CLgsBuffer::UnGetLastChar()
{
	if (m_ix > 0)
      m_ix--;
}
// -------------------------------------------------------------------
char CLgsBuffer::LookAhead()
{
   if (m_nIndex < m_ix)
      m_nIndex = m_ix;
   if (m_nIndex >= m_bytesInBuf)
      return 0;
   else
      return m_inBuf[m_nIndex++];
}
