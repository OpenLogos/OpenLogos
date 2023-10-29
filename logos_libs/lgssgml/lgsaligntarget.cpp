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
// LgsAlignTarget.cpp: implementation of the CLgsAlignTarget class.
//
//////////////////////////////////////////////////////////////////////
#include <logos_include/logoscommon.h>
#include <logos_libs/lgssgml/lgsutil.h>
#include <logos_libs/lgssgml/lgsaligntarget.h>

// -------------------------------------------------------------------
CLgsAlignTarget::CLgsAlignTarget(const LgsString& filename, int bufsize)
{
   m_inBuf = NULL;
	CLgsUtil::Assert(!filename.empty(), "Output filename not specfied");
   CLgsUtil::Assert(bufsize > 0, "Invalid Buffer size");
   m_bufSize = bufsize;

   //Open the target file for writing
#ifdef _MSC_VER
	m_outFile = new ofstream(filename.c_str(), ios::out);
#else //UNIX_STL
	m_outFile = new ofstream(filename.c_str(), ios::out);
#endif //_MSC_VER

   //Throw an exception on error!!
   LgsString Message = LgsString("Could not open the file ") + filename.c_str(); 
   CLgsUtil::Assert(m_outFile->good(), Message);
   m_inBuf = new char[m_bufSize];
   CLgsUtil::Assert(m_inBuf, "Memory allocation failed");
   m_bytesInBuf = 0;
}
// -------------------------------------------------------------------
CLgsAlignTarget::~CLgsAlignTarget()
{
	if (m_bytesInBuf > 0)
   {
		m_outFile->write(m_inBuf, m_bytesInBuf);
      m_outFile->flush();
   }
   if (m_outFile)
   {
	   m_outFile->close();
      delete m_outFile;
   }
   if (m_inBuf)
   {
      delete[] m_inBuf;
   }
}
// -------------------------------------------------------------------
void CLgsAlignTarget::write(const char *buf, int bufLength)
{
   if ((m_bytesInBuf + bufLength) > m_bufSize)
   {
      m_outFile->write(m_inBuf, m_bytesInBuf);
      m_outFile->flush();
      m_bytesInBuf = 0;
   }
   memcpy((m_inBuf + m_bytesInBuf), buf, bufLength);
   m_bytesInBuf += bufLength;
}
