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
// QueryToDictionary.cpp: implementation of the QueryToDictionary class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <lgs_db_io/querytodictionary.h>

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

QueryToDictionary *QueryToDictionary::m_pQueryToDictionary = NULL;
//--------------------------------------------------------------------
QueryToDictionary::QueryToDictionary()
                  :m_pStream(NULL)
{
   Open();
}
//--------------------------------------------------------------------
QueryToDictionary::~QueryToDictionary()
{
   Close();
}
//--------------------------------------------------------------------
void QueryToDictionary::DestroyObject()
{
   if (m_pQueryToDictionary)
   {
      delete m_pQueryToDictionary;
      m_pQueryToDictionary = 0;
   }
}
//--------------------------------------------------------------------
QueryToDictionary *QueryToDictionary::GetObject()
{
   if (!m_pQueryToDictionary)
   {
      m_pQueryToDictionary = new QueryToDictionary;
   }
   return m_pQueryToDictionary;
}
//--------------------------------------------------------------------
void QueryToDictionary::Open()
{
   // Check logQuery flag
   if( LgsDBCommonObjects::GetJobControlArguments().LogQueryFlag() )
   {
      char strFileName[512];
      sprintf(strFileName, "%s%c%d.search",
              LgsDBCommonObjects::GetServerProperties().ScratchFileDirectory().c_str(),
              DIR_SEP_CHAR,
              LgsDBCommonObjects::GetJobControlArguments().JobID());

      m_pStream = new ofstream(strFileName);
   }
}
//--------------------------------------------------------------------
void QueryToDictionary::Print(const char *str)
{
   if (m_pStream)
   {
      *m_pStream << str << endl << flush;
   }
}
//--------------------------------------------------------------------
void QueryToDictionary::PrintWord(const char *str)
{
   if (m_pStream)
   {
      *m_pStream << "word \t" << str << endl << flush;
   }
}
//--------------------------------------------------------------------
void QueryToDictionary::PrintWordCap(const char *str1, const char *str2)
{
   if (m_pStream)
   {
      *m_pStream << "wordcap \t" << str1 << " " << str2 << endl << flush;
   }
}
//--------------------------------------------------------------------
void QueryToDictionary::PrintPhrase(const char *str)
{
   if (m_pStream)
   {
      *m_pStream << "phrase \t" << str << endl << flush;
   }
}
//--------------------------------------------------------------------
void QueryToDictionary::Close()
{
   if (m_pStream)
   {
      *m_pStream << flush;
      delete m_pStream;
      m_pStream = 0;
   }
}
