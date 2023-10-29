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
#ifdef _MSC_VER
#include "StdAfx.h"
#endif
#include <engine_api/APITest/Lgsout.h>

#include <fstream>

//**************************************************
// CLgsOut - Class implementation
// This class overrides a handful of << operator
// ie with basic type arguments like char, int 
// etc. Lot more needs to be added.
// At this time this version is more than sufficient
//**************************************************

CLgsOut::CLgsOut()
        :pLogFile(NULL) 
{
}
//---------------------------------------------------------------------
CLgsOut::~CLgsOut()
{
   if( pLogFile && pLogFile->is_open() )
   {
      pLogFile->close();
   }
   if( pLogFile )
   {
      delete pLogFile;
   }
}
//---------------------------------------------------------------------
void CLgsOut::setLogFile(const string &fileName) 
{
   m_fileName = fileName;
   if( !fileName.empty() )
   {
      pLogFile = new ofstream(fileName.c_str(), ios_base::out);
   }
}
//---------------------------------------------------------------------
bool CLgsOut::AppendFile(const string &fileName)
{
   bool res = false;
   ifstream input(fileName.c_str());
   if( input.is_open() )
   {
      res = true;
      if( pLogFile )
      {
         string str;
         while( input.good() && !input.eof() )
         {
            getline( input, str);
            *pLogFile << str << endl;
         }
         pLogFile->flush();
      }
      input.close();
   }
   return res;
}
//---------------------------------------------------------------------
CLgsOut &CLgsOut::operator<<(const char *s)
{
   cout << s;
   if( pLogFile )
   {
      *pLogFile << s;
      pLogFile->flush();
   }
   return *this;
}
//---------------------------------------------------------------------
CLgsOut &CLgsOut::operator<<(char c)
{
   cout << c;
   if( pLogFile )
   {
      *pLogFile << c;
      pLogFile->flush();
   }
   return *this;
}
//---------------------------------------------------------------------
CLgsOut &CLgsOut::operator<<(bool n)
{
   cout << n;
   if( pLogFile )
   {
      *pLogFile << n;
      pLogFile->flush();
   }
   return *this;
}
//---------------------------------------------------------------------
CLgsOut &CLgsOut::operator<<(short n)
{
   cout << n;
   if( pLogFile )
   {
      *pLogFile << n;
      pLogFile->flush();
   }
   return *this;
}
//---------------------------------------------------------------------
CLgsOut &CLgsOut::operator<<(unsigned short n)
{
   cout << n;
   if( pLogFile )
   {
      *pLogFile << n;
      pLogFile->flush();
   }
   return *this;
}
//---------------------------------------------------------------------
CLgsOut &CLgsOut::operator<<(int n)
{
   cout << n;
   if( pLogFile )
   {
      *pLogFile << n;
      pLogFile->flush();
   }
   return *this;
}
//---------------------------------------------------------------------
CLgsOut &CLgsOut::operator<<(unsigned int n)
{
   cout << n;
   if( pLogFile )
   {
      *pLogFile << n;
      pLogFile->flush();
   }
   return *this;
}
//---------------------------------------------------------------------
CLgsOut &CLgsOut::operator<<(long n)
{
   cout << n;
   if( pLogFile )
   {
      *pLogFile << n;
      pLogFile->flush();
   }
   return *this;
}
//---------------------------------------------------------------------
CLgsOut &CLgsOut::operator<<(unsigned long n)
{
   cout << n;
   if( pLogFile )
   {
      *pLogFile << n;
      pLogFile->flush();
   }
   return *this;
}
//---------------------------------------------------------------------
CLgsOut &CLgsOut::operator<<(float n)
{
   cout << n;
   if( pLogFile )
   {
      *pLogFile << n;
      pLogFile->flush();
   }
   return *this;
}
//---------------------------------------------------------------------
CLgsOut &CLgsOut::operator<<(double n)
{
   cout << n;
   if( pLogFile )
   {
      *pLogFile << n;
      pLogFile->flush();
   }
   return *this;
}
//---------------------------------------------------------------------
CLgsOut &CLgsOut::operator<<(long double n)
{
   cout << n;
   if( pLogFile )
   {
      *pLogFile << n;
      pLogFile->flush();
   }
   return *this;
}
//---------------------------------------------------------------------
CLgsOut &CLgsOut::operator<<(string &s) 
{
   cout << s;
   if( pLogFile )
   {
      *pLogFile << s;
      pLogFile->flush();
   }
   return *this;
}
//---------------------------------------------------------------------
CLgsOut &CLgsOut::operator<<(const string &s) 
{
   cout << s;
   if( pLogFile )
   {
      *pLogFile << s;
      pLogFile->flush();
   }
   return *this;
}
