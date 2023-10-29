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
#ifndef __CLGSOUT_H__
#define __CLGSOUT_H__

//*************************************************
// CLgsOut - Class definition
// used as a surrogate for cout ostream object
// This class exactly the same as tee command in Unix
//**************************************************

#include<string>
#include<iostream>

using namespace std;

class CLgsOut 
{
public:
   CLgsOut(); //default constructor
   ~CLgsOut(); //destructor
   
   void setLogFile(const string &fileName);
   const string &getFileName() const;
   bool AppendFile(const string &fileName);
   
   CLgsOut &operator<<(string &s);
   CLgsOut &operator<<(const string &s);
   CLgsOut &operator<<(const char *s);
   CLgsOut &operator<<(char c); 
   CLgsOut &operator<<(bool n);
   CLgsOut &operator<<(short n);
   CLgsOut &operator<<(unsigned short n);
   CLgsOut &operator<<(int n);
   CLgsOut &operator<<(unsigned int n);
   CLgsOut &operator<<(long n);
   CLgsOut &operator<<(unsigned long n);
   CLgsOut &operator<<(float n);    
   CLgsOut &operator<<(double n);
   CLgsOut &operator<<(long double n);
   
   void flush() { cout.flush(); }  //dummy method
   
private:
   string m_fileName;
   ofstream *pLogFile;
};
//---------------------------------------------------------------------
inline const string &CLgsOut::getFileName() const
{
   return m_fileName;
}

#endif //__CLGSOUT_H__
