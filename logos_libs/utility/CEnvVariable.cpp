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
//*************************************************************
// CEnvVariable.cpp: implementation of the CEnvVariable class.
//*************************************************************
#include <logos_include/logoscommon.h>
#include <logos_libs/utility/CEnvVariable.h>
#include <logos_libs/utility/stringutil.h>

#ifndef _MSC_VER
#include "logos_include/writeprivateprofilestring.h"
#endif
//global variable
//bool CEnvVariable::traceOn = false;

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
CEnvVariable::CEnvVariable()
{
// Default constructor - cannot be initialises due to private:
}
//---------------------------------------------------------------------
CEnvVariable::CEnvVariable(const LgsString &Name)
             :m_Name(Name)
{
   GetEnvVariable();
}
//---------------------------------------------------------------------
CEnvVariable::CEnvVariable(const char *pszName)
             :m_Name(pszName)
{
   GetEnvVariable();
}
//---------------------------------------------------------------------
CEnvVariable::CEnvVariable(const LgsString &Name, const LgsString &Value)
             :m_Name(Name),
              m_Value(Value)
{
}
//---------------------------------------------------------------------
CEnvVariable::CEnvVariable(const LgsString &Name, const char *pszValue)
             :m_Name(Name),
              m_Value(pszValue)
{
}
//---------------------------------------------------------------------
CEnvVariable::CEnvVariable(const char *pszName, const char *pszValue)
             :m_Name(pszName),
              m_Value(pszValue)
{
}
//---------------------------------------------------------------------
CEnvVariable::CEnvVariable(const char *pszName, const LgsString &Value)
             :m_Name(pszName),
              m_Value(Value)
{
}
//---------------------------------------------------------------------
CEnvVariable::~CEnvVariable()
{
}
//---------------------------------------------------------------------
bool CEnvVariable::SetEnvVariable()
{
   LgsString TempStr = m_Name + "=" + m_Value;
   char *tmp = new char[TempStr.size() + 1];
   strcpy(tmp, TempStr.c_str());
   bool res = (putenv(tmp) == 0);
   delete[] tmp;
   if (res)
   {
      /*
      if (traceOn)
      {
         Dump(cout);
      }
      */
      return true;
   }
   else
      return false;
}
//---------------------------------------------------------------------
bool CEnvVariable::GetEnvVariable()
{
   m_Value.erase();
   char *temp = getenv(m_Name.c_str());
   if (temp)
   {
      m_Value = temp;
   }
   return true;
}
//---------------------------------------------------------------------
bool CEnvVariable::ExportFromProfile(const LgsString &FName, const LgsString &Key, int nConvert)
{
   char cBuf[256];
   cBuf[0] = '\0';
   int retval = GetPrivateProfileString("settings", Key.c_str(), "", cBuf, 256, FName.c_str());
   if (retval == 0)
   {
      /*
      if (traceOn)
      {
         cout << "Error reading from profile file : "<< FName << "  key = " << Key << "\n";
      }
      */
      return false;
   }
   m_Value = cBuf;
   switch(nConvert)
   {
   case 0: // no conversion
      break;
   case 1: // to lower
      StringUtil::toLower(m_Value);
      break;
   case 2: // to upper
      StringUtil::toUpper(m_Value);
      break;
   default:
      return false;
      break;
   }
   return SetEnvVariable();
}
//---------------------------------------------------------------------
bool CEnvVariable::SetValueFromProfile(const LgsString &FName, const LgsString &Key, int nConvert)
{
   char cBuf[256];
   cBuf[0] = '\0';
   int retval = GetPrivateProfileString("settings", Key.c_str(), "", cBuf, 256, FName.c_str());
   if (retval == 0)
   {
      /*
      if (traceOn)
      {
         cout << "Error reading from profile file : "<< FName << "  key = " << Key << "\n";
      }
      */
      return false;
   }
   m_Value = cBuf;
   switch(nConvert)
   {
   case 0: // no conversion
      break;
   case 1: // to lower
      StringUtil::toLower(m_Value);
      break;
   case 2: // to upper
      StringUtil::toUpper(m_Value);
      break;
   default:
      return false;
      break;
   }
   return true;
}
//---------------------------------------------------------------------
LgsString CEnvVariable::Cut(unsigned nStart, unsigned nEnd)
{
   LgsString Temp;
   int nFrom;
   int nCount;
   int nSize = m_Value.length();

   if ((nEnd < nStart) || (nStart > nSize))
   {
      return Temp;
   }

   nFrom = nStart - 1; // convert to zero based index

   if (nEnd > nSize)nEnd = nSize;
      nCount = nEnd - nStart + 1;

   return m_Value.substr(nFrom, nCount); 
}
//---------------------------------------------------------------------
//overloaded operators.
CEnvVariable CEnvVariable::operator=(const char *pszValue)
{
   m_Value = pszValue;
   return *this;
}
//---------------------------------------------------------------------
CEnvVariable CEnvVariable::operator=(const LgsString &Value)
{
   m_Value = Value;
   return *this;
}
//---------------------------------------------------------------------
CEnvVariable CEnvVariable::operator=(const CEnvVariable &ev)
{
   m_Value = ev.GetValue();
   return *this;
}
//---------------------------------------------------------------------
//overloaded operators as friend function
CEnvVariable operator+(const CEnvVariable &op1, const CEnvVariable &op2)
{
   LgsString tempstr = op1.GetValue() + op2.GetValue();
   CEnvVariable tempev("TempEV", tempstr);
   return tempev;
}
//---------------------------------------------------------------------
CEnvVariable operator+(const CEnvVariable &ev, const LgsString &Value)
{
   LgsString tempstr = ev.GetValue() + Value;
   CEnvVariable tempev("TempEV", tempstr);
   return tempev;
}
//---------------------------------------------------------------------
CEnvVariable operator+(const LgsString &Value, const CEnvVariable &ev)
{
   LgsString tempstr = Value + ev.GetValue();
   CEnvVariable tempev("TempEV",tempstr);
   return tempev;
}
//---------------------------------------------------------------------
CEnvVariable operator+(const CEnvVariable &ev, const char *pszValue)
{
   LgsString tempstr = pszValue;
   tempstr =  ev.GetValue() + tempstr;
   CEnvVariable tempev("TempEV", tempstr);
   return tempev;
}
//---------------------------------------------------------------------
CEnvVariable operator+(const char *pszValue, const CEnvVariable &ev)
{
   LgsString tempstr = pszValue;
   tempstr = tempstr +   ev.GetValue();
   CEnvVariable tempev("TempEV", tempstr);
   return tempev;
}
//---------------------------------------------------------------------
CEnvVariable operator+(const CEnvVariable &ev, char c)
{
   LgsString tempstr = ev.GetValue() + c;
   CEnvVariable tempev("TempEV", tempstr);
   return tempev;
}
//---------------------------------------------------------------------
CEnvVariable operator+(char c, const CEnvVariable &ev)
{
   LgsString tempstr = c + ev.GetValue();
   CEnvVariable tempev("TempEV", tempstr);
   return tempev;
}
//---------------------------------------------------------------------
bool operator==(const CEnvVariable &op1, const CEnvVariable &op2)
{
   return (op1.GetValue() == op2.GetValue());
}	
//---------------------------------------------------------------------
bool operator==(const CEnvVariable &ev, const LgsString &Value)
{
   return (ev.GetValue() == Value);
}
//---------------------------------------------------------------------
bool operator==(const LgsString &Value, const CEnvVariable &ev)
{
   return (ev.GetValue() == Value);
}
//---------------------------------------------------------------------
bool operator==(const CEnvVariable &ev, const char *pszValue)
{
   return (ev.GetValue() == pszValue);
}
//---------------------------------------------------------------------
bool operator==(const char *pszValue, const CEnvVariable &ev)
{
   return (ev.GetValue() == pszValue);
}
//---------------------------------------------------------------------
bool operator!=(const CEnvVariable &op1, const CEnvVariable &op2)
{
   return (op1.GetValue() != op2.GetValue());
}
//---------------------------------------------------------------------
bool operator!=(const CEnvVariable &ev, const LgsString &Value)
{
   return (ev.GetValue() != Value);
}
//---------------------------------------------------------------------
bool operator!=(const LgsString &Value, const CEnvVariable &ev)
{
   return (ev.GetValue() != Value);
}
//---------------------------------------------------------------------
bool operator!=(const CEnvVariable &ev, const char *pszValue)
{
   return (ev.GetValue() != pszValue);
}
//---------------------------------------------------------------------
bool operator!=(const char *pszValue, const CEnvVariable &ev)
{
   return (ev.GetValue() != pszValue);
}
//---------------------------------------------------------------------
bool CEnvVariable::SaveProfile(const LgsString &FName, const char *pszKey)
{
   if (!WritePrivateProfileString("settings", pszKey, m_Value.c_str(), FName.c_str()))
      return false;
   else
      return true;
}
//---------------------------------------------------------------------
void CEnvVariable::Dump(ostream& DumpStream) const
{
   DumpStream << m_Name << " = " << m_Value << "\n";
}
