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
//--------------------------------------------------------------------------------
// CEnvVariable.h header file 
//	Class CEnvVariable - Environment variable class - cannot be initialised without 
//	a Variable name.
//--------------------------------------------------------------------------------

#ifndef _CEnvVariable_H_
#define _CEnvVariable_H_

//#ifndef SHELL_MACROS
//#define SHELL_MACROS
//#define $(x) (x).GetValue()
//#define _EXPORT(x) (x).Export()
//#define _TRACE(x) (x).Dump(gLgsOut)
//#endif

//------------------------------------------------------------------------------
//  all environment variables are defined here. This is done to catch typo errors when 
//  creating  objects from variable name string.
//-------------------------------------------------------------------------------
#define LGS_ROOT "LGS_ROOT"
#define LGS_DEBUGGER "LGS_DEBUGGER"
//#define LGS_TIME_CMD "LGS_TIME_CMD"
#define CLASSPATH "CLASSPATH"
#define PATH "PATH"
//#define LGS_LOGQUERY "LGS_LOGQUERY"

class CEnvVariable; 
bool operator==(const CEnvVariable &op1, const CEnvVariable &op2);
bool operator==(const CEnvVariable &ev, const LgsString &Value);
bool operator==(const LgsString &Value, const CEnvVariable &ev);
bool operator==(const CEnvVariable &ev, const char *pszValue);
bool operator==(const char *pszValue, const CEnvVariable &ev);

class CEnvVariable  
{
public:
   CEnvVariable(const LgsString &Name);
   CEnvVariable(const char *pszName);
   CEnvVariable(const LgsString &Name, const LgsString &Value);
   CEnvVariable(const LgsString &Name, const char *pszValue);
   CEnvVariable(const char *pszName, const char *pszValue);
   CEnvVariable(const char *pszName, const LgsString &Value);
   virtual ~CEnvVariable();

   //static bool traceFlag(void);
   //static void setTraceFlag(bool value = true);
   void Dump(ostream &DumpStream) const;
   bool SaveProfile(const LgsString &FName, const char *pszKey);
   const LgsString &GetCurrent();
   LgsString Cut(unsigned nStart, unsigned nEnd);
   CEnvVariable operator=(const CEnvVariable &ev);
   CEnvVariable operator=(const LgsString &Value);
   CEnvVariable operator=(const char *pszValue);

   bool Export();
   bool Export(const LgsString &Value);

   const LgsString &GetValue() const;
   const LgsString &GetName() const;
   bool IsDefined() const;

   bool SetValue(const LgsString &Value);
   bool SetValue(const char *pszValue);

   bool ExportFromProfile(const LgsString &FName, const LgsString &Key, int nConvert = 1);
   bool SetValueFromProfile(const LgsString &FName, const LgsString &Key, int nConvert = 1);

   // overloaded operators as friend function
   friend CEnvVariable operator+(const CEnvVariable &op1, const CEnvVariable &op2);
   friend CEnvVariable operator+(const CEnvVariable &ev, const LgsString &Value);
   friend CEnvVariable operator+(const LgsString &Value, const CEnvVariable &ev);
   friend CEnvVariable operator+(const CEnvVariable &ev, const char *pszValue);
   friend CEnvVariable operator+(const char *pszValue, const CEnvVariable &ev);
   friend CEnvVariable operator+(const CEnvVariable &ev, char c);
   friend CEnvVariable operator+(char c, const CEnvVariable &ev);

   friend bool operator==(const CEnvVariable &op1, const CEnvVariable &op2);
   friend bool operator==(const CEnvVariable &ev, const LgsString &Value);
   friend bool operator==(const LgsString &Value, const CEnvVariable &ev);
   friend bool operator==(const CEnvVariable &ev, const char *pszValue);
   friend bool operator==(const char *pszValue, const CEnvVariable &ev);

   friend bool operator!=(const CEnvVariable &op1, const CEnvVariable &op2);
   friend bool operator!=(const CEnvVariable &ev, const LgsString &Value);
   friend bool operator!=(const LgsString &Value, const CEnvVariable &ev);
   friend bool operator!=(const CEnvVariable &ev, const char *pszValue);
   friend bool operator!=(const char *pszValue, const CEnvVariable &ev);

private:
   //static bool traceOn;
   CEnvVariable(); // Default constructor is private 
   bool GetEnvVariable();
   bool SetEnvVariable();

   LgsString m_Value;
   LgsString m_Name;
};
//---------------------------------------------------------------------
inline const LgsString &CEnvVariable::GetName() const
{
   return m_Name;
}
//---------------------------------------------------------------------
inline const LgsString &CEnvVariable::GetValue() const
{
   return m_Value;
}
//---------------------------------------------------------------------
inline bool CEnvVariable::SetValue(const LgsString &Value)
{
   m_Value = Value;
   return true;
}
//---------------------------------------------------------------------
inline bool CEnvVariable::SetValue(const char *pszValue)
{
   m_Value = pszValue;
   return true;
}
//---------------------------------------------------------------------
inline bool CEnvVariable::IsDefined() const
{
   return !m_Value.empty();
}
//---------------------------------------------------------------------
inline bool CEnvVariable::Export()
{
   return SetEnvVariable();
}
//---------------------------------------------------------------------
inline bool CEnvVariable::Export(const LgsString &Value)
{
   m_Value = Value;
   return SetEnvVariable();
}
//---------------------------------------------------------------------
inline const LgsString &CEnvVariable::GetCurrent()
{
   GetEnvVariable();
   return m_Value;
}
/*
inline bool CEnvVariable::traceFlag(void)
{
   return traceOn;
}
//---------------------------------------------------------------------
inline void CEnvVariable::setTraceFlag(bool value)
{
   traceOn = value;
}
*/
#endif // _CEnvVariable_H_
