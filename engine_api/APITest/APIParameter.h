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
// APIParameter.h: interface for the APIParameter class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _APIParameter_H_
#define _APIParameter_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CLgsOut;

#include <vector>
#include <string>
#include <iostream>
#include <fstream>

using namespace std;

typedef vector<string> APIValueVector;
typedef APIValueVector::iterator APIValueIterator;
typedef APIValueVector::const_iterator APIValueConstIterator;

class APIParameter
{
public:
   APIParameter();
   virtual ~APIParameter();

   bool Load( const string &buf ); // load info about parameter and values

   // Getting functions
   const string &Name() const;  // get the name of the parameter
   const string &Value() const; // get the current parameter value
   int Count() const;           // number of values

   void Print( CLgsOut &logFile ) const;
   void PrintCur( CLgsOut &logFile ) const;
   
   void ResetCombination();   // set the first value as a current
   bool SetNextCombination(); // set the next value as a current

protected:
   void Add( const string &value ); // add value

   string m_sName;           // the name of the parameter
   APIValueVector m_vValues; // vector of all parameter values

private:
   APIValueIterator m_iCurValue; // the current value of the parameter
};
// -------------------------------------------------------------------
inline const string &APIParameter::Name() const
{
   return m_sName;
}
// -------------------------------------------------------------------
inline int APIParameter::Count() const
{
   return m_vValues.size();
}
// -------------------------------------------------------------------
inline void APIParameter::Add( const string &value )
{
   m_vValues.push_back( value );
}
// -------------------------------------------------------------------
inline void APIParameter::ResetCombination()
{
   // Set the first value as a current
   m_iCurValue = m_vValues.begin();
}
// -------------------------------------------------------------------
inline bool APIParameter::SetNextCombination()
{
  // illegal test (not boolean)
  if( m_iCurValue != m_vValues.end() && m_iCurValue+1 != m_vValues.end() )
  //if( m_iCurValue+1 != m_vValues.end() )
   {
      // This is not the last value
      m_iCurValue++;
      return true;
   }
   else
   {
      // There is no more values
      return false;
   }
}

#endif // _APIParameter_H_
