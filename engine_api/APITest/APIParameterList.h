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
// APIParameterList.h: interface for the APIParameterList class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _APIParameterList_H_
#define _APIParameterList_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <engine_api/APITest/APIParameter.h>

class CLgsOut;
class XlationSession;

typedef vector<APIParameter> APIParameterVector;
typedef APIParameterVector::iterator APIParameterIterator;
typedef APIParameterVector::const_iterator APIParameterConstIterator;

class APIParameterList
{
public:
   APIParameterList();
   virtual ~APIParameterList();

   bool Load( const char *fileName ); // load file with all parameters

   // Getting functions
   int Count() const; // number of parameters
   
   void Print( CLgsOut &logFile ) const;       // print all values of parameters
   void PrintCur( CLgsOut &logFile ) const;    // print the current values of parameters
   void PrintCurRow( ofstream &stream ) const; // print the current values of parameters in one row
   double TotalCombinations() const;           // calculate number of all combinations

   void ResetCombination();           // start creating combinations
   bool SetNextCombination();         // generate the next combination

   // Get the current value of the parameter
   const string &GetParamValue( const string &name ) const;
   // Set input parameter in the session using the current value of parameters
   void SetInputParameters( XlationSession *pSession ) const;

protected:
   APIParameterVector m_vParams; // vector of all parameters

private:
   APIParameterIterator m_iCurParam; // the current parameter to generate the next combination
};
// -------------------------------------------------------------------
inline int APIParameterList::Count() const
{
   return m_vParams.size();
}

#endif // _APIParameterList_H_
