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
// APIParameter.cpp: implementation of the APIParameter class.
//
//////////////////////////////////////////////////////////////////////

#ifdef _MSC_VER
#include "StdAfx.h"
#endif
#include <engine_api/APITest/APIParameter.h>
#include <engine_api/APITest/Lgsout.h>

const char SEP = ';'; // a separator symbol
const char ACT = '#'; // an actual value symbol

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

APIParameter::APIParameter()
             :m_iCurValue(0)
{
}
// -------------------------------------------------------------------
APIParameter::~APIParameter()
{
}
// -------------------------------------------------------------------
bool APIParameter::Load( const string &buf )
{
   // Clear the name and delete all values
   m_sName.erase();
   m_vValues.clear();

   // Analyze the input line
   string line( buf );
   int pos = line.find( SEP );
   if( pos > 1 )  // input line should have at least one separator
   {
      // The name of parameter
      m_sName = line.substr( 0, pos );
      // Get all values
      while( pos != -1 ) 
      {
         line.erase( 0, pos+1 );
         pos = line.find( SEP );
         string value = ( pos != -1 )? line.substr( 0, pos ): line;
         // Add value that starts with ACT symbol
         if( !value.empty() && value[0] == ACT )
         {
            Add( value.substr( 1 ) );
         }
      }
   }

   // Set the first value as the current
   m_iCurValue = m_vValues.begin();

   // Success if parameter has a name and values
   return !Name().empty() && Count() > 0;
}
// -------------------------------------------------------------------
const string &APIParameter::Value() const
{
   static string empty = "";
   // Is this test correct? 
   //return ( m_iCurValue && m_iCurValue != m_vValues.end() )? *m_iCurValue: empty;
   return ( m_iCurValue != m_vValues.end() )? *m_iCurValue: empty;
}
// -------------------------------------------------------------------
void APIParameter::Print( CLgsOut &logFile ) const
{
   logFile << Name();
   for( APIValueConstIterator i = m_vValues.begin(); i != m_vValues.end(); i++ )
   {
      logFile  << SEP << *i;
   }
   logFile << "\n";
}
// -------------------------------------------------------------------
void APIParameter::PrintCur( CLgsOut &logFile ) const
{
   logFile << Name() << "=" << Value() << "\n";
}
