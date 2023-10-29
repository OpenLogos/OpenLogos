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
// APIParameterList.cpp: implementation of the APIParameterList class.
//
//////////////////////////////////////////////////////////////////////

#ifdef _MSC_VER
#include "StdAfx.h"
#include <comdef.h>
#else
#include "logos_include/bstr_t.h"
#endif
#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <engine_api/APITest/APIParameterList.h>
#include <engine_api/APITest/Lgsout.h>
#include <engine_api/xlationinterface/xlationsession.h>
#include <cstring>

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

APIParameterList::APIParameterList()
                 :m_iCurParam(m_vParams.end())
{
}
// -------------------------------------------------------------------
APIParameterList::~APIParameterList()
{
}
// -------------------------------------------------------------------
bool APIParameterList::Load( const char *fileName )
{
   // Open file
   ifstream stream( fileName );
   if( !stream.good() )
   {   
      printf( "Error open file \"%s\"\n", fileName );
      return false;
   }

   // Delete all parameters
   m_vParams.clear();

   // Read file
   const int BUF_LEN = 10240;
   char buf[BUF_LEN];
   string line;
   while( !stream.bad() && !stream.eof() )
   {
      // Read line
      stream.getline( buf, BUF_LEN );
      int len = strlen( buf );
      if( (len > 0) && (buf[len-1] == '\\') )
      {
         // Got continuation symbol '\'
         buf[len-1] = '\0';
         line+= buf;
      }
      else
      {
         line+= buf;
         // Construct parameter
         APIParameter param;
         if( param.Load( line ) )
         {
            // Add into list
            m_vParams.push_back( param );
         }
         // Erase line
         line.erase();
      }
   }

   // Close file
   stream.close();

   // Set the first parameter as a current
   m_iCurParam = m_vParams.begin();

   return true;
}
// -------------------------------------------------------------------
void APIParameterList::ResetCombination()
{
   // Reset all parameters
   for( APIParameterIterator i = m_vParams.begin(); i != m_vParams.end(); i++ )
   {
      i->ResetCombination();
   }
   // Set the first parameter as a current
   m_iCurParam = m_vParams.begin();
}
// -------------------------------------------------------------------
void APIParameterList::Print( CLgsOut &logFile ) const
{
   // Print all values for each parameter
   for( APIParameterConstIterator i = m_vParams.begin(); i != m_vParams.end(); i++ )
   {
      i->Print( logFile );
   }
   logFile.flush();
}
// -------------------------------------------------------------------
void APIParameterList::PrintCur( CLgsOut &logFile ) const
{
   // Print the current value for each parameter
   for( APIParameterConstIterator i = m_vParams.begin(); i != m_vParams.end(); i++ )
   {
      i->PrintCur( logFile );
   }
   logFile.flush();
}
// -------------------------------------------------------------------
void APIParameterList::PrintCurRow( ofstream &stream ) const
{
   // Print the current value for each parameter
   for( APIParameterConstIterator i = m_vParams.begin(); i != m_vParams.end(); i++ )
   {
      string value = i->Value();
      if( i->Name() == "SOURCE_LOCALE" || i->Name() == "TARGET_LOCALE" )
      {
         value = '\"' + value + '\"';
      }

      stream << "," << value;
   }
}
// -------------------------------------------------------------------
bool APIParameterList::SetNextCombination()
{
  if( m_iCurParam != m_vParams.end() )
  {
      // Find the next combination starting from the current parameter
      for( APIParameterIterator i = m_iCurParam; i != m_vParams.end(); i++ )
      {
         if( i->SetNextCombination() )
         {
            // Reset all previous parameters
            for( APIParameterIterator j = m_vParams.begin(); j != i; j++ )
            {
               j->ResetCombination();
            }
            // Set the first parameter as a current
            m_iCurParam = m_vParams.begin();
            return true;
         }
      }
  }

   // There is no more combinations
   return false;
}
// -------------------------------------------------------------------
double APIParameterList::TotalCombinations() const
{
   double total = 0.;
   for( APIParameterConstIterator i = m_vParams.begin(); i != m_vParams.end(); i++ )
   {
      if( i == m_vParams.begin() )
      {
         total = (double) i->Count();
      }
      else
      {
         total *= (double) i->Count();
      }
   }

   return total;
}
// -------------------------------------------------------------------
const string &APIParameterList::GetParamValue( const string &name ) const
{
   static string empty = "";

   for( APIParameterConstIterator i = m_vParams.begin(); i != m_vParams.end(); i++ )
   {
      if( i->Name() == name )
      {
         return i->Value(); // parameter is found
      }
   }

   return empty; // parameter is not found
}
// -------------------------------------------------------------------
void APIParameterList::SetInputParameters( XlationSession *pSession ) const
{
   // Use the current value of each parameter
   // to set input parameter in the session
   for( APIParameterConstIterator i = m_vParams.begin(); i != m_vParams.end(); i++ )
   {
      const string &name = i->Name();
      const string &value = i->Value();

      if( name == "OPERATION_TYPE" )
      {
         // Set type of operation
         if( value == "TRANSLATE_DOC" )
         {
            pSession->setOperationType( TRANSLATE_DOC );
         }
         else if( value == "TRANSLATE_TEXT" )
         {
            pSession->setOperationType( TRANSLATE_TEXT );
         }
         else if( value == "TERM_SEARCH_DOC" )
         {
            pSession->setOperationType( TERM_SEARCH_DOC );
         }
         else if( value == "TERM_SEARCH_TEXT" )
         {
            pSession->setOperationType( TERM_SEARCH_TEXT );
         }
         else if( value == "QRY_CONFIGURATION" )
         {
            pSession->setOperationType( QRY_CONFIGURATION );
         }
      }
      else {
        // Convert to _bstr_t
	_bstr_t bstr_name( name.c_str() );
	_bstr_t bstr_value( value.c_str() );
        // Set input parameter
        pSession->setInputParameter( bstr_name, bstr_value );
      }
   }
}
