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
// lgstokenizer.cpp: implementation of the LgsTokenizer class.
//
//////////////////////////////////////////////////////////////////////

#include "lgstokenizer.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

LgsTokenizer::LgsTokenizer()
{

}

LgsTokenizer::~LgsTokenizer()
{

}

// Name       : LgsTokenizer::Tokenize
//
// Description: Tokenizes the input string and populates the vector of unicode strings
//              with tokens.
//
// Parameters : oSource (Input): unicode character string to be 
//              tokenized.
//              oTokens (Output): Reference to vector which has to be
//              populated with tokens.
//              oDelimiters (Input): string of unicode characters which are delimiters
//              for tokenizing
//
// Returns    : None
//
// Notes      : 

void LgsTokenizer::Tokenize(const UString & oSource, UStringVector & oTokens, 
                            const UString & oDelimiters)
{
   UString::size_type stStart = 0;
   UString::size_type stEnd = 0;
   UString::size_type stTemp = 0;

   if (oSource.empty())
   {
        return;
   }
   do
   {
      stStart = oSource.find_first_not_of(L' ', stStart); 
      stEnd = oSource.find_first_of(oDelimiters, stStart);
      stTemp = stEnd;
      if(UString::npos == stEnd)
      {
         stEnd = oSource.length();
      }
      stEnd = oSource.find_last_not_of(L' ',stEnd - 1);

      UString::size_type sCount = (UString::npos == stEnd) ? stEnd : (stEnd - stStart) + 1 ;
      oTokens.push_back(oSource.substr(stStart, sCount));

      stStart = stTemp + 1; // Point stStart to the next character after
                           // stEnd (stEnd points to the delimiter)
   } while(UString::npos != stTemp);
}

//***************************************************************************
//                              
//
//
// Name       : LgsTokenizer::GetColumnString
//
// Description: Extracts the unicode string which corresponds to the.
//              column id specified as a parameter from the
//              list of character arrays which are seperated by
//              a delimiter. 
//
// Parameters : dwColumnId (Input): column number of the unicode string to extract.
//              oSource (Input): Source string with arrays of unicode characters
//              seperated by a certain delimiter..
//              oValue (Output): The extracted string will be returned
//              through this parameter.
//
// Returns    : None
//
// Notes      : 
//
//***************************************************************************
void LgsTokenizer::GetColumnString(int dwColumnId, const UString & oSource, 
                                   UString & oValue, const UString & oDelimiters) 
{
    UString::size_type stStart = 0;
    UString::size_type stEnd = 0;
    UString::size_type stTemp = 0;
    if (oSource.empty())
    {
        return;
    }

    dwColumnId++; 
    do
    {
        dwColumnId--;
        stStart = oSource.find_first_not_of(L' ', stStart); 
        stEnd = oSource.find_first_of(oDelimiters, stStart);
        stTemp = stEnd;

        if(UString::npos == stEnd)
        {
            stEnd = oSource.length();
        }

        stEnd = oSource.find_last_not_of(L' ',stEnd - 1);

        if (0 == dwColumnId)
        {
            break;
        }
        stStart = stTemp + 1; // Point stStart to the next character after
                           // stEnd (stEnd points to the delimiter)
    } while (UString::npos != stTemp);

    if (0 != dwColumnId) 
    {
        return;
    }

    std::string::size_type sCount = (UString::npos == stEnd) ? stEnd : (stEnd - stStart) + 1;
    oValue.erase();
    oValue = oSource.substr(stStart, sCount);
}
