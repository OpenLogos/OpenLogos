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
//-------------------------------------------------------------------
// File - ParseInto.h
//
// template function - gParseInto
//
// generic function to parse a sequence into a vector of strings
//
//-------------------------------------------------------------------

#ifndef __ParseInto_h__
#define __ParseInto_h__

//-------------------------------------------------------------------
// generic parse algorithm
template<class InputIterator> void gParseInto(
    InputIterator begin,
    InputIterator end,
    LgsStringVector& strings,
    char delimiter = ' ',
    bool delimitersAreTokens = false)
{
    InputIterator beginToken = begin;    // iterator at beginning of a token
    while (beginToken != end)
    {
        // skip over leading delimiters
        InputIterator nextToken = find_if(beginToken, end,
#ifdef OS_NO_TYPEDEF_4
			bind2nd(not_equal_to<char>(), delimiter, (char*)0, (char*)0 )
#else
			bind2nd(not_equal_to<char>(), delimiter)
#endif
			);

        // delimeter group
        if (delimitersAreTokens && beginToken != nextToken)
		{
			//ObjectSpace does not support member template interface
			//strings.push_back(LgsString(beginToken, nextToken));
			LgsString sToken = "";
			for(InputIterator beginIter = beginToken; beginIter < nextToken; beginIter++) 
				sToken += *beginIter ;	
	        strings.push_back(sToken);
			
		}

        if (nextToken != end)
        {
            // nextToken points at the next non-delimiter
            beginToken = nextToken;
        }
        else
        {
            return;
        }

        // find the end of the token - the next delimiter or the end of the sequence
        InputIterator endToken = find(beginToken, end, delimiter);
        if (beginToken != endToken)
		{
			//ObjectSpace does not support member template interface
			//strings.push_back(LgsString(beginToken, endToken));
			LgsString sToken = "";
			for(InputIterator beginIter = beginToken; beginIter < endToken; beginIter++) 
				sToken += *beginIter ;	
	        strings.push_back(sToken);
		}
        beginToken = endToken;
    }
}

#endif



