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
// File - uourceuentenceunit.cpp
//
// Class - UnfoundSourceUnit (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/unfoundsourceunit.h>
#include <logos_libs/linguistic/swork.h>

//-------------------------------------------------------------------
UnfoundSourceUnit::UnfoundSourceUnit()
{
    // This is a default constructor. It should never be used. It is
    // required for STL vectors.
}
//-------------------------------------------------------------------
UnfoundSourceUnit::UnfoundSourceUnit( LDictionary& dictionary )
    : SourceSentenceUnit( dictionary )
{
}
//-------------------------------------------------------------------
UnfoundSourceUnit::UnfoundSourceUnit( const UnfoundSourceUnit& rhs )
    : SourceSentenceUnit( rhs )
{
}
//-------------------------------------------------------------------
UnfoundSourceUnit::UnfoundSourceUnit( LDictionary& dictionary,
                                      const SWork& swork )
    : SourceSentenceUnit( dictionary )
{
    position( swork.position() );
    setSentenceAddress( swork.SourcePosition() );

    setEndOfSentence( swork.isEndOfSentence() );
}
//-------------------------------------------------------------------
UnfoundSourceUnit::~UnfoundSourceUnit()
{
}
//-------------------------------------------------------------------
SentenceUnit::CaseState
UnfoundSourceUnit::caseState() const
{
    if (StringUtil::isAllUpperCase(surfaceExpressionAsString()))
    {
        return AllUpperCase;
    }
    return SentenceUnit::caseState();
}
//-------------------------------------------------------------------
int UnfoundSourceUnit::henum1()
{
    return -1;
}
//-------------------------------------------------------------------
int UnfoundSourceUnit::henum2()
{
    return -1;
}
//---------------------------------------------------------------------
void
UnfoundSourceUnit::persistOut( ostream& stream )
{
    SentenceUnit::persistOut( stream );
}
//-------------------------------------------------------------------
void
UnfoundSourceUnit::persistIn( istream& stream )
{
    SentenceUnit::persistIn( stream );
}
//---------------------------------------------------------------------
int
UnfoundSourceUnit::meaningID()
{
        return 0;
}
//---------------------------------------------------------------------
int
UnfoundSourceUnit::protectionCode()
{
        return 0;
}
//---------------------------------------------------------------------
int
UnfoundSourceUnit::hashCode1()
{
        return 0;
}
//---------------------------------------------------------------------
int
UnfoundSourceUnit::hashCode2()
{
        return 0;
}
//---------------------------------------------------------------------
int
UnfoundSourceUnit::blackHoleLocation()
{
        return 0;
}
//---------------------------------------------------------------------
int
UnfoundSourceUnit::hashLocation()
{
        return 0;
}
//---------------------------------------------------------------------
bool
UnfoundSourceUnit::isQuestionMark()
{
    return false;
}
//---------------------------------------------------------------------
void
UnfoundSourceUnit::makeBeginningOfQuestion()
{
}
//---------------------------------------------------------------------
inline int UnfoundSourceUnit::ssuCount ()
{
    return 1;
}
//---------------------------------------------------------------------
bool
UnfoundSourceUnit::isUnfoundWord() const
{
    return true;
}
