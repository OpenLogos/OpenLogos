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
#ifndef __StartLocale_h__
#define __StartLocale_h__

//----------------------------------------------------------------------------
// File - Locale.h
//
// Class - ST_Locale
//
// Description - A structure to hold locale specific information
//
//----------------------------------------------------------------------------

#include <logos_libs/linguistic/llanguage.h>

// represents locale information
struct ST_Locale
{
    enum DateFormat { mdy, dmy };
    // enum Country  {cnUSA,cnUK,  cnGer,cnSwi,  cnFra,  cnIta,  cnSpa,cnArg,cnMex};

    // key fields
    LLanguage::ID  language_;              // language
    // Country      country_;              // country

    char         dec_sep_;              // decimal separator
    char         thou_sep_;             // thousands separator
    char         date_sep_;             // date separator
    char         time_sep_;             // time separator
    char         time_dec_sep_;         // time decimal separator

    DateFormat   dateFormat_;           // date format
    LgsString       ord_suffix_;           // ordinal suffix values eg st
    LgsString       time_suffix_;          // time suffix values eg AM

    ST_Locale(
        LLanguage::ID language, /* Country country, */
        char dec_sep, char thou_sep, char date_sep, char time_sep, char time_dec_sep,
        DateFormat dateFormat, const LgsString& ord_suffix, const LgsString& time_suffix);
};
const int ST_Locale_count = 7;

inline ST_Locale::ST_Locale(
    LLanguage::ID language, /* Country country, */
    char dec_sep, char thou_sep, char date_sep, char time_sep, char time_dec_sep,
    DateFormat dateFormat, const LgsString& ord_suffix, const LgsString& time_suffix)
    : language_(language)
    // , country_(country)
    , dec_sep_(dec_sep)
    , thou_sep_(thou_sep)
    , date_sep_(date_sep)
    , time_sep_(time_sep)
    , time_dec_sep_(time_dec_sep)
    , dateFormat_(dateFormat)
    , ord_suffix_(ord_suffix)
    , time_suffix_(time_suffix)
{
}

#endif



