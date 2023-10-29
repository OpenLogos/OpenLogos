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
#ifndef __LookupTokenType_h__
#define __LookupTokenType_h__

//-------------------------------------------------------------------
// File - LookupTokenType.h
//
// Class - LookupTokenType
//
//-------------------------------------------------------------------

//-------------------------------------------------------------------
// token-types are associated with Lwords - a token spans one or more whole words
// some words will not be recognized tokens - they will have tok_none as the token-type
// some tokens will consist of a single word - they will have a token-type > tok_continuation
//      and will not be followed by a word with token-type as tok_continuation
// some tokens will consist of a group of consecutive words - the first word will have a
//      token-type > tok_continuation.
//      subsequent words in the token will have tok_continuation as the token-type
class LookupTokenType
{
public:
    enum Type
    {
        tok_none,             // no token associated yet - token needs to be looked up
        tok_continuation,     // continuation - more than one word makes up the token
        tok_lookup,           // temporary value - token needs to be looked up in the
                              // dictionary as tok_none, but to prevent other token types
                              // from being assigned to the lookup this value is used

        // refer to document "alphanu1.xls" Name column for these (without the "tok_" prefix)
        tok_AdjAdv_Num_1st_E_Hyphen,
        tok_AdjAdv_Num_st_E_Hyphen,
        tok_AdjAdv_Num_nd_E_Hyphen,
        tok_AdjAdv_Num_rd_E_Hyphen,
        tok_AdjAdv_Num_th_E_Hyphen,
        tok_AdjAdv_Num_1st_E,
        tok_AdjAdv_Num_st_E,
        tok_AdjAdv_Num_nd_E,
        tok_AdjAdv_Num_rd_E,
        tok_AdjAdv_Num_th_E,
        tok_AdjNoun_fachem,
        tok_AdjNoun_fachen,
        tok_AdjNoun_facher,
        tok_AdjNoun_faches,
        tok_Adj_Num_1_G,
        tok_Adj_Num_21_G,
        tok_Adj_Num_2_G,
        tok_Adj_Num_3_G,
        tok_Adj_Num_4_G,
        tok_Adj_fach,
        tok_Adj_ig,
        tok_Adj_ige,
        tok_Adj_igen,
        tok_Adj_iger,
        tok_Adj_iges,
        tok_Adv_mal,
        tok_Appos_Date,
        tok_At_Sym,
        tok_Date,
        tok_Date_Range,
        tok_Deg_Sym,
        tok_Ellipsis,
        tok_Enumeration,
        tok_Equal_Sym,
        tok_FT_Sym,
        tok_IN_Sym,
        tok_Left_Angle,
        tok_Math_Express,
        tok_Math_Sym,
        tok_Minus_Sym,
        tok_Misc_Char,
        tok_Monetary_Sym,
        tok_No_04,
        tok_No_16,
        tok_No_16_04,
        tok_No_16_14,
        tok_No_Curr,
        tok_No_Curr_14,
        tok_No_Decimal,
        tok_No_Fraction,
        tok_No_Hyphen_0,
        tok_No_Hyphen_1,
        tok_No_Hyphen_2,
        tok_No_Hyphen_3_4,
        tok_No_Hyphen_5_31,
        tok_No_Hyphen_32_99,
        tok_No_Hyphen_100_999,
        tok_No_Hyphen_1000_1399,
        tok_No_Hyphen_1400_2100,
        tok_No_Hyphen_2101_,
        tok_No_Mixed,
        tok_No_Non_Curr_Unit,
        tok_No_Range_0,
        tok_No_Range_1,
        tok_No_Range_2,
        tok_No_Range_3_4,
        tok_No_Range_5_31,
        tok_No_Range_32_99,
        tok_No_Range_100_999,
        tok_No_Range_1000_1399,
        tok_No_Range_1400_2100,
        tok_No_Range_2101_,
        tok_Noun_er_G,
        tok_Noun_erin_G,
        tok_Noun_erinnen_G,
        tok_Noun_ern_G,
        tok_Noun_ers_G,
        tok_Noun_stel_G,
        tok_Noun_steln_G,
        tok_Noun_stels_G,
        tok_Outline_Notation,
        tok_PerC_Sym,
        tok_Pl_Date,
        tok_Plus_Sym,
        tok_Poss_Date,
        tok_Right_Angle,
        tok_Sym_Parens,
        tok_Temp_Scale,
        tok_TempC_Scale,
        tok_TempF_Scale,
        tok_Time,
        tok_Time_16,
        tok_Time_Noun,
        tok_Times_Sym,
        tok_Unfound_Alpha_Num,
        tok_Unfound_Agent,
        tok_Left_Hiphen,
        tok_Right_Hiphen,
		tok_no_pl,
		tok_Ampersand_Sym,
		tok_Left_doubledash,
		tok_Right_doubledash,
		tok_Doubledash,
		tok_No_Misc,
		tok_Ascending,
		tok_Descending,
      tok_No_HyphenatedFraction,
      tok_Unfound_Agent_Poss,
      tok_No_Post_Hyphen
    };

    int tok_Start();
    int tok_End();
    static LgsString tokenTypeDesc(Type tokenType);
};

inline int LookupTokenType::tok_Start()
{
    return tok_AdjAdv_Num_1st_E_Hyphen;
}

inline int LookupTokenType::tok_End()
{
    return tok_No_Post_Hyphen + 1;
}

#endif



