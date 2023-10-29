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
//----------------------------------------------------------------------------
// File - EntryMap.cpp
//
// Class - ST_EntryMap
//
//----------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/startrules/entrymap.h>

//       WC, SS, Set, Sub, Fm, OFL2B, OFL3B, Gender
ST_EntryMap::DictionaryEntrySubset ST_EntryMap::entrySubset_[] =
{
    {
        LookupTokenType::tok_AdjAdv_Num_1st_E, LLanguage::EnglishID,
        {
            { 4, 13,  20, 455, 23},
            { 3, 14,  14,  14,  1}
        }
    },
    {
        LookupTokenType::tok_AdjAdv_Num_st_E, LLanguage::EnglishID,
        {
            { 4, 13,  21, 455, 23},
            { 3, 14,  14,  14,  1}
        }
    },
    {
        LookupTokenType::tok_AdjAdv_Num_nd_E, LLanguage::EnglishID,
        {
            { 4, 13,  22, 455, 23},
            { 3, 14,  14,  14,  1}
        }
    },
    {
        LookupTokenType::tok_AdjAdv_Num_rd_E, LLanguage::EnglishID,
        {
            { 4, 13,  23, 455, 23},
            { 3, 14,  14,  14,  1}
        }
    },
    {
        LookupTokenType::tok_AdjAdv_Num_th_E, LLanguage::EnglishID,
        {
            { 4, 13,  24, 455, 23},
            { 3, 14,  14,  14,  1}
        }
    },
    {
        LookupTokenType::tok_AdjAdv_Num_1st_E_Hyphen, LLanguage::EnglishID,
        {
            { 4, 13,  20, 455, 6},
            { 3, 14,  14,  14,  6}
        }
    },
    {
        LookupTokenType::tok_AdjAdv_Num_st_E_Hyphen, LLanguage::EnglishID,
        {
            { 4, 13,  21, 455, 6},
            { 3, 14,  14,  14,  6}
        }
    },
    {
        LookupTokenType::tok_AdjAdv_Num_nd_E_Hyphen, LLanguage::EnglishID,
        {
            { 4, 13,  22, 455, 6},
            { 3, 14,  14,  14,  6}
        }
    },
    {
        LookupTokenType::tok_AdjAdv_Num_rd_E_Hyphen, LLanguage::EnglishID,
        {
            { 4, 13,  23, 455, 6},
            { 3, 14,  14,  14,  6}
        }
    },
    {
        LookupTokenType::tok_AdjAdv_Num_th_E_Hyphen, LLanguage::EnglishID,
        {
            { 4, 13,  24, 455, 6},
            { 3, 14,  14,  14,  6}
        }
    },
    {
        LookupTokenType::tok_AdjNoun_fachem, LLanguage::GermanID,
        {
            { 4,  13,  80, 306,  5},
            { 1,  13,  95, 306,  5}
        }
    },
    {
        LookupTokenType::tok_AdjNoun_fachen, LLanguage::GermanID,
        {
            { 4,  13,  80, 306,  6},
            { 1,  13,  95, 306,  6}
        }
    },
    {
        LookupTokenType::tok_AdjNoun_facher, LLanguage::GermanID,
        {
            { 4,  13,  80, 306,  1},
            { 1,  13,  95, 306,  1}
        }
    },
    {
        LookupTokenType::tok_AdjNoun_faches, LLanguage::GermanID,
        {
            { 4,  13,  80, 306,  3},
            { 1,  13,  95, 306,  3}
        }
    },
    {
        LookupTokenType::tok_Adj_Num_1_G, LLanguage::GermanID,
        {
            { 4, 13,  20, 455,  9, 9, 5}
        }
    },
    {
        LookupTokenType::tok_Adj_Num_21_G, LLanguage::GermanID,
        {
            { 4, 13,  21, 455,  9, 9, 5}
        }
    },
    {
        LookupTokenType::tok_Adj_Num_2_G, LLanguage::GermanID,
        {
            { 4, 13,  22, 455,  9, 9, 5}
        }
    },
    {
        LookupTokenType::tok_Adj_Num_3_G, LLanguage::GermanID,
        {
            { 4, 13,  23, 455,  9, 9, 5}
        }
    },
    {
        LookupTokenType::tok_Adj_Num_4_G, LLanguage::GermanID,
        {
            { 4, 13,  24, 455,  9, 9, 5}
        }
    },
    {
        LookupTokenType::tok_Adj_fach, LLanguage::GermanID,
        {
            { 4,  13,  80, 306,  7}
        }
    },
    {
        LookupTokenType::tok_Adj_ig, LLanguage::GermanID,
        {
            { 4,  13,  80, 306,  7}
        }
    },
    {
        LookupTokenType::tok_Adj_ige, LLanguage::GermanID,
        {
            { 4,  13,  80, 306,  2}
        }
    },
    {
        LookupTokenType::tok_Adj_igen, LLanguage::GermanID,
        {
            { 4,  13,  80, 306,  6}
        }
    },
    {
        LookupTokenType::tok_Adj_iger, LLanguage::GermanID,
        {
            { 4,  13,  80, 306,  1}
        }
    },
    {
        LookupTokenType::tok_Adj_iges, LLanguage::GermanID,
        {
            { 4,  13,  80, 306,  3}
        }
    },
    {
        LookupTokenType::tok_Adv_mal, LLanguage::GermanID,
        {
            { 3,  15,  15, 156,  1}
        }
    },
    {
        LookupTokenType::tok_Appos_Date, LLanguage::EnglishID,
        {
            {16,  4,  90,  90,  1, 9, 9}
        }
    },
    {
        LookupTokenType::tok_Appos_Date, LLanguage::GermanID,
        {
            {16,  4,  90,  90,  1, 9, 5}
        }
    },
    {
        LookupTokenType::tok_At_Sym, LLanguage::EnglishID,
        {
            {13, 25,  25, 566, 15}
        }
    },
    {
        LookupTokenType::tok_At_Sym, LLanguage::GermanID,
        {
            {13, 25,  25, 566, 15}
        }
    },
    {
        LookupTokenType::tok_Date, LLanguage::EnglishID,
        {
            {16,  4,  10, 178,  1, 9, 9}
        }
    },
    {
        LookupTokenType::tok_Date, LLanguage::GermanID,
        {
            {16,  4,  10, 178,  1, 9, 5}
        }
    },
    {
        LookupTokenType::tok_Date_Range, LLanguage::EnglishID,
        {
            {16,  4,  52, 681,  1, 9, 9},
            { 4, 13,  52, 681,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_Date_Range, LLanguage::GermanID,
        {
            {16,  4,  52, 681,  1, 9, 5},
            { 4, 13,  52, 681,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_Deg_Sym, LLanguage::EnglishID,
        {
            { 1,  8,  61, 617, 10, 0, 0, 1}
        }
    },
    {
        LookupTokenType::tok_Deg_Sym, LLanguage::GermanID,
        {
            { 1,  8,  61, 617, 21, 0, 0, 1}
        }
    },
    {
        LookupTokenType::tok_Ellipsis, LLanguage::EnglishID,
        {
            //{ 6, 22,  22, 457,  1} - save this - need to use this sometime after 8.2 release - talk to Liz
            { 20, 3,  3, 3,  10}
        }
    },
    {
        LookupTokenType::tok_Ellipsis, LLanguage::GermanID,
        {
            { 6, 22,  22, 457,  1}
        }
    },
    {
        LookupTokenType::tok_Enumeration, LLanguage::EnglishID,
        {
            { 1,  1,   1, 394, 33}
        }
    },
    {
        LookupTokenType::tok_Enumeration, LLanguage::GermanID,
        {
            { 1,  1,   1, 394, 33}
        }
    },
    {
        LookupTokenType::tok_Equal_Sym, LLanguage::EnglishID,
        {
            {13, 25,  25, 564, 15}
        }
    },
    {
        LookupTokenType::tok_Equal_Sym, LLanguage::GermanID,
        {
            {13, 25,  25, 564, 15}
        }
    },
    {
        LookupTokenType::tok_FT_Sym, LLanguage::EnglishID,
        {
            { 1,  8,  61, 615, 10}
        }
    },
    {
        LookupTokenType::tok_FT_Sym, LLanguage::GermanID,
        {
            { 1,  8,  61, 615, 21}
        }
    },
    {
        LookupTokenType::tok_IN_Sym, LLanguage::EnglishID,
        {
            { 1,  8,  61, 616, 10}
        }
    },
    {
        LookupTokenType::tok_IN_Sym, LLanguage::GermanID,
        {
            { 1,  8,  61, 616, 21}
        }
    },
    {
        LookupTokenType::tok_Left_Angle, LLanguage::EnglishID,
        {
            {20,  6,  26, 386,  1}
        }
    },
    {
        LookupTokenType::tok_Left_Angle, LLanguage::GermanID,
        {
            {20,  6,  26, 386,  1}
        }
    },
    {
        LookupTokenType::tok_Math_Express, LLanguage::EnglishID,
        {
            {16,  4,  66,  66,  1}
        }
    },
    {
        LookupTokenType::tok_Math_Express, LLanguage::GermanID,
        {
            {16,  4,  66,  66,  1}
        }
    },
    {
        LookupTokenType::tok_Math_Sym, LLanguage::EnglishID,
        {
            {13, 25,  25,  25, 15}
        }
    },
    {
        LookupTokenType::tok_Math_Sym, LLanguage::GermanID,
        {
            {13, 25,  25,  25, 15}
        }
    },
    {
        LookupTokenType::tok_Minus_Sym, LLanguage::EnglishID,
        {
            {13, 25,  25, 569, 15}
        }
    },
    {
        LookupTokenType::tok_Minus_Sym, LLanguage::GermanID,
        {
            {13, 25,  25, 569, 15}
        }
    },
    {
        LookupTokenType::tok_Misc_Char, LLanguage::EnglishID,
        {
            {16,  4,  60,  60,  1}
        }
    },
    {
        LookupTokenType::tok_Misc_Char, LLanguage::GermanID,
        {
            {16,  4,  60,  60,  1}
        }
    },
    {
        LookupTokenType::tok_Monetary_Sym, LLanguage::EnglishID,
        {
            {16,  4,  60, 980,  1}
        }
    },
    {
        LookupTokenType::tok_Monetary_Sym, LLanguage::GermanID,
        {
            {16,  4,  60, 980,  1}
        }
    },
    {
        LookupTokenType::tok_No_04, LLanguage::GermanID,
        {
            { 4, 13,  69,  69,  9, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_16, LLanguage::EnglishID,
        {
            {16,  4,  67,  67,  1, 9, 9},
            {14,  9,  67,  67,  8, 9, 9},
            { 4, 13,  67,  67,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_16, LLanguage::GermanID,
        {
            {16,  4,  67,  67,  1, 9, 5},
            {14,  9,  67,  67,  8, 9, 5},
            { 4, 13,  67,  67,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_16_04, LLanguage::EnglishID,
        {
            {16,  4,  68,  68,  1, 9, 9},
            { 4, 13,  68,  68,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_16_04, LLanguage::GermanID,
        {
            {16,  4,  68,  68,  1, 9, 5},
            { 4, 13,  68,  68,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_16_14, LLanguage::EnglishID,
        {
            {16,  4,  72,  72,  1, 9, 9},
            {14,  9,  72,  72,  8, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_16_14, LLanguage::GermanID,
        {
            {16,  4,  72,  72,  1, 9, 5},
            {14,  9,  72,  72,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Curr, LLanguage::EnglishID,
        {
            {16,  4,  62,  62,  1, 9, 9},
			{14,  9,  62,  62,  8, 9, 9},
            { 4, 13,  62,  62,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Curr, LLanguage::GermanID,
        {
            {16,  4,  62,  62,  1, 9, 5},
			{14,  9,  62,  62,  8, 9, 5},
            { 4, 13,  62,  62,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Curr_14, LLanguage::EnglishID,
        {
            {14,  9,  62,  62,  8, 9, 9},
            { 4, 13,  62,  62,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Curr_14, LLanguage::GermanID,
        {
            {14,  9,  62,  62,  8, 9, 5},
            { 4, 13,  62,  62,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Decimal, LLanguage::EnglishID,
        {
            {16,  4,  19, 919,  1, 9, 9},
            {14,  9,  19, 919,  8, 9, 9},
            { 4, 13,  19, 919,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Decimal, LLanguage::GermanID,
        {
            {16,  4,  19, 919,  1, 9, 5},
            {14,  9,  19, 919,  8, 9, 5},
            { 4, 13,  19, 919,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Fraction, LLanguage::EnglishID,
        {
            {16,  4,  19, 919,  1, 9, 9},
            {14,  9,  19, 919,  1, 9, 9},
            { 4, 13,  19, 919,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Fraction, LLanguage::GermanID,
        {
            {16,  4,  19, 919,  1, 9, 5},
            {14,  9,  19, 919,  1, 9, 5},
            { 4, 13,  19, 919,  1, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_0, LLanguage::EnglishID,
        {
            { 4, 13,  20,  20,  6, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_0, LLanguage::GermanID,
        {
            { 4, 13,  20,  20,  9, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_1, LLanguage::EnglishID,
        {
            { 4, 13,  21,  123,  6, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_1, LLanguage::GermanID,
        {
            { 4, 13,  21,  123,  9, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_2, LLanguage::EnglishID,
        {
            { 4, 13,  21,  222,  6, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_2, LLanguage::GermanID,
        {
            { 4, 13,  21,  222,  9, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_3_4, LLanguage::EnglishID,
        {
            { 4, 13,  21,  234,  6, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_3_4, LLanguage::GermanID,
        {
            { 4, 13,  21,  234,  9, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_5_31, LLanguage::EnglishID,
        {
            { 4, 13,  21,  567,  6, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_5_31, LLanguage::GermanID,
        {
            { 4, 13,  21,  567,  9, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_32_99, LLanguage::EnglishID,
        {
            { 4, 13,  23,  567,  6, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_32_99, LLanguage::GermanID,
        {
            { 4, 13,  23,  567,  9, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_100_999, LLanguage::EnglishID,
        {
            { 4, 13,  40,  789,  6, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_100_999, LLanguage::GermanID,
        {
            { 4, 13,  40,  789,  9, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_1000_1399, LLanguage::EnglishID,
        {
            { 4, 13,  51,  789,  6, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_1000_1399, LLanguage::GermanID,
        {
            { 4, 13,  51,  789,  9, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_1400_2100, LLanguage::EnglishID,
        {
            { 4, 13,  53,  789,  6, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_1400_2100, LLanguage::GermanID,
        {
            { 4, 13,  53,  789,  9, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_2101_, LLanguage::EnglishID,
        {
            { 4, 13,  54,  890,  6, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Hyphen_2101_, LLanguage::GermanID,
        {
            { 4, 13,  54,  789,  9, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Mixed, LLanguage::EnglishID,
        {
            {16,  4,  18, 818,  1, 9, 9},
            {14,  9,  18, 818,  8, 9, 9},
            { 4, 13,  18, 818,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Mixed, LLanguage::GermanID,
        {
            {16,  4,  18, 818,  1, 9, 5},
            {14,  9,  18, 818,  8, 9, 5},
            { 4, 13,  18, 818,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Non_Curr_Unit, LLanguage::EnglishID,
        {
            {14,  9,  64,  64,  8, 9, 9},
            { 4, 13,  64,  64,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Non_Curr_Unit, LLanguage::GermanID,
        {
            {14,  9,  64,  64,  8, 9, 5},
            { 4, 13,  64,  64,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Range_0, LLanguage::EnglishID,
        {
            {16,  4,  20,  20,  1, 9, 9},
            {14,  9,  20,  20,  8, 9, 9},
            { 4, 13,  20,  20,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Range_0, LLanguage::GermanID,
        {
            {16,  4,  20,  20,  1, 9, 5},
            {14,  9,  20,  20,  8, 9, 5},
            { 4, 13,  20,  20,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Range_1, LLanguage::EnglishID,
        {
            {16,  4,  21, 123,  1, 9, 9},
            {14,  9,  21, 123,  7, 9, 9},
            { 4, 13,  21, 123,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Range_1, LLanguage::GermanID,
        {
            {16,  4,  21, 123,  1, 9, 5},
            {14,  9,  21, 123,  7, 9, 5},
            { 4, 13,  21, 123,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Range_2, LLanguage::EnglishID,
        {
            {16,  4,  21, 222,  1, 9, 9},
            {14,  9,  21, 222,  8, 9, 9},
            { 4, 13,  21, 222,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Range_2, LLanguage::GermanID,
        {
            {16,  4,  21, 222,  1, 9, 5},
            {14,  9,  21, 222,  8, 9, 5},
            { 4, 13,  21, 222,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Range_3_4, LLanguage::EnglishID,
        {
            {16,  4,  21, 234,  1, 9, 9},
            {14,  9,  21, 234,  8, 9, 9},
            { 4, 13,  21, 234,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Range_3_4, LLanguage::GermanID,
        {
            {16,  4,  21, 234,  1, 9, 5},
            {14,  9,  21, 234,  8, 9, 5},
            { 4, 13,  21, 234,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Range_5_31, LLanguage::EnglishID,
        {
            {16,  4,  21, 567,  1, 9, 9},
            {14,  9,  21, 567,  8, 9, 9},
            { 4, 13,  21, 567,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Range_5_31, LLanguage::GermanID,
        {
            {16,  4,  21, 567,  1, 9, 5},
            {14,  9,  21, 567,  8, 9, 5},
            { 4, 13,  21, 567,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Range_32_99, LLanguage::EnglishID,
        {
            {16,  4,  23, 567,  1, 9, 9},
            {14,  9,  23, 567,  8, 9, 9},
            { 4, 13,  23, 567,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Range_32_99, LLanguage::GermanID,
        {
            {16,  4,  23, 567,  1, 9, 5},
            {14,  9,  23, 567,  8, 9, 5},
            { 4, 13,  23, 567,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Range_100_999, LLanguage::EnglishID,
        {
            {16,  4,  40, 789,  1, 9, 9},
            {14,  9,  40, 789,  8, 9, 9},
            { 4, 13,  40, 789,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Range_100_999, LLanguage::GermanID,
        {
            {16,  4,  40, 789,  1, 9, 5},
            {14,  9,  40, 789,  8, 9, 5},
            { 4, 13,  40, 789,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Range_1000_1399, LLanguage::EnglishID,
        {
            {16,  4,  51, 789,  1, 9, 9},
            {14,  9,  51, 789,  8, 9, 9},
            { 4, 13,  51, 789,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Range_1000_1399, LLanguage::GermanID,
        {
            {16,  4,  51, 789,  1, 9, 5},
            {14,  9,  51, 789,  8, 9, 5},
            { 4, 13,  51, 789,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Range_1400_2100, LLanguage::EnglishID,
        {
            {16,  4,  53, 789,  1, 9, 9},
            {14,  9,  53, 789,  8, 9, 9},
            { 4, 13,  53, 789,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Range_1400_2100, LLanguage::GermanID,
        {
            {16,  4,  53, 789,  1, 9, 5},
            {14,  9,  53, 789,  8, 9, 5},
            { 4, 13,  53, 789,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_No_Range_2101_, LLanguage::EnglishID,
        {
            {16,  4,  54, 890,  1, 9, 9},
            {14,  9,  54, 890,  8, 9, 9},
            { 4, 13,  54, 890,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_No_Range_2101_, LLanguage::GermanID,
        {
            {16,  4,  54, 789,  1, 9, 5},
            {14,  9,  54, 789,  8, 9, 5},
            { 4, 13,  54, 789,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_Noun_er_G, LLanguage::GermanID,
        {
            { 4,  13,  80, 304, 8}
        }
    },
    {
        LookupTokenType::tok_Noun_erin_G, LLanguage::GermanID,
        {
            { 1,  5,  70, 304,  9}
        }
    },
    {
        LookupTokenType::tok_Noun_erinnen_G, LLanguage::GermanID,
        {
            { 1,  5,  70, 304, 19}
        }
    },
    {
        LookupTokenType::tok_Noun_ern_G, LLanguage::GermanID,
        {
            { 1,  5,  70, 304, 13}
        }
    },
    {
        LookupTokenType::tok_Noun_ers_G, LLanguage::GermanID,
        {
            { 1,  5,  70, 304,  2}
        }
    },
    {
        LookupTokenType::tok_Noun_stel_G, LLanguage::GermanID,
        {
            { 1,   2,  24, 305, 23}
        }
    },
    {
        LookupTokenType::tok_Noun_steln_G, LLanguage::GermanID,
        {
            { 1,   2,  24, 305, 13}
        }
    },
    {
        LookupTokenType::tok_Noun_stels_G, LLanguage::GermanID,
        {
            { 1,   2,  24, 305,  2}
        }
    },
    {
        LookupTokenType::tok_Outline_Notation, LLanguage::EnglishID,
        {
            {20,  1,   1, 389,  1}
        }
    },
    {
        LookupTokenType::tok_Outline_Notation, LLanguage::GermanID,
        {
            {20,  1,   1, 389,  1}
        }
    },
    {
        LookupTokenType::tok_PerC_Sym, LLanguage::EnglishID,
        {
            { 1,  8,  95, 895, 10}
        }
    },
    {
        LookupTokenType::tok_PerC_Sym, LLanguage::GermanID,
        {
            { 1,  8,  95, 895,  8}
        }
    },
    {
        LookupTokenType::tok_Pl_Date, LLanguage::EnglishID,
        {
            {16,  4,  52, 789,  2, 9, 9}
        }
    },
    {
        LookupTokenType::tok_Plus_Sym, LLanguage::EnglishID,
        {
            {13, 25,  25, 563, 15}
        }
    },
    {
        LookupTokenType::tok_Plus_Sym, LLanguage::GermanID,
        {
            {13, 25,  25, 563, 15}
        }
    },
    {
        LookupTokenType::tok_Poss_Date, LLanguage::EnglishID,
        {
            {16,  4,  52, 789,  1, 9, 9},
            {14,  9,  52, 789,  8, 9, 9},
            { 4, 13,  52, 789,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_Poss_Date, LLanguage::GermanID,
        {
            {16,  4,  52, 789,  1, 9, 5},
            {14,  9,  52, 789,  8, 9, 5},
            { 4, 13,  52, 789,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_Right_Angle, LLanguage::EnglishID,
        {
            {20,  7,  27, 387,  1}
        }
    },
    {
        LookupTokenType::tok_Right_Angle, LLanguage::GermanID,
        {
            {20,  7,  27, 387,  1}
        }
    },
    {
        LookupTokenType::tok_Sym_Parens, LLanguage::EnglishID,
        {
            { 1,  1,   1, 220, 33}
        }
    },
    {
        LookupTokenType::tok_Sym_Parens, LLanguage::GermanID,
        {
            { 1,  1,   1, 220, 33}
        }
    },
    {
        LookupTokenType::tok_Temp_Scale, LLanguage::EnglishID,
        {
            { 1,  8,  61, 234,  1}
        }
    },
    {
        LookupTokenType::tok_Temp_Scale, LLanguage::GermanID,
        {
            { 1,  8,  61, 234, 21}
        }
    },
    {
        LookupTokenType::tok_TempC_Scale, LLanguage::EnglishID,
        {
            { 1,  8,  61, 234,  1, 0, 0, 1}
        }
    },
    {
        LookupTokenType::tok_TempC_Scale, LLanguage::GermanID,
        {
            { 1,  8,  61, 234, 21, 0, 0, 1}
        }
    },
    {
        LookupTokenType::tok_TempF_Scale, LLanguage::EnglishID,
        {
            { 1,  8,  61, 234,  1, 0, 0, 3}
        }
    },
    {
        LookupTokenType::tok_TempF_Scale, LLanguage::GermanID,
        {
            { 1,  8,  61, 234, 21, 0, 0, 3}
        }
    },
    {
        LookupTokenType::tok_Time, LLanguage::EnglishID,
        {
            {16,  4,  64,  64,  1, 9, 9},
            { 4, 13,  64,  64,  9, 9, 9}
        }
    },
    {
        LookupTokenType::tok_Time, LLanguage::GermanID,
        {
            {16,  4,  64,  64,  1, 9, 5},
            { 4, 13,  64,  64,  8, 9, 5}
        }
    },
    {
        LookupTokenType::tok_Time_16, LLanguage::EnglishID,
        {
            {16,  4,  64,  64,  1, 9, 9}
        }
    },
    {
        LookupTokenType::tok_Time_Noun, LLanguage::EnglishID,
        {
            { 1, 10,  10, 174,  1}
        }
    },
    {
        LookupTokenType::tok_Time_Noun, LLanguage::GermanID,
        {
            { 1, 10,  10, 174, 21}
        }
    },
    {
        LookupTokenType::tok_Times_Sym, LLanguage::EnglishID,
        {
            {13,  25,  25, 561, 15}
        }
    },
    {
        LookupTokenType::tok_Times_Sym, LLanguage::GermanID,
        {
            {13,  25,  25, 561, 15}
        }
    },
    {
        LookupTokenType::tok_Unfound_Alpha_Num, LLanguage::EnglishID,
        {
            { 1,  1,   1, 989, 33}
        }
    },
    {
        LookupTokenType::tok_Unfound_Alpha_Num, LLanguage::GermanID,
        {
            { 1,  1,   1, 989, 33}
        }
    },
    {
        LookupTokenType::tok_Left_Hiphen, LLanguage::EnglishID,
        {
            { 20,  6,   26, 866, 1}
        }
    },
    {
        LookupTokenType::tok_Left_Hiphen, LLanguage::GermanID,
        {
            { 20,  6,   26, 866, 1}
        }
    },
    {
        LookupTokenType::tok_Right_Hiphen, LLanguage::EnglishID,
        {
            { 20,  7,   27, 877, 1}
        }
    },
    {
        LookupTokenType::tok_Right_Hiphen, LLanguage::GermanID,
        {
            { 20,  7,   27, 877, 1}
        }
    },
    {
        LookupTokenType::tok_Unfound_Agent, LLanguage::EnglishID,
        {
            { 1,  1,   1, 228, 33}
        }
    },
    {
        LookupTokenType::tok_Unfound_Agent, LLanguage::GermanID,
        {
            { 1,  1,   1, 228, 33}
        }
    },
	{
		LookupTokenType::tok_no_pl, LLanguage::EnglishID,
		{
			{1, 1, 1, 214, 2, 9, 9}
		}
	},
	{
		LookupTokenType::tok_no_pl, LLanguage::GermanID,
		{
			{1, 1, 1, 214, 19, 9, 5}
		}
	},
	{
		LookupTokenType::tok_Ampersand_Sym, LLanguage::EnglishID,
		{
			{19, 1, 20, 130, 1}
		}
	},
	{
		LookupTokenType::tok_Ampersand_Sym, LLanguage::GermanID,
		{
			{19, 1, 20, 130, 1}
		}
	},
	{
		LookupTokenType::tok_Left_doubledash, LLanguage::EnglishID,
		{
			{20, 6, 26, 866, 1}
		}
	},
	{
		LookupTokenType::tok_Left_doubledash, LLanguage::GermanID,
		{
			{20, 6, 26, 866, 1}
		}
	},
	{
		LookupTokenType::tok_Right_doubledash, LLanguage::EnglishID,
		{
			{20, 7, 27, 877, 1}
		}
	},
	{
		LookupTokenType::tok_Right_doubledash, LLanguage::GermanID,
		{
			{20, 7, 27, 877, 1}
		}
	},
	{
		LookupTokenType::tok_Doubledash, LLanguage::EnglishID,
		{
			{20, 3, 3, 885, 4}
		}
	},
	{
		LookupTokenType::tok_Doubledash, LLanguage::GermanID,
		{
			{20, 3, 3, 885, 4}
		}
	},
	{
		LookupTokenType::tok_No_Misc, LLanguage::EnglishID,
		{
			{16, 4, 71, 71, 1, 9, 9},
			{14, 9, 71, 71, 8, 9, 9},
			{4,  13,71, 71, 9, 9, 9}
		}
	},
	{
		LookupTokenType::tok_No_Misc, LLanguage::GermanID,
		{
			{16, 4, 71, 71, 1, 9, 5},
			{14, 9, 71, 71, 8, 9, 5},
			{4,  13,71, 71, 8, 9, 5}
		}
	},
	{
		LookupTokenType::tok_Ascending, LLanguage::EnglishID,
		{
			{16, 4, 67, 67, 1, 9, 9},
			{14, 9, 67, 67, 8, 9, 9},
			{ 4, 13, 67, 67, 9, 9, 9}
		}
	},
	{
		LookupTokenType::tok_Ascending, LLanguage::GermanID,
		{
			{16, 4, 67, 67, 1, 9, 5},
			{14, 9, 67, 67, 8, 9, 5},
			{4, 13, 67, 67, 8, 9, 5}
		}
	},
	{
		LookupTokenType::tok_Descending, LLanguage::EnglishID,
		{
			{16, 4, 63, 63, 1, 9, 9},
			{14, 9, 63, 63, 8, 9, 9},
			{4, 13, 63, 63, 9, 9, 9}
		}
	},
	{
		LookupTokenType::tok_Descending, LLanguage::GermanID,
		{
			{16, 4, 63, 63, 1, 9, 5},
			{14, 9, 63, 63, 8, 9, 5},
			{4, 13, 63, 63, 8, 9, 5}
		}
	},
	{
		LookupTokenType::tok_No_HyphenatedFraction, LLanguage::EnglishID,
		{
            {16,  4,  19, 919,  6, 9, 9},
            {14,  9,  19, 919,  6, 9, 9},
            { 4, 13,  19, 919,  6, 9, 9}
		}
	},
	{
		LookupTokenType::tok_No_HyphenatedFraction, LLanguage::GermanID,
		{
            {16,  4,  19, 919,  9, 9, 5},
            {14,  9,  19, 919,  9, 9, 5},
            { 4, 13,  19, 919,  9, 9, 5}
		}
	},
	{
		LookupTokenType::tok_Unfound_Agent_Poss, LLanguage::EnglishID,
		{
            {01, 01, 001, 228, 14}
		}
	},
	{
		LookupTokenType::tok_No_Post_Hyphen, LLanguage::EnglishID,
		{
            {16, 04, 004, 004, 1}		
      }
	},
	{
		LookupTokenType::tok_No_Post_Hyphen, LLanguage::GermanID,
		{
            {16, 04, 004, 004, 1}
		}
	},
   {
        LookupTokenType::tok_none
   }
};

LgsMap(int, LDictionaryEntry*) ST_EntryMap::dictionaryEntry_;

void ST_EntryMap::Initialize()
{
    for (const DictionaryEntrySubset* entrySubset = entrySubset_;
         entrySubset->tokenType_ != LookupTokenType::tok_none;
         entrySubset++)
    {
        insert(entrySubset);
    }
}

void ST_EntryMap::insert(const DictionaryEntrySubset* entrySubset)
{
    // create entry
    LDictionaryEntry* entry = new LDictionaryEntry;
    entry->setToUnfoundWord();
    entry->setCached(true);

    LSemantoSyntacticUnit ssu;
    ssu.setCompanyCode("LOG");
    ssu.setWordCount(1);
    ssu.setWordID(-10);            //+++ what is setWordID ?

    for (int i = 0; i < 3; i++)
    {
        ssu.setWordClassCode(entrySubset->ssu[i].wordClass_ );
        ssu.setSetID        (entrySubset->ssu[i].set_       );
        ssu.setFormCode     (entrySubset->ssu[i].form_      );
        ssu.setOverflow2b   (entrySubset->ssu[i].overflow2b_);
        ssu.setOverflow3b   (entrySubset->ssu[i].overflow3b_);
        ssu.setSubSetID     (entrySubset->ssu[i].subset_    );
        ssu.setSuperSetID   (entrySubset->ssu[i].superset_  );
        ssu.setGenderCode   (entrySubset->ssu[i].gender_  );

        entry->addSsu(ssu);
    }

    dictionaryEntry_[makeInt(entrySubset->tokenType_, entrySubset->language_)] = entry;
}

void ST_EntryMap::Cleanup()
{
   LgsMap(int, LDictionaryEntry*)::iterator curr = dictionaryEntry_.begin();
   while (curr != dictionaryEntry_.end())
   {
      LDictionaryEntry* entry = curr->second;
      delete entry;
      curr++;
   }
   dictionaryEntry_.clear();
}
