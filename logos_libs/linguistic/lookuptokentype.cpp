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
// File - LookupTokenType.cpp
//
// Class - LookupTokenType
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/lookuptokentype.h>

#define dumpType(s) case tok_##s: return LgsString(#s)
LgsString LookupTokenType::tokenTypeDesc(LookupTokenType::Type tokenType)
{
    switch (tokenType)
    {
    dumpType(none);
    dumpType(continuation);
    dumpType(lookup);

    dumpType(AdjAdv_Num_1st_E_Hyphen);
    dumpType(AdjAdv_Num_st_E_Hyphen);
    dumpType(AdjAdv_Num_nd_E_Hyphen);
    dumpType(AdjAdv_Num_rd_E_Hyphen);
    dumpType(AdjAdv_Num_th_E_Hyphen);
    dumpType(AdjAdv_Num_1st_E);
    dumpType(AdjAdv_Num_st_E);
    dumpType(AdjAdv_Num_nd_E);
    dumpType(AdjAdv_Num_rd_E);
    dumpType(AdjAdv_Num_th_E);
    dumpType(AdjNoun_fachem);
    dumpType(AdjNoun_fachen);
    dumpType(AdjNoun_facher);
    dumpType(AdjNoun_faches);
    dumpType(Adj_Num_1_G);
    dumpType(Adj_Num_21_G);
    dumpType(Adj_Num_2_G);
    dumpType(Adj_Num_3_G);
    dumpType(Adj_Num_4_G);
    dumpType(Adj_fach);
    dumpType(Adj_ig);
    dumpType(Adj_ige);
    dumpType(Adj_igen);
    dumpType(Adj_iger);
    dumpType(Adj_iges);
    dumpType(Adv_mal);
    dumpType(Appos_Date);
    dumpType(At_Sym);
    dumpType(Date);
    dumpType(Date_Range);
    dumpType(Deg_Sym);
    dumpType(Ellipsis);
    dumpType(Enumeration);
    dumpType(Equal_Sym);
    dumpType(FT_Sym);
    dumpType(IN_Sym);
    dumpType(Left_Angle);
    dumpType(Math_Express);
    dumpType(Math_Sym);
    dumpType(Minus_Sym);
    dumpType(Misc_Char);
    dumpType(Monetary_Sym);
    dumpType(No_04);
    dumpType(No_16);
    dumpType(No_16_04);
    dumpType(No_16_14);
    dumpType(No_Curr);
    dumpType(No_Curr_14);
    dumpType(No_Decimal);
    dumpType(No_Fraction);
    dumpType(No_Hyphen_0);
    dumpType(No_Hyphen_1);
    dumpType(No_Hyphen_2);
    dumpType(No_Hyphen_3_4);
    dumpType(No_Hyphen_5_31);
    dumpType(No_Hyphen_32_99);
    dumpType(No_Hyphen_100_999);
    dumpType(No_Hyphen_1000_1399);
    dumpType(No_Hyphen_1400_2100);
    dumpType(No_Hyphen_2101_);
    dumpType(No_Mixed);
    dumpType(No_Non_Curr_Unit);
    dumpType(No_Range_0);
    dumpType(No_Range_1);
    dumpType(No_Range_2);
    dumpType(No_Range_3_4);
    dumpType(No_Range_5_31);
    dumpType(No_Range_32_99);
    dumpType(No_Range_100_999);
    dumpType(No_Range_1000_1399);
    dumpType(No_Range_1400_2100);
    dumpType(No_Range_2101_);
    dumpType(Noun_er_G);
    dumpType(Noun_erin_G);
    dumpType(Noun_erinnen_G);
    dumpType(Noun_ern_G);
    dumpType(Noun_ers_G);
    dumpType(Noun_stel_G);
    dumpType(Noun_steln_G);
    dumpType(Noun_stels_G);
    dumpType(Outline_Notation);
    dumpType(PerC_Sym);
    dumpType(Pl_Date);
    dumpType(Plus_Sym);
    dumpType(Poss_Date);
    dumpType(Right_Angle);
    dumpType(Sym_Parens);
    dumpType(Temp_Scale);
    dumpType(TempC_Scale);
    dumpType(TempF_Scale);
    dumpType(Time);
    dumpType(Time_16);
    dumpType(Time_Noun);
    dumpType(Times_Sym);
    dumpType(Unfound_Alpha_Num);
    dumpType(Unfound_Agent);
    dumpType(Left_Hiphen);
    dumpType(Right_Hiphen);
	dumpType(no_pl);
	dumpType(Ampersand_Sym);
	dumpType(Left_doubledash);
	dumpType(Right_doubledash);
	dumpType(Doubledash);
	dumpType(No_Misc);
	dumpType(Ascending);
	dumpType(Descending);
   dumpType(No_HyphenatedFraction);
   dumpType(Unfound_Agent_Poss);
   dumpType(No_Post_Hyphen);
    default:
        assert(("invalid value", 0));
        return LgsString("invalid");
    }
}

