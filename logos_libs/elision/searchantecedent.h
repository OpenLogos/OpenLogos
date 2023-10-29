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
#ifndef __ElisionSearchAntecedent_h__
#define __ElisionSearchAntecedent_h__

//-------------------------------------------------------------------
// File - SearchAntecedent.h
//
// Class - EL_SearchAntecedent
//
// Description - Antecedent used to search for a pattern
//
//-------------------------------------------------------------------

#include <logos_libs/ruleengine/antecedent.h>
#include <logos_libs/elision/variable.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Elision {

// this is a compound antecedent, with a search pattern, and a further antecedent.
class EL_SearchAntecedent: public RE_Antecedent<EL_Variable>
{
   DisableCopyAssign(EL_SearchAntecedent);

public:
   enum EL_Rule
   {
      ALL_RuleNo_1   = 1,
      ALL_RuleNo_2   = 2,
      ALL_RuleNo_3   = 3,
      ALL_RuleNo_4   = 4,
      ALL_RuleNo_5   = 5,
      ALL_RuleNo_6   = 6,
      ALL_RuleNo_7   = 7,
      ALL_RuleNo_8   = 8,
      ALL_RuleNo_9   = 9,
      EN_RuleNo_100  = 100,
      EN_RuleNo_101  = 101,
      EN_RuleNo_102  = 102,
      EN_RuleNo_103  = 103,
      FR_RuleNo_200  = 200,
      FR_RuleNo_201  = 201,
      FR_RuleNo_202  = 202,
      FR_RuleNo_203  = 203,
      FR_RuleNo_204  = 204,
      FR_RuleNo_205  = 205,
      FR_RuleNo_206  = 206,
      FR_RuleNo_207  = 207,
      FR_RuleNo_208  = 208,
      FR_RuleNo_209  = 209,
      FR_RuleNo_210  = 210,
      FR_RuleNo_211  = 211,
      FR_RuleNo_212  = 212,
      FR_RuleNo_213  = 213,
      FR_RuleNo_214  = 214,
      FR_RuleNo_215  = 215,
      FR_RuleNo_216  = 216,
      FR_RuleNo_217  = 217,
      FR_RuleNo_218  = 218,
      FR_RuleNo_219  = 219,
      FR_RuleNo_220  = 220,
      FR_RuleNo_221  = 221,
      FR_RuleNo_222  = 222,
      FR_RuleNo_223  = 223,
      FR_RuleNo_224  = 224,
      GR_RuleNo_300  = 300,
      GR_RuleNo_301  = 301,
      GR_RuleNo_302  = 302,
      GR_RuleNo_303  = 303,
      GR_RuleNo_304  = 304,
      GR_RuleNo_305  = 305,
      GR_RuleNo_306  = 306,
      GR_RuleNo_307  = 307,
      GR_RuleNo_308  = 308,
      GR_RuleNo_309  = 309,
      GR_RuleNo_310  = 310,
      GR_RuleNo_311  = 311,
      GR_RuleNo_312  = 312,
      IT_RuleNo_400  = 400,
      IT_RuleNo_401  = 401,
      IT_RuleNo_402  = 402,
      IT_RuleNo_403  = 403,
      IT_RuleNo_404  = 404,
      IT_RuleNo_405  = 405,
      IT_RuleNo_406  = 406,
      IT_RuleNo_407  = 407,
      IT_RuleNo_408  = 408,
      IT_RuleNo_409  = 409,
      IT_RuleNo_410  = 410,
      IT_RuleNo_411  = 411,
      IT_RuleNo_412  = 412,
      IT_RuleNo_413  = 413,
      IT_RuleNo_414  = 414,
      IT_RuleNo_415  = 415,
      IT_RuleNo_416  = 416,
      IT_RuleNo_417  = 417,
      IT_RuleNo_418  = 418,
      IT_RuleNo_419  = 419,
      IT_RuleNo_420  = 420,
      IT_RuleNo_421  = 421,
      IT_RuleNo_422  = 422,
      IT_RuleNo_423  = 423,
      IT_RuleNo_424  = 424,
      IT_RuleNo_425  = 425,
      IT_RuleNo_426  = 426,
      IT_RuleNo_427  = 427,
      IT_RuleNo_428  = 428,
      IT_RuleNo_429  = 429,
      IT_RuleNo_430  = 430,
      IT_RuleNo_431  = 431,
      IT_RuleNo_432  = 432,
      IT_RuleNo_433  = 433,
      IT_RuleNo_434  = 434,
      IT_RuleNo_435  = 435,
      IT_RuleNo_436  = 436,
      IT_RuleNo_437  = 437,
      IT_RuleNo_438  = 438,
      IT_RuleNo_439  = 439,
      IT_RuleNo_440  = 440,
      IT_RuleNo_441  = 441,
      IT_RuleNo_442  = 442,
      IT_RuleNo_443  = 443,
      IT_RuleNo_444  = 444,
      IT_RuleNo_445  = 445,
      IT_RuleNo_446  = 446,
      IT_RuleNo_447  = 447,
      IT_RuleNo_448  = 448,
      SP_RuleNo_500  = 500,
      SP_RuleNo_501  = 501,
      SP_RuleNo_502  = 502,
      SP_RuleNo_503  = 503,
      SP_RuleNo_504  = 504,
      SP_RuleNo_505  = 505,
      SP_RuleNo_506  = 506,
      SP_RuleNo_507  = 507,
      SP_RuleNo_508  = 508,
      SP_RuleNo_509  = 509,
      SP_RuleNo_510  = 510,
      SP_RuleNo_511  = 511,
      SP_RuleNo_512  = 512,
      SP_RuleNo_513  = 513,
      SP_RuleNo_514  = 514,
      SP_RuleNo_515  = 515,
      SP_RuleNo_516  = 516,
      SP_RuleNo_517  = 517,
      SP_RuleNo_518  = 518,
      SP_RuleNo_519  = 519,
      SP_RuleNo_520  = 520,
      SP_RuleNo_521  = 521,
      SP_RuleNo_522  = 522,
      SP_RuleNo_523  = 523,
      SP_RuleNo_524  = 524,
      SP_RuleNo_525  = 525,
      PO_RuleNo_600  = 600,
      PO_RuleNo_601  = 601,
      PO_RuleNo_602  = 602,
      PO_RuleNo_603  = 603,
      PO_RuleNo_604  = 604,
      PO_RuleNo_605  = 605,
      PO_RuleNo_606  = 606,
      PO_RuleNo_607  = 607,
      PO_RuleNo_608  = 608,
      PO_RuleNo_609  = 609,
      PO_RuleNo_610  = 610,
      PO_RuleNo_611  = 611,
      PO_RuleNo_612  = 612,
      PO_RuleNo_613  = 613,
      PO_RuleNo_614  = 614,
      PO_RuleNo_615  = 615,
      PO_RuleNo_616  = 616,
      PO_RuleNo_617  = 617,
      PO_RuleNo_618  = 618,
      PO_RuleNo_619  = 619,
      PO_RuleNo_620  = 620,
      PO_RuleNo_621  = 621,
      PO_RuleNo_622  = 622,
      PO_RuleNo_623  = 623,
      PO_RuleNo_624  = 624,
      PO_RuleNo_625  = 625,
      PO_RuleNo_626  = 626,
      PO_RuleNo_627  = 627,
      PO_RuleNo_628  = 628,
      PO_RuleNo_629  = 629,
      PO_RuleNo_630  = 630,
      PO_RuleNo_631  = 631,
      PO_RuleNo_632  = 632,
      PO_RuleNo_633  = 633,
      PO_RuleNo_634  = 634,
      PO_RuleNo_635  = 635,
      PO_RuleNo_636  = 636,
      PO_RuleNo_637  = 637,
      PO_RuleNo_638  = 638,
      PO_RuleNo_639  = 639,
      PO_RuleNo_640  = 640,
      PO_RuleNo_641  = 641,
      PO_RuleNo_642  = 642,
      PO_RuleNo_643  = 643,
      PO_RuleNo_644  = 644,
      PO_RuleNo_645  = 645,
      PO_RuleNo_646  = 646,
      PO_RuleNo_647  = 647,
      PO_RuleNo_648  = 648,
      PO_RuleNo_649  = 649,
      PO_RuleNo_650  = 650,
      PO_RuleNo_651  = 651,
      PO_RuleNo_652  = 652,
      PO_RuleNo_653  = 653,
      PO_RuleNo_654  = 654
   };

   EL_SearchAntecedent(const LgsString& pattern, RegularExpression::Borders borders, int ruleNumber);
	virtual ~EL_SearchAntecedent();

   int groups();
   void setAntecedent(RE_Antecedent<EL_Variable>* antecedent);
   void setPreScanStrings(LgsStringVector* scanStrings);

   // returns true if the search is true and the further antecedent returns true
   // if the search returns true, but the further antecedent returns false, evaluate
   // will backtrack to find the next match of the search expression
   virtual bool evaluate(EL_Variable& variable);
   virtual bool isViableMatch(EL_Variable& variable);

private:
   RegularExpression pattern_;
   RE_Antecedent<EL_Variable>* antecedent_;
   int ruleNumber_;
   LgsStringVector* preScanStrings;
};

//-------------------------------------------------------------------
inline EL_SearchAntecedent::EL_SearchAntecedent(const LgsString& pattern,
                                                RegularExpression::Borders borders,
                                                int ruleNumber)
                           :pattern_(pattern, borders),
                            antecedent_(0),
                            ruleNumber_(ruleNumber),
                            preScanStrings(0)
{
}
//-------------------------------------------------------------------
inline EL_SearchAntecedent::~EL_SearchAntecedent()
{
	if(antecedent_)
   {
		delete antecedent_;
	   antecedent_ = 0;
   }
   if (preScanStrings)
   {
      preScanStrings->clear();
      delete preScanStrings;
      preScanStrings = 0;
   }
}
//-------------------------------------------------------------------
inline void EL_SearchAntecedent::setAntecedent(RE_Antecedent<EL_Variable>* antecedent)
{
   antecedent_ = antecedent;
}
//-------------------------------------------------------------------
inline void EL_SearchAntecedent::setPreScanStrings(LgsStringVector* scanStrings)
{
   preScanStrings = scanStrings;
}
//-------------------------------------------------------------------
inline int EL_SearchAntecedent::groups()
{
   return pattern_.groups();
}



//}

#endif



