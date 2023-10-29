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
//-------------------------------------------------------------------
// File - SearchAntecedent.cpp
//
// Class - EL_SearchAntecedent
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/elision/searchantecedent.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Elision {

bool EL_SearchAntecedent::evaluate(EL_Variable& variable)
{
   variable.resetStartPos();
   for (;;)
   {
      if (!variable.matches(pattern_))
         return false;

      // no more antecedents - we are done
      // more antecedents - call evaluate on the antecedent - if true - we are done
      if (!antecedent_ || antecedent_->evaluate(variable))
         return true;

      // if beyond the end of the LgsString to match - we are done
      variable.incrementStartPos();
      if (variable.pastEndPos())
         return false;

      // backtrack, searching from beyond the current pattern
   }
}

// --------------------------------------------------------------------------
bool EL_SearchAntecedent::isViableMatch(EL_Variable& variable)
{
   LgsString str = variable.getPhraseManager()->getText();

   if (str.length() == 0)
   {
      return false;
   }

   if (preScanStrings->size() > 0)
   {
	   LgsStringIterator scanStr = preScanStrings->begin();
	   while (scanStr != preScanStrings->end())
	   {
         if (ruleNumber_ == 310)
         {
            int pos;
            if ((pos = str.find(',')) != LgsString::npos)
            {
               return (str.find(',', pos + 1) != LgsString::npos);
            }
            return false;
         }
         else if (str.find(*scanStr) != LgsString::npos)
         {
            // Special cases
            if (ruleNumber_ == 605)
               return (str.find("@QD a a ") == LgsString::npos);
            return true;
         }
         scanStr++;
      }
      return false;
   }
   return true;

/*   switch(ruleNumber_)
   {
   // Rules that apply to all languages
   case ALL_RuleNo_1:
      return (str.find("@INIT_CAP") != LgsString::npos);
   case ALL_RuleNo_2:
      return (str.find("@INIT_CAP @ASPIRE") != LgsString::npos);
   case ALL_RuleNo_3:
   case ALL_RuleNo_5:
      return (str.find("@LC_RGHT") != LgsString::npos);
   case ALL_RuleNo_4:
      return (str.find("@LC_RGHT @ASPIRE") != LgsString::npos);
   case ALL_RuleNo_6:
   case ALL_RuleNo_7:
      return (str.find("@CLS_SP") != LgsString::npos);
   case ALL_RuleNo_8:
      return (str.find("@HYPHEN") != LgsString::npos);
   case ALL_RuleNo_9:
      return (str.find("@APOST") != LgsString::npos);

   // English Rules
   case EN_RuleNo_100:
      return ((str.find("a Eu") != LgsString::npos) ||
              (str.find("a eu") != LgsString::npos) ||
              (str.find("A Eu") != LgsString::npos) ||
              (str.find("A eu") != LgsString::npos) ||
              (str.find("a on") != LgsString::npos) ||
              (str.find("A on") != LgsString::npos) ||
              (str.find("a On") != LgsString::npos) ||
              (str.find("A On") != LgsString::npos));
//   case EN_RuleNo_101:
   case EN_RuleNo_102:
      return ((str.find("a@@Exception@@") != LgsString::npos) ||
              (str.find("A@@Exception@@") != LgsString::npos));
   case EN_RuleNo_103:
      return (str.find("@ASPIRE") != LgsString::npos);

   // French Rules
   case FR_RuleNo_200:
      return ((str.find("beau @QZ") != LgsString::npos) ||
              (str.find("nouveau @QZ") != LgsString::npos));
   case FR_RuleNo_201:
      return ((str.find("mou @QZ") != LgsString::npos) ||
              (str.find("fou @QZ") != LgsString::npos));
   case FR_RuleNo_202:
      return (str.find("vieux @QZ") != LgsString::npos);
   case FR_RuleNo_203:
   case FR_RuleNo_204:
      return (str.find("@QZ @QD") != LgsString::npos);
   case FR_RuleNo_205:
      return ((str.find("de ") != LgsString::npos) ||
              (str.find("le ") != LgsString::npos) ||
              (str.find("que ") != LgsString::npos) ||
              (str.find("je ") != LgsString::npos) ||
              (str.find("me ") != LgsString::npos) ||
              (str.find("te ") != LgsString::npos) ||
              (str.find("se ") != LgsString::npos) ||
              (str.find("ne ") != LgsString::npos));
   case FR_RuleNo_206:
      return (str.find("la ") != LgsString::npos);
   case FR_RuleNo_207:
   case FR_RuleNo_208:
   case FR_RuleNo_209:
   case FR_RuleNo_210:
   case FR_RuleNo_211:
      return (str.find("ce ") != LgsString::npos);
   case FR_RuleNo_212:
      return ((str.find("ma ") != LgsString::npos) ||
              (str.find("ta ") != LgsString::npos) ||
              (str.find("sa ") != LgsString::npos));
   case FR_RuleNo_213:
      return (str.find("@ASPIRE") != LgsString::npos);
   case FR_RuleNo_214:
      return ((str.find("lorsque ") != LgsString::npos) ||
              (str.find("puisque ") != LgsString::npos) ||
              (str.find("quoique ") != LgsString::npos) ||
              (str.find("jusque ") != LgsString::npos));
   case FR_RuleNo_215:
   case FR_RuleNo_216:
      return (str.find("quelque ") != LgsString::npos);
   case FR_RuleNo_217:
      return (str.find("si il") != LgsString::npos);
   case FR_RuleNo_218:
      return (str.find("à le") != LgsString::npos);
   case FR_RuleNo_219:
      return (str.find("de le") != LgsString::npos);
   case FR_RuleNo_220:
      return (str.find("à les") != LgsString::npos);
   case FR_RuleNo_221:
      return (str.find("de les") != LgsString::npos);
   case FR_RuleNo_222:
      return (str.find("plus ") != LgsString::npos);
   case FR_RuleNo_223:
      return (str.find("d' ") != LgsString::npos);
   case FR_RuleNo_224:
      return ((str.find("@QZ") != LgsString::npos) ||
              (str.find("@QD") != LgsString::npos));

   // German Rules
   case GR_RuleNo_300:
   case GR_RuleNo_301:
      return (str.find("@GAPOST") != LgsString::npos);
   case GR_RuleNo_302:
      return (str.find("an dem") != LgsString::npos);
   case GR_RuleNo_303:
      return (str.find("bei dem") != LgsString::npos);
   case GR_RuleNo_304:
      return (str.find("in dem") != LgsString::npos);
   case GR_RuleNo_305:
      return (str.find("von dem") != LgsString::npos);
   case GR_RuleNo_306:
      return (str.find("zu dem") != LgsString::npos);
   case GR_RuleNo_307:
      return (str.find("zu der") != LgsString::npos);
   case GR_RuleNo_308:
   case GR_RuleNo_309:
      return (str.find('ß') != LgsString::npos);
   case GR_RuleNo_310:
      {
         int pos;
         if ((pos = str.find(',')) != LgsString::npos)
         {
            return (str.find(',', pos + 1) != LgsString::npos);
         }
         return false;
      }
   case GR_RuleNo_311:
      return (str.find(":  ") != LgsString::npos);
   case GR_RuleNo_312:
      return ((str.find("@QZ") != LgsString::npos) ||
              (str.find("@QD") != LgsString::npos));
   
   // Italian Rules
   case IT_RuleNo_400:
      return ((str.find("uno qwo") != LgsString::npos) ||
              (str.find("nessuno qwo") != LgsString::npos) ||
              (str.find("buono qwo") != LgsString::npos));
   case IT_RuleNo_401:
      return ((str.find("una qwa") != LgsString::npos) ||
              (str.find("nessuna qwa") != LgsString::npos) ||
              (str.find("buona qwa") != LgsString::npos));
   case IT_RuleNo_402:
      return (str.find("bello qwo") != LgsString::npos);
   case IT_RuleNo_403:
      return ((str.find("bello qwo") != LgsString::npos) ||
              (str.find("bello qwa") != LgsString::npos) ||
              (str.find("bella qwo") != LgsString::npos) ||
              (str.find("bella qwa") != LgsString::npos));
   case IT_RuleNo_404:
   case IT_RuleNo_405:
      return (str.find("belli qwi") != LgsString::npos);
   case IT_RuleNo_406:
      return ((str.find("San qwo") != LgsString::npos) ||
              (str.find("san qwo") != LgsString::npos) ||
              (str.find("San qwa") != LgsString::npos) ||
              (str.find("san qwa") != LgsString::npos));
   case IT_RuleNo_407:
      return ((str.find("San qwa") != LgsString::npos) ||
              (str.find("san qwa") != LgsString::npos));
   case IT_RuleNo_408:
   case IT_RuleNo_409:
   case IT_RuleNo_412:
      return (str.find("re @QZ @QD") != LgsString::npos);
   case IT_RuleNo_410:
   case IT_RuleNo_411:
   case IT_RuleNo_413:
      return ((str.find("o @QZ @QD") != LgsString::npos) ||
              (str.find("e @QZ @QD") != LgsString::npos));
   case IT_RuleNo_414:
      return ((str.find("i @QD l") != LgsString::npos) ||
              (str.find("i @QD ne") != LgsString::npos));
   case IT_RuleNo_415:
      return ((str.find("gli @QD") != LgsString::npos) ||
              (str.find("le @QD") != LgsString::npos));
   case IT_RuleNo_416:
   case IT_RuleNo_417:
   case IT_RuleNo_418:
      return ((str.find("a il") != LgsString::npos) ||
              (str.find("da il") != LgsString::npos) ||
              (str.find("su il") != LgsString::npos));
   case IT_RuleNo_419:
   case IT_RuleNo_420:
      return ((str.find("a i") != LgsString::npos) ||
              (str.find("da i") != LgsString::npos) ||
              (str.find("su i") != LgsString::npos));
   case IT_RuleNo_421:
      return ((str.find("a la") != LgsString::npos) ||
              (str.find("da la") != LgsString::npos) ||
              (str.find("su la") != LgsString::npos));
   case IT_RuleNo_422:
      return ((str.find("a l") != LgsString::npos) ||
              (str.find("da l") != LgsString::npos) ||
              (str.find("su l") != LgsString::npos));
   case IT_RuleNo_423:
      return ((str.find("di il") != LgsString::npos) ||
              (str.find("di la") != LgsString::npos));
   case IT_RuleNo_424:
   case IT_RuleNo_425:
      return (str.find("di il") != LgsString::npos);
   case IT_RuleNo_426:
   case IT_RuleNo_427:
      return (str.find("di i") != LgsString::npos);
   case IT_RuleNo_428:
      return ((str.find("di la") != LgsString::npos) ||
              (str.find("di le") != LgsString::npos));
   case IT_RuleNo_429:
      return ((str.find("in il") != LgsString::npos) ||
              (str.find("in la") != LgsString::npos));
   case IT_RuleNo_430:
   case IT_RuleNo_431:
      return (str.find("in il") != LgsString::npos);
   case IT_RuleNo_432:
   case IT_RuleNo_433:
      return (str.find("in i") != LgsString::npos);
   case IT_RuleNo_434:
      return ((str.find("in la") != LgsString::npos) ||
              (str.find("in le") != LgsString::npos));
   case IT_RuleNo_435:
      return (str.find("quello qd") != LgsString::npos);
   case IT_RuleNo_436:
      return (str.find("quello") != LgsString::npos);
   case IT_RuleNo_437:
      return (str.find("quelloqd") != LgsString::npos);
   case IT_RuleNo_438:
      return ((str.find("quella") != LgsString::npos) ||
              (str.find("quello") != LgsString::npos));
   case IT_RuleNo_439:
   case IT_RuleNo_440:
      return (str.find("quelli") != LgsString::npos);
   case IT_RuleNo_441:
      return ((str.find("ci è") != LgsString::npos) ||
              (str.find("ci era") != LgsString::npos) ||
              (str.find("ci erano") != LgsString::npos));
   case IT_RuleNo_442:
      return ((str.find("il ") != LgsString::npos) ||
              (str.find("la ") != LgsString::npos));
   case IT_RuleNo_443:
      return (str.find("il ") != LgsString::npos);
   case IT_RuleNo_444:
      return (str.find("i ") != LgsString::npos);
   case IT_RuleNo_445:
      return (str.find("l' ") != LgsString::npos);
   case IT_RuleNo_446:
      return (str.find("@QZ @QD") != LgsString::npos);
   case IT_RuleNo_447:
      return ((str.find("@QZ") != LgsString::npos) ||
              (str.find("@QD") != LgsString::npos) ||
              (str.find("qwo") != LgsString::npos) ||
              (str.find("qwa") != LgsString::npos) ||
              (str.find("qwi") != LgsString::npos) ||
              (str.find("qwe") != LgsString::npos));
   case IT_RuleNo_448:
      return ((str.find("più") != LgsString::npos) ||
              (str.find("meno") != LgsString::npos));

   // Spanish Rules
   case SP_RuleNo_500:
   case SP_RuleNo_501:
      return (str.find("r @QZ @QD") != LgsString::npos);
   case SP_RuleNo_502:
   case SP_RuleNo_503:
      return ((str.find("ando @QZ @QD") != LgsString::npos) ||
              (str.find("endo @QZ @QD") != LgsString::npos));
   case SP_RuleNo_504:
   case SP_RuleNo_506:
      return ((str.find("@QZ @QD me @QD") != LgsString::npos) ||
              (str.find("@QZ @QD te @QD") != LgsString::npos) ||
              (str.find("@QZ @QD se @QD") != LgsString::npos) ||
              (str.find("@QZ @QD nos @QD") != LgsString::npos));
   case SP_RuleNo_505:
   case SP_RuleNo_507:
      return ((str.find("@QZ @QD me") != LgsString::npos) ||
              (str.find("@QZ @QD te") != LgsString::npos) ||
              (str.find("@QZ @QD se") != LgsString::npos) ||
              (str.find("@QZ @QD nos") != LgsString::npos) ||
              (str.find("@QZ @QD l") != LgsString::npos));
   case SP_RuleNo_508:
      return ((str.find("primero @QZ") != LgsString::npos) ||
              (str.find("bueno @QZ") != LgsString::npos) ||
              (str.find("tercero @QZ") != LgsString::npos) ||
              (str.find("malo @QZ") != LgsString::npos) ||
              (str.find("uno @QZ") != LgsString::npos));
   case SP_RuleNo_509:
      return (str.find("santo @QZ") != LgsString::npos);
   case SP_RuleNo_510:
      return (str.find("grande @QZ") != LgsString::npos);
   case SP_RuleNo_511:
      return ((str.find("con mí") != LgsString::npos) ||
              (str.find("con sí") != LgsString::npos));
   case SP_RuleNo_512:
      return ((str.find("la á") != LgsString::npos) ||
              (str.find("la Á") != LgsString::npos) ||
              (str.find("la há") != LgsString::npos) ||
              (str.find("la hÁ") != LgsString::npos) ||
              (str.find("la Há") != LgsString::npos) ||
              (str.find("la HÁ") != LgsString::npos));
   case SP_RuleNo_513:
      return ((str.find("la a") != LgsString::npos) ||
              (str.find("la A") != LgsString::npos) ||
              (str.find("la ha") != LgsString::npos) ||
              (str.find("la hA") != LgsString::npos) ||
              (str.find("la Ha") != LgsString::npos) ||
              (str.find("la HA") != LgsString::npos));
   case SP_RuleNo_514:
      return ((str.find("una á") != LgsString::npos) ||
              (str.find("una Á") != LgsString::npos) ||
              (str.find("una há") != LgsString::npos) ||
              (str.find("una hÁ") != LgsString::npos) ||
              (str.find("una Há") != LgsString::npos) ||
              (str.find("una HÁ") != LgsString::npos));
   case SP_RuleNo_515:
      return ((str.find("una a") != LgsString::npos) ||
              (str.find("una A") != LgsString::npos) ||
              (str.find("una ha") != LgsString::npos) ||
              (str.find("una hA") != LgsString::npos) ||
              (str.find("una Ha") != LgsString::npos) ||
              (str.find("una HA") != LgsString::npos));
   case SP_RuleNo_516:
      return ((str.find("guna á") != LgsString::npos) ||
              (str.find("guna Á") != LgsString::npos) ||
              (str.find("guna há") != LgsString::npos) ||
              (str.find("guna hÁ") != LgsString::npos) ||
              (str.find("guna Há") != LgsString::npos) ||
              (str.find("guna HÁ") != LgsString::npos));
   case SP_RuleNo_517:
      return ((str.find("guna a") != LgsString::npos) ||
              (str.find("guna A") != LgsString::npos) ||
              (str.find("guna ha") != LgsString::npos) ||
              (str.find("guna hA") != LgsString::npos) ||
              (str.find("guna Ha") != LgsString::npos) ||
              (str.find("guna HA") != LgsString::npos));
   case SP_RuleNo_518:
      return (str.find("@ASPIRE") != LgsString::npos);
   case SP_RuleNo_519:
      return ((str.find("a el") != LgsString::npos) ||
              (str.find("de el") != LgsString::npos));
   case SP_RuleNo_520:
      return ((str.find("y h") != LgsString::npos) ||
              (str.find("y H") != LgsString::npos) ||
              (str.find("y i") != LgsString::npos) ||
              (str.find("y I") != LgsString::npos) ||
              (str.find("y í") != LgsString::npos) ||
              (str.find("y Í") != LgsString::npos));
   case SP_RuleNo_521:
      return ((str.find("o h") != LgsString::npos) ||
              (str.find("o H") != LgsString::npos) ||
              (str.find("o o") != LgsString::npos) ||
              (str.find("o O") != LgsString::npos) ||
              (str.find("o ó") != LgsString::npos) ||
              (str.find("o Ó") != LgsString::npos));
   case SP_RuleNo_522:
      return ((str.find("más ") != LgsString::npos) ||
              (str.find("menos ") != LgsString::npos));
   case SP_RuleNo_523:
      return (str.find("más más") != LgsString::npos);
   case SP_RuleNo_524:
      return (str.find("@QZ @QD") != LgsString::npos);
   case SP_RuleNo_525:
      return ((str.find("@QZ") != LgsString::npos) ||
              (str.find("@QD") != LgsString::npos));

   // Portuguese Rules
   case PO_RuleNo_600:
      return ((str.find("arei-@QD ") != LgsString::npos) ||
              (str.find("ará-@QD ") != LgsString::npos) ||
              (str.find("aria-@QD ") != LgsString::npos) ||
              (str.find("aremos-@QD ") != LgsString::npos) ||
              (str.find("arão-@QD ") != LgsString::npos) ||
              (str.find("ariam-@QD ") != LgsString::npos) ||
              (str.find("aríamos-@QD ") != LgsString::npos));
   case PO_RuleNo_601:
      return ((str.find("erei-@QD ") != LgsString::npos) ||
              (str.find("erá-@QD ") != LgsString::npos) ||
              (str.find("eria-@QD ") != LgsString::npos) ||
              (str.find("eremos-@QD ") != LgsString::npos) ||
              (str.find("erão-@QD ") != LgsString::npos) ||
              (str.find("eriam-@QD ") != LgsString::npos) ||
              (str.find("eríamos-@QD ") != LgsString::npos));
   case PO_RuleNo_602:
      return ((str.find("irei-@QD ") != LgsString::npos) ||
              (str.find("irá-@QD ") != LgsString::npos) ||
              (str.find("iria-@QD ") != LgsString::npos) ||
              (str.find("iremos-@QD ") != LgsString::npos) ||
              (str.find("irão-@QD ") != LgsString::npos) ||
              (str.find("iriam-@QD ") != LgsString::npos) ||
              (str.find("iríamos-@QD ") != LgsString::npos));
   case PO_RuleNo_603:
      return ((str.find("rei-@QD ") != LgsString::npos) ||
              (str.find("rá-@QD ") != LgsString::npos) ||
              (str.find("ria-@QD ") != LgsString::npos) ||
              (str.find("remos-@QD ") != LgsString::npos) ||
              (str.find("rámos-@QD ") != LgsString::npos) ||
              (str.find("rão-@QD ") != LgsString::npos) ||
              (str.find("riam-@QD ") != LgsString::npos) ||
              (str.find("ríamos-@QD ") != LgsString::npos));
   case PO_RuleNo_604:
      return ((str.find(" a o ") != LgsString::npos) ||
              (str.find(" a os ") != LgsString::npos));
   case PO_RuleNo_605:
      return ((str.find(" a a ") != LgsString::npos) &&
              (str.find("@QD a a ") == LgsString::npos));
   case PO_RuleNo_606:
      return (str.find(" a as ") != LgsString::npos);
   case PO_RuleNo_607:
      return ((str.find(" a aquele") != LgsString::npos) ||
              (str.find(" a aquela") != LgsString::npos));
   case PO_RuleNo_608:
      return ((str.find(" @QZ @QD me @QD ") != LgsString::npos) ||
              (str.find(" @QZ @QD te @QD ") != LgsString::npos) ||
              (str.find(" @QZ @QD lhe @QD ") != LgsString::npos) ||
              (str.find(" @QZ @QD se @QD ") != LgsString::npos));
//   case PO_RuleNo_609:
//      return ((str.find("lhe a") != LgsString::npos) ||
//              (str.find("lhe o") != LgsString::npos));
   case PO_RuleNo_610:
//      return ((str.find("nos a") != LgsString::npos) ||
//              (str.find("nos o") != LgsString::npos));
   case PO_RuleNo_611:
//      return ((str.find("me a") != LgsString::npos) ||
//              (str.find("me o") != LgsString::npos));
   case PO_RuleNo_612:
      return ((str.find("ar @QZ @QD me") != LgsString::npos) ||
              (str.find("ar @QZ @QD te") != LgsString::npos) ||
              (str.find("ar @QZ @QD lhe") != LgsString::npos) ||
              (str.find("ar @QZ @QD nos") != LgsString::npos) ||
              (str.find("ar @QZ @QD lhes") != LgsString::npos) ||
              (str.find("er @QZ @QD me") != LgsString::npos) ||
              (str.find("er @QZ @QD te") != LgsString::npos) ||
              (str.find("er @QZ @QD lhe") != LgsString::npos) ||
              (str.find("er @QZ @QD nos") != LgsString::npos) ||
              (str.find("er @QZ @QD lhes") != LgsString::npos) ||
              (str.find("ir @QZ @QD me") != LgsString::npos) ||
              (str.find("ir @QZ @QD te") != LgsString::npos) ||
              (str.find("ir @QZ @QD lhe") != LgsString::npos) ||
              (str.find("ir @QZ @QD nos") != LgsString::npos) ||
              (str.find("ir @QZ @QD lhes") != LgsString::npos));
   case PO_RuleNo_613:
      return ((str.find("ar @QZ @QD o") != LgsString::npos) ||
              (str.find("ar @QZ @QD a") != LgsString::npos) ||
              (str.find("ar @QZ @QD os") != LgsString::npos) ||
              (str.find("ar @QZ @QD as") != LgsString::npos));
   case PO_RuleNo_614:
      return ((str.find("er @QZ @QD o") != LgsString::npos) ||
              (str.find("er @QZ @QD a") != LgsString::npos) ||
              (str.find("er @QZ @QD os") != LgsString::npos) ||
              (str.find("er @QZ @QD as") != LgsString::npos));
   case PO_RuleNo_615:
      return ((str.find("ando @QZ @QD me @QD ") != LgsString::npos) ||
              (str.find("ando @QZ @QD te @QD ") != LgsString::npos) ||
              (str.find("ando @QZ @QD se @QD ") != LgsString::npos) ||
              (str.find("ando @QZ @QD lhe @QD ") != LgsString::npos) ||
              (str.find("endo @QZ @QD me @QD ") != LgsString::npos) ||
              (str.find("endo @QZ @QD te @QD ") != LgsString::npos) ||
              (str.find("endo @QZ @QD se @QD ") != LgsString::npos) ||
              (str.find("endo @QZ @QD lhe @QD ") != LgsString::npos));
   case PO_RuleNo_616:
      return ((str.find("ando @QZ @QD ") != LgsString::npos) ||
              (str.find("endo @QZ @QD ") != LgsString::npos) ||
              (str.find("indo @QZ @QD ") != LgsString::npos));
   case PO_RuleNo_617:
      return ((str.find("am-@QD a") != LgsString::npos) ||
              (str.find("am-@QD o") != LgsString::npos) ||
              (str.find("em-@QD a") != LgsString::npos) ||
              (str.find("em-@QD o") != LgsString::npos));
   case PO_RuleNo_618:
      return (str.find("mos-@QD nos") != LgsString::npos);
   case PO_RuleNo_619:
      return ((str.find("mos-@QD a") != LgsString::npos) ||
              (str.find("mos-@QD o") != LgsString::npos) ||
              (str.find("mos-@QD as") != LgsString::npos) ||
              (str.find("mos-@QD os") != LgsString::npos));
   case PO_RuleNo_620:
      return ((str.find(" @QZ @QD me ") != LgsString::npos) ||
              (str.find(" @QZ @QD te ") != LgsString::npos) ||
              (str.find(" @QZ @QD lhe ") != LgsString::npos) ||
              (str.find(" @QZ @QD o ") != LgsString::npos) ||
              (str.find(" @QZ @QD a ") != LgsString::npos) ||
              (str.find(" @QZ @QD se ") != LgsString::npos) ||
              (str.find(" @QZ @QD nos ") != LgsString::npos) ||
              (str.find(" @QZ @QD lhes ") != LgsString::npos) ||
              (str.find(" @QZ @QD os ") != LgsString::npos) ||
              (str.find(" @QZ @QD as ") != LgsString::npos));
   case PO_RuleNo_621:
      return (str.find("com mim") != LgsString::npos);
//   case PO_RuleNo_622:
//      return (str.find("com te") != LgsString::npos);
//   case PO_RuleNo_623:
//      return ((str.find("com ele") != LgsString::npos) ||
//              (str.find("com ela") != LgsString::npos));
   case PO_RuleNo_624:
      return (str.find("com você") != LgsString::npos);
   case PO_RuleNo_625:
      return ((str.find("com nós") != LgsString::npos) ||
              (str.find("com vós") != LgsString::npos));
   case PO_RuleNo_626:
      return ((str.find("de o") != LgsString::npos) ||
              (str.find("de os") != LgsString::npos) ||
              (str.find("de um") != LgsString::npos) ||
              (str.find("de uns") != LgsString::npos));
   case PO_RuleNo_627:
      return ((str.find("de a") != LgsString::npos) ||
              (str.find("de as") != LgsString::npos) ||
              (str.find("de uma") != LgsString::npos) ||
              (str.find("de umas") != LgsString::npos));
   case PO_RuleNo_628:
      return ((str.find("de ele") != LgsString::npos) ||
              (str.find("de ela") != LgsString::npos));
   case PO_RuleNo_629:
      return ((str.find("de este") != LgsString::npos) ||
              (str.find("de esse") != LgsString::npos) ||
              (str.find("de esta") != LgsString::npos) ||
              (str.find("de essa") != LgsString::npos));
   case PO_RuleNo_630:
      return ((str.find("de aquele") != LgsString::npos) ||
              (str.find("de aquela") != LgsString::npos));
   case PO_RuleNo_631:
      return ((str.find("de outro") != LgsString::npos) ||
              (str.find("de outra") != LgsString::npos));
   case PO_RuleNo_632:
      return ((str.find("em a") != LgsString::npos) ||
              (str.find("em as") != LgsString::npos) ||
              (str.find("em o") != LgsString::npos) ||
              (str.find("em os") != LgsString::npos) ||
              (str.find("em um") != LgsString::npos) ||
              (str.find("em uma") != LgsString::npos) ||
              (str.find("em uns") != LgsString::npos) ||
              (str.find("em umas") != LgsString::npos));
   case PO_RuleNo_633:
      return ((str.find("em essa") != LgsString::npos) ||
              (str.find("em esta") != LgsString::npos) ||
              (str.find("em esse") != LgsString::npos) ||
              (str.find("em este") != LgsString::npos));
   case PO_RuleNo_634:
      return ((str.find(" em aquele") != LgsString::npos) ||
              (str.find(" em aquela") != LgsString::npos));
   case PO_RuleNo_635:
      return ((str.find("em outro") != LgsString::npos) ||
              (str.find("em outra") != LgsString::npos));
   case PO_RuleNo_636:
      return ((str.find("por a") != LgsString::npos) ||
              (str.find("por o") != LgsString::npos));
   case PO_RuleNo_637:
      return (str.find("mais mais") != LgsString::npos);
//   case PO_RuleNo_638:
//      return ((str.find("r @QZ @QD me @QD ") != LgsString::npos) ||
//              (str.find("r @QZ @QD te @QD ") != LgsString::npos) ||
//              (str.find("r @QZ @QD se @QD ") != LgsString::npos) ||
//              (str.find("r @QZ @QD lhe @QD ") != LgsString::npos));
   case PO_RuleNo_639:
      return (str.find("@QZ @QD") != LgsString::npos);
   case PO_RuleNo_640:
      return ((str.find("@QZ") != LgsString::npos) ||
              (str.find("@QD") != LgsString::npos));
   case PO_RuleNo_642:
      return (str.find("mais ") != LgsString::npos);
   case PO_RuleNo_643:
      return (str.find("menos ") != LgsString::npos);
   case PO_RuleNo_644:
      return ((str.find("pôr @QZ @QD o") != LgsString::npos) ||
              (str.find("pôr @QZ @QD a") != LgsString::npos) ||
              (str.find("pôr @QZ @QD os") != LgsString::npos) ||
              (str.find("pôr @QZ @QD as") != LgsString::npos));
   case PO_RuleNo_645:
      return ((str.find("faz @QZ @QD o") != LgsString::npos) ||
              (str.find("faz @QZ @QD a") != LgsString::npos) ||
              (str.find("faz @QZ @QD os") != LgsString::npos) ||
              (str.find("faz @QZ @QD as") != LgsString::npos));
   case PO_RuleNo_646:
      return ((str.find(" a meu") != LgsString::npos) ||
              (str.find(" a teu") != LgsString::npos) ||
              (str.find(" a seu") != LgsString::npos) ||
              (str.find(" a nosso") != LgsString::npos) ||
              (str.find(" a vosso") != LgsString::npos));
//   case PO_RuleNo_647:
//      return ((str.find(" a minha") != LgsString::npos) ||
//              (str.find(" a tua") != LgsString::npos) ||
//              (str.find(" a sua") != LgsString::npos) ||
//              (str.find(" a nossa") != LgsString::npos) ||
//              (str.find(" a vossa") != LgsString::npos));
   case PO_RuleNo_648:
      return ((str.find(" de meu") != LgsString::npos) ||
              (str.find(" de teu") != LgsString::npos) ||
              (str.find(" de seu") != LgsString::npos) ||
              (str.find(" de nosso") != LgsString::npos) ||
              (str.find(" de vosso") != LgsString::npos));
   case PO_RuleNo_649:
      return ((str.find(" de minha") != LgsString::npos) ||
              (str.find(" de tua") != LgsString::npos) ||
              (str.find(" de sua") != LgsString::npos) ||
              (str.find(" de nossa") != LgsString::npos) ||
              (str.find(" de vossa") != LgsString::npos));
   case PO_RuleNo_650:
      return ((str.find(" em meu") != LgsString::npos) ||
              (str.find(" em teu") != LgsString::npos) ||
              (str.find(" em seu") != LgsString::npos) ||
              (str.find(" em nosso") != LgsString::npos) ||
              (str.find(" em vosso") != LgsString::npos));
   case PO_RuleNo_651:
      return ((str.find(" em minha") != LgsString::npos) ||
              (str.find(" em tua") != LgsString::npos) ||
              (str.find(" em sua") != LgsString::npos) ||
              (str.find(" em nossa") != LgsString::npos) ||
              (str.find(" em vossa") != LgsString::npos));
   case PO_RuleNo_652:
      return ((str.find(" por meu") != LgsString::npos) ||
              (str.find(" por teu") != LgsString::npos) ||
              (str.find(" por seu") != LgsString::npos) ||
              (str.find(" por nosso") != LgsString::npos) ||
              (str.find(" por vosso") != LgsString::npos));
   case PO_RuleNo_653:
      return ((str.find(" por minha") != LgsString::npos) ||
              (str.find(" por tua") != LgsString::npos) ||
              (str.find(" por sua") != LgsString::npos) ||
              (str.find(" por nossa") != LgsString::npos) ||
              (str.find(" por vossa") != LgsString::npos));
   case PO_RuleNo_654:
      return ((str.find("rei @QD se") != LgsString::npos) ||
              (str.find("rá @QD se") != LgsString::npos) ||
              (str.find("ria @QD se") != LgsString::npos) ||
              (str.find("remos @QD se") != LgsString::npos) ||
              (str.find("rámos @QD se") != LgsString::npos) ||
              (str.find("rão @QD se") != LgsString::npos) ||
              (str.find("riam @QD se") != LgsString::npos) ||
              (str.find("ríamos @QD se") != LgsString::npos));
   default:
      return true;
   }*/
}

//}
