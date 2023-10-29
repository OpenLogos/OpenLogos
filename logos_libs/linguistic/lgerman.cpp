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
// File - LGerman.cpp
//
// Class - LGerman
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/lgerman.h>
#include <logos_libs/startrules/startruleslocale.h>

LGerman::LGerman()
        :LLanguage(GermanID, "German")
{
   initCombiningForms();
   setVowels("aeiouy");
   setConsonants("bcdfghjklmnpqrstvwxyz");
   setDiacritics("aou");
}

LgsString LGerman::applyCombiningFormCode(int code, LgsString& target)
{
   switch (code)
   {
   case 1:
   case 2:
   case 3:
   case 4:
   case 5:
   case 6:
   case 7:
      return target + v_combiningForm[code];
      break;
   case 8:
      if (target.substr(target.length() - 1, 1) == "n")
         return target.substr(0, target.length() - 1);
      else
         return target;
      break;
   case 9:
      if (target.substr(target.length() - 2, 2) == "en")
         return target.substr(0, target.length() - 2);
      else
         return target;
      break;
   default:
      return target;
      break;
   }
}

const LgsString* LGerman::combiningForm(int code)
{
   if ((code > 0) && (code < 8))
   {
      return &v_combiningForm[code];
   }
   return 0;
}

void LGerman::initCombiningForms()
{
   v_combiningForm[1] = "s" ;
   v_combiningForm[2] = "es";
   v_combiningForm[3] = "n" ;
   v_combiningForm[4] = "en";
   v_combiningForm[5] = "er";
   v_combiningForm[6] = "-" ;
   v_combiningForm[7] = "" ;
}

bool LGerman::isOrdinalSuffix( const LgsString& s ) const
{
   return s == ".";
}

