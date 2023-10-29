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
/*******************************************************************
 *
 *    DESCRIPTION:
 *
 *    AUTHOR:
 *
 *    HISTORY:    
 *
 *******************************************************************/

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/llanguage.h>
#include <lgs_stemgen/prefixrule.h>

PrefixRule
::PrefixRule(const LgsString& ruleString_,
             const LgsString& separable_,
             const LgsString& inseparable_,
             const LLanguage& l_)
   : _language(l_)
{
   if (!ruleString_.empty())
   {
      _initialize(ruleString_, separable_, inseparable_);
   }
}

PrefixRule
::~PrefixRule()
{
}

void 
PrefixRule
::_initialize(const LgsString& ruleString_,
              const LgsString& separable_,
              const LgsString& inseparable_)
{
   int cursor = 0;

   while (cursor < ruleString_.length())
   {
      if ('&' == ruleString_[cursor])
      {
         if ('-' == ruleString_[++cursor])
         {
            _prefixList.push_back(separable_);
         }
         else if ('+' == ruleString_[cursor])
         {
            _prefixList.push_back(inseparable_);
         }
         
         ++cursor;
      }
      else
      {
         int pos = ruleString_.find('&', cursor);
         LgsString literal = 
            ruleString_.substr(cursor, 
                               pos != LgsString::npos ? pos - cursor : pos);
         
         if (!literal.empty()) 
         {
            // a special treatment for 'zu'
            if (_language.id() == LLanguage::GermanID 
               && separable_[separable_.size() - 1] == ' '
               && (literal == "zu" || literal == "ZU"
               || literal == "Zu" || literal == "zU"))
            {
               literal += '_';
            }
            
            _prefixList.push_back(literal);
         }
         
         cursor = pos;
      }
   }
}

void 
PrefixRule
::fire(LgsString* text_, const LgsString& stem_)
{
   for (LgsVector(LgsString)::iterator i = _prefixList.begin();
      i != _prefixList.end(); ++i)
   {
      text_->append(*i);
   }
   
   text_->append (stem_);
}
