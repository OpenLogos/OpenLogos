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
// File - germanordinal.cpp
//
// Class - ST_GermanOrdinalAction
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/startrules/germanordinal.h>
#include <logos_libs/translutility/translcommonobjects.h>
#include <logos_libs/utility/iniparser.h>
#include <configdatafileinterface/configdatainterfacemain.h>

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// removed namespace due to namespace bugs with VC++ 5.0
// namespace Start {

ST_GermanOrdinalAction::ST_GermanOrdinalAction(const ST_Locale& sourceLocale, const ST_Locale& targetLocale)
                       :ST_Action(sourceLocale, targetLocale)
{
   char generalTableFileName[MAX_FILEPATH_LEN];

   if (GetConfigData("sourcedata", "generaltbl", generalTableFileName, MAX_FILEPATH_LEN) == CONFDATA_SUCCESS)
   {
      IniParser argumentParser;
      argumentParser.open(generalTableFileName, "Language");

      const Argument & arg = argumentParser.GetArgument("OrdinalPrefixList");
      if (!arg.IsNull())
      {
	      StringUtil::parseInto(arg.Value(), _ordinalPrefixList, ',');
      }
   }
}

void ST_GermanOrdinalAction::fire(ST_Variable& variable) const
{
	if (TranslCommonObjects::GetSourceLanguage()->id() == LLanguage::GermanID)
	{

		if (variable.getSubString(5).length() != 0)
		{
			LgsString prefixStr(variable.getSubString(6));
			StringUtil::toLower(prefixStr);
			if (!binary_search(_ordinalPrefixList.begin(), _ordinalPrefixList.end(), prefixStr))
			{
				return;
			}
			int numericVal = atoi(variable.getSubString(7).c_str());
			variable.setTokenType(getTokType(numericVal), 7, false);

			if (variable.getSubString(9).length() != 0)
			{
				PhraseManager::Key startKey, endKey;
				variable.getKeys(startKey, endKey, 9);
				LWordIterator beginWord = variable.getStartWord() + startKey;
				LWordIterator endWord = variable.getStartWord()+ endKey;
				LWordIterator currElem;
				for (currElem = beginWord + 1; currElem < endWord; currElem++)
				{
					if (StringUtil::isAllNumeric(currElem->c_str()))
					{
						numericVal = atoi(currElem->c_str());	
						currElem->setTokenType(getTokType(numericVal));
					}
				}

				RegularExpression::MatchPos match = variable.getMatch(9);
				short currPos = match.pos;

				
				for (currElem = beginWord+1; currElem < endWord; currElem++)
				{
					if (*currElem == ".")
					{
						if (currElem != variable.getEndWord())
						{
							variable.replace(currPos, 1, LgsString(), false);
						}
					} else
					{
						currPos += currElem->precedingSpaces() + currElem->length();
					}
				}
			}
			variable.remove(8);

		} else
		{
			int numericVal = atoi(variable.getSubString(2).c_str());
			variable.setTokenType(getTokType(numericVal), 2, false);
			variable.remove(3);
		}
	}
}

LookupTokenType::Type ST_GermanOrdinalAction::getTokType(int numericVal) const
{
	if (numericVal > 9 && numericVal < 21)
	{
		return LookupTokenType::tok_Adj_Num_4_G;
	} 

	if (numericVal == 1)
	{
		return LookupTokenType::tok_Adj_Num_1_G;
	}

	short valmodten = numericVal % 10;

	switch (valmodten)
	{
	case 1:
		return LookupTokenType::tok_Adj_Num_21_G;
		break;
	case 2:
		return LookupTokenType::tok_Adj_Num_2_G;
		break;
	case 3:
		return LookupTokenType::tok_Adj_Num_3_G;
		break;
	case 4:
		return LookupTokenType::tok_Adj_Num_4_G;
		break;
	default:
		return LookupTokenType::tok_Adj_Num_4_G;
		break;
	}
}

