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
#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/scontable.h>
#include <logos_libs/translutility/translcommonobjects.h>

const int MAX_SCON_ELEMENTS = 20;

SconTable::SconTable()
{
   sconVector.reserve(MAX_SCON_ELEMENTS);

   // Determine the correct degree index.
   if (TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::GermanID)
   {
	   degreeIndex = GERMAN_TARGET_DEGREE_INDEX;
   }
   else
   {
	   degreeIndex = OTHER_TARGET_DEGREE_INDEX;
   }

   scon45_ = 0;		// default value
}

SconTable::SconTable(const SconTable& rhs)
          :sconVector(rhs.sconVector)
{
   // Determine the correct degree index.
   if (TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::GermanID)
   {
	   degreeIndex = GERMAN_TARGET_DEGREE_INDEX;
   }
   else
   {
	   degreeIndex = OTHER_TARGET_DEGREE_INDEX;
   }

   scon45_ = 0;		// default value
}

SconTable::~SconTable()
{
}

void SconTable::checkSconValues()
{
   // If the wordclass code is for Auxiliary, change the word class to Verb.
   if (wordClass() == LLanguage::AUXILIARY)
   {
      sconVector[SOURCE_WORD_CLASS_INDEX] = (short)LLanguage::VERB;
   }

   // Check each of the scon characteric values.
   // Tense must be checked first because it is an integral part of the check performed
   // on gender for verbs.
   checkCaseTense();
   checkGender();
   checkNumber();
   checkDegree();
   checkPerson();
   checkDeclension();
}

void SconTable::checkCaseTense()
{
   // Check the permissible values for the case / tense value based on language and wordclass
   // code. If the values do not fall into acceptable ranges then set to default values.
   LLanguage::ID targetLanguage = TranslCommonObjects::GetTargetLanguage()->id();
   int wordClassCode = wordClass();
   int caseTenseValue = sconVector[TARGET_CASE_INDEX];

   // Process based on word class code and then language code.
   switch (wordClassCode)
   {
   case LLanguage::NOUN:
      switch (targetLanguage)
      {
      case LLanguage::GermanID:
      case LLanguage::FrenchID:
      case LLanguage::SpanishID:
      case LLanguage::ItalianID:
      case LLanguage::PortugueseID:
         // Acceptable values: 1, 2, 3 or 4.
         // If any other value, set to 1.
         if (!((caseTenseValue >= 1) && (caseTenseValue <= 4)))
         {
            sconVector[TARGET_CASE_INDEX] = 1;
         }
         break;
      case LLanguage::EnglishID:
         // Acceptable values: 1 or 2.
         // If value is 5, set to 2. If any other value, set to 1.
         if (!((caseTenseValue == 1) || (caseTenseValue == 2)))
         {
            if (caseTenseValue == 5)
               sconVector[TARGET_CASE_INDEX] = 2;
            else
               sconVector[TARGET_CASE_INDEX] = 1;
         }
         break;
      default:
         break;
      }
      break;

   case LLanguage::VERB:
      switch (targetLanguage)
      {
      case LLanguage::GermanID:
         // Acceptable values: 1 through 17 inclusive.
         // If any other value, set to 4.
         if (!((caseTenseValue >= 1) && (caseTenseValue <= 17)))
         {
            sconVector[TARGET_CASE_INDEX] = 1;
         }
         break;
      case LLanguage::EnglishID:
         // Acceptable values: 1 through 5 inclusive.
         // If any other value, set to 1.
         if (!((caseTenseValue >= 1) && (caseTenseValue <= 5)))
         {
            sconVector[TARGET_CASE_INDEX] = 1;
         }
         break;
      case LLanguage::FrenchID:
      case LLanguage::SpanishID:
      case LLanguage::ItalianID:
         // Acceptable values: 1 through 13 inclusive.
         // If any other value, set to 1.
         if (!((caseTenseValue >= 1) && (caseTenseValue <= 13)))
         {
            sconVector[TARGET_CASE_INDEX] = 1;
         }
         break;
      case LLanguage::PortugueseID:
         // Acceptable values: 1 through 15 inclusive.
         // If any other value, set to 1.
         if (!((caseTenseValue >= 1) && (caseTenseValue <= 15)))
         {
            sconVector[TARGET_CASE_INDEX] = 1;
         }
         break;
      default:
         break;
      }
      break;

   case LLanguage::ADJECTIVE:
      if (targetLanguage == LLanguage::GermanID)
      {
         // Acceptable values: 1 through 5 inclusive.
         // If any other value, set to 1.
         if (!((caseTenseValue >= 1) && (caseTenseValue <= 5)))
         {
            sconVector[TARGET_CASE_INDEX] = 1;
         }
      }
      break;

   case LLanguage::PRONOUN:
      switch (targetLanguage)
      {
      case LLanguage::GermanID:
      case LLanguage::EnglishID:
         // Acceptable values: 1, 2, 3 or 4.
         // If any other value, set to 1.
         if (!((caseTenseValue >= 1) && (caseTenseValue <= 4)))
         {
            sconVector[TARGET_CASE_INDEX] = 1;
         }
         break;
      case LLanguage::FrenchID:
      case LLanguage::SpanishID:
      case LLanguage::ItalianID:
      case LLanguage::PortugueseID:
         // Acceptable values: 1 through 6 inclusive.
         // If any other value, set to 1.
         if (!((caseTenseValue >= 1) && (caseTenseValue <= 6)))
         {
            sconVector[TARGET_CASE_INDEX] = 1;
         }
         break;
      default:
         break;
      }
      break;

   case LLanguage::ARTICLE_DEFINITE:
   case LLanguage::ARTICLE_INDEFINITE:
   case LLanguage::ARITHMATE:
      if (targetLanguage == LLanguage::GermanID)
      {
         // Acceptable values: 1, 2, 3 or 4.
         // If any other value, set to 1.
         if (!((caseTenseValue >= 1) && (caseTenseValue <= 5)))
         {
            sconVector[TARGET_CASE_INDEX] = 1;
         }
      }
      break;

   case LLanguage::PRONOUN_RELATIVE:
      switch (targetLanguage)
      {
      case LLanguage::GermanID:
      case LLanguage::EnglishID:
         // Acceptable values: 1, 2, 3 or 4.
         // If any other value, set to 1.
         if (!((caseTenseValue >= 1) && (caseTenseValue <= 4)))
         {
            sconVector[TARGET_CASE_INDEX] = 1;
         }
         break;
      case LLanguage::FrenchID:
      case LLanguage::SpanishID:
      case LLanguage::ItalianID:
      case LLanguage::PortugueseID:
         // Acceptable values: 1 through 5 inclusive.
         // If any other value, set to 1.
         if (!((caseTenseValue >= 1) && (caseTenseValue <= 5)))
         {
            sconVector[TARGET_CASE_INDEX] = 1;
         }
         break;
      default:
         break;
      }
      break;

   default:
      break;
   }
}

void SconTable::checkGender()
{
   // Check the permissible values for the gender value based on language and wordclass
   // code. If the values do not fall into acceptable ranges then set to default values.
   LLanguage::ID targetLanguage = TranslCommonObjects::GetTargetLanguage()->id();
   int wordClassCode = wordClass();
   int genderValue = sconVector[TARGET_GENDER_INDEX];

   // Process based on word class code and then language code.
   switch (wordClassCode)
   {
   case LLanguage::NOUN:
      switch (targetLanguage)
      {
      case LLanguage::GermanID:
         // Acceptable values: 1, 2 or 3.
         // If value is 5 or 8 then set to 2.
         // If value is 6 or 9 then set to 3.
         // If any other unacceptable value, set to 1.
         if (!((genderValue >= 1) && (genderValue <= 3)))
         {
            if ((genderValue == 5) || (genderValue == 8))
            {
               sconVector[TARGET_GENDER_INDEX] = 2;
            }
            else if ((genderValue == 6) || (genderValue == 9))
            {
               sconVector[TARGET_GENDER_INDEX] = 3;
            }
            else
            {
               sconVector[TARGET_GENDER_INDEX] = 1;
            }
         }
         break;
      case LLanguage::FrenchID:
      case LLanguage::SpanishID:
      case LLanguage::ItalianID:
      case LLanguage::PortugueseID:
         // Acceptable values: 1 or 2.
         // If value is 5 or 8 then set to 2.
         // If any other unacceptable value, set to 1.
         if (!((genderValue == 1) || (genderValue == 2)))
         {
            if ((genderValue == 5) || (genderValue == 8))
            {
               sconVector[TARGET_GENDER_INDEX] = 2;
            }
            else
            {
               sconVector[TARGET_GENDER_INDEX] = 1;
            }
         }
         break;
      default:
         break;
      }
      break;

   case LLanguage::VERB:
   {
      // Get the tense value first.
      int tenseValue = sconVector[TARGET_TENSE_INDEX];
      switch (targetLanguage)
      {
      case LLanguage::GermanID:
         // Check only if tense is 14, 15, 16 or 17.
         if ((tenseValue >= 14) && (tenseValue <= 17))
         {
            // Acceptable values: 1, 2 or 3.
            // If any other value, set to 1.
            if (!((genderValue >= 1) && (genderValue <= 3)))
            {
               sconVector[TARGET_GENDER_INDEX] = 1;
            }
         }
         break;
      case LLanguage::FrenchID:
      case LLanguage::SpanishID:
      case LLanguage::ItalianID:
         // Check only if tense is 10 or 11.
         if ((tenseValue == 10) || (tenseValue == 11))
         {
            // Acceptable value: 1 or 2.
            // If any other value, set to 1.
            if (!((genderValue == 1) || (genderValue == 2)))
            {
               sconVector[TARGET_GENDER_INDEX] = 1;
            }
         }
         break;
      case LLanguage::PortugueseID:
         // Check only if tense is 12 or 13.
         if ((tenseValue == 12) || (tenseValue == 13))
         {
            // Acceptable value: 1 or 2.
            // If any other value, set to 1.
            if (!((genderValue == 1) || (genderValue == 2)))
            {
               sconVector[TARGET_GENDER_INDEX] = 1;
            }
         }
         break;
      default:
         break;
      }
      break;
   }

   case LLanguage::ADJECTIVE:
      switch (targetLanguage)
      {
      case LLanguage::GermanID:
         // Acceptable values: 1, 2 or 3.
         // If any other value, set to 1.
         if (!((genderValue >= 1) && (genderValue <= 3)))
         {
            sconVector[TARGET_GENDER_INDEX] = 1;
         }
         break;
      case LLanguage::FrenchID:
      case LLanguage::SpanishID:
      case LLanguage::ItalianID:
      case LLanguage::PortugueseID:
         // Acceptable value: 1 or 2.
         // If any other value, set to 1.
         if (!((genderValue == 1) || (genderValue == 2)))
         {
            sconVector[TARGET_GENDER_INDEX] = 1;
         }
         break;
      default:
         break;
      }
      break;

   case LLanguage::PRONOUN:
      switch (targetLanguage)
      {
      case LLanguage::GermanID:
      case LLanguage::EnglishID:
      case LLanguage::FrenchID:
      case LLanguage::SpanishID:
         // Acceptable values: 1, 2 or 3.
         // If any other value, set to 1.
         if (!((genderValue >= 1) && (genderValue <= 3)))
         {
            sconVector[TARGET_GENDER_INDEX] = 1;
         }
         break;
      case LLanguage::ItalianID:
      case LLanguage::PortugueseID:
         // Acceptable value: 1 or 2.
         // If any other value, set to 1.
         if (!((genderValue == 1) || (genderValue == 2)))
         {
            sconVector[TARGET_GENDER_INDEX] = 1;
         }
         break;
      default:
         break;
      }
      break;

   case LLanguage::ARTICLE_DEFINITE:
   case LLanguage::ARTICLE_INDEFINITE:
   case LLanguage::ARITHMATE:
   case LLanguage::PRONOUN_RELATIVE:
      switch (targetLanguage)
      {
      case LLanguage::GermanID:
      case LLanguage::SpanishID:
         // Acceptable values: 1, 2 or 3.
         // If any other value, set to 1.
         if (!((genderValue >= 1) && (genderValue <= 3)))
         {
            sconVector[TARGET_GENDER_INDEX] = 1;
         }
         break;
      case LLanguage::FrenchID:
      case LLanguage::ItalianID:
      case LLanguage::PortugueseID:
         // Acceptable value: 1 or 2.
         // If any other value, set to 1.
         if (!((genderValue == 1) || (genderValue == 2)))
         {
            sconVector[TARGET_GENDER_INDEX] = 1;
         }
         break;
      default:
         break;
      }
      break;

   default:
      break;
   }
}

void SconTable::checkNumber()
{
   // Check the permissible values for the number value based on language and wordclass
   // code. If the values do not fall into acceptable ranges then set to default values.
   LLanguage::ID targetLanguage = TranslCommonObjects::GetTargetLanguage()->id();
   int wordClassCode = wordClass();
   int numberValue = sconVector[TARGET_NUMBER_INDEX];

   // Process based on word class code and then language code.
   switch (wordClassCode)
   {
   case LLanguage::NOUN:
   case LLanguage::VERB:
   case LLanguage::PRONOUN:
   case LLanguage::ARTICLE_DEFINITE:
   case LLanguage::ARTICLE_INDEFINITE:
   case LLanguage::ARITHMATE:
   case LLanguage::PRONOUN_RELATIVE:
      switch (targetLanguage)
      {
      case LLanguage::GermanID:
      case LLanguage::EnglishID:
      case LLanguage::FrenchID:
      case LLanguage::SpanishID:
      case LLanguage::ItalianID:
      case LLanguage::PortugueseID:
         // Acceptable values: 1 or 2.
         // If any other value, set to 1.
         if (!((numberValue == 1) || (numberValue == 2)))
            sconVector[TARGET_NUMBER_INDEX] = 1;
         break;
      default:
         break;
      }
      break;

   case LLanguage::ADJECTIVE:
      switch (targetLanguage)
      {
      case LLanguage::GermanID:
      case LLanguage::EnglishID:
      case LLanguage::FrenchID:
      case LLanguage::SpanishID:
      case LLanguage::ItalianID:
      case LLanguage::PortugueseID:
         // Acceptable values: 1 or 2.
         // If any other value, set to 1.
         if (!((numberValue == 1) || (numberValue == 2)))
            sconVector[TARGET_NUMBER_INDEX] = 1;
         break;
      default:
         break;
      }
      break;

   default:
      break;
   }
}

void SconTable::checkDegree()
{
   // Check the permissible values for the degree value based on language and wordclass
   // code. If the values do not fall into acceptable ranges then set to default values.
   LLanguage::ID targetLanguage = TranslCommonObjects::GetTargetLanguage()->id();
   int wordClassCode = wordClass();

   // The location of the degree value is dependent on the target language.
   int degreeValue = sconVector[OTHER_TARGET_DEGREE_INDEX];

   // Process based on word class code and then language code.
   switch (wordClassCode)
   {
   case LLanguage::ADVERB_LOCATIVE:
   case LLanguage::ADVERB:
   case LLanguage::ADJECTIVE:
      switch (targetLanguage)
      {
      case LLanguage::GermanID:
         // The degree value comes from a different location for the German target.
         degreeValue = sconVector[GERMAN_TARGET_DEGREE_INDEX];

         // Acceptable values: 1, 2 or 3.
         // If value is 4 then set to 2.
         // If value is 5 then set to 3.
         // If any other unacceptable value, set to 1.
         if (!((degreeValue >= 1) && (degreeValue <= 3)))
         {
            if (degreeValue == 4)
            {
               sconVector[GERMAN_TARGET_DEGREE_INDEX] = 2;
            }
            else if (degreeValue == 5)
            {
               sconVector[GERMAN_TARGET_DEGREE_INDEX] = 3;
            }
            else
            {
               sconVector[GERMAN_TARGET_DEGREE_INDEX] = 1;
            }
         }
         break;
      case LLanguage::EnglishID:
         // Acceptable values: 1, 2 or 3.
         // If value is 5 then set to 2.
         // If value is 6 then set to 3.
         // If any other unacceptable value, set to 1.
         if (!((degreeValue >= 1) && (degreeValue <= 3)))
         {
            if (degreeValue == 5)
            {
               sconVector[OTHER_TARGET_DEGREE_INDEX] = 2;
            }
            else if (degreeValue == 6)
            {
               sconVector[OTHER_TARGET_DEGREE_INDEX] = 3;
            }
            else
            {
               sconVector[OTHER_TARGET_DEGREE_INDEX] = 1;
            }
         }
         break;
      case LLanguage::FrenchID:
      case LLanguage::SpanishID:
      case LLanguage::ItalianID:
      case LLanguage::PortugueseID:
         // Acceptable values: 1 or 8.
         // If any other value, set to 1.
         if (!((degreeValue == 1) || (degreeValue == 8)))
         {
            sconVector[OTHER_TARGET_DEGREE_INDEX] = 1;
         }
         break;
      default:
         break;
      }
      break;

   default:
      break;
   }
}

void SconTable::checkPerson()
{
   // Check the permissible values for the person value based on language and wordclass
   // code. If the values do not fall into acceptable ranges then set to default values.
   LLanguage::ID targetLanguage = TranslCommonObjects::GetTargetLanguage()->id();
   int wordClassCode = wordClass();
   int personValue = sconVector[TARGET_PERSON_INDEX];

   // Process based on word class code and then language code.
   switch (wordClassCode)
   {
   case LLanguage::VERB:
   case LLanguage::PRONOUN:
      switch (targetLanguage)
      {
      case LLanguage::GermanID:
      case LLanguage::EnglishID:
      case LLanguage::FrenchID:
      case LLanguage::SpanishID:
      case LLanguage::ItalianID:
      case LLanguage::PortugueseID:
         // Acceptable values: 1, 2 or 3.
         // If any other value, set to 3.
         if (!((personValue >= 1) && (personValue <= 3)))
            sconVector[TARGET_PERSON_INDEX] = 3;
         break;
      default:
         break;
      }
      break;

   default:
      break;
   }
}

void SconTable::checkDeclension()
{
   // Check the permissible values for the declension value based on language and wordclass
   // code. If the values do not fall into acceptable ranges then set to default values.
   LLanguage::ID targetLanguage = TranslCommonObjects::GetTargetLanguage()->id();
   int wordClassCode = wordClass();
   int declensionValue = sconVector[TARGET_DECLENSION_INDEX];

   // Process based on word class code and then language code.
   if (wordClassCode == LLanguage::ADJECTIVE)
   {
      // Only German targets need to be checked.
      if (targetLanguage == LLanguage::GermanID)
      {
         // Accecptable values: 3, 6 or 9.
         // If any other value, set to 9.
         if (!((declensionValue == 3) || (declensionValue == 6) || (declensionValue == 9)))
            sconVector[TARGET_DECLENSION_INDEX] = 3;
      }
   }
}

void SconTable::zeroSconTable()
{
   // Zero all the entries in the scon.
   for (int index = 0; index < MAX_SCON_ELEMENTS; index++)
      sconVector[index] = 0;
}

void SconTable::setScon(int whichScon, short newValue)
{
   // Make sure the index is within acceptable range.
   if ((whichScon >= FIRST_SCON_INDEX) && (whichScon <= LAST_SCON_INDEX))
      sconVector[whichScon] = newValue;
}

void SconTable::setAsDummyBOS()
{
   // Create a scon to act as the scon for a BOS to be inserted in a target sentence that
   // had it's BOS dropped in TRAN 4.
   short element = 0;
   for (int iter = FIRST_SCON_INDEX; iter <= LAST_SCON_INDEX; iter++)
      sconVector.push_back(element);
   setWordClass(20);
   setSubSetOf(1);
   setSourceSentenceUnitPosition(1);
   setSuperSetOf(1);
}

ostream& operator<<(ostream& stream, SconTable& object)
{
   SconTable::VectorOfShorts& sconVector = object.getVectorOfValues();
   short n = sconVector.size();

   stream.write((const char *)&n, sizeof(n));

   for (SconTable::VectorOfShorts::const_iterator i = sconVector.begin();
        (i < sconVector.end()); i++)
   {
      stream.write((const char *)&(*i), sizeof(*i));
   }
   return stream;
}

void streamInSconTable(char * & dataIn, SconTable& scon)
{
   SconTable::VectorOfShorts& sconVector = scon.getVectorOfValues();
   short totalElements = 0;
   memcpy((char *)&totalElements, dataIn, sizeof(totalElements));
   dataIn += sizeof(totalElements);

   // Read the first MAX_SCON_ELEMENTS valid elements and store them in the vector.
   for (int index1 = 0; index1 < MAX_SCON_ELEMENTS; index1++)
   {
      short element;

      memcpy((char *)&element, dataIn, sizeof(element));
	   dataIn += sizeof(element);
      sconVector.push_back(element);
   }

   // Read and discard the rest of the elements that are not needed.
   for (int index2 = MAX_SCON_ELEMENTS; index2 < totalElements; index2++)
   {
	   short waste;
	   memcpy((char *)&waste, dataIn, sizeof(waste));
	   dataIn += sizeof(waste);

	   // capture scon45 value if exist
	   if (index2 == 44)
	   {
		   scon.setScon45(waste);
	   }
   }
}
