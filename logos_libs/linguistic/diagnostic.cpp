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
// File - diagnostic.cpp
//
// Class - Diagnostic (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/diagnostic.h>
#include <logos_libs/linguistic/lsentence.h>
#include <logos_libs/linguistic/swork.h>
#include <logos_libs/linguistic/targetdictionaryunit.h>
#include <logos_libs/gerdem/gfactory.h>
#include <logos_libs/utility/argumentexception.h>
#include <logos_libs/translutility/translcommonobjects.h>

//-------------------------------------------------------------------
Diagnostic::Diagnostic()
           :v_startLine(0),
            v_endLine(0),
            v_currLine(0),
            v_blockOutput(true),
            v_outputRestricted(false),
            v_diagnosticsOn(false)

{
}
//-------------------------------------------------------------------
Diagnostic::Diagnostic(int diagType, const LgsString& streamName, int startingLine,
                       int endingLine)
           :v_startLine(startingLine),
            v_endLine(endingLine),
            v_currLine(0),
            v_blockOutput(true),
            v_outputRestricted(false)

{
   v_diagnosticsOn = (diagType >= 2) && (diagType != 5);

   if (v_diagnosticsOn)
   {
      p_stream = new ofstream(streamName.c_str());
      if (!p_stream->good())
      {
         throw(ArgumentException("Unable to open diagnostic file!"));
      }
      if ((v_startLine > 0) && (v_endLine > 0) && (v_startLine <= v_endLine))
         v_outputRestricted = true;
      else
         v_blockOutput = false;
   }
   else
      p_stream = (ostream *)0;
}
//-------------------------------------------------------------------
Diagnostic::~Diagnostic()
{
   if (p_stream)
      delete p_stream;
}
//-------------------------------------------------------------------
void Diagnostic::flush()
{
   if (!v_blockOutput)
      stream().flush();
}
//-------------------------------------------------------------------
void Diagnostic::sconCompare(LSentence& sent)
{
   if (!v_blockOutput)
   {
      stream() << "OPADR CASE DECL DEGR GEND NUMB PERS TENS  PAT   WC SC08  SSU CC  SMC" << endl;

      for (TargetUnitIterator i =  sent.targetSentenceUnits().begin();
           i != sent.targetSentenceUnits().end(); ++i)
      {
         char buffer[100];
         char cmpyCode[4];
         char smc[16];

         sprintf(cmpyCode, "%3s", (*i)->companyCode().c_str());
         if ((*i)->isTargetDictionaryUnit())
         {
            sprintf(smc, "%15s", ((TargetDictionaryUnit*)(*i))->primarySsu().subjectMatterCode().formattedContent().c_str());
         }
         else
         {
            sprintf(smc, "%15s", "               ");
         }
         sprintf(buffer, "%5d %4d %4d %4d %4d %4d %4d %4d %4d %4d %4d %4d %3s %15s ",
                 (*i)->opadr(), (*i)->caseOf(), (*i)->declension(),
                 (*i)->degree(), (*i)->gender(), (*i)->number(),
                 (*i)->person(), (*i)->tense(), (*i)->patNumber(),
                 (*i)->wordClassCode(), (*i)->transferSwitch(),
                 (*i)->sourcePrimarySsuPosition(), cmpyCode, smc);
         stream() << buffer << (*i)->surfaceExpressionAsString() << endl;

         if ((*i)->isTargetDictionaryUnit() && (((TargetDictionaryUnit*)(*i))->wordCount() >= 2))
         {
            if (!(((*i)->language().id() == LLanguage::GermanID) && ((*i)->wordClassCode() == 2)))
            {
               LWordVector& words = (*i)->surfaceWords();
               int location = 0;
               WordInPhraseQuery& wordInPhraseQuery = TranslCommonObjects::GetPersistDataFactory()->getWordInPhraseQuery();

               for (LWordIterator nextWord = words.begin(); nextWord != words.end(); nextWord++)
               {
                  if (++location != ((TargetDictionaryUnit*)(*i))->headWord())
                  {
                     int patNumber = 0;
                     int wordClassCode = 0;

                     wordInPhraseQuery.ExecuteWithWord(((TargetDictionaryUnit*)(*i))->wordID(), location);

                     if (wordInPhraseQuery.FetchIntoPatNumber (&patNumber, &wordClassCode))
                     {
                        sprintf(buffer, "                                         %4d %4d                               ",
                                patNumber, wordClassCode);
                        stream() << buffer << (*nextWord) << endl;
                     }
                  }
               }
            }
         }
      }
      stream() << endl;
   }
}
//-------------------------------------------------------------------
void Diagnostic::write(const LgsString& str)
{
   if (!v_blockOutput)
      stream() << str;
}
//-------------------------------------------------------------------
void Diagnostic::writeAlways(const LgsString& str)
{
   if (v_diagnosticsOn)
      stream() << str;
}
//-------------------------------------------------------------------
void Diagnostic::writeLine(const LgsString& str)
{
   if (!v_blockOutput)
      stream() << str << endl;
}
//-------------------------------------------------------------------
void Diagnostic::writeBrokenLines(const LgsString& str)
{
   if (!v_blockOutput)
   {
      LgsString buffer;
      buffer.reserve(LineSize + 1);
      int stringSize = str.length ();

      if (stringSize <= LineSize)
      {
         stream() << str << endl;
      }
      else
      {
         int cursor = 0;
         while (cursor < stringSize)
         {
            buffer.assign(str, cursor, LineSize);
            stream() << buffer << endl;
            cursor += LineSize;
         }
      }
   }
}
//-------------------------------------------------------------------
inline void appendTokenStart(LgsString& buffer, const LgsString& spaces, LookupTokenType::Type tokenType,
                             const LgsString& word, bool withTypes = false)
{
   buffer += spaces;
   if (withTypes)
      buffer += LookupTokenType::tokenTypeDesc(tokenType);
   if (withTypes)
      buffer += LgsString(1, '{');
   buffer += word;
}
//-------------------------------------------------------------------
inline void appendTokenEnd(LgsString& buffer, bool withTypes)
{
   if (withTypes)
      buffer += LgsString(1, '}');
}
//-------------------------------------------------------------------
void Diagnostic::wordsAsString(const LWordVector& words, bool withTypes)
{
   if (!v_blockOutput)
   {
      LgsString buffer;
      buffer.reserve(150);
      bool inToken = false;

      for (LWordVector::const_iterator iter = words.begin(); iter != words.end(); iter++)
      {
         LgsString spaces = LgsString(iter->precedingSpaces(), ' ');
         LookupTokenType::Type tokenType = iter->getTokenType();

         if (100 < buffer.length())
         {
            stream() << buffer << endl;
            buffer.erase();
         }

         if (inToken)
         {
            switch (tokenType)
            {
            case LookupTokenType::tok_none:
               appendTokenEnd(buffer, withTypes);
               inToken = false;
               appendTokenStart(buffer, spaces, tokenType, *iter);
               break;
            case LookupTokenType::tok_continuation:
               appendTokenStart(buffer, spaces, tokenType, *iter);
               break;
            default:
               appendTokenEnd(buffer, withTypes);
               appendTokenStart(buffer, spaces, tokenType, *iter, withTypes);
               break;
            }
         }
         else
         {
            switch (tokenType)
            {
            case LookupTokenType::tok_none:
               appendTokenStart(buffer, spaces, tokenType, *iter);
               break;
            case LookupTokenType::tok_continuation:
               assert(("continuation token not valid", 0));
               break;
            default:
               inToken = true;
               appendTokenStart(buffer, spaces, tokenType, *iter, withTypes);
               break;
            }
         }
      }

      if (inToken)
         appendTokenEnd(buffer, withTypes);
      stream() << buffer << endl;
   }
}
//-------------------------------------------------------------------
void Diagnostic::lookupSentence(LSentence& sentence)
{
   if (!v_blockOutput)
   {
      stream() << endl;
      writeLine("*Complete Lookup phase*");
      wordsAsString(sentence.sourceWords());
      stream() << endl;
      writeLine("*Dictionary Match*");
      dictionaryAsString(sentence.sourceSentenceUnits());
      stream() << endl;
      dictionaryCompare(sentence.sourceSentenceUnits());
      lookupSentenceUnit(sentence.sourceSentenceUnits());
      lookupSummary(sentence.sourceSentenceUnits());
      stream() << endl << "*Sentence Summary*" << endl << "   Case          = ";
      switch (sentence.caseState())
      {
      case SentenceUnit::BeginsUpperCase:
         stream() << "Begins Upper Case";
         break;
      case SentenceUnit::AllUpperCase:
         stream() << "All Upper Case";
         break;
      default:
         stream() << "Lower Case";
         break;
      }
      stream() << endl << "   Translatable  = ";
      switch (sentence.translationState())
      {
      case LSentence::DoNotTranslate:
         stream() << "No";
         break;
      default:
         stream() << "Yes";
         break;
      }
      stream() << endl << "   Bold          = ";
      if (sentence.isBold())
         stream() << "Yes";
      else
         stream() << "No";
      stream() << endl << "   Italic        = ";
      if (sentence.isItalic())
         stream() << "Yes";
      else
         stream() << "No";
      stream() << endl << "   Underlined    = ";
      if (sentence.isUnderlined())
         stream() << "Yes";
      else
         stream() << "No";
      stream() << endl << "   Single Quoted = ";
      if (sentence.isSingleQuoted())
         stream() << "Yes";
      else
         stream() << "No";
      stream() << endl << "   Double Quoted = ";
      if (sentence.isDoubleQuoted())
         stream() << "Yes";
      else
         stream() << "No";
      stream() << endl << endl;
   }
}
//-------------------------------------------------------------------
void Diagnostic::generateSentence(LSentence& sentence)
{
   if (!v_blockOutput)
   {
      TargetUnitVector& units = sentence.targetSentenceUnits();

      LgsString buffer;
      buffer.reserve(120);

      for (TargetUnitVector::const_iterator iter = units.begin(); iter != units.end(); iter++)
      {
         if (100 < buffer.length ())
         {
            stream() << buffer << endl;
            buffer.erase();
         }

         buffer += (*iter)->surfaceExpressionAsString();
         buffer += LgsString((*iter)->trailingSpaces(), ' ');
      }

      stream() << buffer << endl;
   }
}
//-------------------------------------------------------------------
void Diagnostic::dictionaryAsString(const SourceUnitVector& vec)
{
   if (!v_blockOutput)
   {
      SourceUnitVector& units = const_cast<SourceUnitVector&>(vec);
      LgsString buffer1, buffer2;
      buffer1.reserve(120);
      buffer2.reserve(120);

      for (SourceUnitVector::iterator iter = units.begin(); iter != units.end(); iter++)
      {
         if (100 < buffer1.length())
         {
            stream() << buffer1 << endl << buffer2 << endl;
            buffer1.erase();
            buffer2.erase();
         }

         if (iter != units.begin())
         {
            buffer1 += LgsString(1, ' ');
            buffer2 += LgsString(1, ' ');
         }
         LgsString word = (*iter)->word();
         if (word.length() == 0)
            word = LgsString(1, 'ÿ');
         buffer1 += word;
         buffer2 += LgsString(1, '^');

         int length = word.length() - 1;
         if (length > 0)
            buffer2 += LgsString(length, ' ');
      }

      stream() << buffer1 << endl << buffer2 << endl;
   }
}
//-------------------------------------------------------------------
void Diagnostic::dictionaryCompare(const SourceUnitVector& vec)
{
   if (!v_blockOutput)
   {
      SourceUnitVector& units = const_cast<SourceUnitVector&>(vec);
      SourceUnitVector::iterator iter;

      for (iter =  units.begin(); iter != units.end(); iter++)
      {
         stream() << "INPUT: " << (*iter)->surfaceExpressionAsString() << endl;
         stream() << "DICT:  " << (*iter)->word() << endl;
      }
   }
}
//-------------------------------------------------------------------
void Diagnostic::lookupSentenceUnit(const SourceUnitVector& vec)
{
   if (!v_blockOutput)
   {
      SourceUnitVector& units = const_cast<SourceUnitVector&>(vec);
      SourceUnitVector::iterator iter;

      stream() << endl << "*SWORK RECORDS*" << endl << " #  xx";
      for (int s = 0; s < 3; s++)
      {
         stream() << " wc typ fr sbs sps patstm schg com smc             o2b o3b meaninID|";		// TO BE REMOVED WHEN NEW SMC IS TESTED
		 //stream() << " wc typ fr sbs sps patstm schg com smc o2b o3b meaninID|";			// this is the correct line to use after SMC testing

      }
      stream() << endl;
      for (iter =  units.begin(); iter != units.end(); iter++)
      {
         SWork swork(**iter);
         swork.display(stream());
      }
   }
}

//-------------------------------------------------------------------
void Diagnostic::lookupSummary(const SourceUnitVector& vec)
{
   if (!v_blockOutput)
   {
      SourceUnitVector& units = const_cast<SourceUnitVector&>(vec);
      SourceUnitVector::const_iterator iter = units.begin();

      stream() << endl << "*SentenceUnits*" << endl << "   hash-1 hash-2 roothash-1 roothash-2 hen-1 hen-2 roothen-1 roothen-2 wct CQDUBI" << endl;
      int size = units.size();
      for (int i = 0; i < size; i++, iter++)
      {
         char buffer[100];
         char caseState = ' ';
         switch ((*iter)->caseState())
         {
         case SentenceUnit::BeginsUpperCase:
            caseState = 'I';
            break;
         case SentenceUnit::AllUpperCase:
            caseState = 'A';
            break;
         }
         char isSingleQuoted = (*iter)->markup().isSingleQuoted() ? '1' : ' ';
         char isDoubleQuoted = (*iter)->markup().isDoubleQuoted() ? '1' : ' ';
         char isUnderlined = (*iter)->markup().isUnderlined  () ? '1' : ' ';
         char isBold = (*iter)->markup().isBold() ? '1' : ' ';
         char isItalic = (*iter)->markup().isItalic() ? '1' : ' ';
         sprintf(buffer, "%2d %6d %6d %10d %10d %5d %5d %9d %9d %3d %c%c%c%c%c%c",
                 (*iter)->position(), 
				 (*iter)->hashCode1(), (*iter)->hashCode2(),
				 (*iter)->rootHashCode1(), (*iter)->rootHashCode2(),
                 (*iter)->henum1(), (*iter)->henum2(), 
				 (*iter)->rootHenum1(), (*iter)->rootHenum2(),
				 (*iter)->wordCount(),
                 caseState, isSingleQuoted, isDoubleQuoted, isUnderlined, isBold, isItalic);
         stream() << buffer << endl;
      }
   }
}
//-------------------------------------------------------------------
void Diagnostic::setCurrLineNumber(int lineNo)
{
   v_currLine = lineNo;
   if (v_outputRestricted)
   {
      if ((v_currLine >= v_startLine) && (v_currLine <= v_endLine))
         v_blockOutput = false;
      else
         v_blockOutput = true;
   }
}
