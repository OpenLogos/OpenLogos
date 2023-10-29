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
// --------------------------------------------------------------------------
// TransLCommonObjects.cpp (implementation)
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/translutility/translcommonobjects.h>
#include <logos_libs/sql/sqlconnection.h>
#include <logos_libs/startrules/startruleslocale.h>
#include <logos_libs/gerdem/gfactory.h>
#include <logos_libs/gerdem/dictionaryentrybuilder.h>
#include <logos_libs/gerdem/gmatcher.h>
#include <logos_libs/gerdem/gcompanybuilder.h>
#include <logos_libs/gerdem/gsubjectmatterbuilder.h>
#include <logos_libs/linguistic/context.h>
#include <logos_libs/gerdem/gerdemlanguagebuilder.h>
#include <logos_libs/halfnoundll/germancache.h>
#include <logos_libs/halfnoun/germansearch.h>
#include <logos_libs/linguistic/prissufilter.h>
#include <lgs_db_io/lgsdbcommonobjects.h>
#include <lgs_db_io/querytodictionary.h>
#include <logos_libs/linguistic/noninflectingwordcache.h>
#include <configdatafileinterface/configdatainterfacemain.h>
#include <logos_libs/semtabrule/spio.h>

// --------------------------------------------------------------------------
// Global objects - pointers initialization
// --------------------------------------------------------------------------
SqlConnection* TranslCommonObjects::sqlConnection = 0;
ST_Locale* TranslCommonObjects::sourceLocale = 0;
ST_Locale* TranslCommonObjects::targetLocale = 0;
GFactory* TranslCommonObjects::persistDataFactory = 0;
DictionaryEntryBuilder* TranslCommonObjects::dictionaryEntryBuilder = 0;
GMatcher* TranslCommonObjects::matcher = 0;
GCompanyBuilder* TranslCommonObjects::companyBuilder = 0;
GSubjectMatterBuilder* TranslCommonObjects::subjectMatterBuilder = 0;
Context* TranslCommonObjects::context = 0;
GerdemLanguageBuilder* TranslCommonObjects::gerdemLanguageBuilder = 0;
LLanguage* TranslCommonObjects::sourceLanguage = 0;
LLanguage* TranslCommonObjects::targetLanguage = 0;
LDictionary* TranslCommonObjects::dictionary = 0;
GermanSearch* TranslCommonObjects::germanSearch = 0;
LgsThreadLocalStore<Diagnostic> TranslCommonObjects::_diagnosticStore;


// --------------------------------------------------------------------------
// Initialize all objects that are global to TransL process (they remain static to the
// environment)
// --------------------------------------------------------------------------
void TranslCommonObjects::InitializeCommonObjects()
{
   // Create the global objects
   _diagnosticStore.initialize();
   dictionaryEntryBuilder = new DictionaryEntryBuilder();
   matcher = new GMatcher();
   companyBuilder = new GCompanyBuilder();
   subjectMatterBuilder = new GSubjectMatterBuilder();
   context=new Context();
   gerdemLanguageBuilder = new GerdemLanguageBuilder();

   // Open the database
   sqlConnection = LgsDBCommonObjects::GetSqlConnection();
   persistDataFactory = new GFactory(sqlConnection);

   matcher->Factory(persistDataFactory);
   dictionaryEntryBuilder->setMatcher(matcher);
   dictionaryEntryBuilder->setFactory(persistDataFactory);
   context->createContext();

   sourceLanguage = gerdemLanguageBuilder->CreateSource(LgsDBCommonObjects::GetJobControlArguments().SourceLanguage(),
                                                        *persistDataFactory);
   if (!sourceLanguage)
   {
      throw (LgsString("Source language not supported!"));
   }
   sourceLanguage->context(context);

   // Get finish rules file name
   char ElisionFileName[MAX_FILEPATH_LEN];
   GetConfigData("engine", "elision", ElisionFileName, MAX_FILEPATH_LEN);
   
   targetLanguage = gerdemLanguageBuilder->CreateTarget(LgsDBCommonObjects::GetJobControlArguments().TargetLanguage(),
                                                        *persistDataFactory, ElisionFileName);
   if (!targetLanguage)
   {
      throw (LgsString("Target language not supported!"));
   }
   dictionary = new LDictionary(sourceLanguage, targetLanguage, dictionaryEntryBuilder, persistDataFactory);
   persistDataFactory->Open(dictionary);

   // Load language specific word list of non-phrases
   dictionaryEntryBuilder->loadWordList();

   // Build the source and target locales
   LgsString localeStr = LgsDBCommonObjects::GetJobControlArguments().SourceLocaleData();
   if (localeStr.length() != 8)
   {
	   sourceLocale = BuildDefaultLocale(sourceLanguage->id());
   }
   else
   {
	   sourceLocale = BuildLocale(sourceLanguage->id(), localeStr);
   }

   localeStr = LgsDBCommonObjects::GetJobControlArguments().TargetLocaleData();
   if (localeStr.length() != 8)
   {
	   targetLocale = BuildDefaultLocale(targetLanguage->id());
   }
   else
   {
	   targetLocale = BuildLocale(targetLanguage->id(), localeStr);
   }

   //Get start rule file name.
   char startRulesFileName[MAX_FILEPATH_LEN];
   GetConfigData("engine", "start_rules", startRulesFileName, MAX_FILEPATH_LEN);

   // Start rules can only be built after the local information has become available
   sourceLanguage->loadStartRulesEngine(startRulesFileName);

   // Initialize German dictionary if in lookup phase and source language is German
   if (sourceLanguage->id() == LLanguage::GermanID)
   {
      char HalfNounControlFileName[MAX_FILEPATH_LEN];
      char HalfNounWordListFileName[MAX_FILEPATH_LEN];
      char HalfNounPlaceNamesFileName[MAX_FILEPATH_LEN];
      char HalfNounProperNamesFileName[MAX_FILEPATH_LEN];
      char HalfNounLogFileName[MAX_FILEPATH_LEN];

      GetConfigData("sourcedata", "proper_names", HalfNounProperNamesFileName, MAX_FILEPATH_LEN);
      GetConfigData("sourcedata", "place_names", HalfNounPlaceNamesFileName, MAX_FILEPATH_LEN);
      GetConfigData("sourcedata", "halfnoun_wordlist", HalfNounWordListFileName, MAX_FILEPATH_LEN);
      GetConfigData("sourcedata", "halfnoun_control", HalfNounControlFileName, MAX_FILEPATH_LEN);
      GetConfigData("tempfile", "halfnoun_log", HalfNounLogFileName, MAX_FILEPATH_LEN);

      GermanCache & germanCache = GermanCache::singleton(HalfNounControlFileName,
                                                         HalfNounWordListFileName,
                                                         HalfNounLogFileName,
                                                         HalfNounPlaceNamesFileName,
                                                         HalfNounProperNamesFileName,
                                                         GetContext()->userSelectedCompanyCodes()->toString());
      germanSearch = &germanCache.germanSearch();
   }

   PriorityBasedSSUFilter::initialize();
}


// --------------------------------------------------------------------------
// Build the default locale pertaining to a specific language
// --------------------------------------------------------------------------
ST_Locale* TranslCommonObjects::BuildDefaultLocale(LLanguage::ID langID)
{
   switch (langID)
   {
   case LLanguage::GermanID:
      return new ST_Locale(LLanguage::GermanID, ',', '.', '.', ':', '.', ST_Locale::dmy,
                           LgsString("er|ers|ern|erin|erinnen|stel|stels|steln|fach|facher|faches|fachem|fachen|"
                                     "([[:lower:]]*(ig|ige|igen|iger|iges))|%ig"),
                           LgsString());
      break;
   case LLanguage::FrenchID:
      return new ST_Locale(LLanguage::FrenchID, ',', '.', '.', ':', '.', ST_Locale::dmy, 
                           LgsString(), LgsString());
      break;
   case LLanguage::SpanishID:
      return new ST_Locale(LLanguage::SpanishID, ',', '.', '.', ':', '.', ST_Locale::dmy, 
                           LgsString(), LgsString());
      break;
   case LLanguage::PortugueseID:
      return new ST_Locale(LLanguage::PortugueseID, ',', '.', '.', ':', '.', ST_Locale::dmy, 
                           LgsString(), LgsString());
      break;
   case LLanguage::ItalianID:
      return new ST_Locale(LLanguage::ItalianID, ',', '.', '.', ':', '.', ST_Locale::dmy, 
                           LgsString(), LgsString());
      break;
   case LLanguage::EnglishID:
   default:
      return new ST_Locale(LLanguage::EnglishID, '.', ',', '/', ':', '.', ST_Locale::mdy, 
                           LgsString("st|nd|rd|th"), LgsString("am|AM|pm|PM"));
      break;
   }
}


// --------------------------------------------------------------------------
// Build the locale specific to a language
// --------------------------------------------------------------------------
ST_Locale* TranslCommonObjects::BuildLocale(LLanguage::ID langID, LgsString& locData)
{
   ST_Locale::DateFormat dateFormat;
   LgsString ordSuffix;
   LgsString timeSuffix;

   switch (langID)
   {
   case LLanguage::GermanID:
      ordSuffix = "er|ers|ern|erin|erinnen|stel|stels|steln|fach|facher|faches|fachem|fachen|"
                  "([[:lower:]]*(ig|ige|igen|iger|iges))|%ig";
   case LLanguage::FrenchID:
   case LLanguage::SpanishID:
   case LLanguage::ItalianID:
      dateFormat = ST_Locale::dmy;
      break;
   case LLanguage::EnglishID:
   default:
      dateFormat = ST_Locale::mdy;
      ordSuffix = "st|nd|rd|th";
      timeSuffix = "am|AM|pm|PM";
      break;
   }

   if (locData.substr(5, 3) == "mdy")
   {
	   dateFormat = ST_Locale::mdy;
   }
   else
   {
      if (locData.substr(5, 3) == "dmy")
      {
        dateFormat = ST_Locale::dmy;
      }
   }

   return new ST_Locale(langID, locData[0], locData[1], locData[2], locData[3], locData[4],
                        dateFormat, ordSuffix, timeSuffix);
}


// --------------------------------------------------------------------------
// Clean up memory from all global objects
// --------------------------------------------------------------------------
void TranslCommonObjects::CleanupCommonObjects()
{

   delete targetLanguage;
   delete sourceLanguage;
   delete context;
   delete persistDataFactory;
   delete sourceLocale;
   delete targetLocale;
   delete dictionary;
   delete gerdemLanguageBuilder;
   delete subjectMatterBuilder;
   delete companyBuilder;
   delete matcher;
   delete dictionaryEntryBuilder;
   PriorityBasedSSUFilter::cleanup();
   GermanCache::destroySingleton();
   LSPREADCLEAN(); // cleaning up static SMRCache
}
// --------------------------------------------------------------------------
void TranslCommonObjects::CleanupLookupObjects()
{
   matcher->CleanupMemory();
   dictionaryEntryBuilder->CleanupMemory();
   
   // Destroy QueryToDictionary object
   QueryToDictionary::DestroyObject();
   // Destroy NonInflectingWordCache object
   NonInflectingWordCache::DestroyObject();
}
// --------------------------------------------------------------------------
// Diagnostic for Dictionary Lookup phase of TransL
// --------------------------------------------------------------------------
void TranslCommonObjects::initializeLookupDiag(void)
{
   char DiagnosticInFileName[MAX_FILEPATH_LEN];
   GetConfigData("tempfile", "transl_in_diag", DiagnosticInFileName, MAX_FILEPATH_LEN);

   Diagnostic* diagnostic = new Diagnostic(LgsDBCommonObjects::GetJobControlArguments().DiagnosticLevel(),
                                           DiagnosticInFileName,
                                           LgsDBCommonObjects::GetJobControlArguments().DiagnosticStartLine(),
                                           LgsDBCommonObjects::GetJobControlArguments().DiagnosticEndLine());
   _diagnosticStore.setValue(diagnostic);

}


// --------------------------------------------------------------------------
// Diagnostic for Finalize Transfer (generate) phase of TransL
// --------------------------------------------------------------------------
void TranslCommonObjects::initializeGenerateDiag(void)
{
   char DiagnosticOutFileName[MAX_FILEPATH_LEN];
   GetConfigData("tempfile", "transl_out_diag", DiagnosticOutFileName, MAX_FILEPATH_LEN);

   Diagnostic *diagnostic = new Diagnostic(LgsDBCommonObjects::GetJobControlArguments().DiagnosticLevel(),
                                           DiagnosticOutFileName,
                                           LgsDBCommonObjects::GetJobControlArguments().DiagnosticStartLine(),
                                           LgsDBCommonObjects::GetJobControlArguments().DiagnosticEndLine());
   _diagnosticStore.setValue(diagnostic);
}


// --------------------------------------------------------------------------
// Diagnostic for Term Miner (Term Serach) phase of TransL
// --------------------------------------------------------------------------
void TranslCommonObjects::initializeSearchDiag(void)
{
   char DiagnosticOutFileName[MAX_FILEPATH_LEN];
   GetConfigData("tempfile", "transl_out_diag", DiagnosticOutFileName, MAX_FILEPATH_LEN);

   Diagnostic *diagnostic = new Diagnostic(LgsDBCommonObjects::GetJobControlArguments().DiagnosticLevel(),
                                           DiagnosticOutFileName,
                                           LgsDBCommonObjects::GetJobControlArguments().DiagnosticStartLine(),
                                           LgsDBCommonObjects::GetJobControlArguments().DiagnosticEndLine());
   _diagnosticStore.setValue(diagnostic);
}
