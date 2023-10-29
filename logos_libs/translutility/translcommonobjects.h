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
#ifndef __TranslCommonObjects_h__
#define __TranslCommonObjects_h__

//---------------------------------------------------------------------
// File - TranslCommonObjects.h
//
// Class - TranslCommonObjects (interface)
//
//---------------------------------------------------------------------
#include <logos_libs/linguistic/diagnostic.h>
#include <logos_libs/multithreadlib/lgsthrlocalstore.h>
#include <logos_libs/linguistic/llanguage.h>

class SqlConnection;
class ST_Locale;
class GFactory;
class DictionaryEntryBuilder;
class GMatcher;
class GCompanyBuilder;
class GerdemLanguageBuilder;
class GSubjectMatterBuilder;
class Context;
class LDictionary;
class GermanSearch;

class TranslCommonObjects
{
public:
   static void InitializeCommonObjects();
   static void CleanupCommonObjects();
   static ST_Locale* BuildDefaultLocale(LLanguage::ID langID);
   static ST_Locale* BuildLocale(LLanguage::ID langID, LgsString& locData);

   static ST_Locale& GetSourceLocale();
   static ST_Locale& GetTargetLocale();
   static SqlConnection* GetSqlConnection();
   static GFactory* GetPersistDataFactory();
   static DictionaryEntryBuilder* GetDictionaryEntryBuilder();
   static GCompanyBuilder& GetCompanyBuilder();
   static GSubjectMatterBuilder& GetSubjectMatterBuilder();
	static Context* GetContext();			// user selected company codes and subject matter codes
   static LLanguage* GetSourceLanguage();
   static LLanguage* GetTargetLanguage();
   static LDictionary& GetDictionary();
   static GermanSearch& GetGermanSearch();
   static Diagnostic* GetDiagnostic();
   static void initializeLookupDiag();
   static void initializeGenerateDiag();
   static void initializeSearchDiag();
   static void freeDiag(void);
   static void CleanupLookupObjects();

private:
   static SqlConnection* sqlConnection;
   static ST_Locale* sourceLocale;
   static ST_Locale* targetLocale;
   static GFactory* persistDataFactory;
   static DictionaryEntryBuilder* dictionaryEntryBuilder;
   static GMatcher* matcher;
   static GCompanyBuilder* companyBuilder;
   static GSubjectMatterBuilder* subjectMatterBuilder;
   static Context* context;
   static GerdemLanguageBuilder* gerdemLanguageBuilder;
   static LLanguage* sourceLanguage;
   static LLanguage* targetLanguage;
   static LDictionary* dictionary;
   static GermanSearch* germanSearch;
   static LgsThreadLocalStore<Diagnostic> _diagnosticStore;
};
//-------------------------------------------------------------------
inline ST_Locale& TranslCommonObjects::GetSourceLocale()
{
   return *sourceLocale;
}
//-------------------------------------------------------------------
inline ST_Locale& TranslCommonObjects::GetTargetLocale()
{
   return *targetLocale;
}
//-------------------------------------------------------------------
inline SqlConnection* TranslCommonObjects::GetSqlConnection()
{
   return sqlConnection;
}
//-------------------------------------------------------------------
inline GFactory* TranslCommonObjects::GetPersistDataFactory()
{
   return persistDataFactory;
}
//-------------------------------------------------------------------
inline DictionaryEntryBuilder* TranslCommonObjects::GetDictionaryEntryBuilder()
{
   return dictionaryEntryBuilder;
}
//-------------------------------------------------------------------
inline GCompanyBuilder& TranslCommonObjects::GetCompanyBuilder()
{
   return *companyBuilder;
}
//-------------------------------------------------------------------
inline GSubjectMatterBuilder& TranslCommonObjects::GetSubjectMatterBuilder()
{
   return *subjectMatterBuilder;
}
//-------------------------------------------------------------------
inline Context* TranslCommonObjects::GetContext()
{
   return context;
}
//-------------------------------------------------------------------
inline LLanguage* TranslCommonObjects::GetSourceLanguage()
{
   return sourceLanguage;
}
//-------------------------------------------------------------------
inline LLanguage* TranslCommonObjects::GetTargetLanguage()
{
   return targetLanguage;
}
//-------------------------------------------------------------------
inline LDictionary& TranslCommonObjects::GetDictionary()
{
   return *dictionary;
}
//-------------------------------------------------------------------
inline GermanSearch& TranslCommonObjects::GetGermanSearch()
{
   return *germanSearch;
}
//-------------------------------------------------------------------
inline Diagnostic* TranslCommonObjects::GetDiagnostic()
{
   return _diagnosticStore.getValue();
}
//-------------------------------------------------------------------
inline void TranslCommonObjects::freeDiag()
{
   Diagnostic * diagPtr =  _diagnosticStore.getValue();
   delete diagPtr;
//   _diagnosticStore.cleanup();
}

#endif // __TranslCommonObjects_h__


