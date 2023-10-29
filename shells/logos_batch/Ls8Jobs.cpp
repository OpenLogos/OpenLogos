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
// Ls8Jobs.cpp: implementation of the Ls8Jobs class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <shells/logos_batch/Ls8Jobs.h>
#include <logos_libs/sql/sqlconnection.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/utility/stringutil.h>
#include <logos_libs/utility/PathUtil.h>

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

Ls8Jobs::Ls8Jobs(SqlConnection *aConnection)
        :m_pConnection(aConnection)
{
}
// -------------------------------------------------------------------
Ls8Jobs::~Ls8Jobs()
{
}
// -------------------------------------------------------------------
bool Ls8Jobs::GetData(int jobID)
{
   bool result = false;
   // Construct query
   LgsString query = "select job_type, priority, source_language, target_language, "
      " target_form, word_search_options, word_search_found_start, "
      " word_search_found_limit, word_search_unfound_start, "
      " word_search_unfound_limit, word_count, sent_input_count, "
      " sent_current_complete, tran_passes, diagnostics_level, "
      " diagnostic_line_start, diagnostic_line_end, status, "
      " user_id, company_codes, subject_matter_codes, extended_search, "
      " locale_source, locale_target, input_format, "
      " protectioncharacter, input_file, output_file, output_file_aligned, "
      " diagnostic_file, main_res1, main_res2, main_res22, main_tran1, "
      " main_tran2, main_tran3, main_tran4, main_parse1, main_parse2, "
      " main_parse3, main_parse4, mini_res2, mini_tran1, mini_tran2, "
      " mini_tran3, mini_tran4, mini_parse1, mini_parse2, mini_parse3, "
      " mini_parse4, client_input_file, client_output_file, "
      " client_diag_file, client_aligned_output_file, date_time_submitted, pattern_rules "
      "from ls8jobs where job_id = :jobNumber";

   try
   {
      SqlStatement *st = m_pConnection->CreateStatement();
      st->AddToCommandString(query);
      st->ParseDeferred();

      SqlColumn *colJobType = st->BindOutputColumn(1, SqlColumn::Integer); // job_type
      SqlColumn *colPriority = st->BindOutputColumn(2, SqlColumn::Integer); // priority
      SqlColumn *colSourceLanguage = st->BindOutputColumn(3, SqlColumn::Integer); // source_language
      SqlColumn *colTargetLanguage = st->BindOutputColumn(4, SqlColumn::Integer); // target_language
      SqlColumn *colTargetForm = st->BindOutputColumn(5, SqlColumn::Integer); // target_form
      SqlColumn *colWordSearchOptions= st->BindOutputColumn(6, SqlColumn::Integer); // word_search_options
      SqlColumn *colWordSearchFoundStart = st->BindOutputColumn(7, SqlColumn::Integer); // word_search_found_start
      SqlColumn *colWordSearchFoundLimit = st->BindOutputColumn(8, SqlColumn::Integer); // word_search_found_limit
      SqlColumn *colWordSearchUnfoundStart = st->BindOutputColumn(9, SqlColumn::Integer); // word_search_unfound_start
      SqlColumn *colWordSearchUnfoundLimit = st->BindOutputColumn(10, SqlColumn::Integer); // word_search_unfound_limit
      SqlColumn *colWordCount = st->BindOutputColumn(11, SqlColumn::Integer); // word_count
      SqlColumn *colSentInputCount = st->BindOutputColumn(12, SqlColumn::Integer); // sent_input_count
      SqlColumn *colSentCurrentComplete = st->BindOutputColumn(13, SqlColumn::Integer); // sent_current_complete
      SqlColumn *colTranPasses = st->BindOutputColumn(14, SqlColumn::Integer); // tran_passes
      SqlColumn *colDiagnosticLevel = st->BindOutputColumn(15, SqlColumn::Integer); // diagnostics_level
      SqlColumn *colDiagnosticStartLine = st->BindOutputColumn(16, SqlColumn::Integer); // diagnostic_line_start
      SqlColumn *colDiagnosticEndLine = st->BindOutputColumn(17, SqlColumn::Integer); // diagnostic_line_end
      SqlColumn *colStatus = st->BindOutputColumn(18, SqlColumn::Integer); // status	
      SqlColumn *colUserID = st->BindOutputColumn(19, SqlColumn::StringType); // user_id
      SqlColumn *colCompanyCodes = st->BindOutputColumn(20, SqlColumn::StringType); // company_codes
      SqlColumn *colSubjectMatterCodes = st->BindOutputColumn(21, SqlColumn::StringType); // subject_matter_codes
      SqlColumn *colExtendedSearch = st->BindOutputColumn(22, SqlColumn::Integer); // extended_search
      SqlColumn *colSourceLocaleData = st->BindOutputColumn(23, SqlColumn::StringType); // locale_source
      SqlColumn *colTargetLocaleData = st->BindOutputColumn(24, SqlColumn::StringType); // locale_target
      SqlColumn *colInputFormat = st->BindOutputColumn(25, SqlColumn::StringType); // input_format
      SqlColumn *colProtectionCharacter = st->BindOutputColumn(26, SqlColumn::StringType); // protection_character
      SqlColumn *colInputFile = st->BindOutputColumn(27, SqlColumn::StringType); // input_file
      SqlColumn *colOutputFile = st->BindOutputColumn(28, SqlColumn::StringType); // output_file
      SqlColumn *colAlignedFile = st->BindOutputColumn(29, SqlColumn::StringType); // output_file_aligned
      SqlColumn *colDiagnosticFile = st->BindOutputColumn(30, SqlColumn::StringType); // diagnostic_file
      SqlColumn *colMainRes1 = st->BindOutputColumn(31, SqlColumn::StringType); // main_res1
      SqlColumn *colMainRes2 = st->BindOutputColumn(32, SqlColumn::StringType); // main_res2
      SqlColumn *colMainRes22 = st->BindOutputColumn(33, SqlColumn::StringType); // main_res22
      SqlColumn *colMainTran1 = st->BindOutputColumn(34, SqlColumn::StringType); // main_tran1
      SqlColumn *colMainTran2 = st->BindOutputColumn(35, SqlColumn::StringType); // main_tran2
      SqlColumn *colMainTran3 = st->BindOutputColumn(36, SqlColumn::StringType); // main_tran3
      SqlColumn *colMainTran4 = st->BindOutputColumn(37, SqlColumn::StringType); // main_tran4
      SqlColumn *colMainParse1 = st->BindOutputColumn(38, SqlColumn::StringType); // main_parse1
      SqlColumn *colMainParse2 = st->BindOutputColumn(39, SqlColumn::StringType); // main_parse2
      SqlColumn *colMainParse3 = st->BindOutputColumn(40, SqlColumn::StringType); // main_parse3
      SqlColumn *colMainParse4 = st->BindOutputColumn(41, SqlColumn::StringType); // main_parse4
      SqlColumn *colMiniRes2 = st->BindOutputColumn(42, SqlColumn::StringType); // mini_res2
      SqlColumn *colMiniTran1 = st->BindOutputColumn(43, SqlColumn::StringType); // mini_tran1
      SqlColumn *colMiniTran2 = st->BindOutputColumn(44, SqlColumn::StringType); // mini_tran2
      SqlColumn *colMiniTran3 = st->BindOutputColumn(45, SqlColumn::StringType); // mini_tran3
      SqlColumn *colMiniTran4 = st->BindOutputColumn(46, SqlColumn::StringType); // mini_tran4
      SqlColumn *colMiniParse1 = st->BindOutputColumn(47, SqlColumn::StringType); // mini_parse1
      SqlColumn *colMiniParse2 = st->BindOutputColumn(48, SqlColumn::StringType); // mini_parse2
      SqlColumn *colMiniParse3 = st->BindOutputColumn(49, SqlColumn::StringType); // mini_parse3
      SqlColumn *colMiniParse4 = st->BindOutputColumn(50, SqlColumn::StringType); // mini_parse4
      SqlColumn *colClientInputFile = st->BindOutputColumn(51, SqlColumn::StringType); // client_input_file
      SqlColumn *colClientOutputFile = st->BindOutputColumn(52, SqlColumn::StringType); // client_output_file
      SqlColumn *colClientDiagnosticFile = st->BindOutputColumn(53, SqlColumn::StringType); // client_diag_file
      SqlColumn *colClientAlignedFile = st->BindOutputColumn(54, SqlColumn::StringType); // client_aligned_output_file
      SqlColumn *colDateTimeSubmitted = st->BindOutputColumn(55, SqlColumn::TimeStampType); // date_time_submitted
      SqlColumn *colPatternRules = st->BindOutputColumn(56, SqlColumn::StringType); // pattern_rules

      st->BindInputInteger(":jobNumber", &jobID);
      st->Execute();
      result = st->Fetch();
      if( result )
      {
         m_nJobType = colJobType->AsInteger();
         m_nPriority = colPriority->AsInteger();
         m_nSourceLanguage = colSourceLanguage->AsInteger();
         m_nTargetLanguage = colTargetLanguage->AsInteger();
         m_nTargetForm = colTargetForm->AsInteger();
         m_nWordSearchOptions = colWordSearchOptions->AsInteger();
         m_nWordSearchFoundStart = colWordSearchFoundStart->AsInteger();
         m_nWordSearchFoundLimit = colWordSearchFoundLimit->AsInteger();
         m_nWordSearchUnfoundStart = colWordSearchUnfoundStart->AsInteger();
         m_nWordSearchUnfoundLimit = colWordSearchUnfoundLimit->AsInteger();
         m_nWordCount = colWordCount->AsInteger();
         m_nSentInputCount = colSentInputCount->AsInteger();
         m_nSentCurrentComplete = colSentCurrentComplete->AsInteger();
         m_nTranPasses = colTranPasses->AsInteger();
         m_nDiagnosticLevel = colDiagnosticLevel->AsInteger();
         m_nDiagnosticStartLine = colDiagnosticStartLine->AsInteger();
         m_nDiagnosticEndLine = colDiagnosticEndLine->AsInteger();
         m_nStatus = colStatus->AsInteger();
         m_sUserID = colUserID->AsString(); StringUtil::rightTrim(m_sUserID);
         m_sCompanyCodes = colCompanyCodes->AsString(); StringUtil::rightTrim(m_sCompanyCodes);
         m_sSubjectMatterCodes = colSubjectMatterCodes->AsString(); StringUtil::rightTrim(m_sSubjectMatterCodes);
         m_nExtendedSearch = colExtendedSearch->AsInteger();
         m_sSourceLocaleData = colSourceLocaleData->AsString(); StringUtil::rightTrim(m_sSourceLocaleData);
         m_sTargetLocaleData = colTargetLocaleData->AsString(); StringUtil::rightTrim(m_sTargetLocaleData);
         m_sInputFormat = colInputFormat->AsString(); StringUtil::rightTrim(m_sInputFormat);
         m_sProtectionCharacter = colProtectionCharacter->AsString(); StringUtil::rightTrim(m_sProtectionCharacter);
         m_sInputFile = PathUtil::AsPath(colInputFile->AsString()); StringUtil::rightTrim(m_sInputFile);
         m_sOutputFile = PathUtil::AsPath(colOutputFile->AsString()); StringUtil::rightTrim(m_sOutputFile);
         m_sAlignedFile = PathUtil::AsPath(colAlignedFile->AsString()); StringUtil::rightTrim(m_sAlignedFile);
         m_sDiagnosticFile = PathUtil::AsPath(colDiagnosticFile->AsString()); StringUtil::rightTrim(m_sDiagnosticFile);
         m_sMainRes1 = colMainRes1->AsString(); StringUtil::rightTrim(m_sMainRes1);
         m_sMainRes2 = colMainRes2->AsString(); StringUtil::rightTrim(m_sMainRes2);
         m_sMainRes22 = colMainRes22->AsString(); StringUtil::rightTrim(m_sMainRes22);
         m_sMainTran1 = colMainTran1->AsString(); StringUtil::rightTrim(m_sMainTran1);
         m_sMainTran2 = colMainTran2->AsString(); StringUtil::rightTrim(m_sMainTran2);
         m_sMainTran3 = colMainTran3->AsString(); StringUtil::rightTrim(m_sMainTran3);
         m_sMainTran4 = colMainTran4->AsString(); StringUtil::rightTrim(m_sMainTran4);
         m_sMainParse1 = colMainParse1->AsString(); StringUtil::rightTrim(m_sMainParse1);
         m_sMainParse2 = colMainParse2->AsString(); StringUtil::rightTrim(m_sMainParse2);
         m_sMainParse3 = colMainParse3->AsString(); StringUtil::rightTrim(m_sMainParse3);
         m_sMainParse4 = colMainParse4->AsString(); StringUtil::rightTrim(m_sMainParse4);
         m_sMiniRes2 = colMiniRes2->AsString(); StringUtil::rightTrim(m_sMiniRes2);
         m_sMiniTran1 = colMiniTran1->AsString(); StringUtil::rightTrim(m_sMiniTran1);
         m_sMiniTran2 = colMiniTran2->AsString(); StringUtil::rightTrim(m_sMiniTran2);
         m_sMiniTran3 = colMiniTran3->AsString(); StringUtil::rightTrim(m_sMiniTran3);
         m_sMiniTran4 = colMiniTran4->AsString(); StringUtil::rightTrim(m_sMiniTran4);
         m_sMiniParse1 = colMiniParse1->AsString(); StringUtil::rightTrim(m_sMiniParse1);
         m_sMiniParse2 = colMiniParse2->AsString(); StringUtil::rightTrim(m_sMiniParse2);
         m_sMiniParse3 = colMiniParse3->AsString(); StringUtil::rightTrim(m_sMiniParse3);
         m_sMiniParse4 = colMiniParse4->AsString(); StringUtil::rightTrim(m_sMiniParse4);
         m_sClientInputFile = PathUtil::AsPath(colClientInputFile->AsString()); StringUtil::rightTrim(m_sClientInputFile);
         m_sClientOutputFile = PathUtil::AsPath(colClientOutputFile->AsString()); StringUtil::rightTrim(m_sClientOutputFile);
         m_sClientDiagnosticFile = PathUtil::AsPath(colClientDiagnosticFile->AsString()); StringUtil::rightTrim(m_sClientDiagnosticFile);
         m_sClientAlignedFile = PathUtil::AsPath(colClientAlignedFile->AsString()); StringUtil::rightTrim(m_sClientAlignedFile);
         m_bPatternRules = StringUtil::lower(colPatternRules->AsString()) == "y";
      }
      delete st;
   }
   catch(SqlException& error)
   {
      cout << error.Message() << endl;
      throw(error);
   }

   return result;
}
// -------------------------------------------------------------------
int Ls8Jobs::GetNextJobID(const LgsString &userName, bool highonly, const LgsString &excludeUsers) const
{
   int jobID = -1;
   // Construct query
   const bool bAllUsers = StringUtil::lower(userName) == "all";
   LgsString query = "select job_id, user_id from ls8jobs where status = 1 ";
   if( !bAllUsers )
   {
      query += "and user_id = :userName ";
   }
   if( highonly )
   {
      query += "and priority < 50 ";
   }
   query += "order by priority, date_time_submitted";

   try
   {
      SqlStatement *st = m_pConnection->CreateStatement();
      st->AddToCommandString(query);
      st->ParseDeferred();
      SqlColumn *colJobID = st->BindOutputColumn(1, SqlColumn::Integer);
      SqlColumn *colUserID = st->BindOutputColumn(2, SqlColumn::StringType);
      if( !bAllUsers )
      {
         st->BindInputString(":userName", userName.c_str());
      }
      st->Execute();

      if( !bAllUsers )
      {
         // Process one specific user
         if( st->Fetch() )
         {
            jobID = colJobID->AsInteger();
         }
      }
      else
      {
         // Process all the users excluding those in the list
         LgsStringVector excludeList;
         StringUtil::parseInto(excludeUsers, excludeList, ',');
         while( st->Fetch() )
         {
            jobID = colJobID->AsInteger();
            LgsString userID = colUserID->AsString(); StringUtil::rightTrim(userID);
            for( LgsStringConstIterator i = excludeList.begin(); i != excludeList.end(); i++ )
            {
               if( StringUtil::equalsIgnoreCase(userID, *i) )
               {
                  jobID = -1;
                  break;
               }
            }
            if( jobID != -1 ) break;
         }
      }
      delete st;
   }
   catch(SqlException& error)
   {
      cout << error.Message() << endl;
      throw(error);
   }

   return jobID;
}
// -------------------------------------------------------------------
int Ls8Jobs::GetIntegerArg(int jobID, const LgsString &colName) const
{
   int intArg = -1;
   // Construct query
   LgsString query = "select ";
   query += colName;
   query += " from ls8jobs where job_id = :jobNumber";

   try
   {
      SqlStatement *st = m_pConnection->CreateStatement();
      st->AddToCommandString(query);
      st->ParseDeferred();
      st->BindInputInteger(":jobNumber", &jobID);
      SqlColumn *col = st->BindOutputColumn(1, SqlColumn::Integer);
      st->Execute();
      if( st->Fetch() )
      {
         intArg = col->AsInteger();
      }
      delete st;
   }
   catch(SqlException& error)
   {
      cout << error.Message() << endl;
      throw(error);
   }

   return intArg;
}
// -------------------------------------------------------------------
void Ls8Jobs::UpdateIntegerArg(int jobID, const LgsString &colName, int val) const
{
   // Construct query
   LgsString query = "update ls8jobs set ";
   query += colName;
   query += " = :newInt where job_id = :jobNumber";

   try
   {
      SqlStatement *st = m_pConnection->CreateStatement();
      st->AddToCommandString(query);
      st->ParseDeferred();
      st->BindInputInteger(":newInt", &val);
      st->BindInputInteger(":jobNumber", &jobID);
      st->Execute();
      delete st;
   }
   catch(SqlException& error)
   {
      cout << error.Message() << endl;
      throw(error);
   }
}
// -------------------------------------------------------------------
LgsString Ls8Jobs::GetStringArg(int jobID, const LgsString &colName) const
{
   LgsString stringArg;
   // Construct query
   LgsString query = "select ";
   query += colName;
   query += " from ls8jobs where job_id = :jobNumber";

   try
   {
      SqlStatement *st = m_pConnection->CreateStatement();
      st->AddToCommandString(query);
      st->ParseDeferred();
      st->BindInputInteger(":jobNumber", &jobID);
      SqlColumn *col = st->BindOutputColumn(1, SqlColumn::StringType);
      st->Execute();
      if( st->Fetch() )
      {
         stringArg = col->AsString();
      }
      delete st;
   }
   catch(SqlException& error)
   {
      cout << error.Message() << endl;
      throw(error);
   }

   return stringArg;
}
// -------------------------------------------------------------------
void Ls8Jobs::UpdateStringArg(int jobID, const LgsString &colName, const LgsString &val) const
{
   // Construct query
   LgsString query = "update ls8jobs set ";
   query += colName;
   query += " = :newStr where job_id = :jobNumber";

   try
   {
      SqlStatement *st = m_pConnection->CreateStatement();
      st->AddToCommandString(query);
      st->ParseDeferred();
      st->BindInputString(":newStr", val.c_str());
      st->BindInputInteger(":jobNumber", &jobID);
      st->Execute();
      delete st;
   }
   catch(SqlException& error)
   {
      cout << error.Message() << endl;
      throw(error);
   }
}
// -------------------------------------------------------------------
Time Ls8Jobs::GetTimeArg(int jobID, const LgsString &colName) const
{
   Time timeArg;
   // Construct query
   LgsString query = "select ";
   query += colName;
   query += " from ls8jobs where job_id = :jobNumber";

   try
   {
      SqlStatement *st = m_pConnection->CreateStatement();
      st->AddToCommandString(query);
      st->ParseDeferred();
      st->BindInputInteger(":jobNumber", &jobID);
      SqlColumn *col = st->BindOutputColumn(1, SqlColumn::TimeType);
      st->Execute();
      if( st->Fetch() )
      {
         timeArg = col->AsTime();
      }
      delete st;
   }
   catch(SqlException& error)
   {
      cout << error.Message() << endl;
      throw(error);
   }

   return timeArg;
}
// -------------------------------------------------------------------
void Ls8Jobs::UpdateTimeArg(int jobID, const LgsString &colName, const Time &val) const
{
   // Construct query
   LgsString query = "update ls8jobs set ";
   query += colName;
   query += " = :newTime where job_id = :jobNumber";

   try
   {
      SqlStatement *st = m_pConnection->CreateStatement();
      st->AddToCommandString(query);
      st->ParseDeferred();
      st->BindInputTime(":newTime", &val);
      st->BindInputInteger(":jobNumber", &jobID);
      st->Execute();
      delete st;
   }
   catch(SqlException& error)
   {
      cout << error.Message() << endl;
      throw(error);
   }
}
// -------------------------------------------------------------------
Date Ls8Jobs::GetDateArg(int jobID, const LgsString &colName) const
{
   Date dateArg;
   // Construct query
   LgsString query = "select ";
   query += colName;
   query += " from ls8jobs where job_id = :jobNumber";

   try
   {
      SqlStatement *st = m_pConnection->CreateStatement();
      st->AddToCommandString(query);
      st->ParseDeferred();
      st->BindInputInteger(":jobNumber", &jobID);
      SqlColumn *col = st->BindOutputColumn(1, SqlColumn::DateType);
      st->Execute();
      if( st->Fetch() )
      {
         dateArg = col->AsDate();
      }
      delete st;
   }
   catch(SqlException& error)
   {
      cout << error.Message() << endl;
      throw(error);
   }

   return dateArg;
}
// -------------------------------------------------------------------
void Ls8Jobs::UpdateDateArg(int jobID, const LgsString &colName, const Date &val) const
{
   // Construct query
   LgsString query = "update ls8jobs set ";
   query += colName;
   query += " = :newDate where job_id = :jobNumber";

   try
   {
      SqlStatement *st = m_pConnection->CreateStatement();
      st->AddToCommandString(query);
      st->ParseDeferred();
      st->BindInputDate(":newDate", &val);
      st->BindInputInteger(":jobNumber", &jobID);
      st->Execute();
      delete st;
   }
   catch(SqlException& error)
   {
      cout << error.Message() << endl;
      throw(error);
   }
}
// -------------------------------------------------------------------
TimeStamp Ls8Jobs::GetDateTimeArg(int jobID, const LgsString &colName) const
{
   TimeStamp dateTimeArg;
   // Construct query
   LgsString query = "select ";
   query += colName;
   query += " from ls8jobs where job_id = :jobNumber";

   try
   {
      SqlStatement *st = m_pConnection->CreateStatement();
      st->AddToCommandString(query);
      st->ParseDeferred();
      st->BindInputInteger(":jobNumber", &jobID);
      SqlColumn *col = st->BindOutputColumn(1, SqlColumn::TimeStampType);
      st->Execute();
      if( st->Fetch() )
      {
         dateTimeArg = col->AsTimeStamp();
      }
      delete st;
   }
   catch(SqlException& error)
   {
      cout << error.Message() << endl;
      throw(error);
   }

   return dateTimeArg;
}
// -------------------------------------------------------------------
void Ls8Jobs::UpdateDateTimeArg(int jobID, const LgsString &colName, const TimeStamp &val) const
{
   // Construct query
   LgsString query = "update ls8jobs set ";
   query += colName;
   /* WK:
      on some hosts (e.g. attila) the TimeStamp value creates an [unixODBC]
      input format error
      As a workaround we will use the Postgres function instead
   */
#ifdef USE_POSTGRES
   query += " = LOCALTIMESTAMP(0) ";
#else
   query += " = :newTimeStamp ";
#endif
   query += " where job_id = :jobNumber";


   try
   {
      SqlStatement *st = m_pConnection->CreateStatement();
      st->AddToCommandString(query);
      st->ParseDeferred();
#ifndef USE_POSTGRES
      st->BindInputTimeStamp(":newTimeStamp", &val);
#endif
      st->BindInputInteger(":jobNumber", &jobID);
      st->Execute();
      delete st;
   }
   catch(SqlException& error)
   {
      cout << error.Message() << endl;
      throw(error);
   }
}

bool Ls8Jobs::UpdateRunningStatusSafely(int jobID, int newStatus) const {

// The trick is that if somebody else got the job then
// update will fail and # of rows affected will be 0.

	LgsString q = "update ls8jobs set status = :nstatus where job_id = :jobNumber";
	q += " and status != :n1status";

	bool success = false;
	try {
		SqlStatement *st = m_pConnection->CreateStatement();
		st->AddToCommandString(q);
		st->ParseDeferred();
		st->BindInputInteger(":jobNumber", &jobID);
		st->BindInputInteger(":nstatus", &newStatus);
		st->BindInputInteger(":n1status", &newStatus);
		st->Execute();

		success = (st->getNumberOfProcessedRows()==1);

		delete st;

	} catch(SqlException& error) {
		cout << error.Message() << endl;
		throw(error);
	}

	return success;

}
