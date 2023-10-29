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
// Ls8Jobs.h: interface for the Ls8Jobs class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _LS8JOBS_H_
#define _LS8JOBS_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <logos_libs/utility/timestamp.h>

class SqlConnection;

class Ls8Jobs
{
public:
	Ls8Jobs(SqlConnection *aConnection);
	virtual ~Ls8Jobs();

   // Get all data for this jobid
   bool GetData(int jobID);
   
   // Find the next job to run (returns -1 if no more jobs)
   int GetNextJobID(const LgsString &userName, bool highonly, const LgsString &excludeUsers) const;
   
   // Update output info for this jobid
   void UpdateDateTimeCompleted(int jobID, const TimeStamp &value) const;
   void UpdateDateTimeStarted(int jobID, const TimeStamp &value) const;
   void UpdateStatus(int jobID, int newStatus) const;
   bool UpdateRunningStatusSafely(int jobID, int newStatus) const;
   void UpdateWordCount(int jobID, int count) const;
   void UpdateSentCurrentComplete(int jobID, int count) const;
   void UpdateSentInputCount(int jobID, int count) const;

   // Getting functions for retrieving info when GetData() returns true
   int JobType() const;
   int Priority() const;
   int SourceLanguage() const;
   int TargetLanguage() const;
   int TargetForm() const;
   int WordSearchOptions() const;
   int WordSearchFoundStart() const;
   int WordSearchFoundLimit() const;
   int WordSearchUnfoundStart() const;
   int WordSearchUnfoundLimit() const;
   int WordCount() const;
   int SentInputCount() const;
   int SentCurrentComplete() const;
   int TranPasses() const;
   int DiagnosticLevel() const;
   int DiagnosticStartLine() const;
   int DiagnosticEndLine() const;
   int Status() const;
   const LgsString &UserID() const;
   const LgsString &CompanyCodes() const;
   const LgsString &SubjectMatterCodes() const;
   int ExtendedSearch() const;
   const LgsString &SourceLocaleData() const;
   const LgsString &TargetLocaleData() const;
   const LgsString &InputFormat() const;
   const LgsString &ProtectionCharacter() const;
   const LgsString &InputFile() const;
   const LgsString &OutputFile() const;
   const LgsString &AlignedFile() const;
   const LgsString &DiagnosticFile() const;
   const LgsString &MainRes1() const;
   const LgsString &MainRes2() const;
   const LgsString &MainRes22() const;
   const LgsString &MainTran1() const;
   const LgsString &MainTran2() const;
   const LgsString &MainTran3() const;
   const LgsString &MainTran4() const;
   const LgsString &MainParse1() const;
   const LgsString &MainParse2() const;
   const LgsString &MainParse3() const;
   const LgsString &MainParse4() const;
   const LgsString &MiniRes2() const;
   const LgsString &MiniTran1() const;
   const LgsString &MiniTran2() const;
   const LgsString &MiniTran3() const;
   const LgsString &MiniTran4() const;
   const LgsString &MiniParse1() const;
   const LgsString &MiniParse2() const;
   const LgsString &MiniParse3() const;
   const LgsString &MiniParse4() const;
   const LgsString &ClientInputFile() const;
   const LgsString &ClientOutputFile() const;
   const LgsString &ClientDiagnosticFile() const;
   const LgsString &ClientAlignedFile() const;
   bool PatternRules() const;

private:
   SqlConnection *m_pConnection;

   // Helping functions to get/set field in ls8jobs
   int GetIntegerArg(int jobID, const LgsString &colName) const;
   void UpdateIntegerArg(int jobID, const LgsString &colName, int val) const;
   LgsString GetStringArg(int jobID, const LgsString &colName) const;
   void UpdateStringArg(int jobID, const LgsString &colName, const LgsString &val) const;
   Time GetTimeArg(int jobID, const LgsString &colName) const;
   void UpdateTimeArg(int jobID, const LgsString &colName, const Time &val) const;
   Date GetDateArg(int jobID, const LgsString &colName) const;
   void UpdateDateArg(int jobID, const LgsString &colName, const Date &val) const;
   TimeStamp GetDateTimeArg(int jobID, const LgsString &colName) const;
   void UpdateDateTimeArg(int jobID, const LgsString &colName, const TimeStamp &val) const;

protected:
   // Record in ls8jobs table filled by GetData()
   int m_nJobType;
   int m_nPriority;
   int m_nSourceLanguage;
   int m_nTargetLanguage;
   int m_nTargetForm;
   int m_nWordSearchOptions;
   int m_nWordSearchFoundStart;
   int m_nWordSearchFoundLimit;
   int m_nWordSearchUnfoundStart;
   int m_nWordSearchUnfoundLimit;
   int m_nWordCount;
   int m_nSentInputCount;
   int m_nSentCurrentComplete;
   int m_nTranPasses;
   int m_nDiagnosticLevel;
   int m_nDiagnosticStartLine;
   int m_nDiagnosticEndLine;
   int m_nStatus;
   LgsString m_sUserID;
   LgsString m_sCompanyCodes;
   LgsString m_sSubjectMatterCodes;
   int m_nExtendedSearch;
   LgsString m_sSourceLocaleData;
   LgsString m_sTargetLocaleData;
   LgsString m_sInputFormat;
   LgsString m_sProtectionCharacter;
   LgsString m_sInputFile;
   LgsString m_sOutputFile;
   LgsString m_sAlignedFile;
   LgsString m_sDiagnosticFile;
   LgsString m_sMainRes1;
   LgsString m_sMainRes2;
   LgsString m_sMainRes22;
   LgsString m_sMainTran1;
   LgsString m_sMainTran2;
   LgsString m_sMainTran3;
   LgsString m_sMainTran4;
   LgsString m_sMainParse1;
   LgsString m_sMainParse2;
   LgsString m_sMainParse3;
   LgsString m_sMainParse4;
   LgsString m_sMiniRes2;
   LgsString m_sMiniTran1;
   LgsString m_sMiniTran2;
   LgsString m_sMiniTran3;
   LgsString m_sMiniTran4;
   LgsString m_sMiniParse1;
   LgsString m_sMiniParse2;
   LgsString m_sMiniParse3;
   LgsString m_sMiniParse4;
   LgsString m_sClientInputFile;
   LgsString m_sClientOutputFile;
   LgsString m_sClientDiagnosticFile;
   LgsString m_sClientAlignedFile;
   bool m_bPatternRules;
};
// -------------------------------------------------------------------
inline int Ls8Jobs::JobType() const
{
   return m_nJobType;
}
// -------------------------------------------------------------------
inline int Ls8Jobs::Priority() const
{
   return m_nPriority;
}
// -------------------------------------------------------------------
inline int Ls8Jobs::SourceLanguage() const
{
   return m_nSourceLanguage;
}
// -------------------------------------------------------------------
inline int Ls8Jobs::TargetLanguage() const
{
   return m_nTargetLanguage;
}
// -------------------------------------------------------------------
inline int Ls8Jobs::TargetForm() const
{
   return m_nTargetForm;
}
// -------------------------------------------------------------------
inline int Ls8Jobs::WordSearchOptions() const
{
   return m_nWordSearchOptions;
}
// -------------------------------------------------------------------
inline int Ls8Jobs::WordSearchFoundStart() const
{
   return m_nWordSearchFoundStart;
}
// -------------------------------------------------------------------
inline int Ls8Jobs::WordSearchFoundLimit() const
{
   return m_nWordSearchFoundLimit;
}
// -------------------------------------------------------------------
inline int Ls8Jobs::WordSearchUnfoundStart() const
{
   return m_nWordSearchUnfoundStart;
}
// -------------------------------------------------------------------
inline int Ls8Jobs::WordSearchUnfoundLimit() const
{
   return m_nWordSearchUnfoundLimit;
}
// -------------------------------------------------------------------
inline int Ls8Jobs::WordCount() const
{
   return m_nWordCount;
}
// -------------------------------------------------------------------
inline int Ls8Jobs::SentInputCount() const
{
   return m_nSentInputCount;
}
// -------------------------------------------------------------------
inline int Ls8Jobs::SentCurrentComplete() const
{
   return m_nSentCurrentComplete;
}
// -------------------------------------------------------------------
inline int Ls8Jobs::TranPasses() const
{
   return m_nTranPasses;
}
// -------------------------------------------------------------------
inline int Ls8Jobs::DiagnosticLevel() const
{
   return m_nDiagnosticLevel;
}
// -------------------------------------------------------------------
inline int Ls8Jobs::DiagnosticStartLine() const
{
   return m_nDiagnosticStartLine;
}
// -------------------------------------------------------------------
inline int Ls8Jobs::DiagnosticEndLine() const
{
   return m_nDiagnosticEndLine;
}
// -------------------------------------------------------------------
inline int Ls8Jobs::Status() const
{
   return m_nStatus;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::UserID() const
{
   return m_sUserID;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::CompanyCodes() const
{
   return m_sCompanyCodes;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::SubjectMatterCodes() const
{
   return m_sSubjectMatterCodes;
}
// -------------------------------------------------------------------
inline int Ls8Jobs::ExtendedSearch() const
{
   return m_nExtendedSearch;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::SourceLocaleData() const
{
   return m_sSourceLocaleData;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::TargetLocaleData() const
{
   return m_sTargetLocaleData;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::InputFormat() const
{
   return m_sInputFormat;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::ProtectionCharacter() const
{
   return m_sProtectionCharacter;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::InputFile() const
{
   return m_sInputFile;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::OutputFile() const
{
   return m_sOutputFile;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::AlignedFile() const
{
   return m_sAlignedFile;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::DiagnosticFile() const
{
   return m_sDiagnosticFile;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MainRes1() const
{
   return m_sMainRes1;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MainRes2() const
{
   return m_sMainRes2;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MainRes22() const
{
   return m_sMainRes22;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MainTran1() const
{
   return m_sMainTran1;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MainTran2() const
{
   return m_sMainTran2;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MainTran3() const
{
   return m_sMainTran3;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MainTran4() const
{
   return m_sMainTran4;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MainParse1() const
{
   return m_sMainParse1;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MainParse2() const
{
   return m_sMainParse2;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MainParse3() const
{
   return m_sMainParse3;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MainParse4() const
{
   return m_sMainParse4;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MiniRes2() const
{
   return m_sMiniRes2;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MiniTran1() const
{
   return m_sMiniTran1;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MiniTran2() const
{
   return m_sMiniTran2;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MiniTran3() const
{
   return m_sMiniTran3;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MiniTran4() const
{
   return m_sMiniTran4;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MiniParse1() const
{
   return m_sMiniParse1;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MiniParse2() const
{
   return m_sMiniParse2;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MiniParse3() const
{
   return m_sMiniParse3;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::MiniParse4() const
{
   return m_sMiniParse4;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::ClientInputFile() const
{
   return m_sClientInputFile;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::ClientOutputFile() const
{
   return m_sClientOutputFile;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::ClientDiagnosticFile() const
{
   return m_sClientDiagnosticFile;
}
// -------------------------------------------------------------------
inline const LgsString &Ls8Jobs::ClientAlignedFile() const
{
   return m_sClientAlignedFile;
}
// -------------------------------------------------------------------
inline bool Ls8Jobs::PatternRules() const
{
   return m_bPatternRules;
}
// -------------------------------------------------------------------
inline void Ls8Jobs::UpdateDateTimeCompleted(int jobID, const TimeStamp &value) const
{
   UpdateDateTimeArg(jobID, "date_time_completed", value);
}
//-------------------------------------------------------------------
inline void Ls8Jobs::UpdateDateTimeStarted(int jobID, const TimeStamp &value) const
{
   UpdateDateTimeArg(jobID, "date_time_started", value);
}
//-------------------------------------------------------------------
inline void Ls8Jobs::UpdateStatus(int jobID, int newStatus) const
{
   UpdateIntegerArg(jobID, "status", newStatus);
}
//-------------------------------------------------------------------
inline void Ls8Jobs::UpdateWordCount(int jobID, int count) const
{
   UpdateIntegerArg(jobID, "word_count", count);
}
//-------------------------------------------------------------------
inline void Ls8Jobs::UpdateSentCurrentComplete(int jobID, int count) const
{
   UpdateIntegerArg(jobID, "sent_current_complete", count);
}
//-------------------------------------------------------------------
inline void Ls8Jobs::UpdateSentInputCount(int jobID, int count) const
{
   UpdateIntegerArg(jobID, "sent_input_count", count);
}

#endif // _LS8JOBS_H_
