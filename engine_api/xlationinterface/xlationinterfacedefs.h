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
// xlationinterfacedefs.h
// This contains some definitions required for the Translation Interface classes
// Contains constants for the names of all the parameters that
// can be used with the Translation Interface
// Also defines the various values that parameters can take.
//
// Copyright (C) 2000 Logos Corporation
//////////////////////////////////////////////////////////////////////

#ifndef _XLATIONINTERFACEDEFS_H_
#define _XLATIONINTERFACEDEFS_H_

#include <string>
using namespace std;
typedef basic_string<wchar_t> UString; // Unicode string definition

#ifdef _MSC_VER
#pragma warning(disable : 4786) // VC6.0 specific warning message disabled
#else
#include "logos_include/windows.h"
#endif

#include <wchar.h>

#if defined(XLATIONINTERFACECONSTANTS_NO_IMPORT) || defined(XLATIONINTERFACE_EXPORTS)
#define DLLEXPIMP   extern
#else
#define DLLEXPIMP   __declspec(dllimport)
#endif


// Enumeration of the various operations supported by the 
// Translation Server
enum OperationType
{
    TRANSLATE_DOC,
    TRANSLATE_TEXT,
    TERM_SEARCH_DOC,
    TERM_SEARCH_TEXT,
    QRY_CONFIGURATION
};

enum XlationInterfaceErrors
{
    SUCCESS,
    SESSION_COUNT_EXCEEDED,
    INVALID_SESSION,
    SESSION_ALREADY_STARTED,
    INVALID_OPERATION,
    PROXY_ERR_INIT,
    PROXY_ERR_CREATE,
    PROXY_ERR_LANGUAGE,
    PROXY_ERR_CALL,
    PROXY_ERR_SERVER,
    PROXY_ERR_FILE
};

// Constants for Parameter Names/////////////////////////////////////////
// INPUT PARAMETERS
DLLEXPIMP const wchar_t *WCINPUT_FILE;
DLLEXPIMP const wchar_t *WCINPUT_TEXT; 
DLLEXPIMP const wchar_t *WCSOURCE_LANGUAGE;
DLLEXPIMP const wchar_t *WCTARGET_LANGUAGE;
DLLEXPIMP const wchar_t *WCINPUT_FORMAT;
DLLEXPIMP const wchar_t *WCPROTECTION_CHAR;
DLLEXPIMP const wchar_t *WCSOURCE_LOCALE;
DLLEXPIMP const wchar_t *WCTARGET_LOCALE;
DLLEXPIMP const wchar_t *WCPM_RULES;
DLLEXPIMP const wchar_t *WCEXTENDED_SEARCH;
DLLEXPIMP const wchar_t *WCSUBJECT_MATTER_CODES;
DLLEXPIMP const wchar_t *WCCOMPANY_CODES;
DLLEXPIMP const wchar_t *WCFLAG_UNFOUND_WORDS;
DLLEXPIMP const wchar_t *WCGENERATE_ALIGNMENT;
DLLEXPIMP const wchar_t *WCDIAG_LEVEL;
DLLEXPIMP const wchar_t *WCDIAG_LINE_START;
DLLEXPIMP const wchar_t *WCDIAG_LINE_END;
DLLEXPIMP const wchar_t *WCRULE_OVERRIDE_FILE_NAME;
DLLEXPIMP const wchar_t *WCWORD_SEARCH_OPTIONS;
DLLEXPIMP const wchar_t *WCWORD_SEARCH_FOUND_START;
DLLEXPIMP const wchar_t *WCWORD_SEARCH_FOUND_LIMIT;
DLLEXPIMP const wchar_t *WCWORD_SEARCH_UNFOUND_START;
DLLEXPIMP const wchar_t *WCWORD_SEARCH_UNFOUND_LIMIT;
DLLEXPIMP const wchar_t *WCSEARCH_DEFAULT_FLAG;
DLLEXPIMP const wchar_t *WCTARGET_FORM_IMPERATIVE;
DLLEXPIMP const wchar_t *WCSTATISTICS_FLAG;
DLLEXPIMP const wchar_t *WCSMC_TREE_NAME;
DLLEXPIMP const wchar_t *WCDEBUG_FLAG;
DLLEXPIMP const wchar_t *WCDB_MODE_FLAG;
DLLEXPIMP const wchar_t *WCTRACE_FLAG;
DLLEXPIMP const wchar_t *WCLOG_QUERY_FLAG;
DLLEXPIMP const wchar_t *WCSAVE_SCRATCH_FLAG;
DLLEXPIMP const wchar_t *WCGENERATE_LOG;
DLLEXPIMP const wchar_t *WCMAIN_RES1_FILE;
DLLEXPIMP const wchar_t *WCMAIN_RES2_FILE;
DLLEXPIMP const wchar_t *WCMAIN_RES22_FILE;
DLLEXPIMP const wchar_t *WCMAIN_TRAN1_FILE;
DLLEXPIMP const wchar_t *WCMAIN_TRAN2_FILE;
DLLEXPIMP const wchar_t *WCMAIN_TRAN3_FILE;
DLLEXPIMP const wchar_t *WCMAIN_TRAN4_FILE;
DLLEXPIMP const wchar_t *WCMAIN_PARSE1_FILE;
DLLEXPIMP const wchar_t *WCMAIN_PARSE2_FILE;
DLLEXPIMP const wchar_t *WCMAIN_PARSE3_FILE;
DLLEXPIMP const wchar_t *WCMAIN_PARSE4_FILE;
DLLEXPIMP const wchar_t *WCMINI_RES2_FILE;
DLLEXPIMP const wchar_t *WCMINI_TRAN1_FILE;
DLLEXPIMP const wchar_t *WCMINI_TRAN2_FILE;
DLLEXPIMP const wchar_t *WCMINI_TRAN3_FILE;
DLLEXPIMP const wchar_t *WCMINI_TRAN4_FILE;
DLLEXPIMP const wchar_t *WCMINI_PARSE1_FILE;
DLLEXPIMP const wchar_t *WCMINI_PARSE2_FILE;
DLLEXPIMP const wchar_t *WCMINI_PARSE3_FILE;
DLLEXPIMP const wchar_t *WCMINI_PARSE4_FILE;
DLLEXPIMP const wchar_t *WCUSER_ID;


// OUTPUT PARAMETER NAMES
DLLEXPIMP const wchar_t *WCSENTENCE_COUNT;
DLLEXPIMP const wchar_t *WCWORD_COUNT;
DLLEXPIMP const wchar_t *WCOUTPUT_FILE;
DLLEXPIMP const wchar_t *WCSESSION_ID;
DLLEXPIMP const wchar_t *WCERROR_DESC;
DLLEXPIMP const wchar_t *WCDIAG_FILE;
DLLEXPIMP const wchar_t *WCALIGNMENT_FILE;
DLLEXPIMP const wchar_t *WCOUTPUT_TEXT;
DLLEXPIMP const wchar_t *WCDIAG_TEXT;
DLLEXPIMP const wchar_t *WCALIGNED_TEXT;
DLLEXPIMP const wchar_t *WCOLANGUAGE_PAIRS;
DLLEXPIMP const wchar_t *WCOCOMPANY_CODES;
DLLEXPIMP const wchar_t *WCOSMC_CODES;
DLLEXPIMP const wchar_t *WCLOG_FILE;

////////////////////////////////////////////////////////////////////////////////////

///////////////// Constants for Parameter Values////////////////////////////////////
DLLEXPIMP const wchar_t *WCYES;
DLLEXPIMP const wchar_t *WCNO;
DLLEXPIMP const wchar_t *WCEXT_SEARCH;
DLLEXPIMP const wchar_t *WCREG_SEARCH;
DLLEXPIMP const wchar_t *WCENG;
DLLEXPIMP const wchar_t *WCGER;
DLLEXPIMP const wchar_t *WCSPA;
DLLEXPIMP const wchar_t *WCITA;
DLLEXPIMP const wchar_t *WCPOR;
DLLEXPIMP const wchar_t *WCFRE;
DLLEXPIMP const wchar_t *WCRTF;
DLLEXPIMP const wchar_t *WCSGML;
DLLEXPIMP const wchar_t *WCHTML;
DLLEXPIMP const wchar_t *WCTMX;
DLLEXPIMP const wchar_t *WCXML;
DLLEXPIMP const wchar_t *WCINTERLEAF_ASCII;
DLLEXPIMP const wchar_t *WCFRAME_MIF;
DLLEXPIMP const wchar_t *WCLGS_TXT_EXCHANGE;
DLLEXPIMP const wchar_t *WCMS_WORD8;
DLLEXPIMP const wchar_t *WCRTF_WINHELP;
DLLEXPIMP const wchar_t *WCSHORT_DIAG;
DLLEXPIMP const wchar_t *WCLONG_DIAG;
DLLEXPIMP const wchar_t *WCDEEP_DIAG;
DLLEXPIMP const wchar_t *WCSTAT_DIAG;
DLLEXPIMP const wchar_t *WCUNFOUND;
DLLEXPIMP const wchar_t *WCFOUND_NOUN;
DLLEXPIMP const wchar_t *WCFOUND_VERB;
DLLEXPIMP const wchar_t *WCFOUND_ADJ;
DLLEXPIMP const wchar_t *WCFOUND_ADV;
DLLEXPIMP const wchar_t *WCSMC_TREE_DEFAULT;

// ERROR STRINGS FROM THE TRANSLATION SERVER
DLLEXPIMP const wchar_t *WCERR_INPUT_FILE;
DLLEXPIMP const wchar_t *WCERR_READ_OUTPUT;
DLLEXPIMP const wchar_t *WCERR_READ_DIAG;
DLLEXPIMP const wchar_t *WCERR_READ_ALIGN;

#endif
