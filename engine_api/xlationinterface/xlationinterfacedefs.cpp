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
#include <wchar.h>

#ifdef XLATIONINTERFACE_EXPORTS
#define DLLEXPIMP   __declspec(dllexport)
#else
#define DLLEXPIMP
#endif

// Constants for Parameter Names/////////////////////////////////////////
// INPUT PARAMETERS
DLLEXPIMP const wchar_t *WCINPUT_FILE = L"INPUT_FILE";
DLLEXPIMP const wchar_t *WCINPUT_TEXT = L"INPUT_TEXT"; 
DLLEXPIMP const wchar_t *WCSOURCE_LANGUAGE = L"SOURCE_LANGUAGE";
DLLEXPIMP const wchar_t *WCTARGET_LANGUAGE = L"TARGET_LANGUAGE";
DLLEXPIMP const wchar_t *WCINPUT_FORMAT = L"INPUT_FORMAT";
DLLEXPIMP const wchar_t *WCPROTECTION_CHAR = L"PROTECTION_CHAR";
DLLEXPIMP const wchar_t *WCSOURCE_LOCALE = L"SOURCE_LOCALE";
DLLEXPIMP const wchar_t *WCTARGET_LOCALE = L"TARGET_LOCALE";
DLLEXPIMP const wchar_t *WCPM_RULES = L"PM_RULES";
DLLEXPIMP const wchar_t *WCEXTENDED_SEARCH = L"EXTENDED_SEARCH";
DLLEXPIMP const wchar_t *WCSUBJECT_MATTER_CODES = L"SUBJECT_MATTER_CODES";
DLLEXPIMP const wchar_t *WCCOMPANY_CODES = L"COMPANY_CODES";
DLLEXPIMP const wchar_t *WCFLAG_UNFOUND_WORDS = L"FLAG_UNFOUND_WORDS";
DLLEXPIMP const wchar_t *WCGENERATE_ALIGNMENT = L"GENERATE_ALIGNMENT";
DLLEXPIMP const wchar_t *WCDIAG_LEVEL = L"DIAG_LEVEL";
DLLEXPIMP const wchar_t *WCDIAG_LINE_START = L"DIAG_LINE_START";
DLLEXPIMP const wchar_t *WCDIAG_LINE_END = L"DIAG_LINE_END";
DLLEXPIMP const wchar_t *WCRULE_OVERRIDE_FILE_NAME = L"RULE_OVERRIDE_FILE_NAME";
DLLEXPIMP const wchar_t *WCSEARCH_DEFAULT_FLAG = L"SEARCH_DEFAULT";
DLLEXPIMP const wchar_t *WCWORD_SEARCH_OPTIONS = L"WORD_SEARCH_OPTIONS";
DLLEXPIMP const wchar_t *WCWORD_SEARCH_FOUND_START = L"WORD_SEARCH_FOUND_START";
DLLEXPIMP const wchar_t *WCWORD_SEARCH_FOUND_LIMIT = L"WORD_SEARCH_FOUND_LIMIT";
DLLEXPIMP const wchar_t *WCWORD_SEARCH_UNFOUND_START = L"WORD_SEARCH_UNFOUND_START";
DLLEXPIMP const wchar_t *WCWORD_SEARCH_UNFOUND_LIMIT = L"WORD_SEARCH_UNFOUND_LIMIT";
DLLEXPIMP const wchar_t *WCTARGET_FORM_IMPERATIVE = L"TARGET_FORM_IMPERATIVE";
DLLEXPIMP const wchar_t *WCSTATISTICS_FLAG = L"STATISTICS";
DLLEXPIMP const wchar_t *WCSMC_TREE_NAME = L"SMC_TREE_NAME";
DLLEXPIMP const wchar_t *WCDEBUG_FLAG = L"DEBUG_FLAG";
DLLEXPIMP const wchar_t *WCDB_MODE_FLAG = L"DB_MODE";
DLLEXPIMP const wchar_t *WCTRACE_FLAG = L"TRACE_FLAG";
DLLEXPIMP const wchar_t *WCLOG_QUERY_FLAG = L"LOG_QUERY_FLAG";
DLLEXPIMP const wchar_t *WCSAVE_SCRATCH_FLAG = L"SAVE_SCRATCH_FLAG";
DLLEXPIMP const wchar_t *WCGENERATE_LOG = L"GENERATE_LOG";
DLLEXPIMP const wchar_t *WCMAIN_RES1_FILE = L"MAIN_RES1_FILE";
DLLEXPIMP const wchar_t *WCMAIN_RES2_FILE = L"MAIN_RES2_FILE";
DLLEXPIMP const wchar_t *WCMAIN_RES22_FILE = L"MAIN_RES22_FILE";
DLLEXPIMP const wchar_t *WCMAIN_TRAN1_FILE = L"MAIN_TRAN1_FILE";
DLLEXPIMP const wchar_t *WCMAIN_TRAN2_FILE = L"MAIN_TRAN2_FILE";
DLLEXPIMP const wchar_t *WCMAIN_TRAN3_FILE = L"MAIN_TRAN3_FILE";
DLLEXPIMP const wchar_t *WCMAIN_TRAN4_FILE = L"MAIN_TRAN4_FILE";
DLLEXPIMP const wchar_t *WCMAIN_PARSE1_FILE = L"MAIN_PARSE1_FILE";
DLLEXPIMP const wchar_t *WCMAIN_PARSE2_FILE = L"MAIN_PARSE2_FILE";
DLLEXPIMP const wchar_t *WCMAIN_PARSE3_FILE = L"MAIN_PARSE3_FILE";
DLLEXPIMP const wchar_t *WCMAIN_PARSE4_FILE = L"MAIN_PARSE4_FILE";
DLLEXPIMP const wchar_t *WCMINI_RES2_FILE = L"MINI_RES2_FILE";
DLLEXPIMP const wchar_t *WCMINI_TRAN1_FILE = L"MINI_TRAN1_FILE";
DLLEXPIMP const wchar_t *WCMINI_TRAN2_FILE = L"MINI_TRAN2_FILE";
DLLEXPIMP const wchar_t *WCMINI_TRAN3_FILE = L"MINI_TRAN3_FILE";
DLLEXPIMP const wchar_t *WCMINI_TRAN4_FILE = L"MINI_TRAN4_FILE";
DLLEXPIMP const wchar_t *WCMINI_PARSE1_FILE = L"MINI_PARSE1_FILE";
DLLEXPIMP const wchar_t *WCMINI_PARSE2_FILE = L"MINI_PARSE2_FILE";
DLLEXPIMP const wchar_t *WCMINI_PARSE3_FILE = L"MINI_PARSE3_FILE";
DLLEXPIMP const wchar_t *WCMINI_PARSE4_FILE = L"MINI_PARSE4_FILE";
DLLEXPIMP const wchar_t *WCUSER_ID = L"USER_ID";


// OUTPUT PARAMETERS
DLLEXPIMP const wchar_t *WCSENTENCE_COUNT = L"SENTENCE_COUNT";
DLLEXPIMP const wchar_t *WCWORD_COUNT = L"WORD_COUNT";
DLLEXPIMP const wchar_t *WCOUTPUT_FILE = L"OUTPUT_FILE";
DLLEXPIMP const wchar_t *WCSESSION_ID = L"SESSION_ID";
DLLEXPIMP const wchar_t *WCERROR_DESC = L"ERROR_DESC";
DLLEXPIMP const wchar_t *WCDIAG_FILE = L"DIAG_FILE";
DLLEXPIMP const wchar_t *WCALIGNMENT_FILE = L"ALIGNMENT_FILE";
DLLEXPIMP const wchar_t *WCOUTPUT_TEXT = L"OUTPUT_TEXT";
DLLEXPIMP const wchar_t *WCDIAG_TEXT = L"DIAG_TEXT";
DLLEXPIMP const wchar_t *WCALIGNED_TEXT = L"ALIGNED_TEXT";
DLLEXPIMP const wchar_t *WCOLANGUAGE_PAIRS = L"LANGUAGE_PAIRS";
DLLEXPIMP const wchar_t *WCOCOMPANY_CODES = L"COMPANY_CODES";
DLLEXPIMP const wchar_t *WCOSMC_CODES = L"SUBJECT_MATTER_CODES";
DLLEXPIMP const wchar_t *WCLOG_FILE=L"LOG_FILE";

////////////////////////////////////////////////////////////////////////////////////

///////////////// Constants for Parameter Values////////////////////////////////////
DLLEXPIMP const wchar_t *WCYES = L"Yes";
DLLEXPIMP const wchar_t *WCNO = L"No";
DLLEXPIMP const wchar_t *WCEXT_SEARCH = L"EXT_SRCH";
DLLEXPIMP const wchar_t *WCREG_SEARCH = L"REG_SRCH";
DLLEXPIMP const wchar_t *WCENG = L"ENG";
DLLEXPIMP const wchar_t *WCGER = L"GER";
DLLEXPIMP const wchar_t *WCSPA = L"SPA";
DLLEXPIMP const wchar_t *WCITA = L"ITA";
DLLEXPIMP const wchar_t *WCPOR = L"POR";
DLLEXPIMP const wchar_t *WCFRE = L"FRE";
DLLEXPIMP const wchar_t *WCRTF = L"RTF";
DLLEXPIMP const wchar_t *WCSGML = L"SGML";
DLLEXPIMP const wchar_t *WCHTML = L"HTML";
DLLEXPIMP const wchar_t *WCTMX = L"TMX";
DLLEXPIMP const wchar_t *WCXML = L"XML";
DLLEXPIMP const wchar_t *WCINTERLEAF_ASCII = L"INTERLEAF_ASCII";
DLLEXPIMP const wchar_t *WCFRAME_MIF = L"FRAME_MIF";
DLLEXPIMP const wchar_t *WCLGS_TXT_EXCHANGE = L"LGS_TXT_EXCHANGE";
DLLEXPIMP const wchar_t *WCMS_WORD8 = L"MS_WORD8";
DLLEXPIMP const wchar_t *WCRTF_WINHELP = L"RTF_WINHELP";
DLLEXPIMP const wchar_t *WCSHORT_DIAG = L"Short";
DLLEXPIMP const wchar_t *WCLONG_DIAG = L"Long";
DLLEXPIMP const wchar_t *WCDEEP_DIAG = L"Deep";
DLLEXPIMP const wchar_t *WCSTAT_DIAG = L"Statistics";
DLLEXPIMP const wchar_t *WCUNFOUND = L"UNFOUND";
DLLEXPIMP const wchar_t *WCFOUND_NOUN = L"FOUND_NOUN";
DLLEXPIMP const wchar_t *WCFOUND_VERB = L"FOUND_VERB";
DLLEXPIMP const wchar_t *WCFOUND_ADJ = L"FOUND_ADJ";
DLLEXPIMP const wchar_t *WCFOUND_ADV = L"FOUND_ADV";
DLLEXPIMP const wchar_t *WCSMC_TREE_DEFAULT = L"LOGOS1";


DLLEXPIMP const wchar_t *WCERR_INPUT_FILE = L"Temporary input file error (create/write)";
DLLEXPIMP const wchar_t *WCERR_READ_OUTPUT = L"Error reading translated output file";
DLLEXPIMP const wchar_t *WCERR_READ_DIAG = L"Error reading diagnostic file";
DLLEXPIMP const wchar_t *WCERR_READ_ALIGN = L"Error reading alignment file";
