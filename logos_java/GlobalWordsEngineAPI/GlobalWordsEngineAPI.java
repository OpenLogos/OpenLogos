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

Linux/open source port modifications and additions by Bernd Kiefer, Walter
Kasper, Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken 
*/

package logos_java.GlobalWordsEngineAPI;
import java.lang.*;

public class GlobalWordsEngineAPI
{
   public native boolean InitializeTranslationSession();
   public native boolean StartTranslationSession();
   public native boolean CloseTranslationSession();
   public native void SetInputParameter(String name, String value);
   public native void SetOperationType(int operationType);
   public native String GetOutputValue(String name);
   public native int GetLastError();
   public native String GetLastErrorString();

   public GlobalWordsEngineAPI()
   {
   }

   static
   {
      System.loadLibrary("xlationinterface");
   }

   // Constants for Parameter Names/////////////////////////////////////////
   // INPUT PARAMETERS
   public static final String WCINPUT_FILE = "INPUT_FILE";
   public static final String WCINPUT_TEXT = "INPUT_TEXT";
   public static final String WCSOURCE_LANGUAGE = "SOURCE_LANGUAGE";
   public static final String WCTARGET_LANGUAGE = "TARGET_LANGUAGE";
   public static final String WCINPUT_FORMAT = "INPUT_FORMAT";
   public static final String WCPROTECTION_CHAR = "PROTECTION_CHAR";
   public static final String WCSOURCE_LOCALE = "SOURCE_LOCALE";
   public static final String WCTARGET_LOCALE = "TARGET_LOCALE";
   public static final String WCPM_RULES = "PM_RULES";
   public static final String WCEXTENDED_SEARCH = "EXTENDED_SEARCH";
   public static final String WCSUBJECT_MATTER_CODES = "SUBJECT_MATTER_CODES";
   public static final String WCCOMPANY_CODES = "COMPANY_CODES";
   public static final String WCFLAG_UNFOUND_WORDS = "FLAG_UNFOUND_WORDS";
   public static final String WCGENERATE_ALIGNMENT = "GENERATE_ALIGNMENT";
   public static final String WCDIAG_LEVEL = "DIAG_LEVEL";
   public static final String WCDIAG_LINE_START = "DIAG_LINE_START";
   public static final String WCDIAG_LINE_END = "DIAG_LINE_END";
   public static final String WCRULE_OVERRIDE_FILE_NAME = "RULE_OVERRIDE_FILE_NAME";
   public static final String WCSEARCH_DEFAULT_FLAG = "SEARCH_DEFAULT";
   public static final String WCWORD_SEARCH_OPTIONS = "WORD_SEARCH_OPTIONS";
   public static final String WCWORD_SEARCH_FOUND_START = "WORD_SEARCH_FOUND_START";
   public static final String WCWORD_SEARCH_FOUND_LIMIT = "WORD_SEARCH_FOUND_LIMIT";
   public static final String WCWORD_SEARCH_UNFOUND_START = "WORD_SEARCH_UNFOUND_START";
   public static final String WCWORD_SEARCH_UNFOUND_LIMIT = "WORD_SEARCH_UNFOUND_LIMIT";
   public static final String WCTARGET_FORM_IMPERATIVE = "TARGET_FORM_IMPERATIVE";
   public static final String WCSTATISTICS_FLAG = "STATISTICS";
   public static final String WCSMC_TREE_NAME = "SMC_TREE_NAME";
   public static final String WCDEBUG_FLAG = "DEBUG_FLAG";
   public static final String WCDB_MODE_FLAG = "DB_MODE";
   public static final String WCTRACE_FLAG = "TRACE_FLAG";
   public static final String WCLOG_QUERY_FLAG = "LOG_QUERY_FLAG";
   public static final String WCSAVE_SCRATCH_FLAG = "SAVE_SCRATCH_FLAG";
   public static final String WCGENERATE_LOG = "GENERATE_LOG";
   public static final String WCMAIN_RES1_FILE = "MAIN_RES1_FILE";
   public static final String WCMAIN_RES2_FILE = "MAIN_RES2_FILE";
   public static final String WCMAIN_RES22_FILE = "MAIN_RES22_FILE";
   public static final String WCMAIN_TRAN1_FILE = "MAIN_TRAN1_FILE";
   public static final String WCMAIN_TRAN2_FILE = "MAIN_TRAN2_FILE";
   public static final String WCMAIN_TRAN3_FILE = "MAIN_TRAN3_FILE";
   public static final String WCMAIN_TRAN4_FILE = "MAIN_TRAN4_FILE";
   public static final String WCMAIN_PARSE1_FILE = "MAIN_PARSE1_FILE";
   public static final String WCMAIN_PARSE2_FILE = "MAIN_PARSE2_FILE";
   public static final String WCMAIN_PARSE3_FILE = "MAIN_PARSE3_FILE";
   public static final String WCMAIN_PARSE4_FILE = "MAIN_PARSE4_FILE";
   public static final String WCMINI_RES2_FILE = "MINI_RES2_FILE";
   public static final String WCMINI_TRAN1_FILE = "MINI_TRAN1_FILE";
   public static final String WCMINI_TRAN2_FILE = "MINI_TRAN2_FILE";
   public static final String WCMINI_TRAN3_FILE = "MINI_TRAN3_FILE";
   public static final String WCMINI_TRAN4_FILE = "MINI_TRAN4_FILE";
   public static final String WCMINI_PARSE1_FILE = "MINI_PARSE1_FILE";
   public static final String WCMINI_PARSE2_FILE = "MINI_PARSE2_FILE";
   public static final String WCMINI_PARSE3_FILE = "MINI_PARSE3_FILE";
   public static final String WCMINI_PARSE4_FILE = "MINI_PARSE4_FILE";
   public static final String WCUSER_ID = "USER_ID";

  // OUTPUT PARAMETERS
   public static final String WCSENTENCE_COUNT = "SENTENCE_COUNT";
   public static final String WCWORD_COUNT = "WORD_COUNT";
   public static final String WCOUTPUT_FILE = "OUTPUT_FILE";
   public static final String WCSESSION_ID = "SESSION_ID";
   public static final String WCERROR_DESC = "ERROR_DESC";
   public static final String WCDIAG_FILE = "DIAG_FILE";
   public static final String WCALIGNMENT_FILE = "ALIGNMENT_FILE";
   public static final String WCOUTPUT_TEXT = "OUTPUT_TEXT";
   public static final String WCDIAG_TEXT = "DIAG_TEXT";
   public static final String WCALIGNED_TEXT = "ALIGNED_TEXT";
   public static final String WCOLANGUAGE_PAIRS = "LANGUAGE_PAIRS";
   public static final String WCOCOMPANY_CODES = "COMPANY_CODES";
   public static final String WCOSMC_CODES = "SUBJECT_MATTER_CODES";
   public static final String WCLOG_FILE = "LOG_FILE";

///////////////// Constants for Parameter Values////////////////////////////////////
   public static final String WCYES = "Yes";
   public static final String WCNO = "No";
   public static final String WCEXT_SEARCH = "EXT_SRCH";
   public static final String WCREG_SEARCH = "REG_SRCH";
   public static final String WCENG = "ENG";
   public static final String WCGER = "GER";
   public static final String WCSPA = "SPA";
   public static final String WCITA = "ITA";
   public static final String WCPOR = "POR";
   public static final String WCFRE = "FRE";
   public static final String WCRTF = "RTF";
   public static final String WCSGML = "SGML";
   public static final String WCHTML = "HTML";
   public static final String WCTMX = "TMX";
   public static final String WCXML = "XML";
   public static final String WCINTERLEAF_ASCII = "INTERLEAF_ASCII";
   public static final String WCFRAME_MIF = "FRAME_MIF";
   public static final String WCLGS_TXT_EXCHANGE = "LGS_TXT_EXCHANGE";
   public static final String WCMS_WORD8 = "MS_WORD8";
   public static final String WCRTF_WINHELP = "RTF_WINHELP";
   public static final String WCSHORT_DIAG = "Short";
   public static final String WCLONG_DIAG = "Long";
   public static final String WCDEEP_DIAG = "Deep";
   public static final String WCSTAT_DIAG = "Statistics";
   public static final String WCUNFOUND = "UNFOUND";
   public static final String WCFOUND_NOUN = "FOUND_NOUN";
   public static final String WCFOUND_VERB = "FOUND_VERB";
   public static final String WCFOUND_ADJ = "FOUND_ADJ";
   public static final String WCFOUND_ADV = "FOUND_ADV";
   public static final String WCSMC_TREE_DEFAULT = "LOGOS1";

   public static final String WCERR_INPUT_FILE = "Temporary input file error (create/write)";
   public static final String WCERR_READ_OUTPUT = "Error reading translated output file";
   public static final String WCERR_READ_DIAG = "Error reading diagnostic file";
   public static final String WCERR_READ_ALIGN = "Error reading alignment file";

   public static final int TRANSLATE_DOC = 0;
   public static final int TRANSLATE_TEXT = 1;
   public static final int TERM_SEARCH_DOC = 2;
   public static final int TERM_SEARCH_TEXT = 3;
   public static final int QRY_CONFIGURATION = 4;
}
