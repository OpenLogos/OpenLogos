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
#include "logos_include/logoscommon.h"

// Get the definitions for all the error values and constants from the 
// xlationinterface module
#include <engine_api/xlationinterface/xlationinterfacedefs.cpp>
const char *CRTF = "rtf";
const char *CSGML = "sgml";
const char *CHTML = "sgml";
const char *CTMX = "tmx";
const char *CXML = "xml";
const char *CINTERLEAF_ASCII = "ileaf";
const char *CFRAME_MIF = "frame";
const char *CLGS_TXT_EXCHANGE = "lgssgml";
const char *CMS_WORD8 = "word8";
const char *CRTF_WINHELP = "rtfhelp";

const char* CLGS_ROOT = "LGS_ROOT";
const char* CLGS_API_OUT_DIR = "api_out";
const char* COUT_EXT = ".out";
const char* CDIAG_EXT = ".diag";
const char* CALIGN_EXT = ".align";
const char* CINP_EXT = ".inp";
const char* CLOG_EXT = ".log";
const char* COUT_FILE_PREFIX = DIR_SEP_STR "api";
const char* CXLATION_ENGINE = "SOFTWARE" DIR_SEP_STR "XlationEngine";
const char* CJOB_ID = "NEXT_JOB_ID";
const wchar_t* languagePairs = L"ENG:SPA;ENG:FRE;ENG:ITA;ENG:GER;GER:FRE";
