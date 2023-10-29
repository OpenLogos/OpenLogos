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
#ifndef _XLATIONCONSTS_H_
#define _XLATIONCONSTS_H_

#include <wchar.h>
#include <engine_api/xlationinterface/xlationinterfacedefs.h>
extern const char *CRTF;
extern const char *CSGML;
extern const char *CHTML;
extern const char *CTMX;
extern const char *CXML;
extern const char *CINTERLEAF_ASCII;
extern const char *CFRAME_MIF;
extern const char *CLGS_TXT_EXCHANGE;
extern const char *CMS_WORD8;
extern const char *CRTF_WINHELP;

extern const char* CLGS_ROOT;
extern const char* CLGS_API_OUT_DIR;
extern const char* COUT_EXT;
extern const char* CDIAG_EXT;
extern const char* CALIGN_EXT;
extern const char* CINP_EXT;
extern const char* CLOG_EXT;
extern const char* COUT_FILE_PREFIX;
extern const char* CXLATION_ENGINE;
extern const char* CJOB_ID;
extern const wchar_t* languagePairs;
const int TRANSLATE = 1;
const int TERM_SEARCH = 2;

#endif
