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
#ifndef _CONFIGDATAINTERFACEMAIN_INCLUDE_H_
#define _CONFIGDATAINTERFACEMAIN_INCLUDE_H_

#include <stdlib.h>

/* _fix_me_ this is a hack so i am able to compile eventually 
   (bk Jul 19 2005)
*/
#ifndef _MAX_PATH
#define MAX_FILEPATH_LEN 4096
#else
#define MAX_FILEPATH_LEN _MAX_PATH
//_MAX_PATH is defined in stdlib.h
#endif

#define CONFDATA_SUCCESS 1
#define CONFDATA_RETRIVE_ERROR -1
#define CONFDATA_INIT_INTERFACE_ERROR -2
#define CONFDATA_BUFFER_ERROR -3
#define CONFDATA_UNSPPORTED_SRC -4
#define CONFDATA_UNSPPORTED_TRG -5
#define CONFDATA_MOMERY_ERROR -6

/*****************************************************************************
*   Function    : GetConfigData                                              *
*   Description : This funtion provides "C" style function call interface    *
*   Paramters   :                                                            *
*            char * lpszSecKey (in)   :                                      *
*            char *lpszNameKey (in )  :                                      *
*            char **plpszBuf  (in/out):  The ouput string is copied to       * 
*                                         the buffer pointed by this.        *
*            int* pnBufSize (in/out)  :  size of the buffer to hold the      *
*                                        ouput                               *
*     return value :  1 if successful.                                       *
*                     -1 if the key was not found.                           *
*                     -2 If the buffer size is low                           *
*                     buffer size is not sufficient the required buffer      * 
*                     size is returned in *  pnBufSize                       * 
******************************************************************************/

#ifndef __cplusplus
int GetConfigData(const char *lpszSecKey, const char *lpszNameKey, char *lpszBuf, int nBufSize);
int InitConfigDataInterface(int numPass, int jobID, const char* srcLang, const char* trgLang,
                            const char* res1Main, const char* res2Main, const char* res22Main,
                            const char* tran1Main, const char* tran2Main, const char* tran3Main,
                            const char* tran4Main, const char* parse1Main, const char* parse2Main,
                            const char* parse3Main, const char* parse4Main, const char* res2Mini,
                            const char* tran1Mini, const char* tran2Mini, const char* tran3Mini,
                            const char* tran4Mini, const char* parse1Mini, const char* parse2Mini,
                            const char* parse3Mini, const char* parse4Mini, const char* scratchDir);
#endif

#ifdef __cplusplus
extern "C"
{
   int GetConfigData(const char *lpszSecKey, const char *lpszNameKey, char *lpszBuf, int nBufSize);
   int GetConfigValue(const char *secKey, const char *nameKey, LgsString& value);
   int InitConfigDataInterface(int numPass, int jobID, const char* srcLang, const char* trgLang,
                               const char* res1Main, const char* res2Main, const char* res22Main,
                               const char* tran1Main, const char* tran2Main, const char* tran3Main,
                               const char* tran4Main, const char* parse1Main, const char* parse2Main,
                               const char* parse3Main, const char* parse4Main, const char* res2Mini,
                               const char* tran1Mini, const char* tran2Mini, const char* tran3Mini,
                               const char* tran4Mini, const char* parse1Mini, const char* parse2Mini,
                               const char* parse3Mini, const char* parse4Mini, const char* scratchDir);
}
#endif
#endif //_CONFIGDATAINTERFACEMAIN_INCLUDE_H_
