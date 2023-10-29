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
#ifndef _gettargetcodes_h_
#define _gettargetcodes_h_

/***************************************************************
// This module is C interface to C++ classes to retrieve
// transfer codes stored in relational database.
// This module is written to replace old READTC fortran calls.
//
// The interface provided by this module will be used by
// the translation engine which is still in fortran.
//
// This module should be discarded when tranlation engine
// is rewritten/ported to C++.
//
// Author: Manoj Agarwala
// History:  10/23/96 Originally concieved
****************************************************************/

#ifdef __cplusplus
extern "C" {
#endif
/***************************************************************
INPUT:	Target language code 1=german, etc
		Transfer type of the information requested
			1=regular entry, 2=hi_constant, 3=lo_constant
		meaning id if word type is 1for regular entry
			otherwise Constant ID					 
		Company Code
		Output buffer to return the information. array of 22

OUTPUT: The numbers in [] is 0 based array index
		outputBuffer[2] = overflow2A - set to 0 if wordType is not 1
		outputBuffer[3] = overflow2B
		outputBuffer[4] = overflow3a - set to 0 if wordType is not 1
		outputBuffer[5] = overflow3b - set to 0 if wordType is not 1
		outputBuffer[6] = Pat Number
		outputBuffer[16]= Gender Code
		All other outputBuffer entries are not used and are set to 0 
Return Value:
		0	no errors
		1	no matching target code found
		6   database error such as odbc connection failed
		7   environment variable LGS_SYSTEM_CONFIG for logos ini file
            not defined or, the ini file does not exist etc.
****************************************************************/

int TARG_CODES(
					 short	*targetLangCode,
					 int	*transferType,
					 int	*meaningOrCostantId,
					 char	companyCode[],
					 short	outputBuffer[],
					 short	diagsw,
					 FILE   *_spec_fp
);

void initializeTrObjects(void);
void freeTrObjects(void);

#ifdef __cplusplus
} //End Of Extern C
#endif

#endif

