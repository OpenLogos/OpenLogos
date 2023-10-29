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
#ifndef _spio_h_
#define _spio_h_

/***************************************************************
// This module is C interface to C++ classes to retrieve
// semtab rules stored in relational database.
// This module is written to replace old SPIO fortran calls
// which retrieved the rules for C-Tree.
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

//#if defined(_MSC_VER) 
//	#pragma comment(lib, "semtabrule.lib")
//#endif

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define SP5KEY_ARRAY_SIZE  101
#define SP5DATA_ARRAY_SIZE  1024
#define SP5VTR_ARRAY_SIZE  1024

/***************************************************************
* Function Name: spread
* Parameters:
*		rmode -> read request mode
*				1 = generic read. Use pssed indexKey.
*					pass back the first rule. (Highest in value)
*				2 = read next (if sorted in decending order)
*					high to low
*		indexKey -> use to pass data to call for generic read
*			indexKey[0] ->	Source language  1=german 2=english
*			indexKey[1] ->  Target Language
*			indexKey[2] ->  wc
*			indexKey[3] ->  set
*			indexKey[4] ->  subset
*					must add smc and company code at some point?
*		sp5key	
*			short array [SP5KEY_ARRAY_SIZE]
*			used only to return data from call
*			
*		sp5data
*			short array buffer [SP5DATA_ARRAY_SIZE]
*			used only to return data from call
*		sp5vtr
*			short array buffer [SP5VTR_ARRAY_SIZE]
*			used only to return the data from call	
*
*		retCode
*				0	no errors
*				1	end of rules (past start or end of vector of rules)
*				2	no rules found for generic read
*				3	data will not fit in buffer passed for sp5key
*				4	data will not fit in buffer passed for sp5data
*				5	data will not fit in buffer passed for sp5vtr
*				6   database error such as odbc connection failed
*				7   environment variable LGS_SYSTEM_CONFIG for logos ini file
*                    not defined or, the ini file does not exist etc.
***************************************************************/
void LSPREAD(
			short *rmode,
			short indexKey[5],
			short sp5key[SP5KEY_ARRAY_SIZE],
			short sp5data[SP5DATA_ARRAY_SIZE],
			short sp5vtr[SP5VTR_ARRAY_SIZE],
			short *retCode
);
DLLEXPORT void * LSPREAD_PTR(
			short *rmode,
			short indexKey[5],
			short *retCode,
			char *static_pointer,	// if SMR_CACHE capacity exceeded
			int *bReturningPtr		// rule will be copied into "static_buffer"
									// and "bReturningPtr" flag will
									// set to false
);

void LSPREADCLEAN(void);

#ifdef __cplusplus
} //End Of Extern C
#endif

#endif


