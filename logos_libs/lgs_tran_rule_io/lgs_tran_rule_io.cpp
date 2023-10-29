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
// lgs_tran_rule_io.cpp : Defines the initialization routines for the DLL.
//

extern bool tran_rule_io_init();
extern void tran_rule_io_terminate();
extern bool Stat_File_IO_CriticalSect_init();
extern void Stat_File_IO_Clean();

#ifndef _MSC_VER

int tran_rule_init() {
  if (!tran_rule_io_init() || !Stat_File_IO_CriticalSect_init())
    return 0; // failure
  else
    return 1; // OK
}

int tran_rule_terminate() {
  tran_rule_io_terminate();
  Stat_File_IO_Clean();
  return 1;
}

class tran_rule_io_dll_main {
public:
  tran_rule_io_dll_main() { tran_rule_init(); }
  ~tran_rule_io_dll_main() { tran_rule_io_terminate(); }
};

tran_rule_io_dll_main TRAN_RULE_IO_DLL_MAIN;

#else
#include "stdafx.h"
#include <afxdllx.h>

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

static AFX_EXTENSION_MODULE Lgs_tran_rule_ioDLL = { NULL, NULL };

extern "C" int APIENTRY
DllMain(HINSTANCE hInstance, DWORD dwReason, LPVOID lpReserved)
{
	// Remove this if you use lpReserved
	UNREFERENCED_PARAMETER(lpReserved);

	if (dwReason == DLL_PROCESS_ATTACH)
	{
		TRACE0("LGS_TRAN_RULE_IO.DLL Initializing!\n");

		// one time initializaiton
		if ( !tran_rule_io_init() )
			return 0; // failure
		if(!Stat_File_IO_CriticalSect_init())
			return 0;
		
		// Extension DLL one-time initialization
		if (!AfxInitExtensionModule(Lgs_tran_rule_ioDLL, hInstance))
			return 0;

		// Insert this DLL into the resource chain
		// NOTE: If this Extension DLL is being implicitly linked to by
		//  an MFC Regular DLL (such as an ActiveX Control)
		//  instead of an MFC application, then you will want to
		//  remove this line from DllMain and put it in a separate
		//  function exported from this Extension DLL.  The Regular DLL
		//  that uses this Extension DLL should then explicitly call that
		//  function to initialize this Extension DLL.  Otherwise,
		//  the CDynLinkLibrary object will not be attached to the
		//  Regular DLL's resource chain, and serious problems will
		//  result.

		new CDynLinkLibrary(Lgs_tran_rule_ioDLL);
	}
	else if (dwReason == DLL_PROCESS_DETACH)
	{
		TRACE0("LGS_TRAN_RULE_IO.DLL Terminating!\n");

		tran_rule_io_terminate();

		Stat_File_IO_Clean();
		// Terminate the library before destructors are called
		AfxTermExtensionModule(Lgs_tran_rule_ioDLL);
	}
	return 1;   // ok
}

#endif
