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
/* -*- Mode: C++ -*- */

#ifndef _FAKE_WINDOWS_H
#define _FAKE_WINDOWS_H

typedef unsigned long long __uint64;
typedef unsigned long DWORD;

#include <unistd.h>
#include <fcntl.h>

#define _open open
#define _read read
#define _close close
#define _write write
#ifndef O_BINARY
#define O_BINARY 0
#endif
#define _swab swab

#define Sleep(milliseconds) sleep(milliseconds/1000)

#define DeleteFile remove
#define MoveFile FileUtil::moveFile
// rename doesn't work if source and target are on different file systems
//#define MoveFile(__FromFile, __ToFile) (rename(__FromFile, __ToFile) == 0)

/*
#ifdef __GNUC__

// Note:  Win32-hosted GCC predefines __stdcall and __cdecl, but Unix-
// hosted GCC does not.

#if !defined(__stdcall)
#define __stdcall      __attribute__((stdcall))
#endif
#define _stdcall       __attribute__((stdcall))
#define __fastcall      __stdcall
#define _fastcall       __stdcall

#define __declspec(e) __attribute__((e))
#define _declspec(e)  __attribute__((e))

#define __forceinline   inline

#endif // __GNUC__
*/

#define MAX_PATH 260
#define _MAX_PATH   260 /* max. length of full pathname */
#define _MAX_DRIVE  3   /* max. length of drive component */
#define _MAX_DIR    256 /* max. length of path component */
#define _MAX_FNAME  256 /* max. length of file name component */
#define _MAX_EXT    256 /* max. length of extension component */

#ifndef dllexport
#define dllexport
#endif
#ifndef dllimport
#define dllimport
#endif
#ifndef DLLEXPORT
#define DLLEXPORT
#endif
#ifndef __declspec
#define __declspec(A) 
#endif
#ifndef __cdecl
#define __cdecl
#endif
#ifndef DIR_SEP_CHAR
#define DIR_SEP_CHAR '/'
#endif
#endif
