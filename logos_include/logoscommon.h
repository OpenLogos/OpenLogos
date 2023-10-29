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
/* -*- Mode: C++ */
#ifndef __logoscommon_h__
#define __logoscommon_h__

//-------------------------------------------------------------------
// File - logoscommon.h
//
// Description - common include file for all C++ logos projects
//
//-------------------------------------------------------------------

//#define RWSTD_NO_EXCEPTIONS 1
//#define _RWSTDDLL
#if _MSC_VER
	// Disable warnings from visual C++
	#pragma warning(disable : 4018)
	#pragma warning(disable : 4097)
	#pragma warning(disable : 4099)
	#pragma warning(disable : 4114)
	#pragma warning(disable : 4211)
	#pragma warning(disable : 4800)
	#pragma warning(disable : 4237)
	#pragma warning(disable : 4786) // VC6.0 specific warning message disabled
	// Enable warnings from visual C++
	#pragma warning(3 : 4101)
	#pragma warning(3 : 4189)
#endif

// include files
#include "config.h"

#include <cassert>
#include <cstddef>
#include <cstdlib>
#include <cctype>
#include <memory>
#include <stack>
#include <deque>
#include <algorithm>
#include <functional>

#include <logos_include/lgsstring.h>
#include <logos_include/lgscontainers.h>
using namespace std;
// LgsStringVector
typedef LgsVector(LgsString) LgsStringVector;
typedef LgsStringVector::iterator LgsStringIterator;
typedef LgsStringVector::const_iterator LgsStringConstIterator;
// LgsIntVector
typedef LgsVector(int) LgsIntVector;
typedef LgsIntVector::iterator LgsIntIterator;
typedef LgsIntVector::const_iterator LgsIntConstIterator;

#if _MSC_VER

	#if _MSC_VER >= 1100
		#include <iostream>
		#include <fstream>
		#include <iomanip>
		#include <strstream>
		using namespace std;
		//npos is a member of LgsString class
		#define NPOS LgsString::npos	
		// template versions of min and max:
		template<class T> const T& min(const T& x, const T& y) { return x < y ? x : y; }
		template<class T> const T& max(const T& x, const T& y) { return x < y ? y : x; }
	#else
		#include <iostream.h>
		#include <fstream.h>
		#include <iomanip.h>
		#include <strstrea.h>
	#endif

#else
	#define _UNIX_STL_
// changed: bk Jul 19 2005
//#define npos os_npos
	#define NPOS LgsString::npos //NPOS is not a member of LgsString class
	#include <iostream>
	#include <fstream>
	#include <iomanip>
	#include <sstream>
#endif

// macros to disable copy constructors and assignment operators in a class,
// and to generate an empty default constructor
#define DisableCopy(Class) private: Class(const Class&)
#define DisableAssign(Class) private: const Class& operator=(const Class&)
#define DisableCopyAssign(Class) DisableCopy(Class); DisableAssign(Class)
#define DefaultConstructor(Class) Class() {}      // NB - add no semicolon after this

// macro for dummy comparison operators - since all containers now have
// comparison operators, the underlying types must implement these even if not used
#define DummyLess(Class)     bool operator< (const Class&) const { assert(("operator <" #Class, 0)); return true; }
#define DummyGreater(Class)  bool operator> (const Class&) const { assert(("operator >" #Class, 0)); return true; }
#define DummyEqual(Class)    bool operator==(const Class&) const { assert(("operator==" #Class, 0)); return true; }
#define DummyNotEqual(Class) bool operator!=(const Class&) const { assert(("operator!=" #Class, 0)); return true; }

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif

#ifdef _MSC_VER
inline void ConvertTo64Bit(__uint64 & targetTime, const FILETIME & fileTime)
{
	targetTime = fileTime.dwHighDateTime;
	targetTime <<= 32;
	targetTime |= fileTime.dwLowDateTime;
}
#endif

#ifdef _MSC_VER
#define DIR_SEP_CHAR '\\'
#define DIR_SEP_STR "\\"
#define PATH_SEP_CHAR ';'
#define PATH_SEP_STR ";"
#else 
#define DIR_SEP_CHAR '/'
#define DIR_SEP_STR "/"
#define PATH_SEP_CHAR ':'
#define PATH_SEP_STR ":"
#endif

// object class - derive all classes which are to be inherited from, from Object
// the virtual destructor is then inherited, so that no further empty destructors need be defined
class Object
{
public:
    virtual ~Object() {}
};

// useful template function
// precondition T must support <= operator
template <class T>
bool inrange(T value, T minVal, T maxVal)
{
    return (minVal <= value) && (value <= maxVal);
}

#ifndef HAVE_ITOA
inline char * itoa(int i, char *buffer, int base) {
  int digit, pos = 0;
  bool neg = false;
  if (i < 0) { i = -i ; neg = true; }
  if (i == 0) { buffer[0] = '0' ; pos = 1; }
  else 
    while (i > 0) {
      digit = i % base;
      buffer[pos] = (digit < 10) ? ('0' + digit) : ('A' + (digit - 10));
       i = i / base ; 
       pos ++ ;
    }
  if (neg) { buffer[pos++] = '-'; }
  buffer[pos] = '\0';
  char c;
  for(int j = pos - 1, k = 0; j > k; j--, k++) {
    c = buffer[j];
    buffer[j] = buffer[k];
    buffer[k] = c;
  }
  return buffer;
}
#define _itoa itoa
#define HAVE_ITOA
#endif

#ifndef HAVE_ULTOA
inline char * ultoa(unsigned long i, char *buffer, int base){
  unsigned long digit, pos = 0;
  /* if (i < 0) { buffer[0] = '-'; i = -i ; pos = 1; } */
  if (i = 0) { buffer[0] = '0' ; pos = 1; }
  else 
    while (i > 0) {
      digit = i % base;
      buffer[pos] = (digit < 10) ? ('0' + digit) : ('A' + (digit - 10));
       i = i / base ; 
       pos ++ ;
    }
  buffer[pos] = '\0';
  return buffer;
}
#endif

#define _ultoa ultoa

#ifndef HAVE__STRLWR
inline char * _strlwr(char *s) {
  char *t = s;
  while (*s != '\0') { *s = tolower(*s); s++; }
  return t;
}
#endif

#endif
