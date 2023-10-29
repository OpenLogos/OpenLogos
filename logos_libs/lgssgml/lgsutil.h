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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
// LgsUtil.h: interface for the CLgsUtil class.
//
//////////////////////////////////////////////////////////////////////

#ifndef __lgsutil_h__
#define __lgsutil_h__

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "logos_include/logoscommon.h"
#include <stdio.h>

class CLgsUtil  
{
public:
	CLgsUtil();
	virtual ~CLgsUtil();

   static LgsString itoa(int Integr);
   static void Assert(bool expression, LgsString message);
   static void TrimSpace(LgsString& str);
   static void BreakDown(const LgsString&, const LgsString&, LgsVector(LgsString)&, bool);
   static bool IsIn(const LgsString&,const LgsVector(LgsString)& , int, int);
   static bool beginsUpperCase(LgsString& s);
   static bool StrCmpCase(const LgsString& str1, const LgsString& str2);
   static char Upper(char ch);
};

inline char CLgsUtil::Upper(char ch) 
{
	return ((ch >= 'a') && (ch <= 'z'))? ch - 32 : ch;
}

inline bool CLgsUtil::beginsUpperCase(LgsString& s) 
{
   if (s.empty())
      return false;
   LgsString sTmp = s;
   TrimSpace(sTmp);
   if ((sTmp[0] >= 'A') && (sTmp[0] <= 'Z'))
      return true;
   else
      return false;
}

inline LgsString CLgsUtil::itoa(int Integr) 
{
   char buf[25];
   buf[0] = 0;
   sprintf(buf,"%d",Integr);
   return LgsString(buf);
}

inline void CLgsUtil::Assert(bool expression, LgsString message) 
{
   if (!expression)
   {
     throw(message);
   }
}

#ifdef USEREC
// eight percent performance improvement compared to sequentially going through
// array for arrays of 100 elements
inline bool CLgsUtil::IsIn(const LgsString& s, const LgsVector(LgsString) & a, int ix, int size)
{
	if (0 == size)
		return false;
	else if (1 == size)
	{
		if (s.length() != a[ix].length())
			return false;
		else
			return 0 == s.compare(a[ix]);
	}

	int x = s.compare(a[ix + size/2]);

	if (x < 0 )
		return IsIn(s, a, ix, size/2);
	else if (x > 0 )
		return IsIn(s, a, ix + size/2, size - size/2);
	else
		return true;
}

#else

// inline version of the above recursive function
//
// for some reason it works slower on Intel platform
// than the above recursive function
inline bool CLgsUtil::IsIn(const LgsString & s, const LgsVector(LgsString) & a, int ix, int size)
{
	register int sz = size;
	register int i = ix;

	for (;;)
	{
		if (0 == sz )
			return false;
		else if (1 == sz)
		{
			if (s.length() != a[i].length())
				return false;
			else
				return 0 == s.compare(a[i]);
		}

		int x = s.compare(a[i + (sz>>1)]);

		if (x < 0 )
			sz >>= 1;
		else if (x > 0)
		{
			i += sz >> 1;
			sz -= sz >> 1;
		}
		else
			return true;
	}
}

#endif // USEREC

//Removes leading and trailing spaces in a LgsString
inline void CLgsUtil::TrimSpace(LgsString & strWord)
{
	if (strWord.empty())
		return;
   LgsString::iterator vindex = strWord.begin();
   //Remove leading spaces
   while (*vindex == 0x20 && vindex != strWord.end()) 
      strWord.erase(vindex++);
   //Remove trailing spaces
   vindex = strWord.end();
   vindex--;
   while (vindex != strWord.begin() && *vindex == 0x20)
      strWord.erase(vindex--);
}

//Generalized function to break a LgsString to its basic units i.e words
inline void CLgsUtil::BreakDown(const LgsString & Delimiter, const LgsString & wholeString,
                                LgsVector(LgsString) & sVector, bool excludeDelimiter)
{

   //clear the vector
   sVector.clear();

   //Check if the LgsString passed is empty
   if (wholeString.empty())
      return;
	  
	//Find the delimiter in the LgsString
   int DelimIndex = wholeString.find(Delimiter);
   int PrevIndex = 0;

   if (DelimIndex == -1)
   {
      sVector.push_back(wholeString);
      return;
   }

   //If the delimiter is found in the first position ignore it.
   if (DelimIndex == 0)
      DelimIndex = wholeString.find(Delimiter, DelimIndex + Delimiter.length());
   
   LgsString strUnit = "";
   //Get the words in the LgsString  
   while (DelimIndex != -1)
   {
      if (DelimIndex == 0)
      {
         PrevIndex = PrevIndex + Delimiter.length();   
         DelimIndex = wholeString.find(Delimiter, DelimIndex + Delimiter.length());
      }
      else
      {
	      if (excludeDelimiter)
		      PrevIndex = PrevIndex? PrevIndex + Delimiter.length(): PrevIndex;
      }
      //Skip any number of Delimiter between two words
      strUnit = wholeString.substr(PrevIndex, DelimIndex - PrevIndex);
      if (!strUnit.empty() && Delimiter != strUnit)
	  	   sVector.push_back(strUnit);
      PrevIndex = DelimIndex ;
      DelimIndex = wholeString.find(Delimiter, PrevIndex + Delimiter.length());
   }              

   //Get the Last word after the Last Delimiter
   if (excludeDelimiter)
		   PrevIndex = PrevIndex? PrevIndex + Delimiter.length(): PrevIndex;
   strUnit = wholeString.substr(PrevIndex, wholeString.length()- PrevIndex);
   if (!strUnit.empty() && strUnit != Delimiter)
      sVector.push_back(strUnit);
}

inline bool CLgsUtil::StrCmpCase(const LgsString& str1, const LgsString& str2) 
{
	LgsString::const_iterator s1 = str1.begin();
	LgsString::const_iterator s2 = str2.begin();
	while (s1 != str1.end() && s2 != str2.end())
	{
		if (Upper(*s1++) != Upper(*s2++))
			return 1;
	}
	if ((s1 == str1.end() && s2 != str2.end()) || (s1 != str1.end() && s2 == str2.end()))
		return 1;
	else
		return 0;
}
#endif
