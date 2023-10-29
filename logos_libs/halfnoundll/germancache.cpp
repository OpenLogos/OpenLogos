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
#include <logos_include/logoscommon.h>
#include <logos_libs/halfnoun/germandictionary.h>
#include <logos_libs/halfnoun/germansearch.h>
#include <logos_libs/halfnoundll/germancache.h>

GermanCache *GermanCache::germanCache_ = 0;
GermanCache::GermanCache(void)
{

}

void GermanCache::initGermanCache(const LgsString & controlFileName, 
					 const LgsString & wordListFileName,
                     const LgsString & logFileName,
					 const LgsString & placeNamesFileName,
                     const LgsString & properNamesFileName,
                     const LgsVector(LgsString) & companies)
{
      germanDictionary_ = new GermanDictionary(controlFileName.c_str(),
                                               wordListFileName.c_str(),
                                               logFileName.c_str());
      germanDictionary_->streamIn();

      germanSearch_ = new GermanSearch(placeNamesFileName.c_str(),
                                       properNamesFileName.c_str(),
                                       companies, *germanDictionary_, true);
}

GermanCache & GermanCache::singleton(const LgsString & controlFileName, 
					 const LgsString & wordListFileName,
                     const LgsString & logFileName,
					 const LgsString & placeNamesFileName,
                     const LgsString & properNamesFileName,
                     const LgsVector(LgsString) & companies)
{
	if (!germanCache_)
	{
		germanCache_ = new GermanCache;
		germanCache_->initGermanCache(controlFileName, wordListFileName,
			            logFileName, placeNamesFileName,
				        properNamesFileName, companies);
	}
	return *germanCache_;
}

GermanCache::~GermanCache(void)
{
	if (germanDictionary_)
	{
		delete germanDictionary_;
		germanDictionary_ = 0;
	}
	if (germanSearch_)
	{
		delete germanSearch_;
		germanSearch_ = 0;
	}
}

void GermanCache::destroySingleton(void)
{
	if (germanCache_)
	{
		delete germanCache_;
		germanCache_= 0;
	}
}

GermanSearch & GermanCache::germanSearch(void) const
{
	return *germanSearch_;
}

#ifdef _MSC_VER
#include <windows.h>

extern "C" int APIENTRY
DllMain(HINSTANCE hInstance, DWORD dwReason, LPVOID lpReserved)
{
   if (dwReason == DLL_PROCESS_ATTACH)
   {
   } else if (dwReason == DLL_PROCESS_DETACH) 
   {
	   GermanCache::destroySingleton();
   }
   return 1;   // ok
}
#endif
