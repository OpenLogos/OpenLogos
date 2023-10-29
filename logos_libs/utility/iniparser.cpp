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
//-------------------------------------------------------------------
// File - iniparser.cpp
//
// Class - IniParser (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/utility/iniparser.h>
#include <logos_libs/utility/argumentexception.h>
#include <logos_libs/utility/ascii.h>

//-------------------------------------------------------------------
IniParser::IniParser()
			 :v_isOpen(false)
{
}
//-------------------------------------------------------------------
IniParser::~IniParser()
{
}
//-------------------------------------------------------------------
void IniParser::open(const LgsString& aFileName, const LgsString& aSection)
{
	assert(!isOpen());

#if _MSC_VER >= 1100
	ifstream aStream(aFileName.c_str());
#else
        // changed bk Jul 19 2005
	ifstream aStream(aFileName.c_str());
#endif
	if (!aStream.good())
	{
		throw(ArgumentException("Unable to open configuration file!"));
	}
	fileName(aFileName);
	section (aSection);

	aStream.close();

	v_isOpen = true;
}
//-------------------------------------------------------------------
LgsString IniParser::CreateProfileString(const LgsString& fileName, const LgsString& sectionName,
												  const LgsString& keyword)
{
	return Ascii::getPrivateProfileString(fileName, sectionName, keyword);
}
//-------------------------------------------------------------------
const Argument& IniParser::GetArgument(const LgsString& key)
{
	assert(isOpen());
	
	const Argument& arg = ArgumentParser::GetArgument(key);
	
	if (!arg.IsNull())
	{
		return arg;
	}
	

	LgsString value = CreateProfileString(fileName(), section(), key);

	if (!value.empty())
	{
		Argument newArg(key, value);
		AddArgument(newArg);
		return ArgumentParser::GetArgument(key);
	}
	return Argument::NullArgument();
}
//-------------------------------------------------------------------
void IniParser::PutArgument(const LgsString& key, const LgsString& value)
{
	assert(isOpen());
	Ascii::writePrivateProfileString(fileName(), section(), key, value);
}
