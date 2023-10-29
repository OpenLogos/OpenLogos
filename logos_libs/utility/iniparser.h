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
#ifndef __utility_iniparser_h__
#define __utility_iniparser_h__

//---------------------------------------------------------------------
// File - iniparser.h
//
// Class - IniParser (interface)
//
//---------------------------------------------------------------------

#include <logos_libs/utility/argumentparser.h>

class IniParser: public ArgumentParser
{
public:
	//-----------------------------------------------------------------
	// Class functions (declared static) these are available without
	// instances.
	//-----------------------------------------------------------------
	static LgsString CreateProfileString(const LgsString& fileName,
												 const LgsString& sectionName,
												 const LgsString& keyword);
	//-----------------------------------------------------------------
	//-----------------------------------------------------------------

	IniParser();
	virtual ~IniParser();

	//-----------------------------------------------------------------
	//-----------------------------------------------------------------

	void open(const LgsString& fileName, const LgsString& section);

	bool isOpen() const;

	LgsString& fileName();
	LgsString& section();

   //-----------------------------------------------------------------
	// GetArgument() -
	//-----------------------------------------------------------------

	virtual const Argument& GetArgument(const LgsString&);
	virtual void PutArgument(const LgsString& key, const LgsString& value);
	void section(const LgsString&);


protected:
	void fileName(const LgsString&);
private:
	LgsString v_fileName;
	LgsString v_section;
	bool v_isOpen;
};

//-------------------------------------------------------------------
inline LgsString& IniParser::fileName()
{
	return v_fileName;
}
//-------------------------------------------------------------------
inline LgsString& IniParser::section()
{
	return v_section;
}
//-------------------------------------------------------------------
inline void IniParser::fileName(const LgsString& rhs)
{
	v_fileName = rhs;
}
//-------------------------------------------------------------------
inline void IniParser::section(const LgsString& rhs)
{
   if (v_section.compare(rhs) == 0)
   {
      return; 
   }
   removeArguments(); // remove arguments of the previous section
	v_section = rhs;
}
//-------------------------------------------------------------------
inline bool IniParser::isOpen() const
{
	return v_isOpen;
}

#endif // __utility_iniparser_h__


