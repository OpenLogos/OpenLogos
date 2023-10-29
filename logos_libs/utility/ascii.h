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
#ifndef __utility_ascii_h__
#define __utility_ascii_h__

//---------------------------------------------------------------------
// File - ascii.h
//
// Class - Ascii (interface)
//
// Description - Parses a give LgsString into a vector and passes
//              the vector back.
//
//---------------------------------------------------------------------

class Ascii
{
public:

   static LgsString getEnvironment(const LgsString&);
   static void putEnvironment(const LgsString& name, const LgsString& value);

   static LgsString getPrivateProfileString(const LgsString& fileName,
                                            const LgsString& sectionName,
                                            const LgsString& keyword);

   static void writePrivateProfileString(const LgsString& fileName,
                                         const LgsString& sectionName,
                                         const LgsString& keyword,
                                         const LgsString& value);


private:

   static int WriteWinProfileString(char* section, char* entry,
							               char* buffer, char* file_name);
   static int ReadWinProfileString(char *section, char *entry, char *def, 
				                       char *buffer, int buffer_len, char *file_name);
   enum { MaximumArraySize = 1024 };

};

#endif // __utility_system_h__


