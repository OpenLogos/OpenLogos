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
#ifndef __LogFile_h__
#define __LogFile_h__

//---------------------------------------------------------------------
// File - LogFile.h
//
// class - LogFile
//
// Description - class to log errors etc to a file
//             - singleton class
//
//---------------------------------------------------------------------

class LogFile
{
public:
    // construct the unique instance of this class
    static void initialize(const LgsString& fileName);

    // singleton - get unique instance of this class
    static LogFile& singleton();

    // delete the unique instance of this class
    static void cleanup();

    ofstream& printDateTime();
    ofstream& skipDateTime();

private:
    static LogFile* theLogFile_s;
    ofstream logStream_;              // logging file

    LogFile(const LgsString& fileName);
};

inline void LogFile::initialize(const LgsString& fileName)
{
    theLogFile_s = new LogFile(fileName);
}

inline LogFile& LogFile::singleton()
{
    assert(theLogFile_s != 0);
    return *theLogFile_s;
}

inline void LogFile::cleanup()
{
   if( theLogFile_s )
   {
      delete theLogFile_s;
      theLogFile_s = NULL;
   }
}

#endif



