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

Linux/open source port modifications and additions by Bernd Kiefer, Walter
Kasper, Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken 
*/
/*
 * Author: Walter Kasper (DFKI)
*/
#include <fstream>
#include <iostream>
#include <errno.h>
#include <cstring>
#include "FileUtil.h"

int FileUtil::BUFFERSIZE = 1024  ;

bool FileUtil::copyFile(const char *s, const char *t) {
  if (strcmp(s,t) != 0) {
    ifstream source(s);
    if ( ! source) {
      //      cerr << "Cannot open source file: " << s << "\n";
      return false;
    }
    ofstream target(t);
    if ( ! target) {
      //      cerr << "Cannot open target file: " << t << "\n";
      return false;
    }
    char buffer[BUFFERSIZE];
    while (source.read( buffer, BUFFERSIZE)) {
      target.write(buffer, BUFFERSIZE );
    }
    //last read
    target.write(buffer,source.gcount());
    source.close();
    target.close();
  }
  return true;
}

bool FileUtil::moveFile(const char *s, const char *t) {
  // add some checks? (same file, ...)
  if (strcmp(s,t) != 0) {
    // try rename
    if (rename(s,t) == 0) {
      return true;
    }
    // if rename fails because source and target are on different filesystems, 
    // copy the content and delete source
    if (errno == EXDEV) {
      if (!FileUtil::copyFile(s, t)) {
	return false;
      }
      return FileUtil::deleteFile(s);
    }
  }
  return true;
}

bool FileUtil::deleteFile(const char *s) {
  if ( remove(s) ) {
    return false;
  }
  return true;
}

