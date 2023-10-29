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

/*
This file replaces the JNI interface defined in PatStemGenerator.cpp by an
interface based on stdin/stdout communication.

Author: Walter Kasper (DFKI)
*/
#include "ConnectionManager.h"
#include "PatGeneratorImpl.h"
#include "StemGeneratorImpl.h"
#include <iostream>
#include <string>
#include <vector>

using namespace std;

ConnectionManager& CM = ConnectionManager::theCM();

int main(int argc, char* argv[]) {
  const int MAX_CMD = 500;
  const char *argSep = "\t";
  const int MAX_ARGS = 11;
  vector<string> params(MAX_ARGS);
  char cmdBuf[MAX_CMD];
  string cmd;

  while ( cin.getline(cmdBuf,MAX_CMD )) {
    params.erase(params.begin(),params.end());
    char *tok = strtok(cmdBuf,argSep);
    while (tok != NULL) {
      params.push_back(string(tok));
      tok = strtok(NULL, argSep);
    }
    /*
    cerr << "Params: " << params.size() << endl;
    for (int i = 0; i< params.size(); ++i) {
      cerr << params[i] << " ";
    }
    cerr << endl;
    */
    cmd = params[0];
    if (cmd == "quit") {
      break;
    }
    if (cmd == "OpenDBConnection") {
      string server = params[1];
      string user = params[2];
      string pwd = params[3];

      int hconn = CM.openConnection(server.c_str(),
                                    user.c_str(),
                                    pwd.c_str());
      cout << hconn << endl;
    }
    else if (cmd == "GeneratePat") {
      int connection_ = atoi(params[1].c_str());
      string word = params[2];
      string langCd = params[3];
      string wordClassCd = params[4];
      char genderCd = char(atoi(params[5].c_str()));
      char numberCd = char(atoi(params[6].c_str()));
      int sepPrefixLen = atoi(params[7].c_str());
      int insepPrefixLen = atoi(params[8].c_str());

      int patNo = PatGeneratorImpl(connection_).generatePat
	(word.c_str(), 
	 langCd.c_str(),
	 wordClassCd.c_str(),
	 genderCd, 
	 numberCd, 
	 sepPrefixLen, 
	 insepPrefixLen);
      cout << patNo << endl;
    }
    else if (cmd == "OpenStemgenSession") {
      int connection_ = atoi(params[1].c_str());;
      string word = params[2];
      string langCd = params[3];
      int    wordClassCd = atoi(params[4].c_str());;
      int    patNo = atoi(params[5].c_str());;
      char auxCd = char(atoi(params[6].c_str()));
       int    sepPrefixPos = atoi(params[7].c_str());
      int    sepPrefixLen = atoi(params[8].c_str());
      int    insepPrefixPos = atoi(params[9].c_str()); 
      int    insepPrefixLen = atoi(params[10].c_str());

      int hstmt = StemGeneratorImpl(connection_).openSession
      	(word.c_str(),
       	 langCd.c_str(),
	 wordClassCd,
	 patNo,
	 auxCd,
	 sepPrefixPos,
	 sepPrefixLen,
	 insepPrefixPos,
	 insepPrefixLen);

      cout << hstmt << endl;
    }
    else if (cmd == "NextStem") {
      int hstmt_ = atoi(params[1].c_str());

      LgsString stem = StemGeneratorImpl().nextStem(hstmt_);
      if (!stem.size()) {
	cout << 0;
      }
      else {
	// no converion necessary here?
	cout << stem;
      }
      cout  << endl;
    }
    else if (cmd == "CloseStemgenSession") {
      int hstmt_ = atoi(params[1].c_str());

      StemGeneratorImpl().closeSession(hstmt_);
    }
    else if (cmd == "CloseDBConnection") {
      int connection_ = atoi(params[1].c_str());
      CM.closeConnection(connection_);
    }
    else {
      cerr << "Unrecognized command: " << cmdBuf << endl;
    }
  }
}

    
