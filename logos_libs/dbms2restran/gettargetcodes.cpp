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
/***************************************************************
// This module is C interface to C++ classes to retrieve
// transfer codes stored in relational database.
// This module is written to replace old READTC fortran calls.
//
// The interface provided by this module will be used by
// the translation engine which is still in fortran.
//
// This module should be discarded when tranlation engine
// is rewritten/ported to C++.
//
// Author: Manoj Agarwala
// History:  10/23/96 Originally concieved
****************************************************************/
#include <logos_include/logoscommon.h>


#include <logos_libs/dbms2restran/transfercodebuilder.h>
#include <logos_libs/dbms2restran/constantcodebuilder.h>
#include <logos_libs/dbms2restran/gettargetcodes.h>
#include <logos_libs/odbcsql/globalsqlconnection.h>
#include <lgs_db_io/dbinstances.h>
#include <logos_libs/utility/argumentexception.h>
#include <logos_libs/multithreadlib/lgscritsect.h>

extern LgsCriticalSection globalObjInitCS;

#include /**/ <stdio.h>
   
extern GlobalDBInstances *globalObjects;


// The documentation for TARG_CODES parameters is in .h file
int TARG_CODES(
                 short  *targetLangCode,
                 int    *transferType,
                 int    *meaningOrCostantId,
                 char   companyCode[3],
                 short  outputBuffer[],
                 short	diagsw,
                 FILE   *_spec_fp
)
{
    char buffer[8];

    sprintf(buffer, "%02d", *targetLangCode);
    LgsString sTargetLangCd = buffer;

    sprintf(buffer, "%3.3s", companyCode);
    LgsString sCompanyCode = buffer;
    if(*transferType == 2 && sCompanyCode == "   ")
    {
      sCompanyCode = "LOG";
    }
    if(sCompanyCode == "   ")
    {
      sCompanyCode = "LOG";
    }

    CTransferCodeVector transferCodeVector;
    CTransferCodeVector::iterator p_transferCode;
    bool transferCodeFound = false;

    memset(outputBuffer,0,sizeof(outputBuffer));

    try{

      if( *transferType == 1 )
        {
          CTransferCodeBuilder  & transferCodeBuilder 
            = globalObjects->xferCodeBuilder();
          
          transferCodeBuilder.QueryAndFetch(*meaningOrCostantId,
                                            sTargetLangCd,
                                            sCompanyCode, 
                                            transferCodeVector, true );
          
          CTransferCodeVector::iterator p_alternateTransferCode 
            = p_transferCode = transferCodeVector.end();
          
          //The vector should not have more than 2 entries
          //We are looking for the one with alternate sequence of 0
          for(CTransferCodeVector::iterator i = transferCodeVector.begin();
              i < transferCodeVector.end(); i++)
            {
              if( i->getAlternateSeq() == 0 ) {
                p_transferCode = i;
                transferCodeFound = true;
              }
              else
                p_alternateTransferCode = i;
            }
          
          //If the gender of the main transfer is null,
          //set it to that of the alternate transfer
          if( p_transferCode != transferCodeVector.end() )
            {
              if( p_alternateTransferCode != transferCodeVector.end()
                  && p_transferCode->getGenderCode() == 0 )
                {
                  p_transferCode->setGenderCode(p_alternateTransferCode->getGenderCode());
                }
            }
        }
      else
        {
          CConstantCodeBuilder  & constantCodeBuilder 
            = globalObjects->constCodeBuilder();

          LgsString sConstantType;

          if(*transferType == 2)
            sConstantType = "H";
          else
            sConstantType = "L";

          if( constantCodeBuilder.QueryAndFetch(*meaningOrCostantId,
                                                sConstantType, sTargetLangCd,
                                                sCompanyCode, transferCodeVector, true ) )
            {
              p_transferCode = transferCodeVector.begin();
              transferCodeFound = true;
            }
        }
    }
    catch( SqlException& x)
      {
        cerr << "TARG_CODES ERROR::" << x.Message() << endl;
        return 6; //Database Error
      }
    catch ( ArgumentException &ae)
      {
        cerr << "TARG_CODES ERROR::" << ae.Message() << endl;
        return 7; //INI file realted error
      }

    if( ! transferCodeFound )
    {
        cerr << endl << "TARG_CODES ERROR:: No Transfer code for" << endl
                 << "language=    " << *targetLangCode << endl
                 << "type=        " << *transferType << endl
                 << "id=          " << *meaningOrCostantId << endl
                 << "companycode= ";
        cerr.write(companyCode,3);
        cerr << endl;

        return 1; //No matching transfer codes found
    }


    outputBuffer[2] = p_transferCode->getOverflow2a();
    outputBuffer[3] = p_transferCode->getOverflow2b();
    outputBuffer[4] = p_transferCode->getOverflow3a();
    outputBuffer[5] = p_transferCode->getOverflow3b();
    outputBuffer[6] = p_transferCode->getPatNumber();

    outputBuffer[16] = p_transferCode->getGenderCode();
    outputBuffer[22] = p_transferCode->getWordClassCode();

    if (diagsw != 0)
      {
        fprintf( _spec_fp, 
                 "TARG_CODES: ID=%7ld lang=%1d MorC=%1ld CC=%3.3s ofl2a=%1d ofl2b=%1d ofl3a=%1d ofl3b=%1d pat=%3d Gender=%1d WC=%2d\n", 
                 *meaningOrCostantId, *targetLangCode, *transferType, companyCode,
                 outputBuffer[2], outputBuffer[3], outputBuffer[4], outputBuffer[5], 
                 outputBuffer[6], outputBuffer[16], outputBuffer[22] );
      }
    
    return 0;
}

void initializeTrObjects(void)
{
	globalObjInitCS.enter();
	if (!globalObjects)
	{
		globalObjects = new GlobalDBInstances();
		globalObjects->initialize();
	}
	globalObjects->setThreadLocals();
	globalObjects->incRefCount();
	globalObjInitCS.leave();
}

void freeTrObjects(void)
{
	globalObjInitCS.enter();
	if (globalObjects)
	{
		globalObjects->freeThreadLocals();
		globalObjects->decRefCount();
		if (!globalObjects->refCount())
		{
			delete globalObjects;
			globalObjects = 0;
		}
	}
	globalObjInitCS.leave();
}
