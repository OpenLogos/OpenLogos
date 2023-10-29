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
// target word phrase stored in relational database.
//
// The interface provided by this module will be used for debugging
// the translation engine which is still in fortran.
//
// This module should be discarded when tranlation engine
// is rewritten/ported to C++.
//
// Author: Manoj Agarwala
// History:  1/21/96 Originally concieved
****************************************************************/
#include <logos_include/logoscommon.h>

#include <logos_libs/dbms2restran/targetretriever.h>
#include <logos_libs/dbms2restran/transferretriever.h>
#include <logos_libs/dbms2restran/constantpointerretriever.h>
#include <logos_libs/dbms2restran/gettargetwordphrase.h>
#include <lgs_db_io/dbinstances.h>
#include <logos_libs/utility/argumentexception.h>

#include /**/ <stdio.h>

extern GlobalDBInstances *globalObjects;

// The documentation for TARG_PHRASE parameters is in .h file
int TARG_PHRASE(
                                         short  *targetLangCode,
                                         int    *transferType,
                                         int    *meaningOrCostantId,
                                         char   companyCode[3],
                                         const int      *outputBufferLength,
                                         char   *outputBuffer
)
{
        //Blank pad the entire buffer
        //FORTRAN expects blank padding
        for(int i=0; i < *outputBufferLength; i++)
        {
                outputBuffer[i] = ' ';
        }

        char buffer[8];

        sprintf(buffer, "%02d", *targetLangCode);
        LgsString sTargetLangCd = buffer;

        sprintf(buffer, "%3.3s", companyCode); 
        LgsString sCompanyCode = buffer;

        DWordPhrase  wordPhrase;
        DMorphology  morphology;

        CTargetRetriever  & targetRetriever = globalObjects->targetRetriever();

        try{

                if( *transferType == 1 )
                {
                        CTransferRetriever & transferRetriever = globalObjects->xferRetriever();

                        DTransferVector transferVector;

                        if( transferRetriever.QueryAndFetch(
                                                *meaningOrCostantId,
                                                sTargetLangCd,
                                                sCompanyCode,
                                                transferVector) )
                        {
                                //For now just take the first transfer
                                DTransfer *p_transfer = &transferVector[0];

                                //if( !p_transfer)
                                        //throw( ArgumentException("transfer info not found") );

                                if( !targetRetriever.QueryAndFetch(
                                                                        p_transfer->TargetUsageID(),
                                                                        p_transfer->CompanyCode(),
                                                                        wordPhrase,
                                                                        morphology ) )
                                {
                                        return 1;
                                }
                        }
                        else
                                return 1;
                }
                else
                {
                        CConstantPointerRetriever  & constantPointerRetriever = globalObjects->constPtrRetriever();

                        LgsString sConstantType;

                        if(*transferType == 2)
                                sConstantType = "H";
                        else
                                sConstantType = "L";

                        DConstantPointer constantPointer;

                        if( constantPointerRetriever.QueryAndFetch(
                                                                        *meaningOrCostantId,
                                                sConstantType,
                                        sTargetLangCd,
                                                                        sCompanyCode, //For now hard code
                                                                        constantPointer) )
                        {
                                int usageId = constantPointer.PrimaryUsageID();

                                if( !targetRetriever.QueryAndFetch(
                                                                        usageId,
                                                                        constantPointer.CompanyCode(),
                                                                        wordPhrase,
                                                                        morphology ) )
                                {
                                        return 1;
                                }
                        }
                        else
                        {
                                return 1;
                        }
                }
        }
        catch( SqlException& x)
        {
                cerr << "TARG_PHRASE::" << x.Message() << endl;
                return 6; //Database Error
        }
        catch ( ArgumentException &ae)
        {
                cerr << "TARG_PHRASE::" << ae.Message() << endl;
                return 7; //INI file realted error
        }

        int nChars = wordPhrase.Word().size() ;

        if ( nChars > *outputBufferLength )
                nChars = *outputBufferLength;

        //we copy only nChar characters so that outputBuffer is
        //not null terminated, FORTRAN expects blank padding
        strncpy(outputBuffer, (char *)wordPhrase.Word().c_str(), nChars);

        return 0;
}



