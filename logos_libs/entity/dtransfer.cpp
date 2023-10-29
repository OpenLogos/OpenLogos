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
#include <logos_libs/entity/dtransfer.h>

//-------------------------------------------------------------------
DTransfer::DTransfer()
{
}
//-------------------------------------------------------------------
DTransfer::DTransfer( const DTransfer& rhs )
    : DObject                 ( rhs )
        , v_companyCode           ( rhs.CompanyCode                             () )
        , v_transferID            ( rhs.TransferID              () )
        , v_meaningID             ( rhs.MeaningID                               () )
        , v_targetLanguageCode    ( rhs.TargetLanguageCode      () )
        , v_targetCountryCode     ( rhs.TargetCountryCode       () )
        , v_targetUsageID         ( rhs.TargetUsageID           () )
        , v_alternateSequence     ( rhs.AlternateSequence       () )
        , v_caseGovernanceCode    ( rhs.CaseGovernanceCode      () )
        , v_combiningFormCode     ( rhs.CombiningFormCode       () )
        , v_individualizationCode ( rhs.IndividualizationCode   () )
        , v_adjectivePlacementCode( rhs.AdjectivePlacementCode  () )
        , v_adverbPlacementCode   ( rhs.AdverbPlacementCode     () )
        , v_reflexCode            ( rhs.ReflexCode                              () )
        , v_isDeactivated         ( rhs.IsDeactivated           () )
{
}
//-------------------------------------------------------------------
DTransfer::~DTransfer()
{
}
//-------------------------------------------------------------------
int DTransfer::Compare( const DObject& anObject ) const
{
    // Compare overload.

    DTransfer& rhs = dynamic_cast<DTransfer&>( const_cast<DObject&>(anObject) );

        if( TransferID() > rhs.TransferID() )
        {
                return 1;
        }
        if( TransferID() < rhs.TransferID() )
        {
                return -1;
    }
        if( CompanyCode() > rhs.CompanyCode() )
        {
                return 1;
        }
        if( CompanyCode() < rhs.CompanyCode() )
        {
                return -1;
    }
        return 0;
}
//-------------------------------------------------------------------
const DTransfer&
DTransfer::operator=( const DTransfer& rhs )
{
        DObject::operator= ( rhs );
        SetCompanyCode            ( rhs.CompanyCode           () );
        SetTransferID             ( rhs.TransferID            () );
        SetMeaningID              ( rhs.MeaningID             () );
    SetTargetLanguageCode     ( rhs.TargetLanguageCode    () );
    SetTargetCountryCode      ( rhs.TargetCountryCode     () );
        SetTargetUsageID          ( rhs.TargetUsageID         () );
    SetAlternateSequence      ( rhs.AlternateSequence     () );
    SetCaseGovernanceCode     ( rhs.CaseGovernanceCode    () );
        SetCombiningFormCode      ( rhs.CombiningFormCode     () );
        SetIndividualizationCode  ( rhs.IndividualizationCode () );
        SetAdjectivePlacementCode ( rhs.AdjectivePlacementCode() );
        SetAdverbPlacementCode    ( rhs.AdverbPlacementCode   () );
        SetReflexCode             ( rhs.ReflexCode            () );
        SetIsDeactivated          ( rhs.IsDeactivated         () );
        return *this;
}
//-------------------------------------------------------------------
void DTransfer::SetTargetLanguageCode( const LgsString& s)
{
        int working = 0;

        if (s.size())
        {
                //Added 01, 02 etc. - Manoj 12/02/96
                if (s == "GE" || s == "01")
                        working = 1;
                if (s == "EN" || s == "02")
                        working = 2;
                if (s == "FR" || s == "03")
                        working = 3;
                if (s == "SP" || s == "04")
                        working = 4;
                if (s == "IT" || s == "05")
                        working = 5;
        }

        try
        {
                SetTargetLanguageCode( working );       //call the old constructor
        }
        catch (LgsString&)
        {
                throw;
        }
}
