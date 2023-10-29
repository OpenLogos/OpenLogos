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
#include <logos_libs/entity/dconstantpointer.h>

//-------------------------------------------------------------------
DConstantPointer::DConstantPointer()
{
}
//-------------------------------------------------------------------
DConstantPointer::DConstantPointer( const DConstantPointer& rhs )
    : DObject            ( rhs )
        , v_constantType     ( rhs.ConstantType     () )
        , v_constantID       ( rhs.ConstantID       () )
        , v_languageCode     ( rhs.LanguageCode     () )
        , v_companyCode      (rhs.CompanyCode       () )
        , v_combiningFormCode(rhs.CombiningFormCode () )
        , v_primaryUsageID   (rhs.PrimaryUsageID    () )
        , v_alternateUsageID (rhs.AlternateUsageID  () )
//-------------------------------------------------------------------
{
}
//-------------------------------------------------------------------
DConstantPointer::~DConstantPointer()
{
}
//-------------------------------------------------------------------
int DConstantPointer::Compare( const DObject& anObject ) const
{
        return 0;
}
//-------------------------------------------------------------------
const DConstantPointer&
DConstantPointer::operator=( const DConstantPointer& rhs )
{
        DObject::operator=    ( rhs );
        SetConstantType       ( rhs.ConstantType     () );
        SetConstantID         ( rhs.ConstantID       () );
        SetLanguageCode       ( rhs.LanguageCode     () );
        SetCompanyCode        ( rhs.CompanyCode      () );
        SetCombiningFormCode  ( rhs.CombiningFormCode() );
        SetPrimaryUsageID     (rhs.PrimaryUsageID    () );
        SetAlternateUsageID   (rhs.AlternateUsageID  () );
        return *this;
}
//-------------------------------------------------------------------
void DConstantPointer::SetConstantType( const LgsString& newValue )
{
        v_constantType = newValue;
}
//-------------------------------------------------------------------
void DConstantPointer::SetConstantID( const int newValue )
{
        v_constantID = newValue;
}
//-------------------------------------------------------------------
void DConstantPointer::SetLanguageCode( const int newValue )
{
        v_languageCode = newValue;
}
//-------------------------------------------------------------------
void DConstantPointer::SetCompanyCode( const LgsString& newValue )
{
        v_companyCode = newValue;
}
//-------------------------------------------------------------------
void DConstantPointer::SetCombiningFormCode( const int newValue )
{
        v_combiningFormCode = newValue;
}
//-------------------------------------------------------------------
void DConstantPointer::SetPrimaryUsageID( const int newValue )
{
        v_primaryUsageID = newValue;
}
//-------------------------------------------------------------------
void DConstantPointer::SetAlternateUsageID( const int newValue )
{
        v_alternateUsageID = newValue;
}
