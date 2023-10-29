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
#include <logos_libs/entity/dderivedform.h>

//-------------------------------------------------------------------
DDerivedForm::DDerivedForm()
{
}
//-------------------------------------------------------------------
DDerivedForm::DDerivedForm( const DDerivedForm& rhs )
    : DObject            (rhs)
        , v_derivedFormID    (rhs.DerivedFormID())
        , v_languageCode     (rhs.LanguageCode ())
        , v_patType          (rhs.PatType      ())
        , v_patNumber        (rhs.PatNumber    ())
        , v_stemNumber       (rhs.StemNumber   ())
        , v_ending           (rhs.Ending       ())
        , v_wordClassCode    (rhs.WordClassCode())
        , v_formCode         (rhs.FormCode     ())
//-------------------------------------------------------------------
{
}
//-------------------------------------------------------------------
DDerivedForm::~DDerivedForm()
{
}
//-------------------------------------------------------------------
int DDerivedForm::Compare( const DObject& anObject ) const
{
    // Compare overload.

    DDerivedForm& rhs = dynamic_cast<DDerivedForm&>( const_cast<DObject&>(anObject) );
        return 0;
}
//-------------------------------------------------------------------
const DDerivedForm&
DDerivedForm::operator=( const DDerivedForm& rhs )
{
        DObject::operator= ( rhs );
        SetDerivedFormID            ( rhs.DerivedFormID              () );
        SetLanguageCode             ( rhs.LanguageCode   ()   );
        SetPatType                  (rhs.PatType        ()   );
        SetPatNumber                (rhs.PatNumber        ()   );
        SetStemNumber               (rhs.StemNumber        ()   );
        SetEnding                   (rhs.Ending        ()   );
        SetWordClassCode            (rhs.WordClassCode            ()   );
        SetFormCode                 (rhs.FormCode            ()   );
        return *this;
}
//-------------------------------------------------------------------
void DDerivedForm::SetDerivedFormID( const LgsString& newValue )
{
        v_derivedFormID = newValue;
}
//-------------------------------------------------------------------
void DDerivedForm::SetLanguageCode( int newValue )
{
        v_languageCode = newValue;
}
//-------------------------------------------------------------------
void DDerivedForm::SetPatType( const LgsString& newValue )
{
        v_patType= newValue;
}
//-------------------------------------------------------------------
void DDerivedForm::SetPatNumber( const int newValue )
{
        v_patNumber = newValue;
}
//-------------------------------------------------------------------
void DDerivedForm::SetStemNumber( const int newValue )
{
        v_stemNumber = newValue;
}
//-------------------------------------------------------------------
void DDerivedForm::SetEnding( const LgsString& newValue )
{
        v_ending = newValue;
}
//-------------------------------------------------------------------
void DDerivedForm::SetWordClassCode( int newValue )
{
        v_wordClassCode = newValue;
}
//-------------------------------------------------------------------
void DDerivedForm::SetFormCode( int newValue )
{
        v_formCode = newValue;
}

DerivedFormKey::DerivedFormKey(int pn,int vn,const char *e)
:v_patNumber(pn), v_stemNumber(vn) {
	memset(v_ending, 0, 6); 
	strncpy(v_ending, e, 5);
}
bool operator<(DerivedFormKey k1, DerivedFormKey k2) {
	if(k1.v_patNumber<k2.v_patNumber) 
		return true;
	else if(k1.v_patNumber>k2.v_patNumber)
		return false;

	if(k1.v_stemNumber<k2.v_stemNumber) 
		return true;
	else if(k1.v_stemNumber>k2.v_stemNumber) 
		return false;

	return (strcmp(k1.v_ending, k2.v_ending)<0);
}
