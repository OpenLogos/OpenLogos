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
#include <logos_libs/entity/dmorphology.h>

//-------------------------------------------------------------------
DMorphology::DMorphology()  
	: v_usageID                 ( 0 )
	, v_wordID                  ( 0 )
    , v_wordClassCode           ( 1 )
	, v_languageCode            ( 2 )
    , v_genderCode              ( GenderCodeDomain().Default() )
	, v_inflectionPosition      ( 0 )
	, v_verbPrefixInseparable   ( 0 )
	, v_verbPrefixSeparable     ( 0 )
	, v_verbPrefixInsepLength   ( 0 )
	, v_patNumber               ( 0 )
//	, v_sourcePatNumber         ( 0 )
//	, v_sourceStemNumber        ( 0 )
//	, v_targetPatNumber         ( 0 )
	, v_rootUsageID             ( 0 )
	, v_protectionCode          ( 0 )
	, v_sourceAnalysisCode      ( SourceAnalysisCodeDomain().Default() )
{
}
//-------------------------------------------------------------------
DMorphology::DMorphology( int language_code )  
	: v_usageID                 ( 0 )
	, v_wordID                  ( 0 )
    , v_wordClassCode           ( 1 )
	, v_languageCode            ( language_code )
    , v_genderCode              ( GenderCodeDomain().Default() )
	, v_inflectionPosition      ( 0 )
	, v_verbPrefixInseparable   ( 0 )
	, v_verbPrefixSeparable     ( 0 )
	, v_verbPrefixInsepLength   ( 0 )
	, v_patNumber               ( 0 )
//	, v_sourcePatNumber         ( 0 )
//	, v_sourceStemNumber        ( 0 )
//	, v_targetPatNumber         ( 0 )
	, v_rootUsageID             ( 0 )
	, v_protectionCode          ( 0 )
	, v_sourceAnalysisCode     ( SourceAnalysisCodeDomain().Default() )
{
}
//-------------------------------------------------------------------
DMorphology::DMorphology( const DMorphology& rhs )  
    : DObject                   ( rhs )
	, v_companyCode             ( rhs.CompanyCode          () )
	, v_usageID                 ( rhs.WordID               () )
	, v_wordID                  ( rhs.WordID               () )
    , v_wordClassCode           ( rhs.WordClassCode        () )
	, v_languageCode            ( rhs.LanguageCode         () )
    , v_genderCode              ( rhs.GenderCode           () )
	, v_numericConstraint       ( rhs.NumericConstraint    () )
	, v_auxiliaryCode           ( rhs.AuxiliaryCode        () )
	, v_patNumber               ( rhs.PatNumber			   () )
//	, v_abcCode                 ( rhs.AbcCode              () )
	, v_inflectionPosition      ( rhs.InflectionPosition   () )
	, v_verbPrefixInseparable   ( rhs.VerbPrefixInseparable() )
	, v_verbPrefixSeparable     ( rhs.VerbPrefixSeparable  () )
	, v_verbPrefixInsepLength   ( rhs.VerbPrefixInsepLength() )
//	, v_sourcePatType           ( rhs.SourcePatType        () )
//	, v_sourcePatNumber         ( rhs.SourcePatNumber      () )
	, v_sourceStemNumber        ( rhs.SourceStemNumber     () )
//	, v_targetPatType           ( rhs.TargetPatType        () )
//	, v_targetPatNumber         ( rhs.TargetPatNumber      () )
	, v_rootUsageID             ( rhs.RootUsageID          () )
	, v_protectionCode          ( rhs.ProtectionCode       () )
	, v_sourceAnalysisCode      ( rhs.SourceAnalysisCode   () )
{
}
//-------------------------------------------------------------------
DMorphology::DMorphology( int           genderCode           ,
	                      int           inflectionPosition   ,
	                      int           sourceAnalysisCode   ,
	                      int           languageCode         ,
	                      int           protectionCode       ,
	                      int           rootUsageID          ,
	                      int           patNumber            ,
//	                      int           sourcePatNumber      ,
	                      int           sourceStemNumber     ,
	                      int           usageID              ,
	                      int           verbPrefixInseparable,
	                      int           verbPrefixInsepLength,
	                      int           verbPrefixSeparable  ,
	                      int           verbPrefixSepLength  ,
	                      int           wordClassCode        ,
	                      int           wordID               )
	: v_inflectionPosition   ( inflectionPosition    )   
	, v_sourceAnalysisCode   ( sourceAnalysisCode    ) 
	, v_languageCode         ( languageCode          )
	, v_protectionCode       ( protectionCode        )
	, v_rootUsageID          ( rootUsageID           )
	, v_patNumber            ( patNumber             )
//	, v_sourcePatNumber      ( sourcePatNumber       )
	, v_sourceStemNumber     ( sourceStemNumber      )
//	, v_targetPatNumber      ( 0       )
	, v_usageID              ( usageID               )
	, v_verbPrefixInseparable( verbPrefixInseparable )
	, v_verbPrefixInsepLength( verbPrefixInsepLength )
	, v_verbPrefixSeparable  ( verbPrefixSeparable   )
	, v_verbPrefixSepLength  ( verbPrefixSepLength   )
	, v_wordClassCode        ( wordClassCode         )
	, v_wordID               ( wordID                )
{
    GenderCode( genderCode );
}  
//-------------------------------------------------------------------
DMorphology::~DMorphology()  
{
}
//-------------------------------------------------------------------
int DMorphology::Compare( const DObject& anObject ) const
{
    // Compare overload.

    DMorphology& rhs = dynamic_cast<DMorphology&>( const_cast<DObject&>(anObject) );

	if( UsageID() > rhs.UsageID() )
	{
		return 1;
	}
	if( UsageID() < rhs.UsageID() )
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
const DMorphology& 
DMorphology::operator=( const DMorphology& rhs )
{
	DObject::operator= ( rhs );
	CompanyCode           ( rhs.CompanyCode          () );
	UsageID               ( rhs.UsageID              () );
	WordID                ( rhs.WordID               () );
    WordClassCode         ( rhs.WordClassCode        () );
    LanguageCode          ( rhs.LanguageCode         () );
    GenderCode            ( rhs.GenderCode           () );
    NumericConstraint     ( rhs.NumericConstraint    () );
	AuxiliaryCode         ( rhs.AuxiliaryCode        () );
//	AbcCode               ( rhs.AbcCode              () );
	PatNumber			  ( rhs.PatNumber			 () );
	InflectionPosition    ( rhs.InflectionPosition   () );
	VerbPrefixInseparable (rhs.VerbPrefixInseparable () );
	VerbPrefixSeparable   ( rhs.VerbPrefixSeparable  () );
	VerbPrefixInsepLength ( rhs.VerbPrefixInsepLength() );
	VerbPrefixSepLength   ( rhs.VerbPrefixSepLength  () );
//	SourcePatType         ( rhs.SourcePatType        () );
//	SourcePatNumber       ( rhs.SourcePatNumber      () );
	SourceStemNumber      ( rhs.SourceStemNumber     () );
//	TargetPatNumber       ( rhs.TargetPatNumber      () );
//	TargetPatType         ( rhs.TargetPatType        () );
	RootUsageID           ( rhs.RootUsageID          () );
	ProtectionCode        ( rhs.ProtectionCode       () );
	SourceAnalysisCode    ( rhs.SourceAnalysisCode   () );
	return *this;
}
//-------------------------------------------------------------------
