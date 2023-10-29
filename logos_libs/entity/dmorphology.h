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
#ifndef __dmorphology_h__
#define __dmorphology_h__

//-------------------------------------------------------------------
// File - dmorphology.h
//
// Class - DMorphology
//
//-------------------------------------------------------------------

#include <logos_libs/entity/dobject.h>

class DMorphology: public DObject
{
public:
    //---------------------------------------------------------------
    // The public constructors and destructors. The default
    // constructor is typically made public unless class construction
    // is being limited (in which case it is made protected. The
    // destructor is declared virtual to allow subclassing.
    //---------------------------------------------------------------
                 DMorphology();
                 DMorphology( int language_code );
                 DMorphology( const DMorphology& );
             DMorphology( int           genderCode           ,
                              int           inflectionPosition   ,
                              int           sourceAnalysisCode   ,
                              int           languageCode         ,
                              int           protectionCode       ,
                              int           rootUsageID          ,
                              int           sourcePatNumber      ,
//                            int           sourcePatNumber      ,
                              int           sourceStemNumber     ,
                              int           usageID              ,
                              int           verbPrefixInseparable,
                              int           verbPrefixInsepLength,
                              int           verbPrefixSeparable  ,
                              int           verbPrefixSepLength  ,
                              int           wordClassCode        ,
                              int           wordID               );
        virtual ~DMorphology();

    //---------------------------------------------------------------
    // These are "getter" member functions. They allow read access
        // to the internal data of the class. There is no real behavior
        // in these functions but they are important for use in more
        // complex member functions (these member functions can be made
        // "protected" if we do not want to export this data).
        //
        // These functions are typically defined inline because they are
        // usually only one statement in length. If the compiler leaves
        // them inline they should have absolutely not overhead.
    //---------------------------------------------------------------
    const LgsString& CompanyCode            () const;
    int           UsageID                () const;
    int           WordID                 () const;
    int           WordClassCode          () const;
    int           LanguageCode           () const;
    int           GenderCode             () const;
    int                   NumericConstraint      () const;
    int                   AuxiliaryCode          () const;
//    const LgsString& AbcCode                () const;
    int           PatNumber                              () const;
    int           InflectionPosition     () const;
    int           VerbPrefixInseparable  () const;
    int           VerbPrefixSeparable    () const;
    int           VerbPrefixInsepLength  () const;
    int           VerbPrefixSepLength    () const;
//    const LgsString& SourcePatType          () const;
//    int           SourcePatNumber        () const;
    int           SourceStemNumber       () const;
//    const LgsString& TargetPatType          () const;
//    int           TargetPatNumber        () const;
    int           RootUsageID            () const;
    int           ProtectionCode         () const;
        bool          SourceAnalysisCode     () const;
//---------------------------------------------------------------
    // These are "setter" member functions. They allow write access
        // to the internal data of the class. Again, there is no real
        // behavior here. These functions do perform more abstract
        // behavior in that the not only update a member variable but
        // they set the object to a "dirty" state, indicating that it
        // needs to be saved to the database, etc.,.
    //
        // Since they perform non-simple operations they are not defined
        // as inline.
    //---------------------------------------------------------------
    void CompanyCode          ( const LgsString& );
    void UsageID              ( int  );
    void WordID               ( int  );
    void WordClassCode        ( int  );
    void LanguageCode         ( int  );
    void GenderCode           ( int  );
    void NumericConstraint    ( int  );
    void AuxiliaryCode        ( int  );
//    void AbcCode              ( const LgsString& );
    void PatNumber                        ( int  );
    void InflectionPosition   ( int  );
    void VerbPrefixInseparable( int  );
    void VerbPrefixSeparable  ( int  );
    void VerbPrefixInsepLength( int  );
    void VerbPrefixSepLength  ( int  );
//      void SourcePatType        ( const LgsString& );
//    void SourcePatNumber      ( int  );
        void SourceStemNumber     ( int  );
//      void TargetPatType        ( const LgsString& );
//  void TargetPatNumber      ( int  );
    void RootUsageID          ( int  );
        void ProtectionCode       ( int  );
        void SourceAnalysisCode   ( int  );

 //---------------------------------------------------------------
    // Each subclass of DObject should overload the Compare()
        // member function. All the relational operators are written in
        // terms of this member and are inherited from DObject.
        //
        // This member returns a 0 if the object is equal to the
        // the arguement, a -1 if the object is less thatn the arguement,
        // and a +1 if it is greater than the arguement.If a class does
        // not overload this member the one defined in DObject will
        // be called and will return a 0 (meaning the objects will always
        // appear to be equal to each other).
        //---------------------------------------------------------------

        virtual int Compare( const DObject& ) const;

        //---------------------------------------------------------------
        // The assignment operator. This needs to be overloaded in each
        // class. The compiler supplies a default one that performs a
        // shallow copy.
        //---------------------------------------------------------------

        //---------------------------------------------------------------

        const DMorphology& operator=( const DMorphology& );

protected:

    const DomainInteger& GenderCodeDomain() const;

private:
    //---------------------------------------------------------------
    // The member variables. These are private because all access to
        // them, including access by member functions of this class or
        // derived class should be through the "getter" and "setter"
        // member functions.
    //---------------------------------------------------------------

        LgsString v_companyCode;
        int    v_usageID     ;
        int    v_wordID     ;
        int    v_wordClassCode;
        int    v_languageCode;
        int    v_genderCode;
        int    v_numericConstraint;
        int    v_auxiliaryCode;
//      LgsString v_abcCode;
        int    v_patNumber;
        int    v_inflectionPosition;
        int    v_verbPrefixInseparable;
        int    v_verbPrefixSeparable;
        int    v_verbPrefixInsepLength;
        int    v_verbPrefixSepLength;
//      LgsString v_sourcePatType;
//      int    v_sourcePatNumber;
        int    v_sourceStemNumber;
//      LgsString v_targetPatType;
//      int    v_targetPatNumber;
        int    v_rootUsageID;
        int    v_protectionCode;
        bool   v_sourceAnalysisCode;
};

typedef LgsVector(DMorphology) DMorphologyVector;

//-------------------------------------------------------------------
inline const LgsString& DMorphology::CompanyCode() const
{
        return v_companyCode;
}
//-------------------------------------------------------------------
inline int DMorphology::UsageID() const
{
        return v_usageID;
}
//-------------------------------------------------------------------
inline int DMorphology::WordID() const
{
        return v_wordID;
}
//-------------------------------------------------------------------
inline int DMorphology::WordClassCode() const
{
        return v_wordClassCode;
}
//-------------------------------------------------------------------
inline int DMorphology::LanguageCode() const
{
        return v_languageCode;
}
//-------------------------------------------------------------------
inline int DMorphology::GenderCode() const
{
        return v_genderCode;
}
//-------------------------------------------------------------------
inline int DMorphology::NumericConstraint() const
{
        return v_numericConstraint;
}
//-------------------------------------------------------------------
inline int DMorphology::AuxiliaryCode() const
{
        return v_auxiliaryCode;
}
//-------------------------------------------------------------------
inline int DMorphology::PatNumber() const
{
        return v_patNumber;
}
//-------------------------------------------------------------------
inline int DMorphology::InflectionPosition() const
{
        return v_inflectionPosition;
}
//-------------------------------------------------------------------
inline int DMorphology::VerbPrefixInseparable() const
{
        return v_verbPrefixInseparable;
}
//-------------------------------------------------------------------
inline int DMorphology::VerbPrefixSeparable() const
{
        return v_verbPrefixSeparable;
}
//-------------------------------------------------------------------
inline int DMorphology::VerbPrefixInsepLength() const
{
        return v_verbPrefixInsepLength;
}
//-------------------------------------------------------------------
inline int DMorphology::VerbPrefixSepLength() const
{
        return v_verbPrefixSepLength;
}
//-------------------------------------------------------------------
inline int DMorphology::SourceStemNumber() const
{
        return v_sourceStemNumber;
}
//-------------------------------------------------------------------
inline int DMorphology::RootUsageID() const
{
        return v_rootUsageID;
}
//-------------------------------------------------------------------
inline int DMorphology::ProtectionCode() const
{
        return v_protectionCode;
}
//-------------------------------------------------------------------
inline bool DMorphology::SourceAnalysisCode() const
{
        return v_sourceAnalysisCode;
}
//-------------------------------------------------------------------
inline void DMorphology::CompanyCode( const LgsString& newValue )
{
        if (newValue.size() != 3)
        {
                throw ("Invalid Company Code.  Must be 3 characters.");
        }
        v_companyCode = newValue;
}
//-------------------------------------------------------------------
inline void DMorphology::UsageID( int newValue )
{
        v_usageID = newValue;
}
//-------------------------------------------------------------------
inline void DMorphology::WordID( int newValue )
{
        v_wordID = newValue;
}
//-------------------------------------------------------------------
inline void DMorphology::WordClassCode( int newValue )
{
        v_wordClassCode = newValue;
}
//-------------------------------------------------------------------
inline void DMorphology::LanguageCode( int newValue )
{
        v_languageCode = newValue;
}
//-------------------------------------------------------------------
inline void DMorphology::GenderCode( int newValue )
{
    if( GenderCodeDomain().IsValid( newValue ) )
    {
        v_genderCode = newValue;
    }
    else
    {
            v_genderCode = GenderCodeDomain().Default();
    }
}
//-------------------------------------------------------------------
inline void DMorphology::NumericConstraint( int newValue )
{
        v_numericConstraint = newValue;
}
//-------------------------------------------------------------------
inline void DMorphology::AuxiliaryCode( int newValue )
{
        v_auxiliaryCode = newValue;
}
//-------------------------------------------------------------------
inline void DMorphology::PatNumber( int newValue )
{
        v_patNumber = newValue;
}
//-------------------------------------------------------------------
inline void DMorphology::InflectionPosition( int newValue )
{
        v_inflectionPosition = newValue;
}
//-------------------------------------------------------------------
inline void DMorphology::VerbPrefixInseparable( int newValue )
{
        v_verbPrefixInseparable = newValue;
}
//-------------------------------------------------------------------
inline void DMorphology::VerbPrefixSeparable( int newValue )
{
        v_verbPrefixSeparable = newValue;
}
//-------------------------------------------------------------------
inline void DMorphology::VerbPrefixInsepLength( int newValue )
{
        v_verbPrefixInsepLength = newValue;
}
//-------------------------------------------------------------------
inline void DMorphology::VerbPrefixSepLength( int newValue )
{
        v_verbPrefixSepLength = newValue;
}
//-------------------------------------------------------------------
inline void DMorphology::SourceStemNumber( int newValue )
{
        v_sourceStemNumber = newValue;
}
//-------------------------------------------------------------------
inline void DMorphology::RootUsageID( int newValue )
{
        v_rootUsageID = newValue;
}
//-------------------------------------------------------------------
inline void DMorphology::ProtectionCode( int newValue )
{
        v_protectionCode = newValue;
}
//-------------------------------------------------------------------
inline void DMorphology::SourceAnalysisCode( int newValue )
{
        v_sourceAnalysisCode = newValue;
}
//-------------------------------------------------------------------
inline const DomainInteger&
DMorphology::GenderCodeDomain() const
{
    return DObject::GenderCodeDomain();
}

#endif // __dmorphology_H__

