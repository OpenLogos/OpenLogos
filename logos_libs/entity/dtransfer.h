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
#ifndef __DTransfer_h__
#define __DTransfer_h__

//-------------------------------------------------------------------
// File - DTransfer.h
//
// Class - DTransfer
//
//-------------------------------------------------------------------

#include <logos_libs/entity/dobject.h>

class DTransfer: public DObject
{
public:
    //---------------------------------------------------------------
    // The public constructors and destructors. The default
    // constructor is typically made public unless class construction
    // is being limited (in which case it is made protected. The
    // destructor is declared virtual to allow subclassing.
    //---------------------------------------------------------------
                 DTransfer();
                 DTransfer( const DTransfer& );
        virtual ~DTransfer();

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
    const LgsString&       CompanyCode            () const;
    int                         TransferID             () const;
    int                         MeaningID              () const;
    int                         TargetLanguageCode     () const;
    int                         TargetCountryCode      () const;
    int                         TargetUsageID          () const;
    int                         AlternateSequence      () const;
    int                         CaseGovernanceCode     () const;
    int                         CombiningFormCode      () const;
    int                         IndividualizationCode  () const;
    int                         AdjectivePlacementCode () const;
    int                         AdverbPlacementCode    () const;
    int                         ReflexCode             () const;
        bool                    IsDeactivated          () const;

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

    void SetCompanyCode            ( const LgsString& );
    void SetTransferID             ( int  );
    void SetMeaningID              ( int  );
    void SetTargetLanguageCode     ( int  );
//GRW
        void SetTargetLanguageCode         ( const LgsString& );
    void SetTargetCountryCode      ( int  );
    void SetTargetUsageID          ( int  );
    void SetAlternateSequence      ( int  );
    void SetCaseGovernanceCode     ( int  );
        void SetCombiningFormCode      ( int  );
    void SetIndividualizationCode  ( int  );
        void SetAdjectivePlacementCode ( int  );
        void SetAdverbPlacementCode    ( int  );
        void SetReflexCode             ( int  );
        void SetIsDeactivated          ( bool );
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

        const DTransfer& operator=( const DTransfer& );

private:
    //---------------------------------------------------------------
    // The member variables. These are private because all access to
        // them, including access by member functions of this class or
        // derived class should be through the "getter" and "setter"
        // member functions.
    //---------------------------------------------------------------

        LgsString v_companyCode;
        int    v_transferID     ;
        int    v_meaningID     ;
        int    v_targetLanguageCode;
        int    v_targetCountryCode;
        int    v_targetUsageID     ;
        int    v_alternateSequence;
        int    v_caseGovernanceCode;
        int        v_combiningFormCode;
        int    v_individualizationCode;
        int    v_adjectivePlacementCode;
        int    v_adverbPlacementCode;
        int    v_reflexCode;
        bool   v_isDeactivated;
};

typedef LgsVector(DTransfer) DTransferVector;

//-------------------------------------------------------------------
inline const LgsString& DTransfer::CompanyCode() const
{
        return v_companyCode;
}
//-------------------------------------------------------------------
inline int DTransfer::TransferID() const
{
        return v_transferID;
}
//-------------------------------------------------------------------
inline int DTransfer::MeaningID() const
{
        return v_meaningID;
}
//-------------------------------------------------------------------
inline int DTransfer::TargetLanguageCode() const
{
        return v_targetLanguageCode;
}
//-------------------------------------------------------------------
inline int DTransfer::TargetCountryCode() const
{
        return v_targetCountryCode;
}
//-------------------------------------------------------------------
inline int DTransfer::TargetUsageID() const
{
        return v_targetUsageID;
}
//-------------------------------------------------------------------
inline int DTransfer::AlternateSequence() const
{
        return v_alternateSequence;
}
//-------------------------------------------------------------------
inline int DTransfer::CaseGovernanceCode() const
{
        return v_caseGovernanceCode;
}
//-------------------------------------------------------------------
inline int DTransfer::CombiningFormCode() const
{
        return v_combiningFormCode;
}
//-------------------------------------------------------------------
inline int DTransfer::IndividualizationCode() const
{
        return v_individualizationCode;
}
//-------------------------------------------------------------------
inline int DTransfer::AdjectivePlacementCode() const
{
        return v_adjectivePlacementCode;
}
//-------------------------------------------------------------------
inline int DTransfer::AdverbPlacementCode() const
{
        return v_adverbPlacementCode;
}
//-------------------------------------------------------------------
inline int DTransfer::ReflexCode() const
{
        return v_reflexCode;
}
//-------------------------------------------------------------------
inline bool DTransfer::IsDeactivated() const
{
        return v_isDeactivated;
}
//-------------------------------------------------------------------
//-------------------------------------------------------------------
inline void DTransfer::SetCompanyCode( const LgsString& newValue )
{
        if (newValue.size() != 3)
        {
                throw ("Invalid Company Code.  Must be 3 characters.");
        }
        v_companyCode = newValue;
}
//-------------------------------------------------------------------
inline void DTransfer::SetTransferID( int newValue )
{
        v_transferID = newValue;
}
//-------------------------------------------------------------------
inline void DTransfer::SetMeaningID( int newValue )
{
        v_meaningID = newValue;
}
//-------------------------------------------------------------------
inline void DTransfer::SetTargetLanguageCode( int newValue )
{
        v_targetLanguageCode = newValue;
}
//-------------------------------------------------------------------
inline void DTransfer::SetTargetCountryCode( int newValue )
{
        v_targetCountryCode = newValue;
}
//-------------------------------------------------------------------
inline void DTransfer::SetTargetUsageID( int newValue )
{
        v_targetUsageID = newValue;
}
//-------------------------------------------------------------------
inline void DTransfer::SetAlternateSequence( int newValue )
{
        v_alternateSequence = newValue;
}
//-------------------------------------------------------------------
inline void DTransfer::SetCaseGovernanceCode( int newValue )
{
        v_caseGovernanceCode = newValue;
}
//-------------------------------------------------------------------
inline void DTransfer::SetCombiningFormCode( int newValue )
{
        v_combiningFormCode = newValue;
}
//-------------------------------------------------------------------
inline void DTransfer::SetIndividualizationCode( int newValue )
{
        v_individualizationCode = newValue;
}
//-------------------------------------------------------------------
inline void DTransfer::SetAdjectivePlacementCode( int newValue )
{
        v_adjectivePlacementCode = newValue;
}
//-------------------------------------------------------------------
inline void DTransfer::SetAdverbPlacementCode( int newValue )
{
        v_adverbPlacementCode = newValue;
}
//-------------------------------------------------------------------
inline void DTransfer::SetReflexCode( int newValue )
{
        v_reflexCode = newValue;
}
//-------------------------------------------------------------------
inline void DTransfer::SetIsDeactivated( bool newValue )
{
        v_isDeactivated = newValue;
}

#endif // __DTransfer_H__

