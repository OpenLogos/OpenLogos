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
#ifndef __LSubjectMatter_h__
#define __LSubjectMatter_h__

//---------------------------------------------------------------------
// File - LSubjectMatter.h
//
// Class - LSubjectMatter (interface)
//
//---------------------------------------------------------------------

class LSubjectMatter
{
public:
        //-----------------------------------------------------------------
        //-----------------------------------------------------------------

    LSubjectMatter();
    LSubjectMatter( int genericCode, int atomicCode);
    LSubjectMatter( const LSubjectMatter& );
    virtual ~LSubjectMatter();

        //-----------------------------------------------------------------
        //-----------------------------------------------------------------

	int  AtomicCode() const;
    void AtomicCode( int );
	int GenericCode() const;
	void GenericCode(int);

    const LSubjectMatter& operator= ( const LSubjectMatter& );

    bool operator==( const LSubjectMatter& ) const;
    bool operator< ( const LSubjectMatter& ) const;
    bool operator<=( const LSubjectMatter& ) const;
    bool operator>=( const LSubjectMatter& ) const;
    bool operator> ( const LSubjectMatter& ) const;
    bool operator!=( const LSubjectMatter& ) const;

protected:

private:
        //-----------------------------------------------------------------
        //-----------------------------------------------------------------

	int	v_genericCode;
    int	v_atomicCode  ;
};


typedef LgsVector(LSubjectMatter)           LSubjectMatterVector;

//---------------------------------------------------------------------
inline int LSubjectMatter::AtomicCode() const
{
        return v_atomicCode;
}
//---------------------------------------------------------------------
inline void
LSubjectMatter::AtomicCode ( int anAtomicCode )
{
    v_atomicCode = anAtomicCode;
}
//---------------------------------------------------------------------
inline int LSubjectMatter::GenericCode() const
{
	return v_genericCode;
}
//---------------------------------------------------------------------
inline void
LSubjectMatter::GenericCode ( int genericCode )
{
    v_genericCode = genericCode;
}
//---------------------------------------------------------------------
inline bool
LSubjectMatter::operator!=( const LSubjectMatter& rhs ) const
{
    return !operator==( rhs );
}
//---------------------------------------------------------------------
inline bool
LSubjectMatter::operator<=( const LSubjectMatter& rhs ) const
{
    return operator<( rhs ) || operator==( rhs );
}
//---------------------------------------------------------------------
inline bool
LSubjectMatter::operator>=( const LSubjectMatter& rhs ) const
{
    return !operator<( rhs );
}
//---------------------------------------------------------------------
inline bool
LSubjectMatter::operator>( const LSubjectMatter& rhs ) const
{
    return operator!=( rhs ) && (!operator<( rhs ));
}

#endif // __LSubjectMatter_h__

