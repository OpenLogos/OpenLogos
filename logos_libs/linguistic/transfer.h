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
#ifndef __Transfer_h__
#define __Transfer_h__

//-------------------------------------------------------------------
// File - Transfer.h
//
// Class - Transfer
//
//-------------------------------------------------------------------

class Transfer
{
public:
        DummyEqual(Transfer);

        //---------------------------------------------------------------
        //---------------------------------------------------------------

                 Transfer();
                 Transfer( const Transfer& );
        virtual ~Transfer();

        //---------------------------------------------------------------
        //---------------------------------------------------------------

    int     id () const;
    void setId (int);

    int     combiningFormCode() const;
    void setCombiningFormCode(int);

    int     sequenceNumber() const;
    void setSequenceNumber(int);

        //---------------------------------------------------------------
        //---------------------------------------------------------------

        const Transfer& operator=( const Transfer& );

	bool DLLEXPORT operator< (const Transfer& rhs);
	friend bool operator< (const Transfer& lhs, const Transfer& rhs);

private:

    int v_id;
    int v_combiningFormCode;
    int v_sequenceNumber;
};

typedef LgsVector(Transfer) TransferVector;

//-------------------------------------------------------------------
inline int Transfer::id () const
{
    return v_id;
}

inline void Transfer::setId (int n)
{
    v_id = n;
}
//-------------------------------------------------------------------
inline int Transfer::combiningFormCode() const
{
    return v_combiningFormCode;
}

inline void Transfer::setCombiningFormCode(int n)
{
    v_combiningFormCode = n;
}
//-------------------------------------------------------------------
inline int Transfer::sequenceNumber() const
{
    return v_sequenceNumber;
}

inline void Transfer::setSequenceNumber(int n)
{
    v_sequenceNumber = n;
}

#endif // __Transfer_h__

