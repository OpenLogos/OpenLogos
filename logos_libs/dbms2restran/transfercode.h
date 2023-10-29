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
#ifndef _transfercode_h_
#define _transfercode_h_

//************************************************************
// Author:              Manoj Agarwala
// History:             11/1/96 - Originally Conceived
//************************************************************

class CTransferCode
{
public:
        DummyLess(CTransferCode);
        DummyEqual(CTransferCode);

        CTransferCode();
        ~CTransferCode();

        const LgsString& getCompanyCode() const { return m_company_code;}
        void setCompanyCode(const LgsString& str ) { m_company_code = str;}

        const int& getWordClassCode() const { return m_word_class_code;}
        void setWordClassCode(int wordClassCode) { m_word_class_code = wordClassCode;}

        const int& getPatNumber() const { return m_pat_number;}
        void setPatNumber(int patNumber) { m_pat_number = patNumber;}

        const int& getAlternatePatNumber() const { return m_alternate_pat_number;}
        void setAlternatePatNumber(int patNumber) { m_alternate_pat_number = patNumber;}

        const int& getGenderCode() const { return m_gender_code;}
        void setGenderCode(int genderCode) { m_gender_code = genderCode;}

        const int& getAlternateSeq() const { return m_alternate_sequence;}
        void setAlternateSeq(int alternateSeq) { m_alternate_sequence = alternateSeq;}

        const int& getOverflow2a() const { return m_overflow2a;}
        void setOverflow2a(int overflow2a) { m_overflow2a = overflow2a;}

        const int& getOverflow2b() const { return m_overflow2b;}
        void setOverflow2b(int overflow2b) { m_overflow2b = overflow2b;}

        const int& getOverflow3a() const { return m_overflow3a;}
        void setOverflow3a(int overflow3a) { m_overflow3a = overflow3a;}

        const int& getOverflow3b() const { return m_overflow3b;}
        void setOverflow3b(int overflow3b) { m_overflow3b = overflow3b;}

private:
        LgsString  m_company_code;
        int             m_word_class_code;
        int             m_pat_number;
        int             m_alternate_pat_number;
        int             m_gender_code;
        int             m_alternate_sequence;
        int             m_overflow2a;
        int             m_overflow2b;
        int             m_overflow3a;
        int             m_overflow3b;
};

//We are not using typedef becuase this allows us to
//add member functions as needed
class CTransferCodeVector : public LgsVector(CTransferCode)
{
};

#endif

