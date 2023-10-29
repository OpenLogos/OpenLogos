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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
#ifndef _scontablemapper_h__
#define _scontablemapper_h__

namespace ParseTrans {

using namespace std;

class SconTable;
istream & operator >>(istream & stream, SconTable&);

class SconTable
{
public:
   typedef LgsVector(short) VectorOfShorts;

   //These are 0 base index into m_SconVector
   //the number stored in m_SconVector[index] is the value of the
   //attribute
   enum
   {
      SOURCE_WORD_CLASS_INDEX    =  0,
      SOURCE_SUBSET_INDEX        =  1,
      TARGET_DECLENSION_INDEX    =  2,
      TARGET_GENDER_INDEX        =  3,
      TARGET_NUMBER_INDEX        =  4,
      TARGET_PERSON_INDEX        =  5,
      TARGET_CASE_INDEX          =  6,
      TARGET_TENSE_INDEX         =  6,
      WHICH_TRANSFER_INDEX       =  7,
      SOURCE_POSITION_INDEX      =  9,
      SOURCE_SET_INDEX           = 10,
      GERMAN_TARGET_DEGREE_INDEX = 11, //This is unfortunate that the scon
      OTHER_TARGET_DEGREE_INDEX  =  7, //index depends on the target language
      SOURCE_SUPERSET_INDEX      = 12,
      BLACK_HOLE_TARGET_ADDRESS  = 19
   };

   SconTable();
   SconTable(const SconTable&);
   virtual ~SconTable();

   int sourceSentenceUnitPosition() const;
   void addScon(short scon);
   short getScon(int index) const;

   friend void streamOutSconTable(char * & outData, const SconTable&);
   friend istream & operator >>(istream & stream, SconTable&);

private:
   VectorOfShorts m_SconVector;
};
//----------------------------------------------------------------------
inline int SconTable::sourceSentenceUnitPosition() const
{
   return m_SconVector[SOURCE_POSITION_INDEX];
}
//----------------------------------------------------------------------
inline void SconTable::addScon(short scon)
{
   m_SconVector.push_back(scon);
}
//----------------------------------------------------------------------
inline short SconTable::getScon(int index) const
{
   assert(index < m_SconVector.size());

   return m_SconVector[index];
}
}
#endif // _scontablemapper_h__
