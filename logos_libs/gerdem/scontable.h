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
#ifndef _scontable_h__
#define _scontable_h__

class SconTable
{
public:

   typedef LgsVector(short) VectorOfShorts;

   // Enumerated values representing the 0 based index into sconVector.
   // The number stored at sconVector[index] is the value of the attribute.
   enum SconIndex
   {
      FIRST_SCON_INDEX           =  0,
      SOURCE_WORD_CLASS_INDEX    =  0,
      SOURCE_SUBSET_INDEX        =  1,
      TARGET_DECLENSION_INDEX    =  2,
      TARGET_GENDER_INDEX        =  3,
      TARGET_NUMBER_INDEX        =  4,
      TARGET_PERSON_INDEX        =  5,
      TARGET_CASE_INDEX          =  6,
      TARGET_TENSE_INDEX         =  6,
      WHICH_TRANSFER_INDEX       =  7,
      FORMATING_INFO_INDEX       =  8,
      SOURCE_POSITION_INDEX      =  9,
      SOURCE_SET_INDEX           = 10,
      GERMAN_TARGET_DEGREE_INDEX = 11, // It's unfortunate that the scon
      OTHER_TARGET_DEGREE_INDEX  =  6, // index depends on the target language
      SOURCE_SUPERSET_INDEX      = 12,
      SEMTAB_MATCH_INDEX         = 13,
      ATTRIBUTE_INFO_1_INDEX     = 14,
      ATTRIBUTE_INFO_2_INDEX     = 15,
      SYNTAX_INFO_1_INDEX        = 16,
      SYNTAX_INFO_2_INDEX        = 17,
      SYMANTIC_INFO_INDEX        = 18,
      BLACK_HOLE_TARGET_ADDRESS  = 19,
      LAST_SCON_INDEX            = 19
   };

   SconTable();
   SconTable(const SconTable&);
   virtual ~SconTable();
   void zeroSconTable();
   VectorOfShorts& getVectorOfValues();
   void setScon(int whichScon, short newValue);
   short getScon(int whichScon) const;

   void setScon45(short);
   short scon45() const;

   // 1   -> use the main transfer
   // 2   -> use the alternate transfer
   // 3,4 -> use the second alternate transfer
   // If there is no second alternate transfer and the value is 3, use the main
   // transfer. Implement the combining form code if not NULL. If there is no second
   // alternate transfer and the value is 4, use the alternate transfer. Implement the
   // combining form code if not NULL.
   int transferSwitch() const;
   void setTransferSwitch(short newValue);

   //----------------------------------------------------------------
   // Value of wordClass can be -ve number indicating that it is locked
   // number() -  0 or 1 is singular, 2 is plural
   // person() - 1, 2, 3 is ThirdPerson
   // Case is reserved word, hence the name caseTransfer
   // This does not apply to verbs
   // 1 nominative (e.g. I), 2 Genetive (e.g. mother's), 3 Dative, 4 Accusitive (e.g. him, her)
   // This only applies to verbs
   // The meaning of the value depends on the target language
   // In German - 1 -> present in a main clause etc.
   //----------------------------------------------------------------

   int blackHoleTargetAddress() const;
   int declension() const;
   int degree() const;
   int gender() const;
   int number() const;
   int caseOf() const;
   int person() const;
   int setOf() const;
   int sourceSentenceUnitPosition() const;
   int subSetOf() const;
   int superSetOf() const;
   int tense() const;
   int wordClass() const;
   void setBlackHoleTargetAddress(short newValue);
   void setDeclension(short newValue);
   void setDegree(short newValue);
   void setGender(short newValue);
   void setNumber(short newValue);
   void setCaseOf(short newValue);
   void setPerson(short newValue);
   void setSetOf(short newValue);
   void setSourceSentenceUnitPosition(short newValue);
   void setSubSetOf(short newValue);
   void setSuperSetOf(short newValue);
   void setTense(short newValue);
   void setWordClass(short newValue);
   void setAsDummyBOS();
   void checkSconValues();
   // This interface is provided for debugging purpose,
   // individual member functions such as degree should be used instead

   friend ostream& operator<<(ostream& streamObj, SconTable& sconObject);
   friend void streamInSconTable(char * & inData, SconTable&);


private:

   void checkCaseTense();
   void checkGender();
   void checkNumber();
   void checkDegree();
   void checkPerson();
   void checkDeclension();
   VectorOfShorts sconVector;
   int degreeIndex;
   short scon45_;
};

//-------------------------------------------------------------------
inline SconTable::VectorOfShorts& SconTable::getVectorOfValues()
{
   return sconVector;
}
//-------------------------------------------------------------------
inline int SconTable::blackHoleTargetAddress() const
{
   return sconVector[BLACK_HOLE_TARGET_ADDRESS];
}
//-------------------------------------------------------------------
inline void SconTable::setBlackHoleTargetAddress(short newValue)
{
   sconVector[BLACK_HOLE_TARGET_ADDRESS] = newValue;
}
//-------------------------------------------------------------------
inline int SconTable::transferSwitch() const
{
   return sconVector[WHICH_TRANSFER_INDEX];
}
//-------------------------------------------------------------------
inline void SconTable::setTransferSwitch(short newValue)
{
   sconVector[WHICH_TRANSFER_INDEX] = newValue;
}
//-------------------------------------------------------------------
// Value of wordClass can be -ve number indicating that it is locked
inline int SconTable::wordClass() const
{
   return abs(sconVector[SOURCE_WORD_CLASS_INDEX]);
}
//-------------------------------------------------------------------
inline void SconTable::setWordClass(short newValue)
{
   sconVector[SOURCE_WORD_CLASS_INDEX] = newValue;
}
//-------------------------------------------------------------------
inline int SconTable::subSetOf() const
{
   return sconVector[SOURCE_SUBSET_INDEX];
}
//-------------------------------------------------------------------
inline void SconTable::setSubSetOf(short newValue)
{
   sconVector[SOURCE_SUBSET_INDEX] = newValue;
}
//-------------------------------------------------------------------
inline int SconTable::declension() const
{	 
   return sconVector[TARGET_DECLENSION_INDEX];
}
//-------------------------------------------------------------------
inline void SconTable::setDeclension(short newValue)
{
   sconVector[TARGET_DECLENSION_INDEX] = newValue;
}
//-------------------------------------------------------------------
inline int SconTable::degree() const
{
   return sconVector[degreeIndex];
}
//-------------------------------------------------------------------
inline void SconTable::setDegree(short newValue)
{
   sconVector[degreeIndex] = newValue;
}
//-------------------------------------------------------------------
inline int SconTable::gender() const
{
   return sconVector[TARGET_GENDER_INDEX];
}
//-------------------------------------------------------------------
inline void SconTable::setGender(short newValue)
{
   sconVector[TARGET_GENDER_INDEX] = newValue;
}
//-------------------------------------------------------------------
inline int SconTable::number() const
{
   return sconVector[TARGET_NUMBER_INDEX];
}
//-------------------------------------------------------------------
inline void SconTable::setNumber(short newValue)
{
   sconVector[TARGET_NUMBER_INDEX] = newValue;
}
//-------------------------------------------------------------------
inline int SconTable::person() const
{
   return sconVector[TARGET_PERSON_INDEX];
}
//-------------------------------------------------------------------
inline void SconTable::setPerson(short newValue)
{
   sconVector[TARGET_PERSON_INDEX] = newValue;
}
//-------------------------------------------------------------------
inline int SconTable::caseOf() const
{
   return sconVector[TARGET_CASE_INDEX];
}
//-------------------------------------------------------------------
inline void SconTable::setCaseOf(short newValue)
{
   sconVector[TARGET_CASE_INDEX] = newValue;
}
//-------------------------------------------------------------------
inline int SconTable::tense() const
{
   return sconVector[TARGET_TENSE_INDEX];
}
//-------------------------------------------------------------------
inline void SconTable::setTense(short newValue)
{
   sconVector[TARGET_TENSE_INDEX] = newValue;
}
//-------------------------------------------------------------------
inline int SconTable::sourceSentenceUnitPosition() const
{
   return sconVector[SOURCE_POSITION_INDEX];
}
//-------------------------------------------------------------------
inline void SconTable::setSourceSentenceUnitPosition(short newValue)
{
   sconVector[SOURCE_POSITION_INDEX] = newValue;
}
//-------------------------------------------------------------------
inline int SconTable::setOf() const
{
   return sconVector[SOURCE_SET_INDEX];
}
//-------------------------------------------------------------------
inline void SconTable::setSetOf(short newValue)
{
   sconVector[SOURCE_SET_INDEX] = newValue;
}
//-------------------------------------------------------------------
inline int SconTable::superSetOf() const
{
   return sconVector[SOURCE_SUPERSET_INDEX];
}
//-------------------------------------------------------------------
inline void SconTable::setSuperSetOf(short newValue)
{
   sconVector[SOURCE_SUPERSET_INDEX] = newValue;
}
//----------------------------------------------------------------------
inline short SconTable::getScon(int whichScon) const
{
   return sconVector[whichScon];
}
//----------------------------------------------------------------------
inline void SconTable::setScon45(short value)
{
	scon45_ = value;
}
//----------------------------------------------------------------------
inline short SconTable::scon45() const
{
   return scon45_;
}


#endif // _scontable_h__
