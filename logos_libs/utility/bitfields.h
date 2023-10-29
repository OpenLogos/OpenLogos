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
#ifndef _BITFIELDS_H_
#define _BITFIELDS_H_
#include <stddef.h>
#include <logos_libs/halfnoun/germanutil.h>

typedef unsigned long FLD_VALUE_TYPE;
class LgsBitFields
{
public:
   enum {BYTE_SIZE = 8, SIZE_OF_FLD_VALUE = sizeof(FLD_VALUE_TYPE)};
   LgsBitFields(unsigned short usFieldCount, unsigned short usFieldWidth);
   LgsBitFields(const LgsBitFields & rhs);
   virtual ~LgsBitFields(void);
   virtual bool setField(unsigned short usFieldIndex, FLD_VALUE_TYPE ulFieldValue);
   virtual bool getField(unsigned short usFieldIndex, FLD_VALUE_TYPE & ulFieldValue) const;
   virtual void operator|=(const LgsBitFields& rhs);
   void copyFlag(const LgsBitFields & rhs);
   void printOn(ofstream& output) const;
   bool readFrom(istream& input);
   bool blank() const;
private:
   LgsBitFields(void);
   unsigned short m_usFieldCount;
   unsigned short m_usFieldWidth;
   unsigned char *m_cpFieldMap;
   FLD_VALUE_TYPE MaxFieldValue;
   int m_iCharCount;
};

inline LgsBitFields::LgsBitFields(void) :
         m_usFieldCount(0), m_usFieldWidth(0),
         m_cpFieldMap(0), MaxFieldValue(0), m_iCharCount(0)

{

}

inline LgsBitFields::LgsBitFields(unsigned short usFieldCount, 
                                  unsigned short usFieldWidth) :
         m_usFieldCount(usFieldCount), m_usFieldWidth(usFieldWidth),
         m_cpFieldMap(0), MaxFieldValue(0), m_iCharCount(0)
{
   if (m_usFieldWidth > SIZE_OF_FLD_VALUE*BYTE_SIZE)
   {
      return;
   }
   
   m_iCharCount = m_usFieldCount*m_usFieldWidth/(sizeof(unsigned char)*BYTE_SIZE)+1;
   m_cpFieldMap = new unsigned char [m_iCharCount];
   int i = 0;
   for (i = 0; i < m_iCharCount; m_cpFieldMap[i++] = 0);
   for (i = SIZE_OF_FLD_VALUE; i; MaxFieldValue |= static_cast<FLD_VALUE_TYPE>(0xFF) << (i-1)*BYTE_SIZE, i--);
}

inline LgsBitFields::LgsBitFields(const LgsBitFields & rhs) :
         m_usFieldCount(rhs.m_usFieldCount), m_usFieldWidth(rhs.m_usFieldWidth),
         m_cpFieldMap(0), MaxFieldValue(rhs.MaxFieldValue), m_iCharCount(rhs.m_iCharCount)
{
   if (m_iCharCount)
   {
      m_cpFieldMap = new unsigned char [m_iCharCount];
   }
   for (int i = 0; i < m_iCharCount; m_cpFieldMap[i] = rhs.m_cpFieldMap[i], i++);
}
inline bool LgsBitFields::blank() const
{
    for (int i = 0; i < m_iCharCount; i++)
        if (m_cpFieldMap[i] != 0)
            return false;

    return true;
}


inline void LgsBitFields::copyFlag(const LgsBitFields & rhs)
{
   m_usFieldCount = rhs.m_usFieldCount;
   m_usFieldWidth = rhs.m_usFieldWidth;
   MaxFieldValue = rhs.MaxFieldValue;
   if (rhs.m_iCharCount != m_iCharCount )
   {
      m_iCharCount = rhs.m_iCharCount;
      if (m_cpFieldMap)
      {
         delete m_cpFieldMap;
         m_cpFieldMap = new unsigned char [rhs.m_iCharCount];
      }
   }
   for (int i = 0; i < m_iCharCount; m_cpFieldMap[i] = rhs.m_cpFieldMap[i], i++);
}

inline LgsBitFields::~LgsBitFields(void) 
{
   if (m_cpFieldMap)
   {
      delete[] m_cpFieldMap;
      m_cpFieldMap = 0;
   }
}
inline void LgsBitFields::operator |= (const LgsBitFields &rhs)
{
   unsigned short charCount = (m_iCharCount > rhs.m_iCharCount)? rhs.m_iCharCount: m_iCharCount;
   for (charCount--; charCount; m_cpFieldMap[charCount] |= rhs.m_cpFieldMap[charCount], charCount--);
	m_cpFieldMap[charCount] |= rhs.m_cpFieldMap[charCount];
} 

inline bool LgsBitFields::getField(unsigned short usFieldIndex, 
                                   FLD_VALUE_TYPE & ulFieldValue) const
{
   ulFieldValue = 0;
   if (!m_cpFieldMap)
   {
      return false;
   }
   // Calculate Character Offset and Bit offset of the requested Bit Field in the
   // Field Map
   unsigned short usCharacterOffset = usFieldIndex*m_usFieldWidth/(sizeof(unsigned char)*BYTE_SIZE);
   unsigned short usBitOffset = usFieldIndex*m_usFieldWidth%(sizeof(unsigned char)*BYTE_SIZE);

   // Load the Most Significant Part of the Bit Field from the starting character of the 
   // Bit Field; Start by storing the Most Significant Part in the Least Significant Character
   // of the value to be returned
   ulFieldValue |= m_cpFieldMap[usCharacterOffset++] & 0xFF >> usBitOffset;
   int iNoBitsCopied = BYTE_SIZE-usBitOffset;

   // Load the Other Characters of the Bit Field
   for (int i = 1; iNoBitsCopied < m_usFieldWidth && i < SIZE_OF_FLD_VALUE; 
        iNoBitsCopied += BYTE_SIZE, i++)
   {
      // Move the Least Significant character of the value to be returned by shifting
      // the character to the next higher character and then store the next character 
      // in the Least Significant Character of the Value to be returned
      ulFieldValue <<= sizeof(unsigned char)*BYTE_SIZE;
      ulFieldValue |= m_cpFieldMap[usCharacterOffset++];
   }
   if (m_usFieldWidth > iNoBitsCopied)
   {
      unsigned short usBitsToBeCopied = m_usFieldWidth-iNoBitsCopied;
      ulFieldValue <<= usBitsToBeCopied;
      ulFieldValue |= m_cpFieldMap[usCharacterOffset] >> (sizeof(unsigned char)*BYTE_SIZE - usBitsToBeCopied);
   }
   else
   {
      ulFieldValue >>= (iNoBitsCopied - m_usFieldWidth);
   }
   return true;
}

inline bool LgsBitFields::setField(unsigned short usFieldIndex, 
                                   FLD_VALUE_TYPE ulFieldValue)
{
   if (!m_cpFieldMap)
   {
      return false;
   }
   // Calculate the Character Offset and the bit offset of the Bit Field to be set in the map of
   // Bit Fields
   unsigned short usCharacterOffset = usFieldIndex*m_usFieldWidth/(sizeof(unsigned char)*BYTE_SIZE);
   unsigned short usBitOffset = usFieldIndex*m_usFieldWidth%(sizeof(unsigned char)*BYTE_SIZE);
   // calculate the Most Significant Character's bit offset in the Bit Field Value 
   unsigned short usMsbBitOffset = sizeof(unsigned char)*BYTE_SIZE - m_usFieldWidth%(sizeof(unsigned char)*BYTE_SIZE);

   // Get the Bit Field bits of the value passed
   FLD_VALUE_TYPE ulTemp = ulFieldValue &= MaxFieldValue >> (SIZE_OF_FLD_VALUE*BYTE_SIZE-m_usFieldWidth);
;

   int iCharIndex = m_usFieldWidth/(BYTE_SIZE*sizeof(unsigned char)) + ((m_usFieldWidth%(sizeof(unsigned char)*BYTE_SIZE))? 1: 0);
   
   if (usBitOffset > usMsbBitOffset)
   {
      m_cpFieldMap[usCharacterOffset+iCharIndex] |= ulTemp << (sizeof(unsigned char)*BYTE_SIZE - (usBitOffset-usMsbBitOffset));
      ulTemp >>= usBitOffset-usMsbBitOffset;
   }
   else
   {
      ulTemp <<= usMsbBitOffset-usBitOffset;
      m_cpFieldMap[usCharacterOffset+(--iCharIndex)] |= ulTemp;
      ulTemp >>= BYTE_SIZE*sizeof(unsigned char);
   }
   for (int i = 0; iCharIndex; iCharIndex--, i++)
   {
      m_cpFieldMap[usCharacterOffset+iCharIndex-1] |= ulTemp >> i*sizeof(unsigned char)*BYTE_SIZE;
   }
   return true;
}

inline void LgsBitFields::printOn(ofstream& output) const
{
    GermanUtil::printByteArrayOn((const char *)m_cpFieldMap, m_iCharCount * sizeof(unsigned char), output);
}


inline bool LgsBitFields::readFrom(istream& input)
{
    return GermanUtil::readByteArrayFrom((char *)m_cpFieldMap, m_iCharCount * sizeof(unsigned char), input);
}



#endif
