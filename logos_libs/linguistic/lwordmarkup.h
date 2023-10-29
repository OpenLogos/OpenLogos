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
#ifndef __LWordMarkup_h__
#define __LWordMarkup_h__

//-------------------------------------------------------------------
// File - LWordMarkup.h
//
// Class - LWordMarkup (interface)
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/lookuptokentype.h>

class ILgsWordMarkup;
class IWordMarkup;

class LWordMarkup
{
public:
   LWordMarkup();
   LWordMarkup(const LWordMarkup&);
   LWordMarkup(const ILgsWordMarkup&);
   virtual ~LWordMarkup();

   void orMask( const LWordMarkup&);
   void setId(int);
   int id() const;

   bool isProtected() const;
   bool isMixed() const;
   bool isAlphaNumeric() const;
   bool isNeverEos() const;
   bool isInitiallyCap() const;
   bool isBold() const;
   bool isItalic() const;
   bool isUnderlined() const;
   bool isSingleQuoted() const;
   bool isDoubleQuoted() const;
   bool isFrenchSingleQuoted() const;
   bool isFrenchDoubleQuoted() const;

   void setProtected(bool b = true);
   void setMixed(bool b = true);
   void setAlphaNumeric(bool b = true);
   void setNeverEos(bool b = true);
   void setInitiallyCap(bool b = true);
   void setBold(bool b = true);
   void setItalic(bool b = true);
   void setUnderlined(bool b = true);
   void setSingleQuoted(bool b = true);
   void setDoubleQuoted(bool b = true);
   void setFrenchSingleQuoted(bool b = true);
   void setFrenchDoubleQuoted(bool b = true);

   bool hasMarkup(void);

   void fillWordInfo(ILgsWordMarkup& wordInfo) const;
   const LWordMarkup& operator=(const LWordMarkup&);

   friend void streamOutWordMarkup(IWordMarkup * dest, const LWordMarkup&, int, int,
                                   bool, bool, bool, int, short, LookupTokenType::Type);
   friend void streamInWordMarkup(IWordMarkup * source, LWordMarkup&, int&, int&,
                                  bool&, bool&, bool&, int&, short&, LookupTokenType::Type&);

protected:
   bool orMask(bool, bool);

private:
   int v_id;
   bool v_isProtected;
   bool v_isMixed;
   bool v_isAlphaNumeric;
   bool v_isNeverEos;
   bool v_isInitiallyCap;
   bool v_isBold;
   bool v_isItalic;
   bool v_isUnderlined;
   bool v_isSingleQuoted;
   bool v_isDoubleQuoted;
   bool v_isFrenchSingleQuoted;
   bool v_isFrenchDoubleQuoted;
};

//-------------------------------------------------------------------
inline void LWordMarkup::setId(int anID)
{
   v_id = anID;
}
//-------------------------------------------------------------------
inline int LWordMarkup::id() const
{
   return v_id;
}
//-------------------------------------------------------------------
inline bool LWordMarkup::isProtected() const
{
   return v_isProtected;
}
//-------------------------------------------------------------------
inline bool LWordMarkup::isMixed() const
{
   return v_isMixed;
}
//-------------------------------------------------------------------
inline bool LWordMarkup::isAlphaNumeric() const
{
   return v_isAlphaNumeric;
}
//-------------------------------------------------------------------
inline bool LWordMarkup::isNeverEos() const
{
   return v_isNeverEos;
}
//-------------------------------------------------------------------
inline bool LWordMarkup::isInitiallyCap() const
{
   return v_isInitiallyCap;
}
//-------------------------------------------------------------------
inline bool LWordMarkup::isBold() const
{
   return v_isBold;
}
//-------------------------------------------------------------------
inline bool LWordMarkup::isItalic() const
{
   return v_isItalic;
}
//-------------------------------------------------------------------
inline bool LWordMarkup::isUnderlined() const
{
   return v_isUnderlined;
}
//-------------------------------------------------------------------
inline bool LWordMarkup::isSingleQuoted() const
{
   return v_isSingleQuoted;
}
//-------------------------------------------------------------------
inline bool LWordMarkup::isDoubleQuoted() const
{
   return v_isDoubleQuoted;
}
//-------------------------------------------------------------------
inline bool LWordMarkup::isFrenchSingleQuoted() const
{
   return v_isFrenchSingleQuoted;
}
//-------------------------------------------------------------------
inline bool LWordMarkup::isFrenchDoubleQuoted() const
{
   return v_isFrenchDoubleQuoted;
}
//-------------------------------------------------------------------
inline void LWordMarkup::setProtected(bool b)
{
   v_isProtected = b;
}
//-------------------------------------------------------------------
inline void LWordMarkup::setMixed(bool b)
{
   v_isMixed = b;
}
//-------------------------------------------------------------------
inline void LWordMarkup::setAlphaNumeric(bool b)
{
   v_isAlphaNumeric = b;
}
//-------------------------------------------------------------------
inline void LWordMarkup::setNeverEos(bool b)
{
   v_isNeverEos = b;
}
//-------------------------------------------------------------------
inline void LWordMarkup::setInitiallyCap(bool b)
{
   v_isInitiallyCap = b;
}
//-------------------------------------------------------------------
inline void LWordMarkup::setBold(bool b)
{
   v_isBold = b;
}
//-------------------------------------------------------------------
inline void LWordMarkup::setItalic(bool b)
{
   v_isItalic = b;
}
//-------------------------------------------------------------------
inline void LWordMarkup::setUnderlined(bool b)
{
   v_isUnderlined = b;
}
//-------------------------------------------------------------------
inline void LWordMarkup::setSingleQuoted(bool b)
{
   v_isSingleQuoted = b;
}
//-------------------------------------------------------------------
inline void LWordMarkup::setDoubleQuoted(bool b)
{
   v_isDoubleQuoted = b;
}
//-------------------------------------------------------------------
inline void LWordMarkup::setFrenchSingleQuoted(bool b)
{
   v_isFrenchSingleQuoted = b;
}
//-------------------------------------------------------------------
inline void LWordMarkup::setFrenchDoubleQuoted(bool b)
{
   v_isFrenchDoubleQuoted = b;
}
//-------------------------------------------------------------------
inline bool LWordMarkup::orMask(bool lhs, bool rhs)
{
   return lhs || rhs;
}

#endif // __LWordMarkup_h__

