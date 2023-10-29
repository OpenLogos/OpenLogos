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
#ifndef __LSyntaxUnit_h__
#define __LSyntaxUnit_h__

//-------------------------------------------------------------------
// File - LSyntaxUnit.h
//
// Class - LSyntaxUnit
//
// Description - This is the "root" class for many of the classes
//               in the Gerdem subsystem.
//-------------------------------------------------------------------

class LDictionary;

class LSyntaxUnit
{
public:
   //---------------------------------------------------------------
   // Public constructors and destructors. There are no public constructors
   // since this is an abstract class. The constructors are "protected" because
   // only they are only called by constructors in subclasses. The following
   // destructor is virtual to support polymorphism.
   //---------------------------------------------------------------
   virtual ~LSyntaxUnit();

   //---------------------------------------------------------------
   LDictionary& dictionary() const;
   void setDictionary(LDictionary&);

   int position() const;
   void position(int);
   int oid() const;

protected:
   //-----------------------------------------------------------------
   // The default constructor. It has protected access status in order to
   // prevent users from attempting to create an object of this class (this
   // class is intended to be abstract). Copy constructor is included so that
   // subclasses that support a copy constructor can use the copy constructor
   // of this class.
   //-----------------------------------------------------------------
   LSyntaxUnit();
   LSyntaxUnit(LDictionary& dictionary);
   LSyntaxUnit(const LSyntaxUnit&);

   void setOid();
   virtual int translatedWordCount() const = 0;
   void persistOut(int * outData);
   void persistIn (int * inData);

private:
   LDictionary* p_dictionary;
   int v_position;
   int v_oid;
   static int v_nextOid;
};

//---------------------------------------------------------------------
inline int LSyntaxUnit::oid() const
{
   return v_oid;
}
//---------------------------------------------------------------------
inline void LSyntaxUnit::setOid()
{
   v_oid = v_nextOid++;
}
//---------------------------------------------------------------------
inline int LSyntaxUnit::position() const
{
   return v_position;
}

inline void LSyntaxUnit::position(int aPosition)
{
   v_position = aPosition;
}
//---------------------------------------------------------------------
inline LDictionary& LSyntaxUnit::dictionary() const
{
   return *p_dictionary;
}
//---------------------------------------------------------------------
inline void LSyntaxUnit::setDictionary(LDictionary& d)
{
   p_dictionary = &d;
}


#endif // __LSyntaxUnit_h__

