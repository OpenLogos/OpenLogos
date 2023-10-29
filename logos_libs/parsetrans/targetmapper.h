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
// --------------------------------------------------------------------------
// File: targetmapper.h
// --------------------------------------------------------------------------
// Purpose: Defines two classes to define database matches of sentence units
// * TargetMapper:      each instance of this class represents one segment of a sentence object
//                      (a sentence unit - e.g., a word) that has a match in the database.
// * TargetMapperTable: each instance of this class corresponds to one sentence
// --------------------------------------------------------------------------
// Created:       ?
// Modifications: 
// --------------------------------------------------------------------------

#ifndef __TargetMapper_h__
#define __TargetMapper_h__

//#include <logos_libs/linguistic/llanguage.h>
// ===> 2 definitions of the same SconTable class!!! Must be merged together
#include "scontablemapper.h"

using ParseTrans::SconTable;

// --------------------------------------------------------------------------
// Some global variables in an OOD????!!!!
// --------------------------------------------------------------------------
const short MAX_TARGET_SSUS = 450;
const short NUM_OF_ORIG_SCONS = 20;
const short NUM_OF_OVERFLOW_SCONS = 130;
const short OVERFLOW_SCON_ARRAY_SIZE = 100;
const short PRIMARY_SSU_ARRAY_SIZE = 70;
const short COMPANY_CODE_SIZE = 3;

class TargetMapper;
istream& operator>>(istream&, TargetMapper&);

// --------------------------------------------------------------------------
// Definition of class TargetMapper
// --------------------------------------------------------------------------
class TargetMapper
{
public:
   DummyLess(TargetMapper);
   DummyEqual(TargetMapper);

   //---------------------------------------------------------------
   // The public constructors and destructors. The default
   // constructor is typically made public unless class construction
   // is being limited (in which case it is made protected). Here,
   // the default constructor is protected since this is an abstract
   // class that should not be directly instantiated. The destructor
   // is declared virtual to allow subclassing.
   //---------------------------------------------------------------
   TargetMapper();
   TargetMapper(const TargetMapper&);
   TargetMapper(short opadr, short sconPointer, short primarySsu = 0);
   virtual ~TargetMapper();

   //---------------------------------------------------------------
   int sourceSentenceUnitPosition() const; // Returns a 1 based index if there is a corresponding SSU, returns 0 otherwise
   int constantId() const;                 // Returns the constant id or 0 if not a constant
   bool isLowConstant() const;             // Returns true if low constant
   bool isHighConstant() const;            // Returns true if high constant
   bool isConstant() const;                // Returns true if it is a constant
   bool isUnfoundWord() const;             // Returns true if the object represents an unfound word
   bool isPunctuation() const;             // Returns true if the object represents an unfound word
   bool isBeginningOfSentence() const;

   short opadr() const;
   void opadr(short x);
   short sconPointer() const;
   void sconPointer(short x);
   short primarySsu() const;
   void primarySsu(short x);
   LgsString& companyCode();
   void companyCode(LgsString& cc);

   // The non constant getter is in same spirit as a vector class where they provide constant and non
   // constant iterators. The non constant getter is also needed for efficiency sake so that scon tabel
   // can be directly populated and does not need to copied.
   const SconTable& sconTable() const;
   SconTable& sconTable();
   void sconTable(const SconTable& x);

   friend void streamOutTargetMapper(char*& outData, const TargetMapper&);
   friend istream& operator>>(istream&, TargetMapper&);

   void display();

private:
   short v_opadr;			// This is a overloaded number, can represent index of source sentence unit, or constant_id
   short v_sconPointer;	// This will only be used to reconstruct the original scon table from a vector of
							   // TargetMapper objects, we might have to do that for legacy fortran code.
   short v_primarySsu;
   SconTable v_sconTable;
   LgsString v_companyCode;
};


// --------------------------------------------------------------------------
// Definition of class TargetMapperTable
// --------------------------------------------------------------------------
class TargetMapperTable : public LgsVector(TargetMapper)
{
public:
   TargetMapperTable() {};			// Default constructor
   ~TargetMapperTable() {};		// Default destructor
   TargetMapperTable(short countOfTargetSSUs,
                     const short opadr[MAX_TARGET_SSUS],
                     const short sconPosition[MAX_TARGET_SSUS],         // Also known as scon pointer
                     const short primarySSU[PRIMARY_SSU_ARRAY_SIZE],
                     short sconArray[][NUM_OF_ORIG_SCONS],              // Main scon table
                     const short overflowSconPosition[MAX_TARGET_SSUS],
                     short overflowSconArray[][NUM_OF_OVERFLOW_SCONS],  // Used to store scons if they don't fit into sconTable
                     char cmpcod[][3][3]);                              // array of company codes for SSUs. there are 3 for each, of which only one is to be used.
   
   short streamOutSize(void) const;
   friend void streamOutTargetMapperTable(char* outData, const TargetMapperTable&);
   friend istream& operator>> (istream& stream, TargetMapperTable&);
   void display();

private:
   short _streamOutSize;
};

// --------------------------------------------------------------------------
// Definition of an iterator for a vector of TargetMapper objects
// --------------------------------------------------------------------------
typedef LgsVector(TargetMapper)::iterator TargetMapperVectorIterator;

// --------------------------------------------------------------------------
inline short TargetMapper::opadr() const
{
   return v_opadr;
}
// --------------------------------------------------------------------------
inline void TargetMapper::opadr(short x)
{
   v_opadr = x;
}
// --------------------------------------------------------------------------
inline short TargetMapper::sconPointer() const
{
   return v_sconPointer;
}
// --------------------------------------------------------------------------
inline void TargetMapper::sconPointer(short x)
{
   v_sconPointer = x;
}
// --------------------------------------------------------------------------
inline short TargetMapper::primarySsu() const
{
   return v_primarySsu;
}
// --------------------------------------------------------------------------
inline void TargetMapper::primarySsu(short x)
{
   v_primarySsu = x;
}
// --------------------------------------------------------------------------
inline const SconTable& TargetMapper::sconTable() const
{
   return v_sconTable;
}
// --------------------------------------------------------------------------
inline SconTable& TargetMapper::sconTable()
{
   return v_sconTable;
}
// --------------------------------------------------------------------------
inline void TargetMapper::sconTable(const SconTable& x)
{
   v_sconTable = x;
}
// --------------------------------------------------------------------------
inline LgsString& TargetMapper::companyCode()
{
   return v_companyCode;
}
// --------------------------------------------------------------------------
inline void TargetMapper::companyCode(LgsString& cc)
{
   v_companyCode = cc;
}
// --------------------------------------------------------------------------
inline bool	TargetMapper::isConstant() const
{
   return (isLowConstant() || isHighConstant());
}
// --------------------------------------------------------------------------
inline bool	TargetMapper::isLowConstant() const
{
   return (v_opadr > 70);
}
// --------------------------------------------------------------------------
inline bool	TargetMapper::isHighConstant() const
{
   return (v_opadr < -35);
}
// --------------------------------------------------------------------------
inline bool	TargetMapper::isUnfoundWord() const
{
   return ((v_opadr < -11) && (v_opadr >= -35));
}
// --------------------------------------------------------------------------
inline bool	TargetMapper::isPunctuation() const
{
   return ((v_opadr < -1) && (v_opadr >= -11));
}
// --------------------------------------------------------------------------
inline bool	TargetMapper::isBeginningOfSentence() const
{
   return (v_opadr == -1);
}

#endif
