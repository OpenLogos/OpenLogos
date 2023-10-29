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
// File: targetmapper.cpp
// --------------------------------------------------------------------------
// Purpose: Implementation of TargetMapper and TargetMapperTable classes
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
//cat-102 #include <fortran/libraries/trans/targetmapper.h>
#include "targetmapper.h"
//#include <logos_libs/linguistic/llanguage.h>


// --------------------------------------------------------------------------
TargetMapper::TargetMapper()
             :v_opadr(0),
              v_sconPointer(0),
              v_primarySsu(0),
              v_companyCode("LOG")

{}


// --------------------------------------------------------------------------
TargetMapper::TargetMapper(const TargetMapper& x)
             :v_opadr(x.v_opadr),
              v_sconPointer(x.v_sconPointer),
              v_primarySsu(x.v_primarySsu),
              v_sconTable(x.v_sconTable),
              v_companyCode(x.v_companyCode)
{}

// --------------------------------------------------------------------------
TargetMapper::TargetMapper(short opadr, short sconPointer, short primarySsu)
             :v_opadr(opadr),
              v_sconPointer(sconPointer),
              v_primarySsu(primarySsu),
              v_companyCode("LOG")
{}

// --------------------------------------------------------------------------
TargetMapper::~TargetMapper()
{}

// --------------------------------------------------------------------------
void streamOutTargetMapper(char*& outData, const TargetMapper& object)
{
   memcpy(outData, (char *)&object.v_opadr, sizeof(object.v_opadr));
   outData += sizeof(object.v_opadr);
   memcpy(outData, (char *)&object.v_sconPointer, sizeof(object.v_sconPointer));
   outData += sizeof(object.v_sconPointer);
   memcpy(outData, (char *)&object.v_primarySsu, sizeof(object.v_primarySsu));
   outData += sizeof(object.v_primarySsu);
   memcpy(outData, object.v_companyCode.c_str(), (COMPANY_CODE_SIZE * sizeof(char)));
   outData += (COMPANY_CODE_SIZE * sizeof(char));
   streamOutSconTable(outData, object.v_sconTable);
}

// --------------------------------------------------------------------------
int TargetMapper::sourceSentenceUnitPosition() const
{
   int sourcePosition = 0;

   if((v_opadr > 0) && (v_opadr <= 70))
   {
      sourcePosition = v_opadr;
   }
   else
   {
      sourcePosition = v_sconTable.sourceSentenceUnitPosition();
   }

   return sourcePosition;
}

// --------------------------------------------------------------------------
int TargetMapper::constantId() const
{
   int cstid = 0;			// default value

   if(isHighConstant())
   {
      cstid = -v_opadr;
   }
   if(isLowConstant())
   {
      cstid = v_opadr;
   }

   return cstid;
}

// --------------------------------------------------------------------------
istream& operator>>(istream& stream, TargetMapper& object)
{
   stream.read((char *)&object.v_opadr, sizeof(object.v_opadr));
   stream.read((char *)&object.v_sconPointer, sizeof(object.v_sconPointer));
   stream.read((char *)&object.v_primarySsu, sizeof(object.v_primarySsu));
   for (int iter = 0; iter < COMPANY_CODE_SIZE; iter++)
   {
      char nextChar;
      stream.read(&nextChar, sizeof(char));
      object.v_companyCode.replace(iter, 1, &nextChar, 1);
   }

   stream >> object.v_sconTable;
   return stream;
}


// --------------------------------------------------------------------------
// Arguments to create vector of targetmapper objects:
//
//countOfTargetSSUs
//		count of target ssus.  Could be high as 450
//opadr[] short array of 450
//		offset/addresses (1 based) of opadr (One per ssu)
//sconPosition[] short array of 450
//		scon index (1 based) value (one per ssu)
//primarySSU[] short array  of 70
//		primary ssu index (1 based) for only the non constant
//		offest/addresses opadr
//		must use the offset in arg2 to index this short array.
//		Set this value to 0 unless, the value in arg2 array is 1-70. If 1-70
//		then use arg2 number as an index into arg4 array. The value should be
//		1,2,or 3.
//sconArray[][] short array of [450][20]
//		first 20 values for each ssu. use arg3 to index into which
//		450 to get.
//overflowSconPosition[] short array of 450
//		pointer index (1 based)  to additional values beyond first 20. Not always set
//		use arg3 to index into this array. Check if non zero then use this non zero
//		value as index into arg 7 array.
//overflowSconArray	short array of [100][130]
//		next 130 values of arg 5 array. Use the value gotten from
//		arg6 logic. This is not always used. So you may not get here if arg6 logic
//		comes up with a zero index.
//cmpcod char array of [][3][3]
//		3 character company code for each SSU of each element(word) in the original swork
//      must use value in position 10 of scon table
//      must use the value from the primarySSU array mentioned above for the second index
//		  (tells us which of the 3 is the selected one.)
//      last dimension of array is to hold the 3 characters of the company code.
// --------------------------------------------------------------------------
TargetMapperTable::TargetMapperTable(short countOfTargetSSUs, const short opadr[],
                                     const short sconPosition[], const short primarySSU[],
                                     short sconArray[][NUM_OF_ORIG_SCONS],
                                     const short overflowSconPosition[],
                                     short overflowSconArray[][NUM_OF_OVERFLOW_SCONS],
                                     char cmpcod[][3][3])
{
   assert(countOfTargetSSUs <= MAX_TARGET_SSUS);
   assert(countOfTargetSSUs > 0);

   _streamOutSize = sizeof(short);		// to hold no of units

   for(short i = 0; i < countOfTargetSSUs; i++)
   {
      assert(sconPosition[i] <= MAX_TARGET_SSUS);
      // not sure what a zero opadr is but we will skip it for now.
      if(opadr[i] == 0)
      {
         continue;
      }

      push_back(TargetMapper());

      TargetMapper& tm = back();

      tm.opadr(opadr[i]);
      tm.sconPointer(sconPosition[i]);

      if(sconArray[sconPosition[i] - 1][9] > 0)
      {
         tm.primarySsu(primarySSU[sconArray[sconPosition[i] - 1][9] - 1]);   // -1 to make the index 0 based
      }

      _streamOutSize += 3 * sizeof(short);

      // Get the company code
      LgsString compCode = "LOG";
      if(opadr[i] > 0)
      {
         int index = sconArray[sconPosition[i] - 1][9] - 1;
         int whichSSU = primarySSU[index] - 1;
         for (int cntr = 0; cntr < COMPANY_CODE_SIZE; cntr++)
         {
            char nextChar = cmpcod[index][whichSSU][cntr];
            if (nextChar == '\0')
            {
               compCode = "LOG";
               break;
            }
            else
            {
               compCode.replace(cntr, 1, &nextChar, 1);
            }
         }
         tm.companyCode(compCode);
      }
      _streamOutSize += COMPANY_CODE_SIZE * sizeof(char);

      SconTable& sconTable = tm.sconTable();

      short sconIndex = sconPosition[i] - 1;

      _streamOutSize += sizeof(short);						// to store size of Scon Table

      for(int j = 0; j < NUM_OF_ORIG_SCONS; j++)
      {
         sconTable.addScon(sconArray[sconIndex][j]);		//-1 to make the index 0 based
         _streamOutSize += sizeof(short);
      }

      assert(overflowSconPosition[sconIndex] <= OVERFLOW_SCON_ARRAY_SIZE);

      short overflowSconIndex = overflowSconPosition[sconIndex] - 1;

      if(overflowSconIndex >= 0)
      {
         for(int k = 0; k < NUM_OF_OVERFLOW_SCONS; k++)
         {
            sconTable.addScon(overflowSconArray[overflowSconIndex][k]);
            _streamOutSize += sizeof(short);
         }
      }
   }
}

// --------------------------------------------------------------------------
short TargetMapperTable::streamOutSize(void) const
{
   return _streamOutSize;
}

// --------------------------------------------------------------------------
void streamOutTargetMapperTable(char* outData, const TargetMapperTable& object)
{
   short n = object.size();

   memcpy(outData, (const char *)&n, sizeof(n));
   outData += sizeof(n);

   for(TargetMapperTable::const_iterator i = object.begin(); i < object.end(); i++)
   {
      streamOutTargetMapper(outData, *i);
   }
}

// --------------------------------------------------------------------------
istream& operator>>(istream& stream, TargetMapperTable& object)
{
	short numOfElements = 0;

	stream.read((char*)&numOfElements, sizeof(numOfElements));

	for(int i = 0; i < numOfElements; i++)
   {
		// We are creating an empty TargetMapper and adding it to the vector before populating
      // becuase this way we can populate the instance of TargetMapper added to the vector
      // directly. This is definitely more efficient as are avoiding the cost of copying all
      // the elements of TargetMapper while adding to the vector.

		// Create an empty TargetMapper object and add it to the end of the vector
		object.push_back(TargetMapper());

		//Populate the TargetMapper just added to the end of the vector
		stream >> object.back();
	}

	return stream;
}

// --------------------------------------------------------------------------
void TargetMapper::display()
{
   cout << "\topadr=" << v_opadr;
   cout << "\tsconPtr=" << v_sconPointer;
   cout << "\tprimSSU=" << v_primarySsu;
   cout << endl;
}

// --------------------------------------------------------------------------
void TargetMapperTable::display()
{
   TargetMapperVectorIterator i;
   cout << "============== content of TargetMapperTable =================" << endl;
   int cnt = 0;
   for (i = begin(); i != end(); i++)
   {
      cout << "[" << ++cnt << "]";
      (*i).display();
   }
   cout << "=============================================================" << endl;
}
