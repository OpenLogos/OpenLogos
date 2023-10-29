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
#include <logos_include/logoscommon.h>
//cat-102 #include <fortran/libraries/trans/scontable.h>
#include "scontablemapper.h"

namespace ParseTrans {

//----------------------------------------------------------------------
SconTable::SconTable()
{
    m_SconVector.reserve(150);
}
//----------------------------------------------------------------------
SconTable::SconTable(const SconTable& rhs)
          :m_SconVector(rhs.m_SconVector)
{
   m_SconVector.reserve(150);
}
//----------------------------------------------------------------------
SconTable::~SconTable()
{
}
//----------------------------------------------------------------------
void streamOutSconTable(char * & outData, const SconTable& object)
{
   short n = object.m_SconVector.size();

   memcpy(outData, (const char *)&n, sizeof(n));
   outData += sizeof(n);

   for (SconTable::VectorOfShorts::const_iterator i = object.m_SconVector.begin();
        i < object.m_SconVector.end(); i++)
   {
      short s = *i;
      memcpy(outData, (const char *)&s, sizeof(s));
      outData += sizeof(s);
   }
}
//----------------------------------------------------------------------
istream & operator >>(istream & stream , SconTable& object)
{
   short numOfElements = 0;
   stream.read((char *)&numOfElements, sizeof(numOfElements));

   for (int i = 0; i < numOfElements; i++)
   {
      short element;

      stream.read((char *)&element, sizeof(element));
      object.m_SconVector.push_back(element);
   }
   return stream;
}

}
