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
#ifndef _CacheInflectionDataClass
#define _CacheInflectionDataClass

#define INFLECTION_KEYLEN 0

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CacheData.h>

struct infl_row {
	int removal_sequence;
	char ending[32];	// defined as VARCHAR(500), but
				// current max length is 11
};

class DLLEXPORT CacheInflectionData : public CacheData {
protected:
  virtual void mapAndLoadInternal();

public:

  CacheInflectionData(SqlConnection *con, int langcode,
                      bool loader, bool ff, bool sav);
  void makeKey();
  virtual bool 	populateFromDatabase();
  virtual void getSizeFromDB();
  inline struct infl_row * getRow(int i) {
    struct infl_row *lrp = 
      (struct infl_row *)((char *)data_array + i*sizeof(struct infl_row));
    return lrp;
  }

  /*

  select Ending, Removal_Sequence from Inflection
  where language_code=:lc and Removal_Sequence is not null
  order by removal_sequence
  */

  inline char * getEnding(int i) { return getRow(i)->ending; }
  inline int getRemovalSequence(int i) {
    return getRow(i)->removal_sequence; }
};

#endif // _CacheInflectionDataClass
