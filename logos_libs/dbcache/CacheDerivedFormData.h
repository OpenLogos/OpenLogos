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
#ifndef _CacheDerivedFormDataClass
#define _CacheDerivedFormDataClass

#define DERIVED_FORM_KEYLEN 0

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CacheData.h>

struct df_row {
	int pat_number;
	int stem_number;
	int word_class_code;
	int form_code;
	char derived_form_id[61];
	char ending[32];	// definition is varchar(500), but max==11
};

class DLLEXPORT CacheDerivedFormData : public CacheData {
protected:
  virtual void mapAndLoadInternal();

public:

  CacheDerivedFormData(SqlConnection *con, int langcode,
                       bool loader, bool ff, bool sav);
  void makeKey();
  virtual bool 	populateFromDatabase();
  virtual void getSizeFromDB();
  inline struct df_row * getRow(int i) {
    struct df_row *lrp = 
      (struct df_row *)((char *)data_array + i*sizeof(struct df_row));
    return lrp;
  }

  /*

  select derived_form_id, pat_number, stem_number, ending, word_class_code,
  form_code
  from derived_form
  where language_code = :lc
  */

  inline char * getDerivedFormId(int i) {
    return getRow(i)->derived_form_id;
  }
  inline char * getEnding(int i) { return getRow(i)->ending; }
  inline int getWordClassCode(int i) {
    return getRow(i)->word_class_code;
  }
  inline int getFormCode(int i) { return getRow(i)->form_code; }
  inline int getPatNumber(int i) { return getRow(i)->pat_number; }
  inline int getStemNumber(int i) { return getRow(i)->stem_number; }
};

#endif // _CacheDerivedFormDataClass
