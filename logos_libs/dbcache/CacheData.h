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
#ifndef _CacheDataClass
#define _CacheDataClass

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#ifdef _MSC_VER
#define SHM_HANDLE HANDLE
#endif

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_MMAP
#define SHM_HANDLE class memory_mapper *
#endif

class CacheKey;
class CacheHashTab;
class SqlConnection;
class SqlStatement;

#define DATASET_STATE_EMPTY 0
#define DATASET_STATE_LOADING 1
#define DATASET_STATE_READY 2

// for now info holds only number of rows
struct data_info {
	long size;
	int state;
};

#ifdef HAVE_TIME_H
#include <time.h>
#define CLOCK_T clock_t
#define GetTickCount clock
#endif

class DLLEXPORT CacheData {

public:

	CacheData(SqlConnection *con, int language, char stmtLetters[], 
		bool whoami, bool loadfromfile, bool saveondisk);
	CacheData(SqlConnection *con, int srcl, int targl, 
		char stmtLetters[], bool whoami, 
		bool loadfromfile, bool saveondisk);
	void constructCacheData(SqlConnection *con, int srcl, int targl, 
		char stmtLetters[], bool whoami, 
		bool loadfromfile, bool saveondisk);
	void mapAndLoad();
	~CacheData();

	inline int getSize() { return size; }
	inline int getCacheKeyLength() { return max_key_len; }
	inline int getRowLength() { return row_len; }
	long getNRows();

	inline char * getStatementText() { return stmtText; }
	inline char * getName() { return basename; }

	bool isValid();

	int	getRowIndex(CacheKey *key);

	void save();
	bool load();
	void printAll();

	inline long getMemoryUsage() { return memory; }
	inline long getLoadTime() { return loadtime; }

	virtual bool populateFromDatabase();	
		// If this is holder process, i.e.
		// server, load the data from DB.
		// Otherwise, just map shared data region.
	virtual void getSizeFromDB();

	static void	setDebug(bool b);

protected:
        void mapAndLoadFull();
        void mapAndLoadReduced();
        virtual void mapAndLoadInternal();
        
	void * mapit(char *name, int sz, SHM_HANDLE *hdl);

        void unmapit(void *addr, SHM_HANDLE &handle);
        
	int		size;
	char	basename[64];
	char	*stmtText;
	char	data_area_name[64];
	char	keys_area_name[64];
	char	heads_area_name[64];
	char	weights_area_name[64];
	char	info_area_name[64];
	int		language_code;
	int		source_language;
	int		target_language;
	char language_id[10];
	char source_language_id[10];
	char target_language_id[10];
	int		max_key_len;
	int		row_len;
	bool	amIHolder;
	bool	loadFromFile;
	bool	saveOnDisk;
	long	loadtime;
	long	nrows;
	long	memory;

	CacheHashTab	*	hashtab;

	SHM_HANDLE	heads_handle;
	int	*heads_array;

	SHM_HANDLE	keys_handle;
	CacheKey *	keys_array;

	void	*data_array;
	SHM_HANDLE	data_handle;

	SHM_HANDLE	weights_handle;
	double	*weights_array;

	SHM_HANDLE	info_handle;
	struct data_info *info;
	struct data_info * loadInfo();

	SqlConnection *oracleConnection;
	SqlStatement *stmt;

	static bool debug;
};

#endif // _CacheDataClass
