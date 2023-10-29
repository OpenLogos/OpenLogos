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

#include "CacheManager.h"
#include <logos_libs/dbcache/CacheData.h>
#include <lgs_db_io/serverproperties.h>

#ifdef _MSC_VER
#include <direct.h>
#include <io.h>
#define SHM_HANDLE HANDLE
#else 
#include "logos_include/memory_mapper.h"
#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif

#define _chdir chdir
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#define _stat stat
#define _fstat fstat
#endif
#include <fcntl.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <logos_libs/utility/lgsout.h>
//#include <shells/logos_batch/logos_batch.h>
//#include <shells/logos_batch/Env_Variables.h>

#include <configdatafileinterface/configdatainterfacemain.h>

extern "C" void res_rule_load(int *, int *, int);
extern "C" int rule_load_into_cache(char *tran1name, int j);

void * mapit(char *name, int nb, SHM_HANDLE *handle);
void * mapFile(char *name, SHM_HANDLE *h);

static char *names[NLANG] = {"", "german", "english", "french", "spanish", "italian", "portuguese"};
static int available_sources[NLANG] = { 0, 1, 1, 0, 0, 0, 0 };
static int available_targets[NLANG] = { 0, 1, 1, 1, 1, 1, 1 };
static int available_semtabs[NLANG][NLANG] = {
	0, 0, 0, 0, 0, 0, 0,
	0, 0, 1, 1, 1, 1, 1,
	0, 1, 0, 1, 1, 1, 1,
	0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0
};

static int memory_opt[NDATASET];

void usage(char *progname) {
	printf("Usage: %s ( -s[ource] slist | -t[arget] ) ", progname);
	printf("tlist [-d[ir] workdir]\n");
	printf("\t[-u[id] uid] [-p[wd] ");
	printf("password] \n\t");
	printf("[-o[racle] datasource]\n");
	printf("\t[-reload]\n");
	printf("\t[-nodb]\n");
	printf("\t[-quit]\n");
	printf("\t[-h[elp]]\n");
}

int
main(int argc, char **argv) {

	int i, sources[NLANG], targets[NLANG], srcindex, trgindex;
	char *cp, one[2];
	bool stayForever = true;

	for(i=0;i<NLANG;i++)
		sources[i] = targets[i] = 0;

	if(argc==1) {
		usage(argv[0]);
		exit(1);
	}

	for(i=1;i<argc;i++) {
		if((strcmp(argv[i], "-h")==0) || (strcmp(argv[i], "-?")==0)) {
			usage(argv[0]);
			printf("Options:\n");
			printf("\t-s slist - load datasets for given source language\n");
			printf("\t\tcodes. E.g. \"-s 12\" - German (1) and English(2)\n");
			printf("\t-t tlist - load datasets for given target language\n");
			printf("\t\tcodes. E.g. \"-t 23\" - English(2) and French(3)\n");
			printf("\t-d dir - set cache working directory to \"dir\".\n");
			printf("\t\tDatasets disk images are supposed to be stored there.\n");
			printf("\t-u uid - database user id. By default is taken\n");
			printf("\t\tfrom E-Sense \"server.properties\" file.\n");
			printf("\t-p pwd - database password. By default is taken\n");
			printf("\t\tfrom E-Sense \"server.properties\" file.\n");
			printf("\t-o datasource - Oracle datasource name.By default is taken\n");
			printf("\t\tfrom E-Sense \"server.properties\" file.\n");
			printf("\t-reload - force reloading data from database.\n");
			printf("\t-nodb - dont connect to DB no matter what.\n");
			printf("\t-quit - load and quit. Use to save to a disk any missing datasets.\n");
			printf("\t-memory nmeg - calculate which datasets to load\n");
			printf("\t\tassuming cache has \"nmeg\" Mb of free memory.\n");
			printf("\t-automemory - calculate amount of free memory\n");
			printf("\t\tand load the best possible dataset combination.\n");
			printf("\t\tThis is the default behaviour.\n");
			printf("\t-help - print this message\n");
			exit(1);
		}
	}

// -------- Source option
	printf("Source datasets to be loaded:");
	for(i=1;i<argc-1;i++) {
		if((strncmp(argv[i], "-s", 2)==0)) {
			cp = argv[i+1];
			int len = strlen(cp);
			for(int j=0;j<len;j++) {
				one[1] = '\0';
				one[0] = cp[j];
				srcindex = atoi(one);
				sources[srcindex] = 1 & available_sources[i];
				if(sources[srcindex])
					printf(" %s", names[srcindex]);
			}
		}
	}
	
// -------- Target option
	printf("\nTarget datasets to be loaded:");
	for(i=1;i<argc-1;i++) {
		if((strncmp(argv[i], "-t", 2)==0)) {
			cp = argv[i+1];
			int len = strlen(cp);
			for(int j=0;j<len;j++) {
				one[1] = '\0';
				one[0] = cp[j];
				trgindex = atoi(one);
				targets[trgindex] = 1 & available_targets[trgindex];
				if(targets[trgindex])
					printf(" %s", names[trgindex]);
			}
		}
	}
	printf("\n");

	// long t0, t1;

//----------- Directory
	char *homedir;
	char datadir[1024];

/*
	unsigned char homedir[1024];
	HKEY rkey;
	unsigned long buflen = 0;
	long lret = RegOpenKeyEx(HKEY_LOCAL_MACHINE,
		"SOFTWARE\\Logos Corporation\\e.Sense", NULL, KEY_READ, &rkey);

	ULONG sz=0;
	ULONG tp;
	do {
		lret = RegQueryValueExA(rkey, "LogosHome", NULL, 
			&tp , homedir, &sz);
	} while(lret==ERROR_MORE_DATA);
*/

	char *lgs_root = "LGS_ROOT";
	homedir = getenv(lgs_root);
	if(homedir==NULL) {
		printf("ERROR: \"%s\" environment variable is not set\n", lgs_root);
		exit(3);
	}
	strcpy(datadir, homedir);
	strcat(datadir, "\\bin_data");

	printf("E-Sense Home directory: \"%s\"\n", homedir);

	bool workDirValid = false;
	for(i=1;i<argc-1;i++) {
		if((strncmp(argv[i], "-d", 2)==0)) {
			cp = argv[i+1];
			int ret = _chdir(cp);
			if(ret==-1) {
				printf("ERROR: cant chdir to \"%s\" - error \"%s\"\n",
					cp, strerror(errno));
				_exit(2);
			}
			workDirValid = true;
			printf("DB-cache directory: \"%s\"\n", cp);
		}
	}
	if(!workDirValid) {
		printf("Setting working directory to \"%s\"\n", datadir);
		int ret = _chdir((const char *)datadir);
		if(ret==-1) {
			printf("ERROR: cant chdir to \"%s\" - error \"%s\"\n",
						datadir, strerror(errno));
			_exit(2);
		}
	}

	ServerProperties *props = new ServerProperties();
// variables for initializing config data interface 
	int nsrc, ntrg, npass, njob;

//----------- Halfnoundll-related
	if(sources[1]) {
		CLOCK_T t0 = GetTickCount();
		SHM_HANDLE hdl;
		char halfnoun_fname[256];
		nsrc = 1; ntrg = 2; npass = 2; njob = -1;
      InitConfigDataInterface(npass, njob, names[nsrc], names[ntrg], "", "", "", "", "",
                              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                              props->ScratchFileDirectory().c_str());
		GetConfigData("sourcedata", "halfnoun_wordlist", halfnoun_fname, 256);
// backslashes have to be replaced by smthg else
// because memory mapping finctions would not want any.
		int n = strlen(halfnoun_fname);
		for(i=0;i<n;i++)
			if(halfnoun_fname[i] == '\\')
				halfnoun_fname[i] = '/';
//		halfnoun_fname[0] = toupper(halfnoun_fname[0]);
		void *halfnp = mapFile(halfnoun_fname, &hdl);
		if(halfnp) {
			printf("\"%s\" mapped successfully\n", halfnoun_fname);
		} else {
			printf("\"%s\" mapping failure\n", halfnoun_fname);
		}
		CLOCK_T t1 = GetTickCount();
		printf("\"%s\" loading took %4.3f seconds\n", 
			halfnoun_fname, (t1-t0)/1000.);
	}

// Preloading RES rules
	int dummy_res1_mini=0;
	int dummy_res2_mini=0;
	char tranName[512];
	CLOCK_T t0 = GetTickCount();

	for(i=1;i<NLANG;i++) {
		ntrg = 2; npass = 2; njob = -1;
		if(sources[i]) {

// this is idiocy, but this is the way ConfigDataFileInterface is designed
// In fact, tran and res rules dont care what the target language is
// that's why it's set to an arbitrary value (2).
// But, ConfigDataFileInterface checks src==trc and fails if so.
			if(i==2) ntrg = 1;

         InitConfigDataInterface(npass, njob, names[i], names[ntrg], "", "", "", "",
                                 "", "", "", "", "", "", "", "", "", "", "", "", "",
                                 "", "", "", props->ScratchFileDirectory().c_str());
			res_rule_load(&dummy_res1_mini, &dummy_res2_mini, 1);
			printf("Res rules file for source \"%s\" loaded\n", names[i]);

// now tran rules
			for(int j=1;j<5;j++) {
				char keyname[256];
				sprintf(keyname, "trandata_tran%-d", j);
				GetConfigData("sourcedata", keyname, tranName, 256);
				rule_load_into_cache(tranName, j);
				printf("Tran rules file \"%s\" loaded\n", tranName);
			}
		}
	}
	CLOCK_T t1 = GetTickCount();
	printf("Res & Tran loading took %4.3f seconds\n", (t1-t0)/1000.);

//---------- OracleDataSource/uid/passwd
	char userid[64], password[64], datasource[64];

	strncpy(userid, props->DatabaseUserID().c_str(), 64);
	strncpy(password, props->DatabasePassword().c_str(), 64);
	strncpy(datasource, props->OracleDataSource().c_str(), 64);

//---------- 
	for(i=1;i<argc-1;i++) {
		if((strncmp(argv[i], "-u", 2)==0)) {
			strcpy(userid, argv[i+1]);
		}
		if((strncmp(argv[i], "-p", 2)==0)) {
			strcpy(password, argv[i+1]);
		}
		if((strncmp(argv[i], "-o", 2)==0)) {
			strcpy(datasource, argv[i+1]);
		}
	}
	printf("Datasource: \"%s\"\n", datasource);
	printf("DB User Id: \"%s\"\n", userid);
	printf("DB Password: \"%s\"\n", password);

	int strategy = LOAD_TRY_BEST;
	for(i=1;i<argc;i++) {
		if((strcmp(argv[i], "-reload")==0)) {
			strategy = LOAD_FROM_DB_ONLY;
			break;
		}
		if((strcmp(argv[i], "-nodb")==0)) {
			strategy = LOAD_FROM_DISK_ONLY;
			break;
		}
	}

	for(i=1;i<argc;i++) {
		if((strcmp(argv[i], "-quit")==0)) {
			stayForever = false;
			break;
		}
	}

#ifdef _MSC_VER
	MEMORYSTATUS gms;
	GlobalMemoryStatus(&gms);
	int memory_avail = gms.dwAvailPhys/1024./1024.;
	int memory_limit = memory_avail - 50;
 	for(i=1;i<argc-1;i++) {
		if((strcmp(argv[i], "-memory")==0) ||
			(strncmp(argv[i], "-m", 2)==0)) {
			memory_limit = atoi(argv[i+1]);
			break;
		}
	}

	printf("Total physical memory: %4.2f Mb\n", gms.dwTotalPhys/1024./1024.);
	printf("Avail. physical memory: %d Mb\n", memory_avail);
//	printf("Cache memory: %d Mb\n", memory_limit);
#endif

	bool connectNeeded = (strategy!=LOAD_FROM_DISK_ONLY);
	CacheManager *cm =
		CacheManager::singleton(datasource, userid, password, connectNeeded);

//	cm->calculateBestDatasetCombination(2, 3, memory_limit, memory_opt);

	cm->loadNeutrals();

	for(i=1;i<NLANG;i++) {
		if(sources[i])
			cm->loadSource(i, strategy);
		if(targets[i])
			cm->loadTarget(i, strategy);
	}
// load semtabs
	int j;
	for(i=1;i<NLANG;i++)
		for(j=1;j<NLANG;j++)
			if(sources[i] & targets[j] & available_semtabs[i][j])
				cm->loadSourceTarget(i, j, strategy);

	printf("CacheManager consumes %4.2f Mb of memory\n", 
		cm->getMemoryUsage()/1024./1024.);
	printf("CacheManager has loaded %d records in %4.2f secs\n", 
		cm->getNRecords(), cm->getLoadTime()/1000.);
	printf("=== Cache has been loaded ===\n");

	if(stayForever)
		while(1)
			Sleep(1000000);
}

#ifdef _MSC_VER
void *
mapit(char *name, int nb, HANDLE *handle) {

	DWORD desired_access;

	desired_access = FILE_MAP_WRITE;
	*handle = CreateFileMapping((HANDLE)0xFFFFFFFF,
		NULL, PAGE_READWRITE, 0, nb, name);
	if(handle==NULL) {
		fprintf(stderr, "CacheData.ctor: area handle \"%s\" error %d\n", 
				name, GetLastError());
		return NULL;
	}

	void *vp = MapViewOfFile(*handle, desired_access, 0, 0, nb);
	if(vp==NULL) {
		fprintf(stderr, "CacheData.ctor: mapping area \"%s\" %d bytes error %d\n", 
			name, nb, GetLastError());
		return NULL;
	}
	return vp;
}
#endif

#ifdef HAVE_MMAP
void *mapit(char *name, int nb, SHM_HANDLE *handle)
{
  *handle = new memory_mapper();
  void *vp =
    (*handle)->open(name, nb, memory_mapper::RDWR_CREATE);

  if (vp == NULL) {
    fprintf(stderr, "CacheData.ctor: mapping area \"%s\" error %d\n"
            , name, errno);
    return NULL;
  }
  
  return vp;
}
#endif

void * mapFile(char *name, SHM_HANDLE *h) {

	int fd = _open(name, O_RDONLY|O_BINARY);
	if(fd==-1) { perror(name); return NULL;}

	struct _stat statbuf;
	int ret = _fstat(fd, &statbuf);
	if(ret==-1) { perror(name); return NULL;}

	void *vp = mapit(name, statbuf.st_size + sizeof(int), h);
	if(vp==NULL)
		return NULL;

	ret = _read(fd, (char *)vp + sizeof(int), statbuf.st_size);
	if(ret==-1) { perror(name); return NULL;}
	printf("%d bytes of \"%s\" have been read\n", ret, name);

	_close(fd);

	*(int *)vp = statbuf.st_size;
	return vp;
}
