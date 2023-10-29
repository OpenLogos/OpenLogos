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
#include <logos_libs/sql/sqlconnection.h>
#include <logos_libs/sql/sqlstatement.h>
#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CacheData.h>
#include <logos_libs/dbcache/CacheKey.h>
#include <logos_libs/dbcache/CacheHashTab.h>

#include <fcntl.h>

#ifdef HAVE_MMAP
#include "logos_include/memory_mapper.h"
#include <errno.h>
#endif


bool CacheData::debug = false;

void CacheData::setDebug(bool b) { debug = b; }

CacheData::CacheData(SqlConnection *con, int lc, char stmtLetters[], 
                     bool whoami, bool loadFromFile, bool saveOnDisk)
  :hashtab(0)
{
  language_code = lc;
  constructCacheData(con, lc, -1, stmtLetters, whoami, loadFromFile, saveOnDisk);
}

CacheData::CacheData(SqlConnection *con, int lc, int trg, char stmtLetters[],
                     bool whoami, bool loadFromFile, bool saveOnDisk)
  :hashtab(0)
{

  language_code = trg;
  constructCacheData(con, lc, trg, stmtLetters, whoami, loadFromFile, saveOnDisk);
}

void CacheData::constructCacheData(SqlConnection *con, int srcl, int trgl,
                                   char stmtLetters[], bool whoami, 
                                   bool loadFromFile, bool saveOnDisk)
{

  if (whoami)
    {
      if (trgl == -1)
        printf("Loading %s(%d) from ", stmtLetters, srcl);
      else
        printf("Loading %s(%d,%d) from ", stmtLetters, srcl, trgl);
      if (loadFromFile)
        printf("disk\n");
      else
        printf("database\n");
      fflush(stdout);
    }
  oracleConnection = con;
  source_language = srcl;
  target_language = trgl;
  amIHolder = whoami;
  this->loadFromFile = loadFromFile;
  this->saveOnDisk = saveOnDisk;
  sprintf(basename, "logos-statement-%s-", stmtLetters);

  switch(srcl)
    {
    case 0:
      break;	// WordInPhrase does not depend on language
    case 1:
      strcat(basename, "DE");
      break;
    case 2:
      strcat(basename, "EN");
      break;
    case 3:
      strcat(basename, "FR");
      break;
    case 4:
      strcat(basename, "ES");
      break;
    case 5:
      strcat(basename, "IT");
      break;
    case 6:
      strcat(basename, "PT");
      break;
    default:
      strcat(basename, "ERROR");
      break;
    }
  switch(trgl)
    {
    case -1:
      break;
    case 1:
      strcat(basename, "2DE");
      break;
    case 2:
      strcat(basename, "2EN");
      break;
    case 3:
      strcat(basename, "2FR");
      break;
    case 4:
      strcat(basename, "2ES");
      break;
    case 5:
      strcat(basename, "2IT");
      break;
    case 6:
      strcat(basename, "2PT");
      break;
    default:
      strcat(basename, "ERROR");
      break;
    }

  strcpy(data_area_name, basename);
  strcat(data_area_name, "-data");
  strcpy(keys_area_name, basename);
  strcat(keys_area_name, "-keys");
  strcpy(heads_area_name, basename);
  strcat(heads_area_name, "-heads");
  strcpy(weights_area_name, basename);
  strcat(weights_area_name, "-weights");

  // max_key_len HAS to be set in derived classes.
  max_key_len = 0;
  row_len = 0;

  // Below size of a table is being calculated.
  // Logic is to load from file if no refresh/reload requested
  // and calculate from database (call "select count(*) ...")
  // otherwise.
  strcpy(info_area_name, basename);
  strcat(info_area_name, "-info");
}

bool CacheData::isValid()
{
  bool ret;
  if (info)
    ret = info->state == DATASET_STATE_READY; 
  else
    ret = false;
  return ret;
}

void CacheData::getSizeFromDB()
{
  printf("\nERROR: CacheData::getSizeFromDB should be overriden\n");
  fflush(stdout);
}

void CacheData::mapAndLoadReduced() {
  if (row_len == 0)
    {
      printf("CacheData.mapAndLoad \"%s\" ERROR: row_len not set\n", basename);
      fflush(stdout);
      return;
    }

  // data
  int nbytes = size * row_len;
  data_array = mapit(data_area_name, nbytes, &data_handle);
  memory += nbytes;

  keys_array = NULL;
  heads_array = NULL;
  weights_array = NULL;

  if ((data_array != NULL) && info && amIHolder)
    info->state = DATASET_STATE_LOADING;
}

void CacheData::mapAndLoadFull() {
  if (max_key_len == 0 || row_len == 0)
    {
      printf("CacheData.mapAndLoad \"%s\" ERROR: max_key_len or row_len not set\n", basename);
      fflush(stdout);
      return;
    }

  // data
  int nbytes = size * row_len;
  data_array = mapit(data_area_name, nbytes, &data_handle);
  memory += nbytes;

  // keys
  nbytes = size * max_key_len;
  keys_array = (CacheKey *)mapit(keys_area_name, nbytes, &keys_handle);
  memory += nbytes;

  // head of hash queues
  nbytes = size * sizeof(int);
  heads_array = (int *)mapit(heads_area_name, nbytes, &heads_handle);
  memory += nbytes;

  // weights of each byte value in a key
  nbytes = 256 * sizeof(double);
  weights_array = (double *)mapit(weights_area_name, nbytes, &weights_handle);
  memory += nbytes;

  if ((keys_array != NULL) && (heads_array != NULL) && (weights_array != NULL)
      && (data_array != NULL) && info && amIHolder)
    info->state = DATASET_STATE_LOADING;

  hashtab = new CacheHashTab(size, max_key_len,
                             keys_array, heads_array, weights_array);
  if (amIHolder) hashtab->reset();
}

void CacheData::mapAndLoadInternal() {
  mapAndLoadFull();
}

void CacheData::mapAndLoad()
{

  loadtime = 0;
  memory = 0;
  CLOCK_T t0 = GetTickCount();
  //---- dynamic size
  // create shared area if holder, bind to it if client
  info = (struct data_info *)mapit(info_area_name, sizeof(struct data_info), &info_handle);
  if (info && amIHolder)
    {
      info->state = DATASET_STATE_EMPTY;
    }

  // if i'm a holder and loadFromFile==true - read it
  // if i'm a holder and loadFromFile==false - load from DB
  // if I'm a client - do nothing
  size = 0;
  if (amIHolder)
    {
      if (loadFromFile)
        {
          // _fix_me_ I don't know why this could be necessary. If we created a
          // file mapping to an existing file under Linux, info has the correct
          // values
#ifdef _MSC_VER
          long realRead;
          int fd = _open(info_area_name, O_RDONLY|O_BINARY);
          if (fd != -1)
            {
              realRead = _read(fd, info, sizeof(struct data_info));
              if (realRead == sizeof(struct data_info) && info)
                size = info->size;
              _close(fd);
            }
#else
          size = info->size;
#endif
        }

      // if loading from file either was not specified or failed
      // _fix_me_ It should be noted here that if the mapit fn created a new
      // file mapping, size is zero because the info struct will also contain
      // only zeroes
      if (!loadFromFile || size == 0)
        {
          getSizeFromDB();
          info->size = size;
          // _fix_me_ Again, this is not only unnecessary under Linux, but
          // might even be wrong.
#ifdef _MSC_VER
          int fd = _open(info_area_name, 
                         O_RDWR|O_TRUNC|O_CREAT|O_BINARY, 0664);
          if (fd == -1)
            { 
              perror(info_area_name);
            }
          else
            {
              _write(fd, info, sizeof(struct data_info)); 
              _close(fd);
            }
#endif
        }
    }
  if (info)
    size = info->size;

  // because not all subclasses construct the same tables
  mapAndLoadInternal();

  if (amIHolder)
    {
      bool loadSuccess = false;
      if (loadFromFile)
        {
          loadSuccess = load();
        }
      else
        {
          loadSuccess = populateFromDatabase();
        }
      if (saveOnDisk)
        save();
      // let clients know that dataset is ready
      if (loadSuccess && info)
        info->state = DATASET_STATE_READY;
    }

  CLOCK_T t1 = GetTickCount();
  loadtime += t1 - t0;

  if (amIHolder)
    {
      if (target_language == -1)
        printf("-- %s(%d) done\n", basename, source_language);
      else
        printf("-- %s(%d,%d) done\n", basename, source_language, target_language);
    }
}

CacheData::~CacheData()
{

  if (data_array) {
    unmapit(data_array, data_handle);
  }
  if (keys_array) {
    unmapit(keys_array, keys_handle);
  }
  if (heads_array) {
    unmapit(heads_array, heads_handle);
  }
  if (weights_array) {
    unmapit(weights_array, weights_handle);
  }
  if (info) {
    unmapit(info, info_handle);
  }
  size = 0;
  if (hashtab)
    delete hashtab;
}

int CacheData::getRowIndex(CacheKey *key)
{
  return hashtab->get(key);
}

long CacheData::getNRows()
{
  return nrows;
}

bool CacheData::populateFromDatabase()
{
  return true;
}

#ifdef WIN32
void *CacheData::mapit(char *name, int nb, HANDLE *handle)
{

  DWORD desired_access;

  if (!amIHolder)
    {
      desired_access = FILE_MAP_READ;
      *handle = OpenFileMapping(desired_access, false, name);
      if (*handle == NULL)
        {
          return NULL;
        }
    }
  else
    {
      desired_access = FILE_MAP_WRITE;
      *handle = CreateFileMapping((HANDLE)0xFFFFFFFF, NULL, PAGE_READWRITE, 0, nb, name);
      if (*handle == NULL)
        {
          fprintf(stderr, "CacheData.ctor: area handle \"%s\" error %d\n", name, GetLastError());
          return NULL;
        }
    }

  void *vp = MapViewOfFile(*handle, desired_access, 0, 0, nb);
  if (vp == NULL)
    {
      fprintf(stderr, "CacheData.ctor: mapping area \"%s\" error %d\n", name, GetLastError());
      return NULL;
    }

  if (debug && amIHolder)
    {
      fprintf(stdout, "area \"%s\", size %3.1fMb.\n", name, nb/1024./1024.);
      fflush(stdout);
    }

  return vp;
}

void CacheData::unmapit(void *addr, HANDLE &handle) {
  UnmapViewOfFile(addr);
  CloseHandle(handle);
}

#endif

#ifdef HAVE_MMAP
void *CacheData::mapit(char *name, int nb, SHM_HANDLE *handle)
{
  *handle = new memory_mapper();
  void *vp =
    (*handle)->open(name, nb,
                    (amIHolder
                     ? memory_mapper::RDWR_CREATE : memory_mapper::RDONLY));

  if (vp == NULL && amIHolder) {
    fprintf(stderr, "CacheData.ctor: mapping area \"%s\" error %d\n"
            , name, errno);
    return NULL;
  }
  
  if (debug && amIHolder) {
    fprintf(stdout, "area \"%s\", size %3.1fMb.\n", name, nb/1024./1024.);
    fflush(stdout);
  }

  return vp;
}

void CacheData::unmapit(void *addr, SHM_HANDLE &handle) {
  handle->close();
  delete handle;
  handle = NULL;
}
#endif

void CacheData::save()
{
#ifndef _MSC_VER
  // _fix_me_ Just make sure the data appears on the disk files. Hope that's
  // sufficient 
  data_handle->flush();
  keys_handle->flush();
  heads_handle->flush();
  weights_handle->flush();
#else
  printf("writing \"%s\"...", data_area_name);
  fflush(stdout);
  int nbytes = size * row_len;
  int fd = _open(data_area_name, O_RDWR|O_TRUNC|O_CREAT|O_BINARY, 0664);
  if (fd == -1)
    {
      perror(data_area_name);
    }
  else
    {
      _write(fd, data_array, nbytes);
      _close(fd);
    }
  printf("done\n");

  printf("writing \"%s\"...", keys_area_name);
  fflush(stdout);
  nbytes = size * max_key_len;
  fd = _open(keys_area_name, O_RDWR|O_TRUNC|O_CREAT|O_BINARY, 0664);
  if (fd == -1)
    {
      perror(keys_area_name);
    }
  else
    {
      _write(fd, keys_array, nbytes);
      _close(fd);
    }
  printf("done\n");

  printf("writing \"%s\"...", heads_area_name);
  fflush(stdout);
  nbytes = size * sizeof(int);
  fd = _open(heads_area_name, O_RDWR|O_TRUNC|O_CREAT|O_BINARY, 0664);
  if (fd == -1)
    {
      perror(heads_area_name);
    }
  else
    {
      _write(fd, heads_array, nbytes);
      _close(fd);
    }

  printf("done\n");

  printf("writing \"%s\"...", weights_area_name);
  fflush(stdout);
  nbytes = 256 * sizeof(double);
  fd = _open(weights_area_name, O_RDWR|O_TRUNC|O_CREAT|O_BINARY, 0664);
  if (fd == -1)
    {
      perror(weights_area_name);
    }
  else
    {
      _write(fd, weights_array, nbytes);
      _close(fd);
    }
#endif
  printf("done\n");

}

bool CacheData::load()
{
  // _fix_me_ All the data should be available after mapping. Isn't that what
  // mapping is for??
#ifdef _MSC_VER
  nrows = 0;
  long realread;
  if (amIHolder && debug)
    {
      printf("reading \"%s\"...", data_area_name);
      fflush(stdout);
    }
  int nbytes = size * row_len;
  int fd = _open(data_area_name, O_RDONLY|O_BINARY);
  if (fd == -1)
    {
      perror(data_area_name);
      return false;
    }
  else
    {
      realread = _read(fd, data_array, nbytes);
      nrows += realread/row_len; 
      _close(fd);
    }
  if (amIHolder && debug)
    {
      printf("done\n");
      printf("reading \"%s\"...", keys_area_name);
      fflush(stdout);
    }
  nbytes = size * max_key_len;
  fd = _open(keys_area_name, O_RDONLY|O_BINARY);
  if (fd == -1)
    {
      perror(keys_area_name);
      return false;
    }
  else
    {
      realread = _read(fd, keys_array, nbytes);
      close(fd);
    }
  if (amIHolder && debug)
    {
      printf("done\n");

      printf("reading \"%s\"...", heads_area_name);
      fflush(stdout);
    }
  nbytes = size * sizeof(int);
  fd = _open(heads_area_name, O_RDONLY|O_BINARY);
  if (fd == -1)
    {
      perror(heads_area_name);
      return false;
    }
  else
    {
      realread = _read(fd, heads_array, nbytes);
      close(fd);
    }

  if (amIHolder && debug)
    {
      printf("done\n");
      printf("reading \"%s\"...", weights_area_name);
      fflush(stdout);
    }
  nbytes = 256 * sizeof(double);
  fd = _open(weights_area_name, O_RDONLY|O_BINARY);
  if (fd == -1)
    {
      perror(weights_area_name); 
      return false;
    }
  else
    {
      realread = _read(fd, weights_array, nbytes);
      close(fd);
    }
#endif
  if (amIHolder && debug)
    {
      printf("done\n");
    }
  return true;

}

void CacheData::printAll()
{
  printf("Dump for CacheData \"%s\"\n", basename);	
  printf("\tsize = %d\n", size);	
  printf("\trow_len = %d\n", row_len);
  printf("\tmax_key_len = %d\n", max_key_len);
  printf("\t --- heads \n");
  for (int i = 0; i < size; i++)
    {
      printf("%d=%d\n", i, heads_array[i]);
    }
}
