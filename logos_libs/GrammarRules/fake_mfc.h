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
/* -*- Mode: C++ -*- */

#ifndef _FAKE_MFC_H
#define _FAKE_MFC_H

#include "logos_include/windows.h"

#define AFX_EXT_CLASS
#define UINT size_t

#include <ctime>
#include <cstring>
#include <cstdio>
#include <sys/mman.h>
#include <vector>

#include <string>
#include <cassert>

using namespace std;

class CObject {
public:
  void Serialize(class CArchive &) {}
};

class CFile {
  friend class CArchive;
private:
  /** File name if disk file */
  std::string _fname;
  /** File descriptor if disk file */
  FILE *_f;

protected:
  CFile() : _f(NULL) {} ;

public:
  enum SeekVal { begin = SEEK_SET, current = SEEK_CUR, end = SEEK_END };
  /** shareDenyWrite denies other processes write access to the file */
  enum AccessMode { modeRead = 1, shareDenyWrite = 2, modeWrite = 4,
                    modeCreate = 8 };

  CFile(const char *fname, int mode) {
    const char * openmode = "wb";
    if (mode & modeRead) openmode = "rb";
    _fname = fname;
    _f = fopen(fname, openmode);
    std::string s = (string) "Unable to open file " + fname
      + (string)" with mode " + openmode ; 
    if (_f == NULL) throw(s);
  }

  virtual ~CFile() {
    Close();
  }
  
  virtual void Seek(long offset, SeekVal whence) {
    if (NULL != _f)
      // _fix_me_ a correct reimplementation has to check that the pointer is
      // not moved past the end-of-file
      fseek(_f, offset, whence);
  }

  void Close() {
    if (NULL != _f) {
      if (ftell(_f) != GetLength())
        fprintf(stderr, "Not read until end of file: %s\n", _fname.c_str());
      fclose(_f);
      _f = NULL;
    }
  }

  virtual long GetPosition() const {
    if (NULL != _f) {
      return ftell(_f);
    } else {
      return -1;
    }
  }

  virtual long GetLength() const {
    if (NULL != _f) {
      long res, current = ftell(_f);
      fseek(_f, 0, SEEK_END);
      res = ftell(_f);
      fseek(_f, current, SEEK_SET);
      return res;
    } else {
      return -1;
    }
  }

  // This is different from the real MFC call, which always reads bytes (with
  // the exception of cr/lf pairs, which count as one byte when read from text
  // files
  virtual size_t Write(const void *ptr, size_t nmemb, size_t size = 1) {
    if (NULL != _f) {
      return fwrite(ptr, size, nmemb, _f);
    } else {
      return 0;
    }
  }

  virtual size_t Read(void *ptr, size_t nmemb, size_t size = 1) {
    if (NULL != _f) {
      return fread(ptr, size, nmemb, _f);
    } else {
      return 0;
    }
  }

  void Flush() {
    if (NULL != _f) fflush(_f);
  }

};

class CMemFile : public CFile {
  int BLOCK_SIZE;
  std::vector<char *> _blocks;
  int _last_offset;
  int _curr_block;
  int _curr_offset;

  bool get_new_block() {
    // Anonymous mapping, the last two arguments are ignored
    void *new_block = mmap(NULL, BLOCK_SIZE, PROT_READ | PROT_WRITE, 
                           MAP_ANONYMOUS | MAP_SHARED, -1, 0);
    if (NULL != new_block)
      _blocks.push_back((char *)new_block);
    _last_offset = 0;
    return (new_block != NULL);
  }

  int last_block() { return _blocks.size() - 1; }

public:
  CMemFile() {
    BLOCK_SIZE = getpagesize();
    get_new_block();
    _curr_block = 0;
    _curr_offset = 0;
  }

  virtual ~CMemFile() {
    for( int i = last_block(); i >= 0; i--) {
      munmap(_blocks[i], BLOCK_SIZE);
    }
    _blocks.resize(0);
  }
  
  virtual size_t Write(const void *iptr, size_t nmemb, size_t size = 1) {
    char *ptr = (char *)iptr;
    size_t still_to_copy = (size * nmemb);
    size_t remains = BLOCK_SIZE - _curr_offset;
    // Check if the current block gets completely filled
    if (still_to_copy >= remains) {
      memcpy(_blocks[_curr_block] + _curr_offset, ptr, remains);
      ptr += remains;
      still_to_copy -= remains;
      _curr_block++;
      _curr_offset = 0;
      if (_curr_block > last_block()) {
        get_new_block();
      }
    }
    // Copy as many remaining complete blocks as necessary: if i > 0, then the
    // previous if condition was true and _curr_offset must be zero
    for (int i = still_to_copy / BLOCK_SIZE; i > 0; i--) {
      memcpy(_blocks[_curr_block], ptr, BLOCK_SIZE);
      _curr_block++;
      ptr += BLOCK_SIZE;
      still_to_copy -= BLOCK_SIZE;
      if (_curr_block > last_block()) {
        get_new_block();
      }
    }
    // Now: still_to_copy is less than what is left in the current block
    if (still_to_copy > 0) {
      memcpy(_blocks[_curr_block] + _curr_offset, ptr, still_to_copy);
      _curr_offset += still_to_copy;
      if ((_curr_block == last_block()) && (_curr_offset > _last_offset))
        _last_offset = _curr_offset;
    }
    return size * nmemb;
  }

  virtual void Seek(long offset, SeekVal whence) {
    switch (whence) {
    case CFile::begin:
      _curr_block = 0;
      _curr_offset = 0;
      break;
    case CFile::end:
      _curr_block = last_block();
      _curr_offset = _last_offset;
      break;
    }
    
    long off_blocks = offset / BLOCK_SIZE;
    _curr_block = _curr_block + off_blocks;
    _curr_offset = _curr_offset + offset - (off_blocks * BLOCK_SIZE);
    
    if (_curr_block > last_block()) {
      _curr_block = last_block();
      _curr_offset = _last_offset;
    } else {
      if ((_curr_block == last_block()) && (_curr_offset > _last_offset)) {
        _curr_offset = _last_offset;
      }
    }
  }

  virtual long GetPosition() const {
    return _curr_block * BLOCK_SIZE + _curr_offset;
  }
  
  virtual long GetLength() const {
    return (_blocks.size() - 1) * BLOCK_SIZE + _last_offset;
  }

  virtual size_t Read(void *iptr, size_t nmemb, size_t size = 1)
  {
    char *ptr = (char *)iptr;
    size_t toread = size * nmemb;
    long capacity = GetLength() - GetPosition();
    // is the file long enough ??
    if (capacity < toread) {
      // No: we can only read capacity bytes
      toread = capacity;
    } else {
      // Yes: save the value of toread to return it
      capacity = toread;
    }
    // Do we have to read beyond the block border?
    long restlength = BLOCK_SIZE - _curr_offset;
    if (toread >= restlength) {
      memcpy(ptr, _blocks[_curr_block] + _curr_offset, restlength);
      ptr += restlength;
      toread -= restlength;
      _curr_block++;
      _curr_offset = 0;
    }

    // Read as many full blocks as necessary
    while (toread > BLOCK_SIZE) {
      memcpy(ptr, _blocks[_curr_block], BLOCK_SIZE);
      ptr += BLOCK_SIZE;
      toread -= BLOCK_SIZE;
      _curr_block++;
    }
    // Now toread can be completely taken from this block
    if (toread > 0) {
      memcpy(ptr, _blocks[_curr_block] + _curr_offset, toread);
      _curr_offset += toread;
    }
    return capacity;
  }
};

class CArchive : public CObject {
public:
  enum Mode { load, store };

  CArchive(CFile *f, Mode m) : _f(f), _mode(m) {
    /*
    if (_f->_f != NULL) {
      fprintf(stderr, "File archive %s opened for %s",
              _f->_fname.c_str(), IsStoring() ? "writing" : "reading");
      assert(dynamic_cast<CMemFile *>(f) == NULL) ;
    }
    */
  }
 
  bool IsStoring() { return _mode == CArchive::store; }

  CArchive &Write(const void *val, size_t len){
    // write the bytes to file
    _f->Write(val, len);
    return *this;
  }

  size_t Read(void *val, size_t len){
    // read the bytes from file
    return _f->Read(val, len);
  }

  void Close() {
    // ?? No internal state ??
    _f->Flush();
  }

private:
  Mode _mode;
  CFile *_f;
};


/* 
// This is how it should be done if the only files were deserialized which were
// serialized by the same code/architecture. But since we are deserializing
// pre-dumped files, we have to fix the width of the data structures for all
// architectures 

template <typename __Ty> CArchive &operator<< (CArchive &ar, const __Ty &val){
  // write val as sizeof(__Ty) bytes
  ar.Write((const void *) &val, sizeof(val));
  return ar;
}

template <typename __Ty> CArchive &operator>> (CArchive &ar, __Ty &val){
  // read val as sizeof(__Ty) bytes
  ar.Read((void *) &val, sizeof(val));
  return ar;
}
*/

CArchive &operator<< (CArchive &ar, unsigned long val);
CArchive &operator>> (CArchive &ar, unsigned long &val);

CArchive &operator<< (CArchive &ar, int val);
CArchive &operator>> (CArchive &ar, int &val);

CArchive &operator<< (CArchive &ar, unsigned int val);
CArchive &operator>> (CArchive &ar, unsigned int &val);

CArchive &operator<< (CArchive &ar, short val);
CArchive &operator>> (CArchive &ar, short &val);

CArchive &operator<< (CArchive &ar, unsigned short val);
CArchive &operator>> (CArchive &ar, unsigned short &val);

// These seem to be the correct counterparts to the Win types
typedef const char * LPCTSTR;
#define lstrcmp strcmp
typedef char * LPTSTR;

#define DECLARE_SERIAL(__TYPE) \
  friend CArchive& ::operator<<(CArchive &, __TYPE &); \
  friend CArchive& ::operator>>(CArchive &, __TYPE &);
//  CArchive& operator<<(CArchive &, const __TYPE &); \
//  CArchive& operator>>(CArchive &, const __TYPE &);

// wSchema is somewhat of a "version number" to enable a deserializing programe
// identify and handle the data created by earlier versions. It is encoded in
// the archive, at least that is what the documentation says. It seems that if
// it is zero, nothing's written to the archive.
#define IMPLEMENT_SERIAL(__Ty, __STy, __wSchema) \
  CArchive& operator<<(CArchive &ar, __Ty& obj) { \
    static_cast<__STy &>(obj).Serialize(ar); \
    obj.Serialize(ar); return ar; } \
  CArchive& operator>>(CArchive &ar, __Ty& obj) { \
    static_cast<__STy &>(obj).Serialize(ar); \
    obj.Serialize(ar); return ar; }


class CTime : public CObject {
private:
  time_t _time;
public:
  static CTime GetCurrentTime() { CTime res; time(&res._time); return res; }
  DECLARE_SERIAL(CTime);
  void Serialize(CArchive &ar);
};

// IMPLEMENT_SERIAL(CTime, CObject, 1)

inline void * ZeroMemory(void *mem, size_t n) { return memset(mem, 0, n); }

#endif
