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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
/* -*- Mode: C++ -*- */

#ifndef __MEMORY_MAPPER_H__
#define __MEMORY_MAPPER_H__

//#include <sys/mman.h>
//#include <fcntl.h>
#include <string>
#include <cstring>
#include <list>
#include <functional>

class memory_mapper {
  /** file descriptor of mapped file */
  int _fd;
  /** start address of mapped area */
  void *_addr;
  /** length of mapped area */
  size_t _length;
  /** Modes for opening the files, resp. mmap */
  int _openmode, _mmapmode;

public:
  enum access_mode { RDONLY, RDWR_CREATE, RDWR_MAYBE_CREATE, RDWR_OPEN };

  memory_mapper() ; //{
//     _fd = -1;
//     _addr = NULL;
//   }

  memory_mapper(const char *name, size_t size, access_mode mode); // {
//     _fd = -1;
//     _addr = NULL;
//     open(name, size, mode);
//   }

  /** Copy constructor makes a new mmap view with the same fd */
  memory_mapper(const memory_mapper &orig); // {
//     _fd = orig._fd;
//     _openmode = orig._openmode;
//     _mmapmode = orig._mmapmode;
//     _addr = NULL;
//     map_view(orig._length);
//   }

  ~memory_mapper(); // {
//     close();
//   }

  void *open(const char *name, size_t size, access_mode mode); // {
//     close();
//     switch (mode) {
//     case RDONLY:
//       _openmode = O_RDONLY; _mmapmode = PROT_READ;
//       break;
//     case RDWR_CREATE:
//       _openmode = O_RDWR|O_CREAT|O_TRUNC; _mmapmode = PROT_READ|PROT_WRITE;
//       break;
//     case RDWR_MAYBE_CREATE:
//       _openmode = O_RDWR|O_CREAT; _mmapmode = PROT_READ|PROT_WRITE;
//       break;
//     case RDWR_OPEN:
//       _openmode = O_RDWR; _mmapmode = PROT_READ|PROT_WRITE;
//       break;
//     default:
//       return NULL;
//     }
      
//     if ((_fd = ::shm_open(name, _openmode, 00777)) == -1) return NULL;
//     if (map_view(size) == NULL) {
//       close();
//     }
    
//     return _addr;
//   }

  void *map_view(size_t size); // {
//     _length = size;
//     return map_view();
//   }

  void *map_view(); // {
//     unmap_view();
//     if (_fd != -1) {
//       _addr = mmap(NULL, _length, _mmapmode, MAP_SHARED, _fd, 0);
//     }
//     return _addr;
//   }

  void flush() { if (_fd != -1) fsync(_fd); }

  void *addr() const ; //{ return _addr; }

  void unmap_view() ; //{
//     if (_addr != NULL) munmap(_addr, _length);
//     _addr = NULL;
//   }

  void close() ; // {
//     unmap_view();
//     if (_fd != -1) ::close(_fd);
//     _fd = -1;
//   }
};

class memmap_manager {
private:
  typedef std::pair<std::string, memory_mapper> key_map_pair;
  std::list< key_map_pair > _mappers;
  typedef std::list< key_map_pair >::iterator map_it;

  class hasName : public std::unary_function< key_map_pair, bool > {
    const char *_name;
  public:
    hasName(const char *name) : _name(name) {}
    bool operator()(const key_map_pair &s) {
      return (strcmp(_name, s.first.c_str()) == 0) ;
    }
  };

  class hasAddress : public std::unary_function< key_map_pair, bool > {
    const void *_address;
  public:
    hasAddress(const void *address) : _address(address) {}
    bool operator()(const key_map_pair &s) {
      return _address == s.second.addr() ;
    }
  };

public:
  /** Is the current map already available?
   * There are two implementation alternatives. Either check for the existence
   * of the (closed on unmap) file or keep the names of the existing open
   * files.
   */
  bool isMapped(const char* mem_name);//  {
//     return (find_if(_mappers.begin(), _mappers.end(), hasName(mem_name))
//             != _mappers.end());
//   }
  
  /** mapChunk may only be called if isMapped is false, that is, there is no
   * memory mapper with the appropriate contents.
   * This function should rather be called getNewMappedChunk
   */
  char *mapChunk(const char *mem_name, int sz); // {
//     map_it curr = find_if(_mappers.begin(), _mappers.end(), hasName(mem_name));

//     // Create new memory mapping if a mapping exists ??
//     // All threads run in the same address space, so what could that be good
//     // for? 
//     /*
//     _mappers.push_back(pair<string,memory_mapper>
//                        (mem_name, 
//                         curr != _mappers.end()
//                         ? memory_mapper(*curr)
//                         : memory_mapper(mem_name, sz
//                                         , memory_mapper::RDWR_MAYBE_CREATE)));
//     if (_mappers.back().addr() == NULL) {
//       _mappers.pop_back();
//       return NULL;
//     }
//       return _mappers.back().addr();
//     */
//     if (curr != _mappers.end()) {
//       if (curr->second.addr() != NULL)
//         return (char *) curr->second.addr();
//       else {
//         _mappers. push_back(pair<string,memory_mapper>
//                             (mem_name, 
//                              memory_mapper(mem_name, sz,
//                                            memory_mapper::RDWR_MAYBE_CREATE)));
//         curr = --_mappers.end();
//       }
//     }
//     if (curr->second.addr() == NULL) {
//       _mappers.erase(curr);
//       return NULL;
//     }
//     return (char *) curr->second.addr();
//   }

  /** Get the chunk of an already mapped view */
  char *getMappedChunk(const char *mem_name); // {
//     map_it curr_it 
//       = find_if(_mappers.begin(), _mappers.end(), hasName(mem_name));
//     if (curr_it == _mappers.end()) return NULL;

//     // Create new memory mapping if a mapping exists ??
//     // All threads run in the same address space, so what could that be good
//     // for? 
//     char * addr = (char *) curr_it->second.addr();
//     if (addr == NULL) 
//       addr = (char *) (curr_it->second).map_view();
//     return addr;
//   }

  void unloadChunk(const void *mem_loc); // {
//     map_it curr_it 
//       = find_if(_mappers.begin(), _mappers.end(), hasAddress(mem_loc));
//     if (curr_it != _mappers.end())
//       (curr_it->second).unmap_view();
//   }
};
#endif
