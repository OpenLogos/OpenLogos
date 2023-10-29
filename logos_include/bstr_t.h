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

#ifndef _BSTR_T_H
#define _BSTR_T_H

#include <string>
#include <cstdlib>

using namespace std;

#define BSTR wchar_t *

class _bstr_t {
private:

  class bstr_refcount {
    friend class _bstr_t;
    BSTR _cont;
    unsigned int _refcount;
    
    ~bstr_refcount() {
    }
    
  public:

    // Create new refcount object with copied string (newly allocated)
    bstr_refcount(BSTR &s, bool fCopy = true) {
      if (fCopy) {
        _cont = new wchar_t[char_traits<wchar_t>::length(s) + 1];
        char_traits<wchar_t>::copy(_cont, s, 
                                   char_traits<wchar_t>::length(s) + 1);
      } else {
        _cont = s;
      }
      _refcount = 1;
    }


    // Create new refcount object with copied string (newly allocated)
    bstr_refcount(const BSTR &s) {
      _cont = new wchar_t[char_traits<wchar_t>::length(s) + 1];
      char_traits<wchar_t>::copy(_cont, s, 
                                 char_traits<wchar_t>::length(s) + 1);
      _refcount = 1;
    }

    // Create new refcount object with copied string (newly allocated)
    // _fix_me_ check that this is doing the correct conversion , because maybe
    // the unicode conversion is missing 
    bstr_refcount(const char *s) {
      _cont = new wchar_t[char_traits<char>::length(s) + 1];
      mbstowcs(_cont, s, (char_traits<char>::length(s) + 1) * sizeof(wchar_t));
      _refcount = 1;
    }

    bstr_refcount *assign() {
      _refcount++;
      return this;
    }

    void decr() {
      if (--_refcount == 0) {
        delete[] _cont;
        delete this;
      }
    }

    BSTR detach() {
      --_refcount;
      return _cont;
    }

    const BSTR GetBSTR() const { return _cont ; }
  };

  bstr_refcount *_cont;
public:
  _bstr_t( ) throw( ) { _cont = NULL; }
  /** Shallow copy: use same BSTR object and increase refcount */
  _bstr_t(const _bstr_t& s1) throw( ) {
    _cont = s1._cont->assign();
  }

  /** \param s2 a multibyte string: do multibyte to Unicode conversion and
      alloc new string with new BSTR object to encapsulate it */
  _bstr_t(const char* s2) {
    _cont = new bstr_refcount(s2);
  }

  /** \param s3 a Unicode string: alloc new string with new BSTR object to
      encapsulate it */
  _bstr_t(const wchar_t* s3) {
    _cont = new bstr_refcount(s3);
  }

  //_bstr_t(const _variant_t& var);

  /** Create new _bstr_t object from given BSTR object by either copying or
   *  simply referring to it
   * \param bstr the object to copy from or refer to
   * \param fCopy if true, copy the BSTR object and its underlying string,
   *        otherwise refer to it and increase its refcount.
   * \attn if fCopy is \c false, we can not implement this correctly if the
   *       BSTR comes from another _bstr_t. The programmer has to take care
   *       of that, but hopefully this is intended (it's not clear at MSDN)
   */
  _bstr_t(BSTR bstr, bool fCopy) {
    _cont = new bstr_refcount(bstr, fCopy);
  }

  BSTR Detach() {
    BSTR res = _cont->detach();
    if (0 == _cont->_refcount) {
      delete _cont;
      _cont == NULL;
    }
  }

  ~_bstr_t() { 
    _cont->decr();
  }

  /** Return the encapsulated BSTR object */
  const BSTR GetBSTR() { return _cont->GetBSTR(); }

  operator const wchar_t*( ) const throw( ) {
    return _cont->GetBSTR();
  }

  //operator wchar_t*( ) const throw( );
  // operator const char*( ) const;
  //operator char*( ) const;

  size_t length() { return char_traits<wchar_t>::length(_cont->GetBSTR()); }

  BSTR copy(bool fCopy = true) {
    if(fCopy) {
      BSTR res = 
        new wchar_t[char_traits<wchar_t>::length(_cont->GetBSTR()) + 1];
      char_traits<wchar_t>::
        copy(res, _cont->GetBSTR(),
             char_traits<wchar_t>::length(_cont->GetBSTR()) + 1);
      return res;
    } else {
      return const_cast<BSTR>(_cont->GetBSTR());
    }
  }
};


#endif
