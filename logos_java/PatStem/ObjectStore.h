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
/*******************************************************************
 *
 *    DESCRIPTION: 	Manages objects by associating them with pseudo handles
 *
 *    AUTHOR:		   Vlad Yakimetz
 *
 *    HISTORY:    	Created 05/08/98
 *
 *******************************************************************/

#ifndef _OBJECTSTORE_H_
#define _OBJECTSTORE_H_


#include <logos_include/lgscontainers.h>


///////////////////////////////////////////////////////////////////////////////
// class ObjectStore definition
//

template <class Handle, class Object, class Param>
class ObjectStore
{  
   typedef map<Handle, Object*, less<Handle> > object_store;
   typedef stack<Handle> handle_stack;
   typedef typename map<Handle, Object*, less<Handle> >::iterator iterator;
   typedef typename map<Handle, Object*, less<Handle> >::const_iterator const_iterator;

public:
   ObjectStore();
   ~ObjectStore();

   Handle create(const Param& param_);
   bool isValid(Handle handle_) const;
   Object& fromHandle(Handle handle_) const;
   void erase(Handle handle_);

private:
   object_store     _objStore;
   handle_stack     _handleStack;
   
   Handle _nextHandle();

};



///////////////////////////////////////////////////////////////////////////////
// class ObjectStore implementation
//

template <class Handle, class Object, class Param>
ObjectStore<Handle, Object, Param>::ObjectStore()
{
    
}


template <class Handle, class Object, class Param>
ObjectStore<Handle, Object, Param>::~ObjectStore()
{
   iterator it;
   
   for (it = _objStore.begin(); it != _objStore.end();)
   {
      delete (*it).second;      // delete object
   }
   _objStore.clear();

   while (!_handleStack.empty())
   {
      _handleStack.pop();
   }
}


template <class Handle, class Object, class Param>
Handle ObjectStore<Handle, Object, Param>
::create(const Param& param_)
{
   Handle handle = _nextHandle();

   try
   {
      _objStore[handle] = new Object(param_);
   }
   catch(bad_alloc)
   {
      handle = 0;
   }

   return handle;
}


template <class Handle, class Object, class Param>
bool ObjectStore<Handle, Object, Param>
::isValid(Handle handle_) const
{
   return handle_ <= 0 ||
      _objStore.find(handle_) == _objStore.end() ? false : true;
}


template <class Handle, class Object, class Param>
Object& ObjectStore<Handle, Object, Param>
::fromHandle(Handle handle_) const
{
   const_iterator it = _objStore.find(handle_);

   if (handle_ <= 0 || it == _objStore.end())
   {
      throw "invalid handle";
   }

   return *(*it).second;
}


template <class Handle, class Object, class Param>
void ObjectStore<Handle, Object, Param>
::erase(Handle handle_)
{
   iterator it = _objStore.find(handle_);

   if (handle_ > 0 && it != _objStore.end())
   {
      delete (*it).second;             // close connection point
      _handleStack.push((*it).first);  // push freed handle on stack
      _objStore.erase(it);
   }
}


template <class Handle, class Object, class Param>
Handle ObjectStore<Handle, Object, Param>
::_nextHandle()
{
   // returned connection handle is always >= 1
   static Handle handleCount = 0;
   Handle handle;

   if (_handleStack.empty())
   {
      handle = ++handleCount;
   }
   else
   {
      handle = _handleStack.top();
      _handleStack.pop();
   }

   return handle;
}


#endif //_OBJECTSTORE_H_
