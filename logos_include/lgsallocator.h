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
// LGSALLOCATOR internal header (from <memory>)

#if     _MSC_VER > 1000 /*IFSTRIP=IGN*/
#pragma once
#endif

#ifndef _LGSALLOCATOR_
#define _LGSALLOCATOR_

#ifdef  _MSC_VER
#pragma pack(push,8)
#endif  /* _MSC_VER */
// #include <utility>

#ifndef _DESTRUCTOR
#define _DESTRUCTOR(__Tp, __p) (__p->~__Tp())
#endif

#ifndef _FARQ	/* specify standard memory model */
 #define _FARQ
 #define _PDFT	ptrdiff_t
 #define _SIZT	size_t
#endif
 #define _POINTER_X(T, A)	T _FARQ *
 #define _REFERENCE_X(T, A)	T _FARQ &
		// TEMPLATE FUNCTION LgsAllocate
template<class _Ty> inline
	_Ty _FARQ *LgsAllocate(_PDFT __N, _Ty _FARQ *)
	{if (__N < 0)
		__N = 0;
	return ((_Ty _FARQ *)operator new(
		(_SIZT)__N * sizeof (_Ty))); }
		// TEMPLATE FUNCTION LgsConstruct
template<class _T1, class _T2> inline
	void LgsConstruct(_T1 _FARQ *__P, const _T2& _V)
	{new ((void _FARQ *)__P) _T1(_V); }
		// TEMPLATE FUNCTION LgsDestroy
template<class _Ty> inline
	void LgsDestroy(_Ty _FARQ *__P)
	{_DESTRUCTOR(_Ty, __P); }
inline void LgsDestroy(char _FARQ *__P)
	{}
inline void LgsDestroy(wchar_t _FARQ *__P)
	{}

		// TEMPLATE CLASS LgsAllocator
template<class _Ty> class LgsAllocator;

template<> class LgsAllocator<void> {
public:
	typedef void _FARQ *pointer;
	typedef const void _FARQ *const_pointer;
        // reference to void members are impossible
	typedef void value_type;
        template <class U> struct rebind { typedef LgsAllocator <U> other; };
};

template<class _Ty>
	class LgsAllocator {
public:
	typedef _SIZT size_type;
	typedef _PDFT difference_type;
	typedef _Ty _FARQ *pointer;
	typedef const _Ty _FARQ *const_pointer;
	typedef _Ty _FARQ& reference;
	typedef const _Ty _FARQ& const_reference;
	typedef _Ty value_type;
        template <class U> struct rebind { typedef LgsAllocator <U> other; };

        LgsAllocator() throw() {}
        LgsAllocator(const LgsAllocator&) throw() {}
        template<typename _Tp1>
          LgsAllocator(const LgsAllocator<_Tp1>&) throw() {}
        ~LgsAllocator() throw() {}

	pointer address(reference __X) const
		{return (&__X); }
	const_pointer address(const_reference __X) const
		{return (&__X); }
	pointer allocate(size_type __N,
                         typename LgsAllocator< void >::const_pointer hint = 0)
		{return (LgsAllocate((difference_type)__N, (pointer)0)); }
	char _FARQ *_Charalloc(size_type __N)
		{return (LgsAllocate((difference_type)__N,
			(char _FARQ *)0)); }
	void deallocate(void _FARQ *__P, size_type)
		{operator delete(__P); }
	void construct(pointer __P, const _Ty& _V)
		{LgsConstruct(__P, _V); }
	void destroy(pointer __P)
		{LgsDestroy(__P); }
	_SIZT max_size() const
		{_SIZT __N = (_SIZT)(-1) / sizeof (_Ty);
		return (0 < __N ? __N : 1); }
	};
template<class _Ty, class __U> inline
	bool operator==(const LgsAllocator<_Ty>&, const LgsAllocator<__U>&)
	{return (true); }
template<class _Ty, class __U> inline
	bool operator!=(const LgsAllocator<_Ty>&, const LgsAllocator<__U>&)
	{return (false); }
#ifdef  _MSC_VER
#pragma pack(pop)
#endif  /* _MSC_VER */

#endif /* _LGSALLOCATOR_ */

/*
 * Copyright (c) 1995 by P.J. Plauger.  ALL RIGHTS RESERVED. 
 * Consult your license regarding permissions and restrictions.
 */

/*
 * This file is derived from software bearing the following
 * restrictions:
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this
 * software and its documentation for any purpose is hereby
 * granted without fee, provided that the above copyright notice
 * appear in all copies and that both that copyright notice and
 * this permission notice appear in supporting documentation.
 * Hewlett-Packard Company makes no representations about the
 * suitability of this software for any purpose. It is provided
 * "as is" without express or implied warranty.
 */
