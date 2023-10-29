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
/* fcrt.h - definitions to support FORTRAN translations to C
 *               made by FOR_C (TM) from COBALT BLUE 
 *             copyright Cobalt Blue, Inc., 1987-1996
 *                      ALL RIGHTS RESERVED
 */

#ifndef FCRT_INCLUDED
#  define FCRT_INCLUDED

/* MACRO SWITCHES,

	INLINE Code,

	Inline code makes use of macros where F77 employed a function call,
	and is faster!  By default, these macro switches are UNdefined to
	duplicate the F77 behavior.  Function calls are NOT normally needed
	for these functions, UNLESS they are passed as arguments to other
	functions; and in the case of the max/min/posdif functions, the 
	arguments contain potential side effects (e.g., other function calls,
	or uses of the C "++" or "--" operators, etc.).  In all cases, if 
	you decide to switch to macro definitions of these functions, beware
	of name conflicts, i.e., a variable name which is also the same as
	a macro name.  The C compiler can resolve differences between names
	and function calls, however the C preprocessor can not, and will 
	produce an error if a macro defined with arguments is ever used without
	any.

	We recommend that you define INLN_DPROD, INLN_MOD, INLN_SIGN, & INLN_TRUNC
	if the functions in these groups are NOT passed as arguments to other 
	functions in your code.  Close scrutiny of your code is recommended
	before defining INLN_MXMN or INLN_POSDIF. 

      Switch        Macros defined                 Notes
      -----------   -------------------            --------------------
      INLN_DPROD    dprod()
      INLN_MOD      mod(), smod(), sabs()
      INLN_SIGN     isign(), sign(), ssign()
      INLN_TRUNC    fnint(),nint(),snint(),trunc()
      INLN_MXMN     fmax(),max(),smax()            BEWARE SIDE EFFECTS!
                    fmin(),min(),smin()
      INLN_POSDIF   iposdif(),posdif(),sposdif()   BEWARE SIDE EFFECTS!


	Long double type,

	Define LONG_DBL as 1 if your C compiler supports this new ANSI type.  
	If your C doesn't support it AND it shows up in the translated code
	(from the REAL*16 translation), then the default typedef'ing of
	longdbl as double will allow you to compile your program - at the
	expense of reduced accuracy.

	   NOTE: "long double" is supported by MS C, Turbo C, and HIGH C,
		among others on MSDOS.  HIGH C and MS C v6.0 use 10 bytes for
		long double, others often simply consider long double as double.


	Prototypes,

	The macro switch:		PROTOTYPES
	is normally defined in this file.  It simply enables prototype
	definitions of the various runtime functions.  All of the
	prototypes are defined in abstract form for compatiblity with older
	compilers.  Function prototypes merely provide compile time error
	checking of the function interfaces.  IF your target C compiler
	doesn't support prototypes, simply UNdefine the macro definition
	immediately below.  This simply declares the function return type.

	NOTE: The macro __STDC__ is automatically defined w/ANSI C 
		compliant compilers.
 */

	/* Define MACRO SWITCHES HERE! */

#ifdef __alpha
	/*DEC ALPHA*/
#	if defined(unix)
		/*ALPHA DEC UNIX cc must be compiled with the -std1 switch
		 * for ANSI C behavior and to define the __STDC__ macro;
		 *64bit C compiler; by default, char decls are signed 
		 *in addition, the cc options "-assume noaligned_objects" and 
		 * "-nomember_alignment" allow byte and word boundary alignment,
		 * vs. the default alignment to long words
		 */
#		define alphaunix 1
#		ifndef __STDC__
			? error, Is the -std1 option on ?
#		endif
#	elif defined(vms)
		/*ALPHA VMS C*/
#		define alphavms 1
#	else
		? Alpha Windows NT ? - not supported at present
#	endif
#endif

#ifdef __clix__
# define risc_cpu 1
# ifndef unix
#	define unix 1
# endif
#endif

#ifdef CRAY
# define risc_cpu 1 /*at least for unfio alignment purposes*/
# ifndef unix
#	define unix 1
# endif
#endif

#ifdef hpuxpa
# define risc_cpu 1
# ifndef unix
#	define unix 1
# endif
#endif

#ifdef __sgi
# define risc_cpu 1
# ifndef unix
#	define unix 1
# endif
#endif

#ifdef sparc
# define risc_cpu 1
# ifndef unix
#	define unix 1
# endif
#endif

#ifdef sun
# ifndef unix
#	define unix 1
# endif
#endif

#ifdef m88k
# define risc_cpu 1
# ifndef unix
#	define unix 1
# endif
#endif

#ifdef VAX
# ifndef vms
#	define vms	1
# endif
#endif

#ifdef _IBMR2
# define risc_cpu 1
# ifndef unix
#	define unix 1
# endif
#endif

#ifndef ANSI_C_TARGET
#	define ANSI_C_TARGET 0
  /*NOTE: This macro is defined as 1 (immediately prior to the
   *      include for "fcrt.h") when the CT=a option is used
   *      when translating.                added 1/26/95, cwl
   */
#endif

	/* Enable PROTOTYPE definitions in the FOR_C Runtime */
#if defined(__STDC__) || ANSI_C_TARGET
#  define PROTOTYPES
#elif defined(_IBMR2)
#  define PROTOTYPES
#elif defined(sun) || defined(unix)
  /* NOTE: prototypes aren't supported for UNIX in general
   * IBM RS6000, SCO UNIX are exceptions
   */
#else /* UNKNOWN target - NO prototypes */
#endif

/*
 *If you will be translating DG F77, then build the runtime library
 * with this macro defined, to enable unique DG features.
#define DG_F77
 *If you will be translating DG FORTRAN-5, then build the runtime 
 * library with this macro defined, to enable unique DG features.
#define DG_FORTRAN5
 */
	/* end of MACRO SWITCH settings */

#if defined(__STDC__) || ANSI_C_TARGET
  /*no action required*/
#else
#    ifndef const
#      define const
#    endif
#endif

#ifdef PROTOTYPES
#  define ALTRETN 	short*
#  define INTRNS
	/*INTRNS is defined as nothing for use in FOR_C generated prototype files*/
   typedef void*	structp;	/* structure ptr */
#endif

#define TRUE	1
#define FALSE	0
#define _ERR_	-1
#define _NUL_ 	NULL  /* missing arg. transl. (VAX defines as a NULL) */
  /*NOTE: other F77 extensions may define missing args as pointers to 0*/
#define to_log(x)	!!(x)

#define BYTE	byte	/* for compatibility w/earlier versions */
typedef char	byte;
typedef char	LOGICAL;
typedef char	LOGICAL8;
typedef short 	LOGICAL16;
typedef long	LOGICAL32;
#ifdef longdbl
#  undef longdbl
#endif
#ifdef LONG_DBL
	typedef long double longdbl;
#else
	typedef double longdbl;
#endif	/* LONG_DBL */

/* Cray support (only accurate on Cray) */
typedef long dbl_long;
typedef long LOGICAL64;

#ifdef DG_FORTRAN5
   /*get the value a byte offset given an address*/
#  define BYTVAL(a,n) *( (char*)a + n-1 )
#endif

			/*		*		*		*		*/

				/* GENERAL RUNTIME EXTERNALS */

#ifndef EXTERN
	/*  Definition of EXTERN generated w/com=e option.
	 *  NOTE: EXTERN must be defined as nothing once for each common   
	 *    translation in your code (typically prior to the common 
	 *    translation in main), but possibly elsewhere.  This will 
	 *    need to be done manually!
	 *    See the EXTERNL logic below and in f77ini() for examples.
	 */
#  define EXTERN extern
#endif

#ifndef EXTERNL
#  define EXTERNL extern
	/* NOTE: EXTERNL is defined as nothing in f77ini.c to define the
			the EXTERNLs in that file.  This K&R approach avoids problems
			with certain older Librarians in use. */
	/* NOTE: WITH ANSI C Compiler's, simply remove "extern" from the above
			to simplify maintenance. */
#endif	/* EXTERNL */

	/* EXTERNLs for use in macros TO AVOID side effects */
EXTERNL char	*f_s_, f_c_;
EXTERNL short 	f_si_;
EXTERNL long 	f_l_;
EXTERNL double	f_d_;
	/* NOTE: These EXTERNLs are defined once in f77ini.c to avoid conflict
		with certain older librarians. */

#ifdef DEBUG_FUN
	/* Support for C Debug code, (CD option) */
	EXTERNL FILE *debug_fp=stderr;	/* file ptr for optional C Debug code */
#	define INI_DEBUG_FP	/* default: use stderr */
	/* NOTE: INI_DEBUG_FP can be defined in terms of an fopen() with a
	 *       special file if you desire.  For example,
	 *#define INI_DEBUG_FP if((debug_fp=fopen("debug.out","w"))==NULL)exit(1);
	 */
#	define DEBUG_ARGS /*default output args w/debug - change if you wish*/
#endif
			/*		*		*		*		*/

/*PASS BY ADDRESS MACRO,
 * macro to assign an expr, x, to a temp variable, t, and then return 
 * the address of the temporary for passing as an argument
 */
#define ADR(t,x)	( t=(x), &t )
/*CONVERT CHARACTER LITERAL TO SINGLE CHAR STRING,
 * assign int to 1st char in 2 byte LgsString temp, terminate LgsString,
 * and then return the address of the LgsString temp
 */
#define STR1(t,c)	( (t[0]=c,t[1]=0), t )

/*VAX %DESCR() translation support:
 * defines an equivalent structure to the VAX dsc$descriptor structure,
 * and defines several macros that will initialize a given vaxdescr
 * structure with the required argument type, returning the address
 * of the descriptor structure.  See section 13.2.3 of the VAX C
 * manual, published in Feb., 1989, for VAX C v3.0.
 *12/93, If on a VAX VMS system using DEC C, then define DSC_DESCRIP
 * to use the VMS "descrip.h" definitions of the structure, class,
 * and types.  Note: you may have to specify the location of the 
 * descrip.h file to the DEC C compiler.
 */ 
#ifdef DSC_DESCRIP
   /*VMS environment*/
#  include <descrip.h>
   /*make vaxdescr effectively an alias for dsc$descriptor*/
   typedef struct dsc$descriptor vaxdescr;

   /*data classes (from table 13-4 of the VAX C manual)*/
   /*(defined in terms of the VAX VMS definitions)*/
#  define DCLASS_S DSC$K_CLASS_S  /*scalar, LgsString*/
#  define DCLASS_A DSC$K_CLASS_A  /*array*/

   /*data types (from table 13-5 of the VAX C manual)*/
   /*(defined in terms of the VAX VMS definitions)*/
#  define DTYPE_B DSC$K_DTYPE_B /*byte (signed)*/
#  define DTYPE_W DSC$K_DTYPE_W /*word integer (signed)*/
#  define DTYPE_L DSC$K_DTYPE_L /*longword integer (signed)*/
#  define DTYPE_F DSC$K_DTYPE_F /*real/float*/
#  define DTYPE_D DSC$K_DTYPE_D /*double*/
#  define DTYPE_T DSC$K_DTYPE_T /*ascii LgsString*/

#  define DESCRS(t,s) /*returns addr of scalar LgsString descriptor*/ \
     (t.dsc$b_dtype=DTYPE_T,t.dsc$b_class=DCLASS_S,t.dsc$a_pointer=s, \
      t.dsc$w_length=strlen(t.dptr),&t)

#  define DESCRB(t,a) /*returns addr of byte descriptor*/ \
     (t.dsc$b_dtype=DTYPE_B,t.dsc$b_class=DCLASS_S,t.dsc$a_pointer=(char*)a,\
      t.dsc$w_length=1,&t)

#  define DESCRI(t,a) /*returns addr of integer descriptor*/ \
     ((t.dsc$b_dtype=DTYPE_L,t.dsc$b_class=DCLASS_S,t.dsc$a_pointer=(char*)a,\
      t.dsc$w_length=4),&t)
#  define DESCRL(t,a) /*returns addr of logical descriptor*/ \
     DESCRI(t,a)  /*(defined in terms of integer descriptor)*/

#  define DESCRR(t,a) /*returns addr of real/float descriptor*/ \
     (t.dsc$b_dtype=DTYPE_F,t.dsc$b_class=DCLASS_S,t.dsc$a_pointer=(char*)a,\
      t.dsc$w_length=4,&t)

#  define DESCRD(t,a) /*returns addr of double precision descriptor*/ \
     (t.dsc$b_dtype=DTYPE_D,t.dsc$b_class=DCLASS_S,t.dsc$a_pointer=(char*)a,\
      t.dsc$w_length=8,&t)
#else /*non-VMS environment*/
   typedef struct {
   	unsigned short dlen; /*data length*/
   	char          dtype, /*data type code*/
   	             dclass, /*descriptor class code*/
   	              *dptr; /*data address*/
   	} vaxdescr;

   /*data classes (from table 13-4 of the VAX C manual)*/
#  define DCLASS_S 1 /*scalar, LgsString*/
#  define DCLASS_A 4 /*array*/

   /*data types (from table 13-5 of the VAX C manual)*/
#  define DTYPE_B 6 /*byte (signed)*/
#  define DTYPE_W 7 /*word integer (signed)*/
#  define DTYPE_L 8 /*longword integer (signed)*/
#  define DTYPE_F 10 /*real/float*/
#  define DTYPE_D 11 /*double*/
#  define DTYPE_T 14 /*ascii LgsString*/

#  define DESCRS(t,s) /*returns addr of scalar LgsString descriptor*/ \
     (t.dtype=DTYPE_T,t.dclass=DCLASS_S,t.dptr=s, t.dlen=strlen(t.dptr),&t)

#  define DESCRB(t,a) /*returns addr of byte descriptor*/ \
     (t.dtype=DTYPE_B,t.dclass=DCLASS_S,t.dptr=(char*)a, t.dlen=1,&t)

#  define DESCRI(t,a) /*returns addr of integer descriptor*/ \
     ((t.dtype=DTYPE_L,t.dclass=DCLASS_S,t.dptr=(char*)a, t.dlen=4),&t)
#  define DESCRL(t,a) /*returns addr of logical descriptor*/ \
     DESCRI(t,a)  /*(defined in terms of integer descriptor)*/

#  define DESCRR(t,a) /*returns addr of real/float descriptor*/ \
     (t.dtype=DTYPE_F,t.dclass=DCLASS_S,t.dptr=(char*)a, t.dlen=4,&t)

#  define DESCRD(t,a) /*returns addr of double precision descriptor*/ \
     (t.dtype=DTYPE_D,t.dclass=DCLASS_S,t.dptr=(char*)a, t.dlen=8,&t)

#ifdef READY
#  define DESCRQ(t,a) /*returns addr of quad precision descriptor*/ \
    (t.dtype=DTYPE_?,t.dclass=DCLASS_S,t.dptr=(char*)a, t.dlen=sizeof(longdbl),&t)
#  define DESCRZ(t,a) /*returns addr of complex descriptor*/ \
    (t.dtype=DTYPE_?,t.dclass=DCLASS_S,t.dptr=(char*)a, t.dlen=8,&t)
#endif
#endif
/*end of VAX %DESCR() support*/

/*repeated data initialization*/
/*if RC_INI(rs) could be expressed as a funct it would be written,
 * if( rs[_r].rc > 1 ){
 * 	rs[_r].rc--;	return( rs[_r].ini );	}	<decr the rept cnt>
 * else if( rs[_r].rc == 1 ){
 * 	_r++;	return( rs[_r-1].ini );	}			<incr the index>
 * else
 * 	return( 0 );			<end of the struct, retn 0 & don't advance>
 * 	NOTE: a neg. repeat count is considered the end of the struct
 *NOTE: since the repeat count in the struct is decremented, the initialization
 * 	with the struct is a ONE SHOT init., since the repeat count info is
 * 	lost!
 */
#define RC_INI(rs)	(rs[_r].rc>1 ? DEC_RC_(rs) : (rs[_r].rc==1 ? INC_NDX_(rs) : rs[_r].ini ))
#define  DEC_RC_(rs)	(rs[_r].rc--,rs[_r].ini)
#define  INC_NDX_(rs)	(_r++,rs[_r-1].ini)

	/*F77 Char Translation Support*/
#define CHRFUNC_SIZ	121	/*size of the char array associated w/a char funct*/
#define CHRFUNC_LEN	(CHRFUNC_SIZ-1)
#ifdef PROTOTYPES
#  define CHAR_INT   	char*,int
#  define CHAR_UNSGN 	char*,unsigned
	/* NOTE: CHAR_UNSGN is provided for compatibility w/prior versions */
#endif
#define ntstr	ntS  /*ntstr() renamed to ntS(), 1/31/96*/
/* typedef char* STRING; */

typedef struct {
	int siz; /*current size assoc. w/LgsString 's'*/
	char *s; /*ptr to LgsString space*/
	} CHRTMP;

	/*support for DO and arithmetic IF translations*/
/*formula:  DOCNT(ini,tst,inc) = max( (long)((tst-ini+inc)/inc), 0 )*/
#define DOCNT(i,t,n)	(_d_l=(n), (_d_m=(t-(i)+_d_l)/_d_l) > 0 ? _d_m : 0L )
 /*NOTE: _d_l & _d_m are local dbl & long defined whenever DOCNT() is gen'd*/

#define _ZERO_	1.0e-14 /*effective fp dbl prec zero*/
#define _SZERO_	1.0e-5 /*effective fp sgl prec zero*/
#define IS_FPZERO(f) ((f < _ZERO_ && f > -_ZERO_) ? YES : NO )

 /*NOTE: ARITHIF() now defines 0 as +-epsilon to allow for fp 0 differences*/
#define ARITHIF(x) (f_d_=(x),(f_d_ > _ZERO_ ? 1 :(f_d_ < -_ZERO_ ? -1:0)))
#define IARITHIF(x) (f_l_=(x),(f_l_ > 0 ? 1 : (f_l_ < 0 ? -1 : 0)) )
#define SARITHIF(x) (f_d_=(x),(f_d_ > _SZERO_ ? 1 :(f_d_ < -_SZERO_ ? -1:0)))

	/*support for pause transls.*/
EXTERNL int pause_chr;	/*the char returned from the PAUSE*/

#define _ABS_(v)	( (v) < 0 ? -(v) : (v) )
#define TOLOWER(c)	((f_c_=(c))>='A' && f_c_<='Z' ? (f_c_+'a'-'A') : f_c_ )
#define TOUPPER(c)	((f_c_=(c))>='a' && f_c_<='z' ? (f_c_-'a'+'A') : f_c_ )
#define ISBINARY(c) ((c)=='0' || (c)=='1')
#define ISOCTAL(c) ((c) >= '0' && (c) <= '7')

	/*misc optimization macros*/
#define SQ(x)	((x)*(x))
#define CUBE(x)	((x)*(x)*(x))

	/* Functions Introduced by the FOR_C Translations */

#ifdef PROTOTYPES
		/* character transl. support */
	/* concat VARIABLE no. of strs, retn temp sp addr */
	char *f_concat( CHRTMP*, ... );	/* concat strs to temp, & retn addr */
	int f_strcmp(char*,char*);	/* F77 str comparison */
	                     /* retns <0 if l < r, 0 if equal, or >0 if l > r */
	char *f_strncpy( char*,char*,int );	/* F77 str assignment transl. */
	char *f_subscpy(char*,int,int,int,char*);/*F77 substr assgn*/
	void ini_chrtmp(CHRTMP*,int);	/* initialize the chrtmp array */
	int istrstr(char*,char*);	/* return int location of substr in str */
	long lstrstr(char*,char*);	/* return long int location of substr in str */
	void nulltermsa(char*,int,int);	/* null term. an array of strings */
	char *ntS(void*,long);	/* null term. any object, retn str ptr */
	void free_nts(void);  /*free up the nts[] array, normally on exit*/
	void rel_chrtmp(CHRTMP*,int);	/* release space assoc. w/chrtmp array */
	char *strcate(char*,char*); /* strcat(), but retn the new END of 'to' */
        //char *strlcpy(char*, const char*, unsigned);
	char *strpad(char*, int, unsigned);	/* pad LgsString s w/char c, up to n */
     int strcmpip(char const*,char const*);	/*padded str compare ignoring case*/
	char *vcpyncat(char*,int,...); /*copy & concat to target, ltd by len*/
		/* misc. */
	void blk_data(void);	/* block data transl. */
	void blkdata0(void), blkdata1(void), blkdata2(void),
		blkdata3(void), blkdata4(void);
	void f77ini(int,char*[]);
	void f_err(char*);		/* output F77 Runtime error msg & quit */
	void f_warn(char*);		/* output F77 Runtime warning msg & return */
	void memerr(char*); /*output memory error & quit*/
	double powi( double, long int );	/* float to integer power */
	long int ipow( long, long );
	short sipow( short, short );
	char *inschr(int, char*);  /* insert char 'c' in front of 'str' */
	void ck_allocs(void**,unsigned,char*);	/* ck for successful malloc's */
	void rel_allocs(void**,unsigned);	/* free the malloc'd object list */
#else
	char *f_concat(), *f_strncpy(), *f_subscpy();
	void nulltermsa();
	char *ntS();
	void free_nts();
	char *strcate(), *strlcpy(), *strpad();
	int strcmpip();
	int f_strcmp(); 
	int istrstr();
	long lstrstr();
	void ini_chrtmp(), rel_chrtmp(); 
	void blk_data(), f77ini(), f_err(), f_warn(), memerr();
	void blkdata0(), blkdata1(), blkdata2(), blkdata3(), blkdata4();
	double powi();
	long ipow();
	short sipow();
	char *inschr();
	void ck_allocs(), rel_allocs();
	char *vcpyncat();
#endif	/* PROTOTYPES */

/* rename old name of f77_ini() to new name */
#define f77_ini	f77ini

	/* 
		Support for the Std F77 Library Translations to C
	 */

/* double precision product of 2 'single' precision numbers */
#ifdef PROTOTYPES
	double dprod(double,double);
#else
	double dprod();
#endif

#ifdef INLN_DPROD
#	define dprod(a,b)	((double)(a)*(b))
#endif

/* NOTE: Even though the mod() function can be represented by a '%' 
	operation in C, standard F77 defines them as functions, hence
	they are provided as such in case the program depends on them
	as such.
	The same can be said for several other F77 intrinsic functions.
 */ 
#ifdef PROTOTYPES
	short sabs(short);
	short smod(short,short);
	long mod(long,long);
	long labs_(long*);
	short sabs_(short*);
	short smod_(short*,short*);
	long mod_(long*,long*);
#else	/* no prototypes */
#  if	sun
	  long labs();
#  endif
	short sabs();
	short smod();
	long mod();
	long labs_();
	short sabs_();
	short smod_();
	long mod_();
#endif	/* PROTOTYPES */

#ifdef INLN_MOD
#  define sabs(a) 	((f_si_=(a)) < 0 ? -f_si_ : f_si_)
#  define smod(a,b)	mod(a,b)
#  define mod(a,b)	((a) % (b))
#endif	/* INLN_MOD */

/* insure no max(), min() conflicts exist! */
#ifdef max
#  undef max
#endif
#ifdef min
#  undef min
#endif

#define 	MAX(a,b)	( (a) > (b) ? (a) : (b) )
#define 	MIN(a,b)	( (a) < (b) ? (a) : (b) )

#ifdef PROTOTYPES
	long max(long,long), min(long,long);
	short smax(short,short), smin(short,short);
	double fmax(double,double), fmin(double,double);
#else	/* no prototypes */
	long max(), min();
	short smax(), smin();
	double fmax(), fmin();
#endif	/* PROTOTYPES */

#define fmaxi(a,b)	(double) max(a,b)	/* fp MAX of longs (amax0()) */
#define fmaxs(a,b)	(double) smax(a,b)	/* fp MAX of shorts */
#define maxfi(a,b)	(long) fmax(a,b)	/* long MAX of fps (max1()) */
#define maxfs(a,b)	(short) fmax(a,b)	/* short MAX of fps */
#define fmini(a,b)	(double) min(a,b)	/* fp MIN of longs (amin0()) */
#define fmins(a,b)	(double) smin(a,b)	/* fp MIN of shorts */
#define minfi(a,b)	(long) fmin(a,b)	/* long MIN of fps (min1()) */
#define minfs(a,b)	(short) fmin(a,b)	/* short MIN of fps */

#ifdef PROTOTYPES
	/* Variable no. of arg definitions for max & min functs */
	double vfmax( double,... );	/* FLOATING POINT MAXIMUM */
	long vmax( long, ... ); 	/* LONG INTEGER MAXIMUM */
	short vsmax(short,...);		/* short INTEGER MAXIMUM */
	double vfmin( double,... );	/* FLOATING POINT MINIMUM */
	long vmin( long, ... ); 	/* LONG INTEGER MINIMUM */
	short vsmin(short,...);		/* short INTEGER MINIMUM */
#else
	double vfmax(), vfmin();
	long vmax(), vmin();
	short vsmax(), vsmin();
#endif	/* PROTOTYPES */

#ifdef INLN_MXMN
	/* NOTE: 2 arg versions of the basic max & min functions are provided 
		as macros because of their frequent usage.
		BEWARE OF ARGUMENT SIDE EFFECTS! ! ! */
#  define 	max(a,b)	(long)( (a) > (b) ? (a) : (b) )
#  define 	smax(a,b)	(short)( (a) > (b) ? (a) : (b) )
#  define 	fmax(a,b)	(double)( (a) > (b) ? (a) : (b) )
#  define 	min(a,b)	(long)( (a) < (b) ? (a) : (b) )
#  define 	smin(a,b)	(short)( (a) < (b) ? (a) : (b) )
#  define 	fmin(a,b)	(double)( (a) < (b) ? (a) : (b) )
#endif	/* INLN_MXMN */

	/* Variable arg definitions of max & min functs w/type conv
		in terms of the three basic function types of each */
#define vfmaxi	(double)vmax
#define vfmaxs	(double)vsmax
#define vmaxfi	(long)vfmax
#define vmaxfs	(short)vfmax
#define vfmini	(double)vmin
#define vfmins	(double)vsmin
#define vminfi	(long)vfmin
#define vminfs	(short)vfmin

	/* Variable argument list terminators (used by max & min functs) */
#ifndef INT_MAX
#  include <limits.h>
#endif	/* INT_MAX */
#define SEND	(SHRT_MAX-1)
#define IEND	(LONG_MAX-1)
#define FEND	(1.e+38-1.)


	/* F77 std character intrinsic functions */
#define lge(a,b)	(f_strcmp(a,b) >= 0)
#define lgt(a,b)	(f_strcmp(a,b) >  0)
#define lle(a,b)	(f_strcmp(a,b) <= 0)
#define llt(a,b)	(f_strcmp(a,b) <  0)
	/* NOTE: f_strcmp() ASSUMES ASCII (or similar char set) */
/*#define ichar(str)	(int)(f_s_=(str),*f_s_)*/
#define ichar(str)	(int)(*(str))
#ifdef PROTOTYPES
	char *itochr(long);  /*convert int to char str, retn ptr to str*/
	char *sitochr(short); /*convert short int to char str, retn ptr to str*/
#else
	char *itochr(), *sitochr();
#endif	/* PROTOTYPES */

#ifdef PROTOTYPES
	long iposdif(long,long); /*long int positive difference (idim() in F77)*/
	double posdif(double,double); /*floating point positive difference*/
	                              /*dim(), ddim() in F77)*/
	short sposdif(short,short);	/*short int positive difference*/
#else
	long iposdif();
	double posdif();
	short sposdif();
#endif	/* PROTOTYPES */
#ifdef INLN_POSDIF
/* NOTE: BEWARE OF ARGUMENT SIDE EFFECTS! ! ! */
#  define iposdif(a,b)	(long)( (a) > (b) ? (a) - (b) : 0L )
#  define posdif(a,b) 	(double)( (a) > (b) ? (a) - (b) : 0. )
#  define sposdif(a,b)	(short)( (a) > (b) ? (a) - (b) : 0 )
#endif	/* INLN_POSDIF */

#ifdef __alpha	/*DEC ALPHA C also defines nint(), just differently*/
#	define nint nint000 
#	undef INLN_TRUNC /*don't allow inline on Alphas - too confusing*/
#endif
#ifdef PROTOTYPES
	double fnint( double ); /*nearest whole no. (anint(),dnint() in F77)*/
	long nint( double );    /*nearest long int (nint(), idnint() in F77)*/
	short snint( double );  /*nearest short int (nint(),idnint() in F77)*/
	double trunc( double ); /*truncation (aint(), dint() in F77)*/
	double fnint_(double*), fnintf_(float*);
	long nint_(double*),    nintf_(float*);
	short snint_(double*),  snintf_(float*);
	double trunc_(double*), truncf_(float*);
#else
	double fnint();
	long nint();
	short snint();
	double trunc();
	double fnint_(), fnintf_();
	long nint_(),    nintf_();
	short snint_(),  snintf_();
	double trunc_(), truncf_();
#endif	/* PROTOTYPES */
#ifdef INLN_TRUNC
#  define fnint(f) ((f_d_=(f)) < 0 ? trunc(f_d_-0.5):trunc(f_d_+0.5))
#  define nint(f)  (long)( (f_d_=(f)) < 0 ? f_d_-0.5 : f_d_+0.5 )
#  define snint(f) (short)( (f_d_=(f)) < 0 ? f_d_-0.5 : f_d_+0.5 )
#  define trunc(f) (modf(f,&f_d_),f_d_)
#endif	/* INLN_TRUNC */

#ifdef PROTOTYPES
	short ssign( short,short ); /*short int sign transfer (F77 isign())*/
	long isign( long,long );    /*long int sign transfer (F77 isign()) */
	double sign(double,double); /*floating pt sign transf (sign(),dsign())*/
	short ssign_( short*,short* );
	long isign_( long*,long* );
	double sign_(double*,double*), signf_(float*,float*);
#else
	short ssign();
	long isign();
	double sign();
	short ssign_();
	long isign_();
	double sign_(), signf_();
#endif	/* PROTOTYPES */
#ifdef INLN_SIGN
#  define ssign(a,b)	(short)( (b) < 0 ? -_ABS_(a) : _ABS_(a) )
#  define isign(a,b)	(long)( (b) < 0 ? -_ABS_(a) : _ABS_(a) )
#  define sign(a,b)	(double)( (b) < 0 ? -_ABS_(a) : _ABS_(a) )
#endif	/* INLN_SIGN */

	/* MILSPEC Bit manipulation 'Functions' */
#define ior(m,n)	((m)|(n))
#define iand(m,n)	((m)&(n))
#define inot(m)		(~(m))
#define ieor(m,n)	((m)^(n))

#ifdef PROTOTYPES
	short sshft( short, short );	/* bit shift */
	short sshftc(short,short,short);/* circular shift */
	short sbits(short,short,short);	/* bit extraction */
	int   sbtest( short, short );	/* bit test */
	short sbset( short, short );	/* bit set */
	short sbclr( short, short );	/* bit clear */
	void mvsbits( short, short, short, short*, short );	/* bit move */
	long ishft( long, long );		/* bit shift */
	long ishftc(long, long, long );	/* long circular shift */
	long ibits( long, long, long ); /* long bit extraction */
	int  btest( long, long );		/* long bit test */
	long ibset( long, long );		/* long bit set */
	long ibclr( long, long );		/* long bit clear */
	void mvbits( long, long, long, long*, long );	/* long bit move */
#else
	short sshft(), sshftc(), sbits(), sbset(), sbclr();
	int btest(), sbtest();
	long ishft(), ishftc(), ibits(), ibset(), ibclr();
	void mvbits(), mvsbits();
#endif	/* PROTOTYPES */

#define _PI_	3.14159265359
#ifdef VAX_FORTRAN
	/* VAX FORTRAN Degree Functions */
#  define sind(d)	sin((d)*_PI_/180)
#  define cosd(d)	cos((d)*_PI_/180)
#  define tand(d)	tan((d)*_PI_/180)
#  define asind(d)	(asin(d)*180/_PI_)
#  define acosd(d)	(acos(d)*180/_PI_)
#  define atand(d)	(atan(d)*180/_PI_)
#endif

	/*pass by address intrinsic transl wrappers*/
#ifdef PROTOTYPES
	double sqrt_(double*), sqrtf_(float*);
	double exp_(double*), expf_(float*);
	double log_(double*), logf_(float*);
	double log10_(double*), log10f_(float*);
	double sin_(double*), sinf_(float*);
	double cos_(double*), cosf_(float*);
	double tan_(double*), tanf_(float*);
	double atan2_(double*,double*), atan2f_(float*,float*);
	double sinh_(double*), sinhf_(float*);
	double cosh_(double*), coshf_(float*);
	double tanh_(double*), tanhf_(float*);
#else
	double sqrt_(), sqrtf_();
	double exp_(), expf_();
	double log_(), logf_();
	double log10_(), log10f_();
	double sin_(), sinf_();
	double cos_(), cosf_();
	double tan_(), tanf_();
	double atan2_(), atan2f_();
	double sinh_(), sinhf_();
	double cosh_(), coshf_();
	double tanh_(), tanhf_();
#endif	/* PROTOTYPES */

	/*		*		*		*		*/

#if unix || vms
  /* Cobalt Blue supplied functions, (most available w/MSDOS C's) */
#  ifdef PROTOTYPES
     int strcmpi(char const*,char const*);	/* str compare ignoring case */
     char *strlwr(char*), *strrev(char*);
#  else
     int strcmpi();
     char *strlwr(), *strrev();
#  endif  /* PROTOTYPES */
#endif

#if unix
#  if defined(__STDC__) || ANSI_C_TARGET  /*ANSI C on UNIX*/
     /*no action required*/
#  else  /*UNIX System V C*/
	 /*simulate ANSI C*/
#    ifndef SIZE_T_DEFINITION
       typedef unsigned size_t;
#      define SIZE_T_DEFINITION
#    endif

#    define remove unlink

#    define NEED_STRSTR
#    include <memory.h>  /*declares memcpy, memcmp, memchr, and memset*/

#    ifdef PROTOTYPES
       /*missing ANSI declarations in UNIX "LgsString.h"*/
       char *strcpy(char*, const char*);
       char *strncpy(char*, const char*, size_t); 
       char *strcat(char*, const char*);
       char *strncat(char*, const char*, size_t);
       char *strstr(const char*, const char*);
       char *strerror(int);
#    else
       char *strcpy();
       char *strncpy();
       char *strcat();
       char *strncat();
       char *strstr();
       char *strerror();
#    endif/* PROTOTYPES */
#  endif  /* ANSI C or UNIX System V C on UNIX */
#endif	/* UNIX */

#endif	/* FCRT_INCLUDED - avoids problems if file is included twice */
