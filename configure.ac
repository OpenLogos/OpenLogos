#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([OpenLogos], [1.0.3], [info@logos-mt.com])
AC_CONFIG_SRCDIR([config.h.in])
AM_CONFIG_HEADER([config.h])

AM_INIT_AUTOMAKE([foreign])

# Checks for programs.
AC_PROG_LIBTOOL
AC_PROG_CXX
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
# AC_PROG_RANLIB

# Check for appropriate java include path and headers
# 4linux
#JAVADIR=/usr/java/current
JAVADIR=/lt/pkg/jdk1.5.0_02
# 4windows
#JAVADIR=/cygdrive/c/dvp/jdk1.5.0_04
AC_ARG_WITH(java,
[AC_HELP_STRING([--with-java@<:@=DIR@:>@],
                [java installation root directory])],
[
# This does not really make sense. Java is required, this variable is just to 
# be able to set the path from outside
case "${withval}" in
    yes) java=true ;;
    no)  java=false ;;
    *)   java=true
         JAVADIR=${withval};;
esac
],[java=true])

AC_ARG_ENABLE(testprog, 
[AC_HELP_STRING([--enable-testprogs],
                [Build available test programs (default: no)])],
[case "${enableval}" in
      yes) testprog=true ;;
      no) testprog=false ;;
      *) AC_MSG_ERROR(bad value ${enableval} for --enable-testprogs) ;;
esac],[testprog=false])
AM_CONDITIONAL(TESTPROGS, test "x$testprog" = "xtrue")

OLDCPPFLAGS=$CPPFLAGS
case $host in 
     *-*-msdos* | *-*-windows* | *-*-cygwin* | *-*-go32* | *-*-mingw*)
     JAVAINCLUDE="-I${JAVADIR}/include -I${JAVADIR}/include/win32"
     ADDINCLUDE="#define __int64 long long"
     ;;
     *-*-linux*)
     JAVAINCLUDE="-I${JAVADIR}/include -I${JAVADIR}/include/linux"
     ;;
esac
CPPFLAGS="$CPPFLAGS $JAVAINCLUDE"
AC_MSG_NOTICE([javainclude FOR $host is $CPPFLAGS])
AC_CHECK_HEADERS([jni.h jni_md.h], [], [AC_MSG_ERROR([No java dir])],
                 [$ADDINCLUDE])
CPPFLAGS=$OLDCPPFLAGS
unset OLDCPPFLAGS ADDINCLUDE
AC_SUBST([JAVAINCLUDE])

# use ODBC instead of Oracle
AC_DEFINE([USE_ODBC],[],[Use the odbc database interface])

# Flags to distinguish DB specific SQL code
AC_DEFINE([USE_POSTGRES],[],[Use the PostgreSQL database])


# Checks for libraries.

# Checks for header files.
AC_FUNC_ALLOCA
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h limits.h malloc.h memory.h netinet/in.h stddef.h stdlib.h string.h strings.h time.h sys/time.h dirent.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T
AC_STRUCT_TM
AC_C_VOLATILE

# Checks for library functions.
AC_HEADER_STDC
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_REALLOC
AC_FUNC_SELECT_ARGTYPES
AC_FUNC_STAT
AC_CHECK_FUNCS([bzero isascii memmove memset memcpy putenv re_comp regcomp select strchr strdup strerror strpbrk strstr strcasecmp strncasecmp mmap])

CPPFLAGS="$CPPFLAGS -I\$(top_srcdir)"
# CFLAGS="-Wall"

AC_CHECK_LIB([rt], [shm_open])
AC_CHECK_LIB([pthread], [pthread_create])

AC_CHECK_HEADERS([sql.h], [], [AC_MSG_ERROR([ODBC headers are missing])], [])
AC_CHECK_LIB([odbc], [SQLFreeConnect])

AC_CONFIG_FILES([Makefile
                 doc/Makefile
                 doc/API/Makefile
                 logos_include/Makefile
                 logos_include_res_pt/Makefile
                 TermMiner/Makefile
                 CacheMgr/Makefile
                 ConvertMini/Makefile
                 configdatafileinterface/Makefile
                 engine_api/Makefile
                 engine_api/APITest/Makefile
                 engine_api/xlationinterface/Makefile
                 engine_api/proxylayer/Makefile
                 engine_api/translationserver/Makefile
                 lgs_base/Makefile
                 lgs_db_io/Makefile
                 lgs_stemgen/Makefile
                 logos_java/Makefile
                 logos_java/GermanHalfNoun/Makefile
                 logos_java/GlobalWordsEngineAPI/Makefile
                 logos_java/PatStem/Makefile
                 logos_libs/Makefile
                 logos_libs/dbcache/Makefile
                 logos_libs/dbms2restran/Makefile
                 logos_libs/elision/Makefile
                 logos_libs/elision/english/Makefile
                 logos_libs/elision/french/Makefile
                 logos_libs/elision/german/Makefile
                 logos_libs/elision/spanish/Makefile
                 logos_libs/elision/portuguese/Makefile
                 logos_libs/elision/italian/Makefile
                 logos_libs/entity/Makefile
                 logos_libs/gerdem/Makefile
                 logos_libs/GrammarRules/Makefile
                 logos_libs/halfnoun/Makefile
                 logos_libs/halfnoundll/Makefile
                 logos_libs/lgs_parsetran1/Makefile
                 logos_libs/lgs_parsetran2/Makefile
                 logos_libs/lgs_parsetran3/Makefile
                 logos_libs/lgs_parsetran4/Makefile
                 logos_libs/lgssgml/Makefile
                 logos_libs/linguistic/Makefile
                 logos_libs/lgs_tran_rule_io/Makefile
                 logos_libs/multithreadlib/Makefile
                 logos_libs/newdebug/Makefile
		 logos_libs/odbcsql/Makefile
                 logos_libs/parsetrans/Makefile
                 logos_libs/patternmatcher/Makefile
                 logos_libs/PatternRecognition/Makefile
                 logos_libs/regex/Makefile
                 logos_libs/res_parsetran_utils/Makefile
                 logos_libs/ruleengine/Makefile
                 logos_libs/semtabrule/Makefile
                 logos_libs/sql/Makefile
                 logos_libs/startrules/Makefile
                 logos_libs/SubjectMatterCodes/Makefile
                 logos_libs/translutility/Makefile
                 logos_libs/utility/Makefile
                 transl/Makefile
                 transl/lgs_parsetran1dll/Makefile
                 transl/lgs_parsetran2dll/Makefile
                 transl/lgs_parsetran3dll/Makefile
                 transl/lgs_parsetran4dll/Makefile
                 transl/lgs_resdll/Makefile
                 transl/lgs_tran1dll/Makefile
                 transl/lgs_tran2dll/Makefile
                 transl/lgs_tran3dll/Makefile
                 transl/lgs_tran4dll/Makefile
		 shells/Makefile
		 shells/logos_batch/Makefile
                 ])

#

AC_OUTPUT
