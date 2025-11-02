# configure.in for mat
dnl Written by Stephane Carrez (Stephane.Carrez@gmail.com)
dnl
dnl  Licensed under the Apache License, Version 2.0 (the "License");
dnl  you may not use this file except in compliance with the License.
dnl  You may obtain a copy of the License at
dnl
dnl     http://www.apache.org/licenses/LICENSE-2.0
dnl
dnl  Unless required by applicable law or agreed to in writing, software
dnl  distributed under the License is distributed on an "AS IS" BASIS,
dnl  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
dnl  See the License for the specific language governing permissions and
dnl  limitations under the License.
dnl 

AC_INIT
AC_CONFIG_SRCDIR([Makefile.in])

cache_file=config.cache

# Current release settings
MAT_MAJOR_VERSION=1
MAT_MINOR_VERSION=2
MAT_MICRO_VERSION=0
MAT_VERSION=$MAT_MAJOR_VERSION.$MAT_MINOR_VERSION.$MAT_MICRO_VERSION
AC_SUBST(MAT_VERSION)

EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)

AC_CANONICAL_TARGET

AC_CONFIG_HEADERS([libmat/src/mat-config.h])

AC_EXEEXT

AC_CHECK_PROGS(GPRBUILD, gprbuild, "")
AC_PROG_CC
AC_PROG_CPP
AC_LANG(C)
AC_PROG_INSTALL

dnl These must be called before AM_PROG_LIBTOOL, because it may want
dnl to call AC_CHECK_PROG.
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(NM, nm)
AC_CHECK_TOOL(RANLIB, ranlib, :)
AC_CHECK_TOOL(STRIP, strip, :)
AC_PROG_LN_S

AC_SEARCH_LIBS([strerror],[cposix])

MAT_DEBUG=
ac_enable_debug=
AC_ARG_ENABLE(debug,
[  --enable-debug          Enable debug mode (disable)],
[case "${enableval}" in
  no|none)  ac_enable_debug= ;;
  *)        ac_enable_debug=yes
            MAT_DEBUG=-DDEBUG
			;;
esac])dnl

AC_SUBST(MAT_DEBUG)

if test "$GCC" = yes; then
  mach_cflags=
  AC_MSG_CHECKING([for specific compilation flags for ${target}])
  case "${target}" in
    mips*|arm*)
      mach_cflags="-funwind-tables"
      mach_result="using $mach_cflags"
      ;;

    *)
      mach_result=no
      ;;

  esac
  AC_MSG_RESULT($mach_result)

  CFLAGS="${CFLAGS} ${mach_cflags}"
fi

MAT_TLS=
ac_enable_tls=
AC_ARG_ENABLE(tls,
[  --enable-tls            Enable or disable use of tls code (disable)],
[case "${enableval}" in
  no|none)  ac_enable_tls= ;;
  *)        ac_enable_tls=yes
            MAT_TLS=-DHAVE_TLS
			;;
esac])dnl

AC_SUBST(MAT_TLS)

test_CFLAGS=${CFLAGS+set}

dnl If compiler is gcc, then use some specific flags.
dnl But don't touch user other flags.
if test "$test_CFLAGS" != set && test -n "$GCC"; then
  CFLAGS="-Wall -Wmissing-prototypes -Wpointer-arith $CFLAGS"
  if test -n "$disable_gcc_format_warning" ; then
   CFLAGS="$CFLAGS -Wno-format"
  fi
fi

AC_C_CONST
AC_C_VOLATILE

AC_CHECK_HEADERS(sys/types.h sys/shm.h sys/ipc.h sys/sem.h sys/syscall.h sys/time.h time.h stdlib.h signal.h setjmp.h unwind.h pthread.h unistd.h)
AC_CHECK_FUNCS(sigaction)
AC_CHECK_FUNCS(backtrace)
AC_CHECK_FUNCS(__curbrk)
AC_CHECK_FUNCS(__libc_malloc)

if test "x$ac_cv_func_backtrace" = xyes; then
   cpu_files="src/frame-backtrace.c"
   cpu_obj_files="frame-backtrace.o"
else
 AC_CHECK_FUNCS(_Unwind_Backtrace)
 AC_CHECK_FUNCS(_Unwind_GetIP)
 AC_CHECK_FUNCS(_Unwind_GetCFA)

 if test "x$ac_cv_func__Unwind_Backtrace" = xyes; then
   cpu_files="src/frame-unwind.c"
   cpu_obj_files="frame-unwind.o"
 fi

fi

CPU_FILES=$cpu_files
CPU_OBJS=$cpu_obj_files
AC_SUBST(CPU_FILES)
AC_SUBST(CPU_OBJS)
AC_SUBST(AR)

SUBDIRS="libmat matl"
SUBDIRS_INSTALL=$SUBDIRS

if test -n "$GPRBUILD"; then
  ac_enable_mat=yes
else
  ac_enable_mat=no
fi

AC_ARG_ENABLE(mat,
  [  --enable-mat            Enable compilation of matp tool which requires GNAT Ada compiler],
  [case "${enableval}" in
    no|none)  ac_enable_mat= ;;
    *)        ac_enable_mat=yes
	      ;;
    esac])

AM_CHECK_HOST_PWD
DIR=`pwd $awa_pwd_option`

if test x$ac_enable_mat = 'xyes'; then
  SUBDIRS="$SUBDIRS mat"
  SUBDIRS_INSTALL="$SUBDIRS_INSTALL mat"
fi

AC_SUBST(SUBDIRS)
AC_SUBST(SUBDIRS_INSTALL)

AM_GNAT_LIBRARY_PROJECT(mat)

ac_enable_gtk=no
AC_ARG_ENABLE(gtk,
[  --enable-gtk            Enable compilation of mat Gtk tool (disabled)],
[case "${enableval}" in
  no|none)  ac_enable_gtk= ;;
  *)        ac_enable_gtk=yes
esac])dnl

MAT_GTKADA=$ac_enable_gtk
AC_SUBST(MAT_GTKADA)

if test x$ac_enable_mat = 'xyes'; then

  AC_CHECK_LIB([readline],[readline])

  GTKADA_DIR=
  if test x$ac_enable_gtk = 'xyes'; then

##########################################
# GtkAda library
##########################################
  AC_MSG_CHECKING([GtkAda library (libre.adacore.com/tools/gtkada)])
  AC_ARG_WITH(gtkada,
  AS_HELP_STRING([--with-gtkada=x], [Path for the Gtk Ada library (libre.adacore.com/tools/gtkada)]),
  [
    GTKADA_DIR=${withval}/
  ],
  [
    echo "with \"gtkada\"; project t is for Source_Dirs use (); end t;" > t.gpr
    gnatmake -p -Pt >/dev/null 2>/dev/null
    if test $? -eq 0; then
      GTKADA_DIR=
    else
      GTKADA_DIR=../gtkada/
    fi;
  ])
  echo "with \"${GTKADA_DIR}gtkada\"; project t is for Source_Dirs use (); end t;" > t.gpr
  gnatmake -p -Pt >/dev/null 2>/dev/null
  if test $? -ne 0 ; then
    rm -f t.gpr
    ac_enable_gtk=
    # AC_MSG_ERROR([GtkAda library not found in ${GTKADA_DIR}])
  fi
  rm -f t.gpr
  AC_MSG_RESULT(${GTKADA_DIR})
  fi
fi

if test x$ac_enable_gtk = 'xyes'; then
  GPRPATH=gtkmat.gpr
else
  GPRPATH=mat.gpr
fi

AC_SUBST(GPRPATH)
AC_SUBST(GTKADA_DIR)

AC_CONFIG_FILES([Makefile
libmat/Makefile
matl/Makefile
mat/Makefile.conf
])
AC_OUTPUT
