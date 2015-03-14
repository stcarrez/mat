# configure.in for libgpmat
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

AC_PREREQ(2.59)
AC_INIT(mat, 1.0,Stephane.Carrez@gmail.com,libgpmat)

AC_CANONICAL_SYSTEM

AC_CONFIG_HEADERS([src/gp-config.h])

AC_EXEEXT

dnl These must be called before AM_PROG_LIBTOOL, because it may want
dnl to call AC_CHECK_PROG.
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(NM, nm)
AC_CHECK_TOOL(RANLIB, ranlib, :)
AC_CHECK_TOOL(STRIP, strip, :)
AC_PROG_LN_S

AC_ISC_POSIX
AC_PROG_CC
AC_PROG_CPP
AC_LANG(C)
AC_PROG_INSTALL

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
    mips*)
      mach_cflags="-funwind-tables"
	  mach_result="using $mach_cflags"
      ;;

    *)
      mach_result=no
      ;;

  esac
  AC_MSG_RESULT($mach_result)

  CFLAGS="${CFLAGS} ${mach_results}"
fi

MAT_TLS=
ac_enable_tls=
AC_ARG_ENABLE(tls,
[  --enable-tls            Enable or disable use of tls code (disable)],
[case "${enableval}" in
  no|none)  ac_enable_tls= ;;
  *)        ac_enable_tls=yes
            MAT_TLS=-DTLS
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

AC_OUTPUT([
Makefile
])

