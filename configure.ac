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

AC_INIT(Makefile.in)

# Current release settings
MAT_MAJOR_VERSION=1
MAT_MINOR_VERSION=1
MAT_MICRO_VERSION=0
MAT_VERSION=$MAT_MAJOR_VERSION.$MAT_MINOR_VERSION.$MAT_MICRO_VERSION
AC_SUBST(MAT_VERSION)

EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)

CONFIG_SUBDIRS="libmat"
SUBDIRS="libmat matl"
SUBDIRS_INSTALL=$SUBDIRS

ac_enable_ada_util=no
AC_ARG_ENABLE(ada-util,
  [  --enable-ada-util       Enable compilation of Ada Utility Library],
  [case "${enableval}" in
    no|none)  ac_enable_ada_util= ;;
    *)        ac_enable_ada_util=yes
		  	  ;;
  esac])

ac_enable_ada_bfd=no
AC_ARG_ENABLE(ada-bfd,
  [  --enable-ada-bfd        Enable compilation of Ada BFD Library],
  [case "${enableval}" in
    no|none)  ac_enable_ada_bfd= ;;
    *)        ac_enable_ada_bfd=yes
		  	  ;;
  esac])

AC_CHECK_PROGS(GPRBUILD, gprbuild, "")
if test -n "$GPRBUILD"; then
  ac_enable_mat=yes
else
  ac_enable_mat=no
fi

AC_ARG_ENABLE(mat,
  [  --enable-mat            Enable compilation of matp tool which requires GNAT Ada compiler (disabled)],
  [case "${enableval}" in
    no|none)  ac_enable_mat= ;;
    *)        ac_enable_mat=yes
	      ;;
    esac])

AC_CONFIG_SUBDIRS(libmat)

# Build ada-util only when --enable-mat and --enable-ada-util are defined.
if test x$ac_enable_mat = 'xyes'; then
  if test x$ac_enable_ada_util = 'xyes'; then
    SUBDIRS="$SUBDIRS ada-util"
    AC_CONFIG_SUBDIRS(ada-util)
  fi

  if test x$ac_enable_ada_bfd = 'xyes'; then
    SUBDIRS="$SUBDIRS ada-bfd"    
    AC_CONFIG_SUBDIRS(ada-bfd)
  fi

  SUBDIRS="$SUBDIRS mat"
  SUBDIRS_INSTALL="$SUBDIRS_INSTALL mat"
  AC_CONFIG_SUBDIRS(mat)
fi

AC_SUBST(SUBDIRS)
AC_SUBST(SUBDIRS_INSTALL)

AC_PROG_CC
AC_PROG_CPP
AC_LANG(C)
AC_PROG_INSTALL

AC_OUTPUT(
Makefile
matl/Makefile
)
