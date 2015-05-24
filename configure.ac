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

AC_CANONICAL_SYSTEM
AC_EXEEXT

# Current release settings
MAT_MAJOR_VERSION=1
MAT_MINOR_VERSION=0
MAT_MICRO_VERSION=0
MAT_VERSION=$MAT_MAJOR_VERSION.$MAT_MINOR_VERSION.$MAT_MICRO_VERSION
AC_SUBST(MAT_VERSION)

# checking for local tools
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_CC
AC_PROG_CPP
AC_LANG(C)

EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)

AC_CONFIG_SUBDIRS(libmat)

pwd_option=
mat_build_root=
mat_util_configure=
mat_bfd_configure=
mat_util_name=ada-util
mat_bfd_name=ada-bfd

mat_build_pwd=`cd ${mat_build_root} && pwd $pwd_option`
if test x${mat_build_pwd} != x${mat_build_root}; then
  mat_build_root=${mat_build_pwd}/
fi

SUBDIRS="libmat matl"
SUBDIRS_INSTALL=$SUBDIRS

ac_enable_ada_util=no
AC_ARG_ENABLE(ada-util,
  [  --enable-ada-util       Enable compilation of Ada Utility Library (external/ada-util-1.7.1.tar.gz)],
  [case "${enableval}" in
    no|none)  ac_enable_ada_util= ;;
    *)        ac_enable_ada_util=yes
              SUBDIRS="$SUBDIRS ada-util-1.7.1"
		  	  ;;
  esac])

ac_enable_ada_bfd=no
AC_ARG_ENABLE(ada-bfd,
  [  --enable-ada-bfd        Enable compilation of Ada BFD Library (external/ada-bfd-1.1.0.tar.gz)],
  [case "${enableval}" in
    no|none)  ac_enable_ada_bfd= ;;
    *)        ac_enable_ada_bfd=yes
              SUBDIRS="$SUBDIRS ada-bfd-1.1.0"
		  	  ;;
  esac])

ac_enable_matp=no
AC_ARG_ENABLE(matp,
  [  --enable-matp           Enable compilation of matp tool which requires GNAT Ada compiler (disabled)],
  [case "${enableval}" in
    no|none)  ac_enable_matp= ;;
    *)        ac_enable_matp=yes
              SUBDIRS="$SUBDIRS matp"
			  SUBDIRS_INSTALL="$SUBDIRS_INSTALL matp"
			  ;;
    esac])

# Build ada-util-1.7.1 only when --enable-matp and --enable-ada-util are defined.
if test x$ac_enable_matp = 'xyes'; then
  if test x$ac_enable_ada_util = 'xyes'; then
    if test -d ${srcdir}/ada-util-1.7.1 ; then
      echo "Package ada-util-1.7.1 already extracted."
    else
      echo "Extracting ${srcdir}/external/ada-util-1.7.1.tar.gz"
      (cd ${srcdir} && tar xzf ${srcdir}/external/ada-util-1.7.1.tar.gz)
    fi
    AC_CONFIG_SUBDIRS(ada-util-1.7.1)
  fi
fi

# Build ada-bfd-1.1.0 only when --enable-matp and --enable-ada-bfd are defined.
if test x$ac_enable_matp = 'xyes'; then
  if test x$ac_enable_ada_bfd = 'xyes'; then
    if test -d ${srcdir}/ada-bfd-1.1.0 ; then
      echo "Package ada-bfd-1.1.0 already extracted."
    else
      echo "Extracting ${srcdir}/external/ada-bfd-1.1.0.tar.gz"
      (cd ${srcdir} && tar xzf ${srcdir}/external/ada-bfd-1.1.0.tar.gz)

      # Hack to solve duplicate project 'config' issue.
      (cd ${srcdir}/ada-bfd-1.1.0 &&
	    sed -e 's,"config","../ada-util-1.7.1/config",' < bfdada.gpr.in > bfdada.gpr.in.fix &&
        mv bfdada.gpr.in.fix bfdada.gpr.in)
    fi
    AC_CONFIG_SUBDIRS(ada-bfd-1.1.0)
  fi
fi

if test x$ac_enable_matp = 'xyes'; then
  AC_CONFIG_SUBDIRS(matp)
fi

AC_SUBST(SUBDIRS)
AC_SUBST(SUBDIRS_INSTALL)

AC_OUTPUT(
Makefile
matl/Makefile
)
