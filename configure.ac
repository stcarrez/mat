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

EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)

AC_CONFIG_SUBDIRS(libgpmat)

SUBDIRS="libgpmat"
ac_enable_mat=yes
AC_ARG_ENABLE(mat,
[  --enable-mat          Enable compilation of mat tool which requires GNAT Ada compiler (enabled)],
[case "${enableval}" in
  no|none)  ac_enable_mat= ;;
  *)        ac_enable_mat=yes
            SUBDIRS="$DIRS mat"
			;;
esac])dnl

if test x$ac_enable_mat = 'xyes'; then
  AC_CONFIG_SUBDIRS(mat)
fi

AC_SUBST(SUBDIRS)

AC_OUTPUT(
Makefile
)
