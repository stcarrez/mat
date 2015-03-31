dnl Autoconf configure script for mat
dnl Process this file with autoconf to produce a configure script.
AC_INIT(src/mat.ads)
AC_CANONICAL_SYSTEM
dnl AM_MAINTAINER_MODE
# Current release settings
MAT_MAJOR_VERSION=0
MAT_MINOR_VERSION=1
MAT_MICRO_VERSION=0
MAT_VERSION=$MAT_MAJOR_VERSION.$MAT_MINOR_VERSION.$MAT_MICRO_VERSION
# checking for local tools
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_LN_S
AC_CHECK_PROGS(GNATMAKE, gnatmake, "")
AC_CHECK_PROGS(GNATCLEAN, gnatclean, "")
# Set the version number of GtkAda
AC_SUBST(MAT_VERSION)
AC_SUBST(MAT_MAJOR_VERSION)
AC_SUBST(MAT_MINOR_VERSION)
AC_SUBST(MAT_MICRO_VERSION)
EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)
MAT_LIBDIR="lib"
AC_SUBST(MAT_LIBDIR)

AC_MSG_CHECKING([number of processors])
NR_CPUS=`getconf _NPROCESSORS_CONF 2>/dev/null || getconf _NPROCESSORS_CONF 2>/dev/null || echo 1`
AC_MSG_RESULT($NR_CPUS)
AC_SUBST(NR_CPUS)

AC_CHECK_PROGS(GNATMAKE, gnatmake, "")

ac_enable_gtk=yes
AC_ARG_ENABLE(gtk,
[  --enable-gtk          Enable compilation of mat Gtk tool (enabled)],
[case "${enableval}" in
  no|none)  ac_enable_gtk= ;;
  *)        ac_enable_gtk=yes
esac])dnl

MAT_GTKADA=$ac_enable_gtk
AC_SUBST(MAT_GTKADA)

AC_CHECK_LIB([readline],[readline])

##########################################
# Ada Util library
##########################################
AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[util],
  [git@github.com:stcarrez/ada-util.git],
  [Building Ada EL requires the Ada Utility Library.],
  [
    UTIL_DIR=${gnat_project_dir_util}
  ])
AC_SUBST(UTIL_DIR)

##########################################
# GNU Bfd Ada library
##########################################
AM_GNAT_FIND_PROJECT([ada-bfd],[Ada Bfd Library],[bfdada],
  [git@github.com:stcarrez/ada-bfd.git],
  [Building NAT requires the Ada Bfd Library.],
  [
    BFD_DIR=${gnat_project_dir_bfdada}
  ])
AC_SUBST(BFD_DIR)

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


if test x$ac_enable_gtk = 'xyes'; then
  GPRPATH=gtkmat.gpr
else
  GPRPATH=mat.gpr
fi

AC_SUBST(GPRPATH)
AC_SUBST(GTKADA_DIR)

AC_OUTPUT(
Makefile mat.gpr gtkmat.gpr tests.gpr
)
