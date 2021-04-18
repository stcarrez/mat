dnl Autoconf configure script for mat

dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/mat.ads)

# Current release settings
MAT_MAJOR_VERSION=0
MAT_MINOR_VERSION=1
MAT_MICRO_VERSION=0
MAT_VERSION=$MAT_MAJOR_VERSION.$MAT_MINOR_VERSION.$MAT_MICRO_VERSION
# Set the version number of the project
AC_SUBST(MAT_VERSION)
AC_SUBST(MAT_MAJOR_VERSION)
AC_SUBST(MAT_MINOR_VERSION)
AC_SUBST(MAT_MICRO_VERSION)

EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)
AM_GNAT_LIBRARY_PROJECT(mat)
# checking for local tools
AC_PROG_CC

ac_enable_gtk=no
AC_ARG_ENABLE(gtk,
[  --enable-gtk          Enable compilation of mat Gtk tool (disabled)],
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
AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[utilada_sys],
  [git@github.com:stcarrez/ada-util.git],
  [Building mat requires the Ada Utility Library.],
  [
    UTIL_DIR=${ac_cv_gnat_project_dir_utilada_sys}
  ])
AC_SUBST(UTIL_DIR)

##########################################
# GNU Bfd Ada library
##########################################
AM_GNAT_FIND_PROJECT([ada-bfd],[Ada Bfd Library],[bfdada],
  [git@github.com:stcarrez/ada-bfd.git],
  [Building MAT requires the Ada Bfd Library.],
  [
    BFD_DIR=${ac_cv_gnat_project_dir_bfdada}
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
Makefile.conf mat.gpr mat_gtk.gpr mat_tests.gpr
)
