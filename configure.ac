#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([lokke], [0.0.1], [rlb@defaultvalue.org])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([foreign -Wall -Werror])
AC_CONFIG_SRCDIR([lib/lokke-reader.c])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIRS([m4])
AC_REQUIRE_AUX_FILE([tap-driver.sh])

# Checks for programs.
AM_PROG_AR
AC_PROG_CC
AC_PROG_LN_S

# Checks for libraries.

PKG_CHECK_MODULES(PCRE2, [libpcre2-8 libpcre2-32])

# Checks for header files.
AC_FUNC_ALLOCA
AC_CHECK_HEADERS([string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T

# Checks for library functions.
AC_CHECK_FUNCS([strchr strstr])

LT_INIT

GUILE_PKG([3.0 2.2])
GUILE_PROGS
GUILE_FLAGS
#GUILE_CHECK([maybe_broken_guile],[(string=? (version) "3.0.7")])

GUILE_CHECK([maybe_broken_guile],
  [(define-syntax foo
    (syntax-rules ()
      ((_ x) (eq? #nil x))))

   (foo #t)])

if test "$maybe_broken_guile" -ne 0; then
   AC_MSG_ERROR([
  The detected $GUILE (3.0.7?) appears to be missing this required patch:
    https://git.savannah.gnu.org/cgit/guile.git/commit/?id=d79a226359d28f4a1dc5df136e5544d699903a96 ])
fi

lokke_major_version="${VERSION%%.*}"
lokke_nomajor_version="${VERSION#*.}"
lokke_minor_version="${lokke_nomajor_version%%.*}"
lokke_nominor_version="${lokke_nomajor_version#*.}"
lokke_increment_version="${lokke_nominor_version%%.*}"

AC_SUBST(LOKKE_MAJOR_VERSION, [${lokke_major_version}])
AC_SUBST(LOKKE_MINOR_VERSION, [${lokke_minor_version}])
AC_SUBST(LOKKE_INCREMENT_VERSION, [${lokke_increment_version}])
AC_SUBST(LOKKE_EFFECTIVE_VERSION, [${VERSION%.*}])

AC_CONFIG_FILES([
  Makefile lokke.1 mod/lokke/base/version.scm
])

AC_OUTPUT
