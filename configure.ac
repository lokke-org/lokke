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
gl_EARLY
gl_INIT

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
GUILE_FLAGS

AC_SUBST(LOKKE_EFFECTIVE_VERSION, [${VERSION%.*}])

AC_CONFIG_FILES([Makefile gnulib/Makefile])
AC_CONFIG_FILES([guile], [chmod +x guile])

AC_OUTPUT
