
AUTOMAKE_OPTIONS = subdir-objects
ACLOCAL_AMFLAGS = -I m4

SUBDIRS = gnulib
LOG_DRIVER = env AM_TAP_AWK='$(AWK)' $(SHELL) $(top_srcdir)/build-aux/tap-driver.sh

EXTRA_DIST = m4/gnulib-cache.m4 $(TESTS)

AM_CFLAGS = -g -I gnulib -Wall -Werror -std=gnu11 -fdiagnostics-color=always

pkglib_LTLIBRARIES = lib/lokke-reader.la  lib/lokke-vector.la
lib_lokke_reader_la_SOURCES = lib/lokke-reader.c
lib_lokke_reader_la_CFLAGS = @GUILE_CFLAGS@ $(AM_CFLAGS)
lib_lokke_reader_la_LDFLAGS = -module @GUILE_LDFLAGS@ @GUILE_LIBS@

lib_lokke_vector_la_SOURCES = lib/lokke-vector.c
lib_lokke_vector_la_CFLAGS = @GUILE_CFLAGS@ $(AM_CFLAGS)
lib_lokke_vector_la_LDFLAGS = -module @GUILE_LDFLAGS@ @GUILE_LIBS@

export LOKKE_TEST_PROTOCOL = tap

TESTS = \
  test/equality \
  test/clojure-defmacro \
  test/clojure-exceptions \
  test/clojure-string \
  test/lokke-compare \
  test/lokke-core \
  test/lokke-destructure \
  test/lokke-hash-map \
  test/lokke-hash-set \
  test/lokke-ns \
  test/lokke-scm-atom \
  test/lokke-scm-vector \
  test/lokke-symbol \
  test/metadata-handling \
  test/reader-basics \
  test/clojure-fn \
  test/clojure-collection

# FIXME...
clean-local:
	for f in $(TESTS); do \
	  rm -f "$$f.log"; \
	done

SUFFIXES = .x
.c.x:
	guile-snarf -o $@ $< @GUILE_CFLAGS@ -I gnulib -std=gnu11

# This is just a hack until/unless we deal with the initial circular dep
# .x depends on .c depends on .x...
.PHONY: bootstrap
bootstrap: lib/lokke-reader.x lib/lokke-vector.x
