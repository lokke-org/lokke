
# Makefile.am is automatically generated from Makefile.am.in via ./gen-makefile

CLEANFILES =
lokke_clean =

state:
	mkdir state
lokke_clean += state

state/module-paths:: state
	git ls-files 'mod/*.clj' 'mod/*.scm' > $@.tmp.$$PPID
	cmp $@.tmp.$$PPID $@ || mv $@.tmp.$$PPID $@
	rm -f $@.tmp.$$PPID

Makefile.am: Makefile.am.in gen-makefile state/module-paths
	./gen-makefile state/module-paths

AUTOMAKE_OPTIONS = subdir-objects
ACLOCAL_AMFLAGS = -I m4

SUBDIRS = gnulib
LOG_DRIVER = env AM_TAP_AWK='$(AWK)' $(SHELL) $(top_srcdir)/build-aux/tap-driver.sh

EXTRA_DIST = m4/gnulib-cache.m4 $(TESTS)

noinst_SCRIPTS = lokke
man1_MANS = lokke.1

AM_CFLAGS = \
  -g -I gnulib -Wall -Werror -std=gnu11 -fdiagnostics-color=always \
  -fno-strict-aliasing

vpkglibdir = $(pkglibdir)/$(LOKKE_EFFECTIVE_VERSION)
vpkglib_LTLIBRARIES = lib/lokke-reader.la  lib/lokke-pcre2.la lib/lokke-vector.la

# -module can't go here because automake only detects it directly in LDFLAGS
lokke_lib_ldflags = -release $(VERSION) @GUILE_LDFLAGS@ @GUILE_LIBS@

lib_lokke_reader_la_SOURCES = lib/lokke-reader.c
lib_lokke_reader_la_CFLAGS = @GUILE_CFLAGS@ $(AM_CFLAGS)
lib_lokke_reader_la_LDFLAGS = -module $(lokke_lib_ldflags)

lib_lokke_vector_la_SOURCES = lib/lokke-vector.c
lib_lokke_vector_la_CFLAGS = @GUILE_CFLAGS@ $(AM_CFLAGS)
lib_lokke_vector_la_LDFLAGS = -module $(lokke_lib_ldflags)

lib_lokke_pcre2_la_SOURCES = lib/lokke-pcre2.c
lib_lokke_pcre2_la_CFLAGS = @GUILE_CFLAGS@ $(AM_CFLAGS) @PCRE2_CFLAGS@
lib_lokke_pcre2_la_LDFLAGS = -module $(lokke_lib_ldflags) @PCRE2_LIBS@

# Avoids the otherwise circular dependency
BUILT_SOURCES = lib/lokke-reader.x lib/lokke-pcre2.x lib/lokke-vector.x


# This section is automatically generated
moduledir = $(pkgdatadir)/$(LOKKE_EFFECTIVE_VERSION)
module_mod_language_lokkedir = $(moduledir)/language/lokke
module_mod_lokke_basedir = $(moduledir)/lokke/base
module_mod_lokke_ns_clojure_coredir = $(moduledir)/lokke/ns/clojure/core
module_mod_lokke_ns_clojuredir = $(moduledir)/lokke/ns/clojure
module_mod_lokke_ns_lokkedir = $(moduledir)/lokke/ns/lokke
module_mod_lokke_readerdir = $(moduledir)/lokke/reader
module_mod_lokke_scmdir = $(moduledir)/lokke/scm
module_mod_lokkedir = $(moduledir)/lokke

module_mod_language_lokke_DATA =
module_mod_lokke_DATA =
module_mod_lokke_base_DATA =
module_mod_lokke_ns_clojure_DATA =
module_mod_lokke_ns_clojure_core_DATA =
module_mod_lokke_ns_lokke_DATA =
module_mod_lokke_reader_DATA =
module_mod_lokke_scm_DATA =

module_mod_language_lokke_DATA += mod/language/lokke/spec.scm
module_mod_lokke_base_DATA += mod/lokke/base/collection.scm
module_mod_lokke_base_DATA += mod/lokke/base/destructure.scm
module_mod_lokke_base_DATA += mod/lokke/base/doc.scm
module_mod_lokke_base_DATA += mod/lokke/base/dynamic.scm
module_mod_lokke_base_DATA += mod/lokke/base/invoke.scm
module_mod_lokke_base_DATA += mod/lokke/base/map-entry.scm
module_mod_lokke_base_DATA += mod/lokke/base/map.scm
module_mod_lokke_base_DATA += mod/lokke/base/metadata.scm
module_mod_lokke_base_DATA += mod/lokke/base/syntax.scm
module_mod_lokke_base_DATA += mod/lokke/base/util.scm
module_mod_lokke_DATA += mod/lokke/boot.scm
module_mod_lokke_DATA += mod/lokke/collection.scm
module_mod_lokke_DATA += mod/lokke/compare.scm
module_mod_lokke_DATA += mod/lokke/compat.scm
module_mod_lokke_DATA += mod/lokke/compile.scm
module_mod_lokke_DATA += mod/lokke/concurrent.scm
module_mod_lokke_DATA += mod/lokke/core.scm
module_mod_lokke_DATA += mod/lokke/exception.scm
module_mod_lokke_DATA += mod/lokke/fash.scm
module_mod_lokke_DATA += mod/lokke/hash-map.scm
module_mod_lokke_DATA += mod/lokke/hash-set.scm
module_mod_lokke_DATA += mod/lokke/lang.scm
module_mod_lokke_DATA += mod/lokke/main.scm
module_mod_lokke_DATA += mod/lokke/metadata.scm
module_mod_lokke_DATA += mod/lokke/ns.scm
module_mod_lokke_ns_clojure_DATA += mod/lokke/ns/clojure/core.clj
module_mod_lokke_ns_clojure_core_DATA += mod/lokke/ns/clojure/core/epl.clj
module_mod_lokke_ns_clojure_DATA += mod/lokke/ns/clojure/string.scm
module_mod_lokke_ns_clojure_DATA += mod/lokke/ns/clojure/test.scm
module_mod_lokke_ns_clojure_DATA += mod/lokke/ns/clojure/walk.clj
module_mod_lokke_ns_clojure_DATA += mod/lokke/ns/clojure/zip.clj
module_mod_lokke_ns_lokke_DATA += mod/lokke/ns/lokke/exception.scm
module_mod_lokke_ns_lokke_DATA += mod/lokke/ns/lokke/io.scm
module_mod_lokke_ns_lokke_DATA += mod/lokke/ns/lokke/math.scm
module_mod_lokke_DATA += mod/lokke/pcre2.scm
module_mod_lokke_DATA += mod/lokke/pr.scm
module_mod_lokke_DATA += mod/lokke/reader.scm
module_mod_lokke_reader_DATA += mod/lokke/reader/literal.scm
module_mod_lokke_DATA += mod/lokke/regex.scm
module_mod_lokke_DATA += mod/lokke/repl.scm
module_mod_lokke_scm_DATA += mod/lokke/scm/atom.scm
module_mod_lokke_scm_DATA += mod/lokke/scm/bit.scm
module_mod_lokke_scm_DATA += mod/lokke/scm/core.scm
module_mod_lokke_scm_DATA += mod/lokke/scm/foreign-object.scm
module_mod_lokke_scm_DATA += mod/lokke/scm/test-anything.scm
module_mod_lokke_scm_DATA += mod/lokke/scm/vector.scm
module_mod_lokke_DATA += mod/lokke/set.scm
module_mod_lokke_DATA += mod/lokke/symbol.scm
module_mod_lokke_DATA += mod/lokke/sysenv.scm
module_mod_lokke_DATA += mod/lokke/transmogrify.scm
module_mod_lokke_DATA += mod/lokke/user.scm
module_mod_lokke_DATA += mod/lokke/vector.scm
# End of automatically generated section


export LOKKE_TEST_PROTOCOL = tap

TESTS = \
  test/equality \
  test/clojure-comparisons \
  test/clojure-defmacro \
  test/clojure-dynamic \
  test/clojure-evaluation \
  test/clojure-exceptions \
  test/clojure-ns \
  test/clojure-string \
  test/clojure-vector \
  test/clojure-pr \
  test/lokke-compare \
  test/lokke-core \
  test/lokke-destructure \
  test/lokke-exception \
  test/lokke-hash-map \
  test/lokke-hash-set \
  test/lokke-ns \
  test/lokke-pr \
  test/lokke-scm-atom \
  test/lokke-scm-vector \
  test/lokke-symbol \
  test/metadata-handling \
  test/ns-lokke-io \
  test/reader-basics \
  test/clojure-collection \
  test/clojure-destructure \
  test/clojure-fn \
  test/clojure-math \
  test/clojure-metadata \
  test/clojure-walk \
  test/clojure-zip \
  test/standalone-install


SUFFIXES = .x
.c.x:
	guile-snarf -o $@ $< @GUILE_CFLAGS@ $(AM_CFLAGS)

lokke: gen-lokke config.status Makefile
	./gen-lokke '$(GUILE)' '' '' > $@.tmp.$$PID
	chmod 0755 $@.tmp.$$PID
	mv $@.tmp.$$PID $@
CLEANFILES += lokke lokke.tmp.*

# Install a slightly different version of ./lokke
# moduledir is defined by gen-makefile
# FIXME: tmp files?
install-data-hook:
	$(install_sh) -d "$(DESTDIR)$(bindir)"
	./gen-lokke '$(GUILE)' '$(vpkglibdir)' '$(moduledir)' \
	  > "$(DESTDIR)$(bindir)"/lokke.tmp
	chmod 0755 "$(DESTDIR)$(bindir)"/lokke.tmp
	mv "$(DESTDIR)$(bindir)"/lokke.tmp "$(DESTDIR)$(bindir)"/lokke

clean-local:
	rm -rf $(lokke_clean)
