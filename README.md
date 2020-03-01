Lokke is Clojure for Guile
==========================

> Note: everything is currently very experimental, including the
> various API "levels" -- they may not all survive, depending in part
> on whether or not they end up seeming valuable enough to be worth
> the maintenance costs.

While the intention is to provide a full dialect of Clojure for Guile,
[Lokke](#Lokke-Danish) also consists of a set of Guile modules
providing much of Clojure's functionality in two different guises.

For Clojure itself
------------------

The Clojure dialect is currently available via `./lokke` which can run
code or provide a REPL.  See [below](#getting-started) for further
information.

For a more Clojure oriented experience in Scheme
------------------------------------------------

Lokke provides one set of modules, starting with (lokke core) that
create an environment that looks suspiciously like Clojure.  In most
cases these modules prefer Clojure's approach when there's a conflict,
and they're not shy about using generic functions.  For example, `=`
is a generic, and implements Clojure's semantics, not Scheme's, and
currently `(lokke core)` and `(lokke base syntax)`, will replace `let`
and `when`, via `use-modules`, so be cautious.  We recommend following
the Clojure practice of explicitly `#:select`ing the symbols you want
to import, or `#:prefix`ing the entire namespace to avoid the problem
entirely, and to make it much easier to discover the origin of a
binding.  In some cases, Scheme vectors may be required in place of
Clojure's immutable vectors, e.g. `(fn #(x 1 y 2) ...)`, though the
approach to these binding forms on the scheme side is still under
discussion and may change.

For a more Scheme oriented experience in Scheme
-----------------------------------------------

Lokke may also provide (lokke scm ...) modules, which define a more
Scheme-friendly, and possibly more efficient interface -- think
`(lokke-vector-length v)` as compared to `(count v)`.

Perhaps the most notable existing module is `(lokke scm vector)` which
is intended to provide a C backed implementation of Clojure's
[persistent vectors](https://hypirion.com/musings/understanding-persistent-vector-pt-1).

Getting started
---------------

Ensure that a version of
[Guile](https://www.gnu.org/software/guile/) 2.2 is available:

    $ guile --version
    guile (GNU Guile) 2.2.4
    Packaged by Debian (2.2.4-deb+1-3)
    Copyright (C) 2018 Free Software Foundation, Inc.
    ...

For Debian:

    # apt-get install guile-2.2 guile-2.2-dev
    # apt-get install gnulib libunistring-dev

Then

    $ git clone ... lokke
    $ cd lokke
    $ ./setup-deps
    $ autoreconf -fi
    $ ./configure
    $ make check

Hopefully the tests will pass.  If not, please report them to
... (FIXME: list address).

Now you should be able to run a Clojure program via the convenience
wrapper:

    $ ./lokke -i hello.clj
    ...
    hello

or run the REPL:

    $ ./lokke
    ...
    lokke@(lokke user)>

Currently the Lokke repl *is* the Guile repl, with the initial
language and environment set for Lokke, and so all of the Guile
features should be available.  Though for now, `lokke` loads
`~/.lokke_guile` (which must be Scheme code) rather than `~/.guile`.

Lokke expects all Clojure namespaces to be located in a lokke/ns/
subdirectory of one of the directories specified by the Guile load
path, which can be adjusted by setting `GUILE_LOAD_PATH` in the
environment.  For example, since `./mod` is in `./lokke`'s default
load path, `clojure.string` can be loaded from
mod/lokke/ns/clojure/string.scm (or `.clj` -- namspaces can be
implemented in Clojure or Scheme).

Assuming your guile was compiled with readline support, it's likely
you'll want to add something like this to `~/.lokke_guile`:

    ;;; -*-scheme-*-
    (use-modules (ice-9 readline))
    (activate-readline)

The REPL history will be stored in the file indicated by the
environment variable `LOKKE_HISTORY` if set, otherwise
`~/.lokke_history`.

There is also a `./guile` wrapper which just runs Guile with the
correct environment for Lokke (and which `./lokke` relies on).  It can
be useful during developement, or if you would like to try out the
Scheme specific facilities:

    $ ./guile
    ...
    scheme@(guile-user)> (use-modules (lokke core))
    scheme@(guile-user) [1]> (take 3 (repeat "?"))
    $1 = #<<class> <lazy-seq> 55bdaff362c0 ("?" "?" "?")>

As you can see, seqs are not written like lists.  Currently the Scheme
`write` representation of many Clojure objects is intentionally
distinct.  Of course `prn` from `(lokke core)` prints the Clojure
representation.

From `./guile`, you can switch to a Lokke REPL manually like this:

    scheme@(guile-user)> ,module (lokke user)
    scheme@(lokke user)> ,language lokke
    Happy hacking with Lokke, a Clojure dialect!  To switch back, type `,L scheme'.
    lokke@(lokke user)> (inc 1)
    $1 = 2

See the DESIGN document for an overview of the implementation.


Differences from Clojure/JVM (an incomplete list)
-------------------------------------------------

* Guile specific information should be available in the
  [Guile Reference Manual](https://www.gnu.org/software/guile/manual/guile.html)
  which should also be available via `info guile` if installed.
* The implementation should be properly tail-recursive.
* Argument evaluation order is unspecified.
* Various functions handle Scheme vectors as they would Clojure
  vectors, i.e. many collection and sequence operations, etc.
* Various functions handle Scheme lists as they would Clojure
  lists, i.e. many collection and sequence operations, etc.
* Multiple `:as` aliases are allowed in destructuring forms.
* The numeric tower is Guile's (fairly sophisticated) numeric tower,
  backed by GMP, and there is currently no distinction between
  functions like `+'` and `+`, or `*'` and `*`, etc.
* At the moment, `format` strings are Guile format strings, but we may
  want to alter or agument that, i.e. perhaps we'll want a formatter
  specifying print or pr format output, though for now pr(int)-str works
  `(format "... ~a ..." (pr-str x))`.
* The reader functions, `read`, `read-string`, etc. return the rnrs
  end-of-file object rather than throwing an exception.
* Lokke's reader conditional identifier is `:cljl`, e.g. `#?(:cljl x)`.
* At the moment reader conditionals are always supported by the reader
  functions and are not restricted to `.cljc` files.
* Uses Scheme's `quotient`, `remainder`, and `modulus` for `quot`,
  `rem`, and `mod`.
* Number is taken to mean <number> (i.e. objects satisfying number?).
* Clojure namespaces *are* Guile modules (which have very comparable
  semantcs), and the Clojure namespace is situatied under `(lokke ns)`
  in the Guile module tree, i.e. `clojure.string` is implemented by the
  `(lokke ns clojure string)` module.
* Qualified Clojure references like the `clojure.string/join` in
  `(clojure.string/join ...)` automatically resolve to the appropriate
  module reference.
* All clojure namspaces starting with `guile` represent direct
  references to the guile module tree,
  e.g. `(guile.guile/current-time)` or `(guile.ice-9/pretty-print
  ...)`.  More specifically, they provide a convenient way to refer to
  guile module references, avoiding the `(lokke ns)` prefix addition.
* `(alias ...)` calls only take full effect at the end of the
  enclosing top level form (because at the moment, the compiler works
  from a snapshot of the alias map, cf. `rewrite-il-calls`).
* Metadata support is very limited.  It currently only works for vars,
  namespaces, and atoms.
* Reader metadata support is evem more limited.
* Dynamic variables must be declared via `(defdyn name init-expr)`
  rather than via metadata, and they are always inherited by
  sub-threads, unlike on the JVM, where only some forms provide
  "binding conveyance".
* You can define dynamic variables that do not convey via `defdynloc`.
* Whether or not `bindings` are established in parallel is undefined.
* `.indexOf` is `index-of`
* `.lastIndexOf` is `last-index-of`
* regex support is provided by guile's facilities on the current host
* regex "Matcher" semantics are not supported yet.
* Many of the coercions haven't been included: float double
* Persistent lists are currently not `counted`, so `count` must
  traverse the list.
* No agents or refs yet.
* `deftest` is very little more than a `do` right now, i.e. it
  executes immediately, there's no support for `*load-tests*`, and it
  doesn't create a test function to run later.
* No BigDecimal (decimal?, bigdec, etc.).
* No support for BASErNUM bases over 16.
* No bigint syntax, e.g. 7N, nor explicit bigints
* No BigDecimal syntax, e.g. 4.2M
* For now, types are implemented via GOOPS which means that you can
  actually modify them via slot-set!.  We may eventually pursue
  immutable GOOPS classes in Guile, but of course you can modify
  anything on the JVM too if you really set your mind to it.
* In addition to nil, the `lokke` command's `-e` option doesn't print
  unspecified values (Guile's `*unspecified*`).
* `lokke.io` is the parallel of `clojure.java.io`.
* There is experimental support for `try/catch/finally` which maps
  very closely to Guile's underlying `catch/throw`, meaning that in
  addition to catching an `ex-info` exception via `(catch
  ExceptionInfo ex ...)`, you can also catch Guile exceptions if you
  know the appropriate tag (symbol) with `(catch 'something ex ...)`.

  When an exception is caught, `ex` will be bound a Scheme list of
  exactly the arguments that were passed to Guile's throw.  For
  `ex-info` exceptions, it will currently be a list starting with the
  (uninterned) tag that is bound to `ExceptionInfo`, which is why
  `(catch ExceptionInfo ex ...)` (no quote) works.  Access the
  elements of `ex-info` exceptions via the normal accessors:
  `ex-message`, `ex-data`, etc.  The `lokke.exception` namespace
  also provides an `ex-info?` predicate.
* Lokke's `ex-info` exceptions have experimental support for
  suppressing exceptions, a concept also found on the JVM and in
  Python, though the details vary.  If an exception is thrown from
  within a `finally` block, and there was a pending `ex-info`
  exception, the exception that was thrown from the `finally` block
  will be added to the `ex-info` as a suppressed exception, and the
  `ex-info` exception will be rethrown.  The collection of suppressed
  exceptions can be retrieved with the `ex-suppressed` function
  provided by `lokke.exception`.  A suppressed exception can be added
  to an `ex-info` exception using the `add-suppressed` function in
  that same namespace.  Note that `add-suppressed` is persistent,
  returning a new `ex-info` exception that may or may not share
  structure with the original, rather than mutating the original.

  As an example:
  ```clojure
    (try
      (print-masterpiece)  ; Throws lp0-on-fire
      (finally
        (turn-off-light)))  ; Throws switch-broken
  ```
  At this point, without suppression you'd know that you need to fix
  your light switch, but have no idea that your printer is on fire.
  But with suppression, that information is preserved:
  ```clojure
    (try
      (print-masterpiece)  ; Throws lp0-on-fire
      (finally
        (turn-off-light)))  ; Throws lp0-on-fire, with switch-broken
                            ; available via (ex-suppressed lp0-on-fire).
  ```
  At least for now, if the pending exception is not an `ex-info`
  exception, then there will be no suppression, and the original
  exception will be lost (as is the case for Java and Clojure/JVM).

  The JVM provides a [related precedent](https://docs.oracle.com/javase/8/docs/api/java/lang/Throwable.html#addSuppressed-java.lang.Throwable-]),
  though it only applies to `try-with-resources` constructs.

  See DESIGN for further details.
- `with-final` is available in `lokke.exception` for more flexible
  resource management.  For example:
  ```clojure
    (defn start-server [...]
      (with-final [foo (open-foo ...) :error close
                   bar (connect-bar ...) :error disconnect
                   ...]
        ...do many things...
        {:foo foo :bar bar ...}))

    (defn stop-server [info]
      (with-final [_ (:foo info) :always close
                   _ (:bar info) :always disconnect
                   ...]
        true)
  ```

On the Scheme side
------------------

* #nil is nil
* There is no `do`, only `begin`.
* Lists are Guile lists.
* Strings are Guile strings.
* As with Clojure and `:refer`, explicit symbol imports are
  recommended, e.g. `#:use-module ((foo) #:select (x))` rather than
  just `#:use-module (foo)`, and Lokke modules assume this is the norm.
* Currently `equal?` is only augmented to handle new types like
  hash-map, hash-set, etc.
* Currently Clojure `=` is only available as an export from `(lokke
  core)`, and for now, it is implemented via `clj=`.
* Bindings starting with /lokke/ are reserved (but they're illegal in
  Clojure anyway).  We use them for internal compiler-communication,
  among other things.  See the DESIGN document for more information.
* In the `(lokke scm)` apis, Scheme vectors are referred to as vector,
  Clojure's as lokke-vector.
* The `num` method can be used to convert characters or any
    <number> to a number.  Characters are converted via Guile's
    `char->integer`.
* The `integer` method is effectively `(truncate (num x))`, using
  Guile's `truncate`.
* For now, `bit-test` treats negative values as twos-complement.
* We prefer to follow the Clojure convention of explcitly `#:select`ing
  symbols for import most of the time.
* We prefer to format module declarations along the same lines
  suggested here: https://stuartsierra.com/2016/clojure-how-to-ns.html

Known Issues
------------

- *Many* things are still broken or incomplete.

- No `rseq` yet.

- May be missing important specializations for say collection/seq
  operations where the fallback is a lot more expensive.

- Unimplemnted syntaxes:
    negative exponents -1.2e-5

- The code has not been evaluated with respect to the need for
  continuation barriers yet.

- atom semantics may not be completely right yet (see code).

- quote is quoting /lokke/reader-vector too, e.g. (prn '[42])

- The `foo.bar/baz` syntactic sugar doesn't work for Scheme modules
  from the ./lokke REPL.  I suspect it's using the Lokke reader to
  load the module instead of the Scheme reader.

- The syntaxes probably aren't always consistent on the scheme side,
  i.e. do we want to support `(fn #(...) ...)` or `(fn (...) ...)` if
  either?  Presumably all destructuring bindings should work the same.

- See DESIGN for additional issues.


Contributing
------------

Patches to Lokke should have a Signed-off-by: header.  Including this
header in your patches signifies that you are licensing your changes
under the terms described in the License section below.  If you like,
git can can add the appropriate pseudo-header for you via the
`--signoff` argument to commit, amend, etc.

Additional information, conventions, etc. may be found in the Hacking
section in the DESIGN.

Lokke (Danish)
--------------

### Etymology

From Old Norse lokka. Cognate with German locken.
Pronunciation

  * IPA: /lɔkə/, [ˈlʌɡ̊ə]

### Verb

**lokke** ‎(imperative **lok**, infinitive **at lokke**, present tense **lokker**, past
tense **lokkede**, past participle **har lokket**)
  1. tempt, entice, lure, seduce
  2. persuade, coax, cajole, wheedle, inveigle

### Source

Definition provided by the [wiktionary](https://en.wiktionary.org/wiki/lokke).
([CC BY-SA 3.0](https://creativecommons.org/licenses/by-sa/3.0/))

License
-------

This project is free software; unless otherwise specified you can
redistribute it and/or modify it under the terms of (at your option)
either of the following two licences:

  1) The GNU Lesser General Public License as published by the Free
     Software Foundation; either version 2.1, or (at your option) any
     later version

  2) The Eclipse Public License; either version 1.0 (EPL-1.0) or (at
     your option) any later version.

Any files covered by another license include a suitable disclaimer
describing the applicable terms, including, but not limited to:

  - The file lib/lokke-reader.c
  - The file mod/fhash.scm
  - Any file named epl.clj
  - mod/lokke/clojure/ns/walk.clj
  - test/clojure-walk

Copyright © 2015-2019 Rob Browning <rlb@defaultvalue.org>

Local Variables:
mode: markdown
End:
