Lokke is Clojure for Guile
==========================

[Note: everything is currently very experimental, including the
 various API "levels" -- they may or may not all survive, depending in
 part on whether or not they end up seeming valuable enough to be
 worth the maintenance costs.]

While the plan is to create a full dialect of Clojure for Guile, Lokke
also consists of a set of Guile modules providing much of Clojure's
functionality in two different guises.

For a more Clojure oriented experience
--------------------------------------

Lokke provides one set of modules, starting with (lokke core) that
create an environment that looks suspiciously like Clojure.  In most
cases these modules prefer Clojure's approach when there's a conflict,
and they're not shy about using generic functions.  For example, `=`
is a generic, and implements Clojure's semantics, not Scheme's.  In
some cases, Scheme vectors are used in place of Clojure's immutable
vectors, e.g. `(fn #(x 1 y 2) ...)`[1].  Currently `(lokke core)` and
`(lokke base syntax)`, will for example replace `let` and `when`, via
`use-modules`, so be cautious, though we recommend following the
Clojure practice of always explicitly `#:select`ing the symbols you
want to import, or `#:prefix`ing the entire namespace to avoid the
problem entirely, and make it much easier to discover the origin of a
binding.

[1] Right now, it's `(let (...) ...)` but `(fn #(...) ...)`.  The
    latter avoids some syntax ambiguities, and we may well change
    `let` to match.

For a more Scheme oriented experience
--------------------------------------

Lokke may also provide (lokke scm ...) modules, which define a more
Scheme-friendly, and possibly more efficient interface (think
`(lokke-vector-length v)` as opposed to `(count v)`.

Perhaps the most notable existing module is `(lokke scm vector)` which
provides a C backed implementation of Clojure's
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
    $ make bootstrap
    $ make check

Hopefully the tests will pass.  If not, please report them to
... (FIXME: list address).

Now you should be able to run a Clojure program via the convenience
wrapper:

    $ ./lokke -i hello.clj
    ...
    hello

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
* Uses Scheme's `quotient`, `remainder`, and `modulus` for `quot`,
  `rem`, and `mod`.
* Number is taken to mean <number> (i.e. objects satisfying number?).
* Clojure namespaces *are* Guile modules (which have very comparable
  semantcs), and the Clojure namespace is situatied under `(lokke ns)`
  in the Guile module tree, i.e. clojure.string is implemented by the
  `(lokke ns clojure string)` module.
* Qualified Clojure references like the `clojure.string/join` in
  `(clojure.string/join ...)` automatically resolve to the appropriate
  module reference.
* All clojure namspaces starting with `guile` represent direct
  references to the guile module tree,
  e.g. `(guile.guile/current-time)` or `(guile.ice-9/pretty-print
  ...)`.  More specifically, they provide a convenient way to refer to
  guile module references, avoiding the `(lokke ns)` prefix addition.
* Metadata is currently more or less broken/ignored, but some of the
  initial pieces are in place to support improved handling.
* regex support is provided by guile's facilities on the current host
* regex "Matcher" semantics are not supported yet.
* Many of the coercions haven't been included: float double
* No agents or refs yet.
* No BigDecimal (decimal?, bigdec, etc.).
* For now, types are implemented via GOOPS which means that you can
  actually modify them via slot-set!.  We may eventually pursue
  immutable GOOPS classes in Guile, but of course you can modify
  anything on the JVM too if you really set your mind to it.
* atoms don't yet support watchers or metadata.

On the Scheme side
------------------

* #nil is nil
* There is no `do`, only `begin`.
* There is no `@`; only `deref`.
* Lists are Guile lists.
* Strings are Guile strings.
* `.indexOf` is `index-of`
* `.lastIndexOf` is `last-index-of`
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
* We favor SRFI-88 postfix keywords like `foo:`.  While there are no
  immediate plans to port bits to other Schemes, all else equal, we
  might as well use the standard when there is one.  Plus it's easier
  to type and easier on the eyes than `#:foo`.
* We prefer to follow the Clojure convention of explcitly `select:`ing
  symbols for import most of the time.
* We prefer to format module declarations along the same lines
  suggested here: https://stuartsierra.com/2016/clojure-how-to-ns.html
* No support for BASErNUM bases over 16.
* No explicit bigints, e.g. 7N
* no BigDecimal, e.g. 4.2M

Known Issues
------------

- *Many* things are still broken or incomplete.

- There's no plan yet with respect to error handling (exceptions,
  etc.), but see DESIGN for almost a plan to have a plan.

- No `rseq` yet.

- May be missing important specializations for say collection/seq
  operations where the fallback is a lot more expensive.

- Unimplemnted syntaxes:
    hex 0xff
    oct 017
    negative exponents -1.2e-5

- Namespace aliases (i.e. :as) don't work, and we haven't decided how
  we want to handle them yet.

- The code has not been evaluated with respect to the need for
  continuation barriers yet.

- atom semantics may not be completely right yet (see code).

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
  - Any file named epl.clj

Copyright © 2015-2019 Rob Browning <rlb@defaultvalue.org>
