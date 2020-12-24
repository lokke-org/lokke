Lokke is Clojure for Guile
==========================

[Lokke](#Lokke-Danish) intends to provide a full dialect of Clojure
for Guile.  It also consists of a set of Guile modules providing some
of Clojure's functionality in two different guises.

> Note: everything is currently very experimental, including the
> various API "levels" -- they may not all survive, depending in part
> on whether or not they end up seeming valuable enough to be worth
> the maintenance costs.

For Clojure itself
------------------

The Clojure dialect is currently available via `./lokke` which can run
code, serve as a `!#` interpreter, or provide a REPL.  See
[below](#getting-started) for further information.

For a more Clojure oriented experience in Scheme
------------------------------------------------

Lokke provides one set of modules, starting with `(lokke core)` that
create an environment that looks suspiciously like Clojure.  In most
cases these modules prefer Clojure's approach when there's a conflict,
and they're not shy about using generic functions.  For example, `=`
is a generic, and implements Clojure's semantics, not Scheme's, and
currently `(lokke core)` and `(lokke base syntax)`, will replace `let`
and `when`, via `use-modules`, so be cautious.  We recommend following
the Clojure practice of explicitly `#:select`ing the symbols you want
to import, (or `#:prefix`ing the namespace) to avoid the problem
entirely, and to make it much easier to discover the origin of a
binding.  In some cases, Scheme vectors may be required in place of
Clojure's immutable vectors, perhaps something like `(let #(x 1 y 2)
...)`, though the approach to these binding forms on the Scheme side
is still experimental.

For a more Scheme oriented experience in Scheme
-----------------------------------------------

Lokke may also provide `(lokke scm ...)` modules, which define a more
Scheme-friendly, and possibly more efficient interface -- think
`(lokke-vector-length v)` as compared to `(count v)`.  Perhaps the
most notable existing module is `(lokke scm vector)` which intends to
provide a C backed implementation of Clojure's [persistent vectors](https://hypirion.com/musings/understanding-persistent-vector-pt-1).

Getting started
---------------

Ensure that a version of [Guile](https://www.gnu.org/software/guile/)
2.2 or 3.0 is available, preferring 3.0 which should perform notably
better:

    $ guile --version
    guile (GNU Guile) 3.0.4
    Packaged by Debian (3.0.4-deb+3.0.4-3)
    Copyright (C) 2020 Free Software Foundation, Inc.
    ...

For Debian:

    # apt-get install autoconf automake libpcre2-dev libunistring-dev
    # apt-get install  make gcc git gnulib

and then for Guile 3.0:

    # apt-get install guile-3.0 guile-3.0-dev

or for Guile 2.2:

    # apt-get install guile-2.2 guile-2.2-dev

and then:

    $ git clone ... lokke
    $ cd lokke
    $ ./setup
    $ autoreconf -fi
    $ ./configure
    $ make check

Hopefully the tests will pass.  If not, please report them to
... (FIXME: list address).

If you have more than one version of Guile installed, you may be able
to select a particular version at configuration time like this:

    $ ./configure GUILE_EFFECTIVE_VERSION=3.0

At this point you should be able to run a Clojure program like this:

    $ ./lok -l hello.clj
    ...
    hello

or run the REPL:

    $ ./lok
    ...
    lokke@(lokke user)>

`./lok ...` is equivalent to `./lokke run ...`.

Currently the Lokke REPL *is* the Guile REPL, with the initial
language and environment set for Lokke, and so all of the Guile
features should be available.  Though for now, `lokke` loads
`~/.lokke_guile` (which must be Scheme code) rather than `~/.guile`.

See `./lokke --help` or `man -l ./lokke.1` for additional information,
or if `man -l` isn't available, perhaps something like `nroff -man
lokke.1 | less`.

Lokke expects all Clojure namespaces to be located in a lokke/ns/
subdirectory of one of the directories specified by the Guile load
path, and the path can be adjusted by setting `GUILE_LOAD_PATH` in the
environment.  For example, since `./mod` is in `./lokke`'s default
load path, Lokke will look for `clojure.string` in
mod/lokke/ns/clojure/string.go (compiled version),
mod/lokke/ns/clojure/string.clj, and mod/lokke/ns/clojure/string.clj
in that order (namspaces can be implemented in Clojure or Scheme).

Assuming your guile was compiled with readline support, it's likely
you'll want to add something like this to `~/.lokke_guile`:

    ;;; -*-scheme-*-
    (use-modules (ice-9 readline))
    (activate-readline)

The REPL history will be stored in the file indicated by the
environment variable `LOKKE_HISTORY` if set, otherwise
`$XDG_CACHE_HOME/lokke/history` if `$XDG_CACHE_HOME` is set, otherwise
`~/.cache/lokke/history`.

There is also a `./guile` wrapper which just runs Guile with the
correct environment for Lokke (and which `./lokke` relies on).  It can
be useful during development, or if you would like to try out the
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

See the [DESIGN](DESIGN.md) document for an overview of the
implementation.

Detailed information about Guile is available in the
[Guile Reference Manual](https://www.gnu.org/software/guile/manual/guile.html)
which should also be available via `info guile` if installed.

General comparison with Clojure/JVM
-----------------------------------

* The implementation should be properly tail-recursive.

* Argument evaluation order is unspecified.

* Clojure namespaces may be implemented in either Clojure (.clj) or
  Scheme (.scm).

* Clojure namespaces *are* Guile modules with the entire Clojure
  namespace tree situated under `(lokke ns)` in the Guile module
  tree.

* Lokke's reader conditional identifier is `:cljl`, for example,
  `#?(:cljl x)`, and at the moment reader conditionals are always
  supported by the reader functions and are not restricted to `.cljc`
  files.

* There are no agents or refs yet.

* At the moment, `format` strings are
  [Guile `format`](https://www.gnu.org/software/guile/manual/html_node/Formatted-Output.html)
  strings.

* The default regular expressions are
  [PCRE2](http://www.pcre.org/current/doc/html/pcre2pattern.html)
  regular expressions, and right now, reader literal patterns `#"x"`
  currently just translate to an equivalent `(re-pattern ...)` at read
  time.  That is, they are not compiled at read time, and so are
  re-evaluated.

* `lokke.io` is analogous `clojure.java.io`, and `lokke.shell` is
  analogous to `clojure.java.shell`.

* There is some experimental, rudimentary
  [compability with Clojure/JVM exception handling](#exception-handling).

* Currently, a Lokke `future` is a Guile `<future>`, which means that
  it draws from a fixed-size thread pool of size `(dec
  (current-processor-count))` while recent versions of Clojure/JVM
  have an unbounded pool.

* Metadata support is limited: vectors, hash-sets, and hash-maps,
  vars, namespaces, and atoms are supported, lists and symbols are
  not.  Often metadata will just be discarded when it's unsupported.

* Persistent lists are currently not `counted?`, so `count` must
  traverse the list.

* [Dynamic variables](#dynamic-variables-and-binding) and `binding`
  behave a bit differently, and they must be defined via `defdyn`.

* The numeric tower is
  [Guile's](https://www.gnu.org/software/guile/manual/html_node/Numerical-Tower.html),
  backed by [GMP](https://gmplib.org/), and there is currently no
  distinction between functions like `+'` and `+`, or `*'` and `*`,
  etc.

* There are no explicit bigints or BigDecimal (bigint, decimal?,
  bigdec, 7N, 4.2M, etc.), but of course arbitrarily large integers
  are supported.

* The integer syntax does not yet support BASErNUM bases over 16.

* Rather than throwing an exception, the reader functions, `read`,
  `read-string`, etc. return the rnrs end-of-file object, which can be
  identified with `guile.guile/eof-object?`.

* There are some differences and limitations with respect to the
  handling of [comparisons, hashes, and equality](#comparisons-hashing-and-equality).

* `fn` condition maps (i.e. `:pre` `:post`, etc.) are currently ignored.

* `deftest` is little more than a `do`, i.e. it executes immediately,
  there's no support for `*load-tests*`, and it doesn't create a test
  function to run later.

* For now, types are implemented via
  [GOOPS](https://www.gnu.org/software/guile/manual/html_node/GOOPS.html)
  which means that you can actually modify them via `slot-set!`.  We
  may eventually pursue immutable GOOPS classes in Guile, but of
  course you can modify anything on the JVM too, if you really set
  your mind to it.

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

* Bindings starting with `/lokke/` are reserved (but they're illegal
  in Clojure anyway).  We use them for internal
  compiler-communication, among other things.  See the DESIGN document
  for more information.

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


Clojure namespaces and Guile modules
------------------------------------

Clojure namespaces *are* Guile modules (which have very comparable
semantics), and the entire Clojure namespace tree is situatied under
`(lokke ns)` in the Guile module tree, i.e. `clojure.string` is
implemented by the `(lokke ns clojure string)` module, and
`clojure.string/join` is exactly equivalent to a Guile reference to
the `join` function in the `(lokke ns clojure string)` module.

All clojure namspaces starting with `guile.` represent direct
references to the root of the guile module tree.  For example,
`(guile.guile/current-time)`, `(guile.srfi.srfi-19/current-date)`, or
`(guile.guile/delete-file ...)`.  More specifically, they provide a
convenient way to refer to modules that are not under a `(lokke ns
...)` prefix, and of course you can use them in forms like `ns` and
`require`.

`(alias ...)` calls only take full effect at the end of the
enclosing top level form (because at the moment, the compiler works
from a snapshot of the alias map, cf. `rewrite-il-calls`).

Exception handling
------------------

There is experimental support for `try/catch/finally` which maps very
closely to Guile's underlying `catch/throw`, meaning that in addition
to catching an `ex-info` exception via `(catch ExceptionInfo ex ...)`,
you can also catch Guile exceptions if you know the appropriate tag
(symbol) with `(catch 'something ex ...)`.

When an exception is caught, `ex` will be bound a Scheme list of
exactly the arguments that were passed to Guile's throw.  For
`ex-info` exceptions, it will currently be a list starting with the
(uninterned) tag that is bound to `ExceptionInfo`, which is why
`(catch ExceptionInfo ex ...)` (no quote) works.  Access the elements
of Lokke-specific exceptions via the normal accessors: `ex-message`,
`ex-data`, etc.  The `lokke.exception` namespace also provides
bindings like `ex-info?`, `ex-cause`, etc.

Note however, that
[changes introduced in Guile 3.0](https://www.gnu.org/software/guile/manual/html_node/Exceptions.html#Exceptions)
may prompt a rework, perhaps to based our exception handling on
`raise-exception`, `with-exception-handler`, and exception objects.
Consider the current support very unstable.

Guile exception keys that map to `&error` in Guile 3.0 and above are
currently caught as `Error`s, even with Guile 2.2, and even though we
don't use the newer style exception objects yet.

### Exception suppression

Lokke's exceptions (`ExceptionInfo`, `Throwable`, etc.) have
experimental support for suppressing exceptions, a concept also found
on the JVM and in Python, though the details vary.  If an exception is
thrown from within a `finally` block, and there was a pending Lokke
exception, the exception that was thrown from the `finally` block will
be added to the exception as a suppressed exception, and the Lokke
exception will be rethrown.  The collection of suppressed exceptions
can be retrieved with the `ex-suppressed` function provided by
`lokke.exception`.  A suppressed exception can be added to an
`ex-info` exception using the `add-suppressed` function in that same
namespace.  Note that `add-suppressed` is persistent, returning a new
`ex-info` exception that may or may not share structure with the
original, rather than mutating the original.

As an example:

```clojure
    (try
      (print-masterpiece)  ; Throws lp0-on-fire
      (finally
        (turn-off-light)))  ; Throws switch-broken
```

At this point, without suppression you'd know that you need to fix
your light switch, but have no idea that your printer is on fire.  But
with suppression, that information is preserved:

```clojure
    (try
      (print-masterpiece)  ; Throws lp0-on-fire
      (finally
        (turn-off-light)))  ; Throws lp0-on-fire, with switch-broken
                            ; available via (ex-suppressed lp0-on-fire).
```

At least for now, if the pending exception is not a Lokke exception,
then there will be no suppression, and the original exception will be
lost (as is the case for Java and Clojure/JVM).

The JVM provides a
[related precedent](https://docs.oracle.com/javase/8/docs/api/java/lang/Throwable.html#addSuppressed-java.lang.Throwable-]),
though it only applies to `try-with-resources` constructs.

See DESIGN for further details.

### with-final

`with-final` is provided by `lokke.exception` for more flexible
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

Dynamic variables and binding
-----------------------------

At the moment, dynamic variables must be declared via `(defdyn name
init-expr)` rather than via metadata, and they are always inherited by
sub-threads, unlike on the JVM, where only some forms provide [binding
conveyance](https://clojure.org/reference/vars#conveyance).  You can
define dynamic variables without conveyance via `defdynloc`.

Whether or not bindings are established in parallel is undefined.

Comparisons, hashing, and equality
----------------------------------

* As on the JVM, `hash` does not produce values *consistent* with `=`
  across Clojure and non-clojure collections, e.g. `(hash [1 2 3])` is
  not likely to be equal to `(hash (guile.guile/vector 1 2 3))`.
  Although as an exception, proper Scheme lists should be handled
  consistently right now, given the way seqs are implemented via
  Scheme `<pair>`s.

* Currently, `hash` values are not cached.  At the moment, they're
  recomputed in full whenever requested, though that's likely to
  change for some types like `hash-map`, `hash-set`, and `vector`.

* `compare` sorts all symbols lexically, without any special
  treatment of namepaces, i.e. `(compare 'z 'x/y)` is negative.  That
  might eventually change for at least Clojure side.

* The `compare` ordering of refs is unspecified; it is not the order
  of their creation.

Additional differences from Clojure/JVM
---------------------------------------

* In addition to `nil`, the `lokke` command's `-e` option doesn't print
  unspecified values (Guile's `*unspecified*`).

* Multiple `:as` aliases are allowed in destructuring forms.

* `quotient`, `remainder`, and `modulus` are Scheme's `quot`, `rem`,
  and `mod`.

* Many of the coercions haven't been included: `float` `double` ...

* Number is taken to mean <number> (i.e. objects satisfying `number?`).

* At the moment, various functions may handle Scheme vectors as they
  would Clojure vectors and Scheme lists as the would clojure lists,
  e.g. some collection and sequence operations, etc.

Known issues
------------

- *Many* things are still broken or incomplete.

- When an error occurs in the REPL, a
  [new (recursive) prompt is created](https://www.gnu.org/software/guile/manual/html_node/Error-Handling.html).
  At the moment, the new prompt will use the Guile printer instead of
  Lokke's.  For example:

        lokke@(lokke user)> true
        $1 = true
        lokke@(lokke user)> (/ :x)
        ice-9/boot-9.scm:1669:16: In procedure raise-exception:
        In procedure /: Wrong type argument in position 1: #:x

        Entering a new prompt.  Type `,bt' for a backtrace or `,q' to continue.
        lokke@(lokke user) [1]> true
        $2 = #t

  An exit back to the top-level prompt will restore the Lokke writer.

- `deref` does not support a timeout for futures yet.

- May be missing important specializations for say collection/seq
  operations where the fallback is a lot more expensive.

- Some versions of Guile prior to 3.0 had a problem with the hash
  function that would cause it to produce a very poor distribution of
  values in some cases (e.g. for symbols and keywords), which is
  likely to decrease performance.  We may attempt to addresss that,
  but for now, prefer Guile 3.0 or newer when possible.

- The code has not been evaluated with respect to the need for
  continuation barriers yet.

- atom semantics may not be completely right yet (see code).

- To the extent that pairs are used right now (and they're currently
  used for lists like `'(1 2 3)`), they don't support metadata, and
  are not `counted?`.

- Lazy sequences (i.e. via `<pair-seq>`) are not counted?.

- The syntaxes probably aren't always consistent on the scheme side,
  i.e. do we want to support `(fn #(...) ...)` or `(fn (...) ...)` if
  either?  Presumably all destructuring bindings should work the same.

- `time` relies on
  [guile's SRFI-19](https://www.gnu.org/software/guile/manual/html_node/SRFI_002d19.html)
  `time-monotonic`, which is not yet truly monotonic.  It's actually
  International Atomic Time.

- See [DESIGN](DESIGN.md) for additional issues.


Contributing
------------

Patches to Lokke should have a Signed-off-by: header.  Including this
header in your patches signifies that you are licensing your changes
under the terms described in the License section below.  If you like,
git can can add the appropriate pseudo-header for you via the
`--signoff` argument to commit, amend, etc.

Additional information, conventions, etc. may be found in the [Hacking
section](DESIGN.md#hacking) in DESIGN.

Additional tests
----------------

You can run the *full* test suite (requires all changes in the working
tree to be committed) like this:

    make LOKKE_TEST_EXT_BEHAVIOR=true check

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
  - mod/lokke/ns/clojure/walk.clj
  - mod/lokke/ns/clojure/zip.clj
  - test/clojure-walk

Copyright © 2015-2020 Rob Browning &lt;<rlb@defaultvalue.org>&gt;

<!--
Local Variables:
mode: markdown
End:
-->
