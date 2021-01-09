/// Copyright (C) 2021 Rob Browning <rlb@defaultvalue.org>
/// SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

#define _GNU_SOURCE  // asprintf
#undef NDEBUG

#include <assert.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <libguile.h>

static void
msg(FILE* f, const char * const msg, ...)
{
    if (fputs(LOKKE_PROGRAM_NAME ": ", f) == EOF)
        exit(3);
    va_list ap;
    va_start(ap, msg);;
    if (vfprintf(f, msg, ap) < 0)
        exit(3);
    va_end(ap);
}

static void
setenv_or_die(const char *name, const char *value)
{
    int rc = setenv(name, value, 1);
    if (rc != 0) {
        msg(stderr, "setenv %s=%s failed (%s)\n", name, value, strerror(errno));
        exit(2);
    }
}

#ifdef LOKKE_JUST_RUN_GUILE

static void
inner_main (void *data, int argc, char **argv)
{
  scm_shell (argc, argv);
}

#else

static void
inner_main (void *data, int argc, char **argv)
{
  SCM lokke_main = scm_c_public_ref ("lokke main", LOKKE_MAIN_FN);
  scm_call_1 (lokke_main, scm_program_arguments ());
  // should be unreachable
}

#endif

int
main (int argc, char **argv)
{
  const char * env = getenv ("GUILE_LOAD_PATH");
  if (!env)
    setenv_or_die ("GUILE_LOAD_PATH", LOKKE_MODULE_ROOT);
  else
    {
      char *path;
      int rc = asprintf (&path, "%s:%s", LOKKE_MODULE_ROOT, env);
      assert (rc >= 0);
      setenv_or_die ("GUILE_LOAD_PATH", path);
      free (path);
    }

  env = getenv ("GUILE_LOAD_COMPILED_PATH");
  if (!env)
    setenv_or_die ("GUILE_LOAD_COMPILED_PATH", LOKKE_MODULE_ROOT);
  else
    {
      char *path;
      int rc = asprintf (&path, "%s:%s", LOKKE_COMPILED_ROOT, env);
      assert (rc >= 0);
      setenv_or_die ("GUILE_LOAD_COMPILED_PATH", path);
      free (path);
    }

  env = getenv ("LTDL_LIBRARY_PATH");
  if (!env)
    setenv_or_die ("LTDL_LIBRARY_PATH", LOKKE_LIBDIR);
  else
    {
      char *path;
      int rc = asprintf (&path, "%s:%s", LOKKE_LIBDIR, env);
      assert (rc >= 0);
      setenv_or_die ("LTDL_LIBRARY_PATH", path);
      free (path);
    }

  scm_boot_guile (argc, argv, inner_main, 0);
  // should be unreachable
  return 0;
}
