/* Copyright (C) 1995-1997, 1999-2001, 2003, 2004, 2006-2012, 2014, 2015
 *   Free Software Foundation, Inc.
 * Copyright (C) 2015-2019 Rob Browning <rlb@defaultvalue.org>
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <unicase.h>
#include <unictype.h>
#include <c-strcase.h>
#include <c-ctype.h>
#include <alloca.h>

#include <libguile.h>

SCM_SYMBOL (lokke_sym_reader_anon_fn, "/lokke/reader-anon-fn");
SCM_SYMBOL (lokke_sym_reader_cond, "/lokke/reader-cond");
SCM_SYMBOL (lokke_sym_reader_cond_splice, "/lokke/reader-cond-splice");
SCM_SYMBOL (lokke_sym_reader_meta, "/lokke/reader-meta");
SCM_SYMBOL (lokke_sym_reader_hash_map, "/lokke/reader-hash-map");
SCM_SYMBOL (lokke_sym_reader_hash_set, "/lokke/reader-hash-set");
SCM_SYMBOL (lokke_sym_reader_vector, "/lokke/reader-vector");
SCM_SYMBOL (sym_syntax_quote, "syntax-quote");
SCM_SYMBOL (sym_ISO_8859_1, "ISO-8859-1");

static SCM sym_quote = SCM_ELISP_NIL;
static SCM sym_unquote = SCM_ELISP_NIL;
static SCM sym_uq_splicing = SCM_ELISP_NIL;

struct char_and_c_name_t {
  char *name;
  SCM chr;
};

static struct char_and_c_name_t aliased_characters[] =
  {
   {"\\newline", SCM_MAKE_CHAR('\n')},
   {"\\space", SCM_MAKE_CHAR(' ')},
   {"\\tab", SCM_MAKE_CHAR('\t')},
   {"\\formfeed", SCM_MAKE_CHAR('\f')},
   {"\\backspace", SCM_MAKE_CHAR('\b')},
   {"\\return", SCM_MAKE_CHAR('\r')},
   {0,}
};

scm_t_option scm_read_opts[] =
  {
    { SCM_OPTION_BOOLEAN, "copy", 0,
      "Copy source code expressions." },
    { SCM_OPTION_BOOLEAN, "positions", 1,
      "Record positions of source code expressions." },
    { SCM_OPTION_BOOLEAN, "hungry-eol-escapes", 0,
      "In strings, consume leading whitespace after an escaped end-of-line."},
    { 0, },
  };

/* Internal read options structure.  This is initialized by 'scm_read'
   from the global and per-port read options, and a pointer is passed
   down to all helper functions. */

struct t_read_opts
{
  unsigned int copy_source_p        : 1;
  unsigned int record_positions_p   : 1;
  unsigned int case_insensitive_p   : 1;
  unsigned int hungry_eol_escapes_p : 1;
};

typedef struct t_read_opts scm_t_read_opts;


/*
  Give meaningful error messages for errors

  We use the format

  FILE:LINE:COL: MESSAGE
  This happened in ....

  This is not standard GNU format, but the test-suite likes the real
  message to be in front.

 */


void
scm_i_input_error (char const *function,
		   SCM port, const char *message, SCM arg)
{
  SCM fn = (scm_is_string (scm_port_filename(port))
	    ? scm_port_filename(port)
	    : scm_from_locale_string ("#<unknown port>"));

  SCM string_port = scm_open_output_string ();
  SCM string = SCM_EOL;
  scm_simple_format (string_port,
		     scm_from_locale_string ("~A:~S:~S: ~A"),
		     scm_list_4 (fn,
				 scm_sum (scm_port_line (port), SCM_INUM1),
				 scm_sum (scm_port_column (port), SCM_INUM1),
				 scm_from_locale_string (message)));

  string = scm_get_output_string (string_port);
  scm_close_output_port (string_port);
  scm_error_scm (scm_from_latin1_symbol ("read-error"),
		 function? scm_from_locale_string (function) : SCM_BOOL_F,
		 string,
		 arg,
		 SCM_BOOL_F);
}


/* SCM_DEFINE (scm_read_options, "read-options-interface", 0, 1, 0,  */
/*             (SCM setting), */
/* 	    "Option interface for the read options. Instead of using\n" */
/* 	    "this procedure directly, use the procedures @code{read-enable},\n" */
/* 	    "@code{read-disable}, @code{read-set!} and @code{read-options}.") */
/* #define FUNC_NAME s_scm_read_options */
/* { */
/*   SCM ans = scm_options (setting, */
/* 			 scm_read_opts, */
/* 			 FUNC_NAME); */
/*   if (SCM_COPY_SOURCE_P) */
/*     SCM_RECORD_POSITIONS_P = 1; */
/*   return ans; */
/* } */
/* #undef FUNC_NAME */

/* A fluid referring to an association list mapping extra hash
   characters to procedures.  */
static SCM *scm_i_read_hash_procedures;

static SCM
scm_i_read_hash_procedures_ref (void)
{
  return scm_fluid_ref (*scm_i_read_hash_procedures);
}

static void
scm_i_read_hash_procedures_set_x (SCM value)
{
  scm_fluid_set_x (*scm_i_read_hash_procedures, value);
}


/* Token readers.  */


/* Size of the C buffer used to read symbols and numbers.  */
#define READER_BUFFER_SIZE            128

/* Number of 32-bit codepoints in the buffer used to read strings.  */
#define READER_STRING_BUFFER_SIZE     128

/* The maximum size of Scheme character names.  */
#define READER_CHAR_NAME_MAX_SIZE      50

#define READER_SHARP_SHARP_NAME_MAX_SIZE 32

/* The maximum size of reader directive names.  */
#define READER_DIRECTIVE_NAME_MAX_SIZE 50


/* `isblank' is only in C99.  */
#define CHAR_IS_BLANK_(_chr)					\
  (((_chr) == ' ') || ((_chr) == '\t') || ((_chr) == '\n')	\
   || ((_chr) == '\f') || ((_chr) == '\r'))

#ifdef MSDOS
# define CHAR_IS_BLANK(_chr)			\
  ((CHAR_IS_BLANK_ (chr)) || ((_chr) == 26))
#else
# define CHAR_IS_BLANK CHAR_IS_BLANK_
#endif

#define CHAR_IS_DELIMITER(c)                                    \
  (CHAR_IS_BLANK (c)						\
   || (c) == ')' || (c) == '(' || (c) == ';' || (c) == '"'      \
   || (c) == ']' || (c) == '['                                  \
   || (c) == '}' || (c) == '{')                                 \
   || (c) == ','

#define CHAR_IS_EXPONENT_MARKER(_chr) ((_chr) == 'e')

/* Read an SCSH block comment.  */
static SCM scm_read_scsh_block_comment (scm_t_wchar, SCM);
static SCM scm_read_commented_expression (scm_t_wchar, SCM, scm_t_read_opts *);
static SCM scm_read_shebang (scm_t_wchar, SCM, scm_t_read_opts *);
static SCM scm_get_hash_procedure (int);

/* Read from PORT until a delimiter (e.g., a whitespace) is read.  Put the
   result in the pre-allocated buffer BUF.  Return zero if the whole token has
   fewer than BUF_SIZE bytes, non-zero otherwise. READ will be set the number of
   bytes actually read.  */
static int
read_token (SCM port, scm_t_read_opts *opts,
            char *buf, size_t buf_size, size_t *read)
{
   *read = 0;

   while (*read < buf_size)
     {
       int chr;

       chr = scm_get_byte_or_eof (port);

       if (chr == EOF)
        return 0;
      else if (CHAR_IS_DELIMITER (chr))
        {
          scm_unget_byte (chr, port);
          return 0;
        }
      else
        {
          *buf = (char) chr;
          buf++, (*read)++;
        }
     }

   return 1;
 }

/* Like `read_token', but return either BUFFER, or a GC-allocated buffer
   if the token doesn't fit in BUFFER_SIZE bytes.  */
static char *
read_complete_token (SCM port, scm_t_read_opts *opts,
                     char *buffer, size_t buffer_size, size_t *read)
{
  int overflow = 0;
  size_t bytes_read, overflow_size = 0;
  char *overflow_buffer = NULL;

  do
    {
      overflow = read_token (port, opts, buffer, buffer_size, &bytes_read);
      if (bytes_read == 0)
        break;
      if (overflow || overflow_size != 0)
        {
          if (overflow_size == 0)
            {
              overflow_buffer = scm_gc_malloc_pointerless (bytes_read, "read");
              memcpy (overflow_buffer, buffer, bytes_read);
              overflow_size = bytes_read;
            }
          else
            {
	      char *new_buf =
		scm_gc_malloc_pointerless (overflow_size + bytes_read, "read");

	      memcpy (new_buf, overflow_buffer, overflow_size);
              memcpy (new_buf + overflow_size, buffer, bytes_read);

	      overflow_buffer = new_buf;
              overflow_size += bytes_read;
            }
        }
    }
  while (overflow);

  if (overflow_size)
    *read = overflow_size;
  else
    *read = bytes_read;

  return (overflow_size > 0 ? overflow_buffer : buffer);
}

/* Skip whitespace from PORT and return the first non-whitespace character
   read.  Raise an error on end-of-file.  */
static int
flush_ws (SCM port, scm_t_read_opts *opts, const char *eoferr)
{
  scm_t_wchar c;
  while (1)
    switch (c = scm_getc (port))
      {
      case EOF:
      goteof:
	if (eoferr)
	  {
	    scm_i_input_error (eoferr,
			       port,
			       "end of file",
			       SCM_EOL);
	  }
	return c;

      case ';':
      lp:
	switch (c = scm_getc (port))
	  {
	  case EOF:
	    goto goteof;
	  default:
	    goto lp;
	  case SCM_LINE_INCREMENTORS:
	    break;
	  }
	break;

      case '#':
	switch (c = scm_getc (port))
	  {
	  case EOF:
	    eoferr = "read_sharp";
	    goto goteof;
	  case '!':
	    scm_read_shebang (c, port, opts);
	    break;
	  case ';':
	    scm_read_commented_expression (c, port, opts);
	    break;
	  default:
	    scm_ungetc (c, port);
	    return '#';
	  }
	break;

      case SCM_LINE_INCREMENTORS:
      case SCM_SINGLE_SPACES:
      case '\t':
      case ',':
	break;

      default:
	return c;
      }

  return 0;
}



/* Token readers.  */

static SCM scm_read_expression (SCM port, scm_t_read_opts *opts);
static SCM scm_read_sharp (int chr, SCM port, scm_t_read_opts *opts,
                           long line, int column);


static SCM
maybe_annotate_source (SCM x, SCM port, scm_t_read_opts *opts,
                       long line, int column)
{
  /* This condition can be caused by a user calling
     set-port-column!.  */
  if (line < 0 || column < 0)
    return x;

  // Just making SCM_COPY_SOURC_P true for now...
  if (opts->record_positions_p)
    scm_set_source_properties_x(x, scm_make_srcprops(line, column, scm_port_filename (port),
                                                     scm_copy_tree(x),
                                                     SCM_EOL));
  return x;
}

static SCM
scm_read_sexp (scm_t_wchar chr, int list_for_curly, SCM port, scm_t_read_opts *opts)
#define FUNC_NAME "scm_i_lreadparen"
{
  int c;
  SCM tmp, tl, ans = SCM_EOL;
  const int start_char = chr;
  int terminating_char;
  switch (chr)
    {
    case '{': terminating_char = '}'; break;
    case '[': terminating_char = ']'; break;
    case '(': terminating_char = ')'; break;
    default:
      scm_i_input_error (FUNC_NAME, port,
                         "invalid sexp opening character: ~a",
                         scm_list_1 (SCM_MAKE_CHAR(chr)));
      break;
    }

  /* Need to capture line and column numbers here. */
  long line = scm_to_long (scm_port_line (port));
  int column = scm_to_int (scm_port_column (port)) - 1;

  c = flush_ws (port, opts, FUNC_NAME);
  if (c == terminating_char)
    switch (c)
      {
      case ')': return SCM_EOL; break;
      case ']': return scm_list_1 (lokke_sym_reader_vector);; break;
      case '}':
        if (list_for_curly)
          return SCM_EOL;
        return scm_cons(lokke_sym_reader_hash_map, SCM_EOL);
        break;
      default:
        scm_i_input_error (FUNC_NAME, port,
                           "invalid sexp termination character: ~a",
                           scm_list_1 (SCM_MAKE_CHAR(c)));
        break;
      }

  scm_ungetc (c, port);
  // FIXME: make sure that scm_read_expression eats the terminator.
  tmp = scm_read_expression (port, opts);

  /* Build the head of the list structure. */
  ans = tl = scm_cons (tmp, SCM_EOL);

  while (terminating_char != (c = flush_ws (port, opts, FUNC_NAME)))
    {
      SCM new_tail;

      if (c == ')' || c == ']' || c == '}')
        scm_i_input_error (FUNC_NAME, port,
                           "in pair: mismatched close paren: ~A",
                           scm_list_1 (SCM_MAKE_CHAR (c)));
      scm_ungetc (c, port);
      tmp = scm_read_expression (port, opts);
      new_tail = scm_cons (tmp, SCM_EOL);
      SCM_SETCDR (tl, new_tail);
      tl = new_tail;
    }

  switch (start_char)
    {
    case '[': ans = scm_cons(lokke_sym_reader_vector, ans); break;
    case '{':
      if (!list_for_curly)
        ans = scm_cons(lokke_sym_reader_hash_map, ans);
      break;
    }

  return maybe_annotate_source (ans, port, opts, line, column);
}
#undef FUNC_NAME


// FIXME: string syntax, or maybe just adopt guile's...


/* Read a hexadecimal number NDIGITS in length.  Put its value into the variable
   C.  If TERMINATOR is non-null, terminate early if the TERMINATOR character is
   found.  */
#define SCM_READ_HEX_ESCAPE(ndigits, terminator)                   \
  do                                                               \
    {                                                              \
      scm_t_wchar a;                                               \
      size_t i = 0;                                                \
      c = 0;                                                       \
      while (i < ndigits)                                          \
        {                                                          \
          a = scm_getc (port);                                     \
          if (a == EOF)                                            \
            goto str_eof;                                          \
          if (terminator                                           \
              && (a == (scm_t_wchar) terminator)                   \
              && (i > 0))                                          \
            break;                                                 \
          if ('0' <= a && a <= '9')                                \
            a -= '0';                                              \
          else if ('A' <= a && a <= 'F')                           \
            a = a - 'A' + 10;                                      \
          else if ('a' <= a && a <= 'f')                           \
            a = a - 'a' + 10;                                      \
          else                                                     \
            {                                                      \
              c = a;                                               \
              goto bad_escaped;                                    \
            }                                                      \
          c = c * 16 + a;                                          \
          i ++;                                                    \
        }                                                          \
    } while (0)

static void
skip_intraline_whitespace (SCM port)
{
  scm_t_wchar c;

  do
    {
      c = scm_getc (port);
      if (c == EOF)
        return;
    }
  while (c == '\t' || uc_is_general_category (c, UC_SPACE_SEPARATOR));

  scm_ungetc (c, port);
}

/* Read either a double-quoted string or an R7RS-style symbol delimited
   by vertical lines, depending on the value of 'chr' ('"' or '|').
   Regardless, the result is always returned as a string.  */
static SCM
scm_read_string_like_syntax (int chr, SCM port, scm_t_read_opts *opts)
#define FUNC_NAME "scm_lreadr"
{
  /* For strings smaller than C_STR, this function creates only one Scheme
     object (the string returned).  */

  SCM str = SCM_EOL;
  size_t c_str_len = 0;
  scm_t_wchar c, c_str[READER_STRING_BUFFER_SIZE];

  /* Need to capture line and column numbers here. */
  long line = scm_to_long (scm_port_line (port));
  int column = scm_to_int (scm_port_column (port)) - 1;

  while (chr != (c = scm_getc (port)))
    {
      if (c == EOF)
        {
        str_eof:
          scm_i_input_error (FUNC_NAME, port,
                             (chr == '|'
                              ? "end of file in symbol"
                              : "end of file in string constant"),
                             SCM_EOL);
        }

      if (c_str_len + 1 >= READER_STRING_BUFFER_SIZE)
	{
	  str = scm_cons (scm_from_utf32_stringn (c_str, c_str_len), str);
	  c_str_len = 0;
	}

      if (c == '\\')
        {
          switch (c = scm_getc (port))
            {
            case EOF:
              goto str_eof;
            case '|':
            case '\\':
            case '(':  /* Accept "\(" for use at the beginning of lines
			  in multiline strings to avoid confusing emacs
			  lisp modes.  */
              break;
            case '\n':
              if (opts->hungry_eol_escapes_p)
                skip_intraline_whitespace (port);
              continue;
            case '0':
              c = '\0';
              break;
            case 'f':
              c = '\f';
              break;
            case 'n':
              c = '\n';
              break;
            case 'r':
              c = '\r';
              break;
            case 't':
              c = '\t';
              break;
            case 'a':
              c = '\007';
              break;
            case 'v':
              c = '\v';
              break;
            case 'b':
              c = '\010';
              break;
            case 'x':
              SCM_READ_HEX_ESCAPE (10, ';');
              break;
            default:
              if (c == chr)
                break;
            bad_escaped:
              scm_i_input_error (FUNC_NAME, port,
                                 "illegal character in escape sequence: ~S",
                                 scm_list_1 (SCM_MAKE_CHAR (c)));
            }
        }

      c_str[c_str_len++] = c;
    }

  if (scm_is_null (str))
    /* Fast path: we got a string that fits in C_STR.  */
    str = scm_from_utf32_stringn (c_str, c_str_len);
  else
    {
      if (c_str_len > 0)
	str = scm_cons (scm_from_utf32_stringn (c_str, c_str_len), str);

      str = scm_string_concatenate_reverse (str, SCM_UNDEFINED, SCM_UNDEFINED);
    }

  str = scm_c_substring_read_only (str, 0, scm_c_string_length (str));
  return maybe_annotate_source (str, port, opts, line, column);
}
#undef FUNC_NAME

static SCM
scm_read_string (int chr, SCM port, scm_t_read_opts *opts)
{
  return scm_read_string_like_syntax (chr, port, opts);
}

static inline int
is_latin1_hex_digit(SCM c) {
  switch (SCM_UNPACK(c))
    {
    case SCM_UNPACK(SCM_MAKE_CHAR ('0')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('1')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('2')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('3')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('4')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('5')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('6')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('7')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('8')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('9')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('a')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('b')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('c')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('d')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('e')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('f')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('A')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('B')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('C')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('D')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('E')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('F')):
      return 1;
    }
  return 0;
}

static inline int
is_latin1_plusminus(SCM c) {
  switch (SCM_UNPACK(c))
    {
    case SCM_UNPACK(SCM_MAKE_CHAR ('+')):
    case SCM_UNPACK(SCM_MAKE_CHAR ('-')):
      return 1;
    }
  return 0;
}


static SCM
radix_stripped_substring_to_integer(SCM str, size_t str_len, size_t offset,
                                   int is_negative, SCM radix)
{
  size_t i;
  for (i = offset; i < str_len; i++) {
    if (!is_latin1_hex_digit(scm_c_string_ref(str, i)))
      return SCM_BOOL_F;
  }
  const SCM subs = scm_c_substring_read_only(str, offset, str_len);
  SCM result = scm_string_to_number(subs, radix);
  if (is_negative)
    return scm_product(SCM_I_MAKINUM(-1), result);
  return result;
}

static SCM
sign_stripped_substring_to_integer(SCM str, size_t str_len, size_t offset,
                                  int is_negative, SCM default_radix)
{
  const size_t remaining = str_len - offset;
  if (remaining == 0)
    return SCM_BOOL_F;
  const SCM c0 = scm_c_string_ref(str, offset);
  if (c0 == SCM_MAKE_CHAR ('0')) {
    // oct or hex: 025 or 0xf7
    switch (remaining)
      {
      case 1:
        return SCM_I_MAKINUM(0);
      case 2:  // Only one char, e.g. 07, so can't be hex
        return radix_stripped_substring_to_integer(str, str_len, offset + 1,
                                                   is_negative,
                                                   SCM_I_MAKINUM(8));
      default:
        {
          const SCM c1 = scm_c_string_ref(str, offset + 1);
          if (c1 == SCM_MAKE_CHAR ('x') || c1 == SCM_MAKE_CHAR ('X'))
            return radix_stripped_substring_to_integer(str, str_len, offset + 2,
                                                       is_negative,
                                                       SCM_I_MAKINUM(16));
          else
            return radix_stripped_substring_to_integer(str, str_len, offset + 1,
                                                       is_negative,
                                                       SCM_I_MAKINUM(8));
        }
      }
  }

  const SCM radix_chr = SCM_MAKE_CHAR('r');
  if (remaining > 2 && scm_c_string_ref(str, offset + 1) == radix_chr) {
    const SCM r1 = scm_c_string_ref(str, offset);
    SCM radix;
    if (r1 == SCM_MAKE_CHAR ('2'))
      radix = SCM_I_MAKINUM(2);
    else if (r1 == SCM_MAKE_CHAR ('8'))
      radix = SCM_I_MAKINUM(8);
    else
      return SCM_BOOL_F;
    return radix_stripped_substring_to_integer(str, str_len, offset + 2,
                                               is_negative, radix);
  } else if (remaining > 3 && scm_c_string_ref(str, offset + 2) == radix_chr) {
    const SCM r1 = scm_c_string_ref(str, offset);
    const SCM r2 = scm_c_string_ref(str, offset + 1);
    SCM radix;
    if (r1 == SCM_MAKE_CHAR ('1') && r2 == SCM_MAKE_CHAR ('0'))
      radix = SCM_I_MAKINUM(10);
    else if (r1 == SCM_MAKE_CHAR ('1') && r2 == SCM_MAKE_CHAR ('6'))
      radix = SCM_I_MAKINUM(16);
    else
      return SCM_BOOL_F;
    return radix_stripped_substring_to_integer(str, str_len, offset + 3,
                                               is_negative, radix);
  }
  return radix_stripped_substring_to_integer(str, str_len, offset,
                                             is_negative, default_radix);
}

SCM_DEFINE (cljg_string_to_integer, "string->integer", 1, 1, 0,
            (SCM string, SCM radix), "...")
#define FUNC_NAME s_cljg_string_to_integer
{
  SCM_VALIDATE_STRING(1, string);
  // FIXME: validate radix?  Looks like string->number doesn't...
  radix = SCM_UNBNDP (radix) ? SCM_I_MAKINUM(10) : radix;
  const size_t len = scm_c_string_length (string);
  // FIXME: faster?
  // FIXME: for now this only handles radix 2, 8, 10, or 16
  if (!len)
    return SCM_BOOL_F;
  switch (SCM_UNPACK(scm_c_string_ref (string, 0)))
    {
    case SCM_UNPACK(SCM_MAKE_CHAR ('-')):
      return sign_stripped_substring_to_integer(string, len, 1, 1, radix);
    case SCM_UNPACK(SCM_MAKE_CHAR ('+')):
      return sign_stripped_substring_to_integer(string, len, 1, 0, radix);
    default:
      return sign_stripped_substring_to_integer(string, len, 0, 0, radix);
    }
}
#undef FUNC_NAME

static int
skip_integer_prefix(SCM string, size_t i, size_t len)
{
  size_t skip = 0;
  for (; i < len; i++) {
    switch (SCM_UNPACK(scm_c_string_ref(string, i)))
      {
      case SCM_UNPACK(SCM_MAKE_CHAR ('0')):
      case SCM_UNPACK(SCM_MAKE_CHAR ('1')):
      case SCM_UNPACK(SCM_MAKE_CHAR ('2')):
      case SCM_UNPACK(SCM_MAKE_CHAR ('3')):
      case SCM_UNPACK(SCM_MAKE_CHAR ('4')):
      case SCM_UNPACK(SCM_MAKE_CHAR ('5')):
      case SCM_UNPACK(SCM_MAKE_CHAR ('6')):
      case SCM_UNPACK(SCM_MAKE_CHAR ('7')):
      case SCM_UNPACK(SCM_MAKE_CHAR ('8')):
      case SCM_UNPACK(SCM_MAKE_CHAR ('9')):
        skip++;
        break;
      default:
        return skip;
      }
  }
  return skip;
}

SCM_DEFINE (cljg_string_to_float, "string->float", 1, 0, 0,
            (SCM string), "...")
#define FUNC_NAME s_cljg_string_to_float
{
  SCM_VALIDATE_STRING(1, string);
  // FIXME: safer?
  const size_t len = scm_c_string_length (string);
  size_t i = 0;
  if (!len)
    return SCM_BOOL_F;
  SCM c = scm_c_string_ref(string, 0);
  if (c == SCM_MAKE_CHAR('+') || c == SCM_MAKE_CHAR('-'))
    i++;
  size_t skip = skip_integer_prefix(string, i, len);
  if (!skip)
    return SCM_BOOL_F;
  i += skip;
  if (i >= len)
    return scm_string_to_number(string, SCM_UNDEFINED);
  c = scm_c_string_ref(string, i);
  // optional decimal point
  if (c == SCM_MAKE_CHAR('.')) {
    i++;
    if (i >= len)
      return scm_string_to_number(string, SCM_UNDEFINED);
    skip = skip_integer_prefix(string, i, len);
    i += skip;
    if (i >= len)
      return scm_string_to_number(string, SCM_UNDEFINED);
    c = scm_c_string_ref(string, i);
  }
  if (c != SCM_MAKE_CHAR('e') && c != SCM_MAKE_CHAR('E'))
    return SCM_BOOL_F;
  i++;
  if (i >= len)
    return SCM_BOOL_F;
  c = scm_c_string_ref(string, i);
  if (c == SCM_MAKE_CHAR('+') || c == SCM_MAKE_CHAR('-')) {
    i++;
    if (i >= len)
      return SCM_BOOL_F;
  }
  skip = skip_integer_prefix(string, i, len);
  if (!skip)
    return SCM_BOOL_F;
  i += skip;
  if (i >= len)
    return scm_string_to_number(string, SCM_UNDEFINED);
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (cljg_string_to_rational, "string->rational", 1, 0, 0,
            (SCM string), "...")
#define FUNC_NAME s_cljg_string_to_rational
{
  SCM_VALIDATE_STRING(1, string);
  const size_t len = scm_c_string_length (string);
  int seen_slash = 0;
  size_t i = 0;
  if (!len)
    return SCM_BOOL_F;
  const SCM first_char = scm_c_string_ref(string, 0);
  if (first_char == SCM_MAKE_CHAR('+') || first_char == SCM_MAKE_CHAR('-'))
    i++;
  for (; i < len; i++) {
    switch (SCM_UNPACK(scm_c_string_ref(string, i)))
      {
      case SCM_UNPACK(SCM_MAKE_CHAR ('/')):
        if (seen_slash)
          return SCM_BOOL_F;
        seen_slash = 1;
        break;
      case SCM_UNPACK(SCM_MAKE_CHAR ('0')):
      case SCM_UNPACK(SCM_MAKE_CHAR ('1')):
      case SCM_UNPACK(SCM_MAKE_CHAR ('2')):
      case SCM_UNPACK(SCM_MAKE_CHAR ('3')):
      case SCM_UNPACK(SCM_MAKE_CHAR ('4')):
      case SCM_UNPACK(SCM_MAKE_CHAR ('5')):
      case SCM_UNPACK(SCM_MAKE_CHAR ('6')):
      case SCM_UNPACK(SCM_MAKE_CHAR ('7')):
      case SCM_UNPACK(SCM_MAKE_CHAR ('8')):
      case SCM_UNPACK(SCM_MAKE_CHAR ('9')):
        break;
      default:
        return SCM_BOOL_F;
      }
  }
  return scm_string_to_number(string, SCM_UNDEFINED);
}
#undef FUNC_NAME

static SCM
scm_read_number (scm_t_wchar chr, SCM port, scm_t_read_opts *opts)
{
  SCM result, str = SCM_EOL;
  char local_buffer[READER_BUFFER_SIZE], *buffer;
  size_t bytes_read;

  /* Need to capture line and column numbers here. */
  long line = scm_to_long (scm_port_line (port));
  int column = scm_to_int (scm_port_column (port)) - 1;

  scm_ungetc (chr, port);
  buffer = read_complete_token (port, opts, local_buffer, sizeof local_buffer,
				&bytes_read);

  str = scm_from_port_stringn (buffer, bytes_read, port);

  // FIXME: less dumb...
  result = cljg_string_to_integer (str, SCM_UNDEFINED);
  if (scm_is_false (result))
    result = cljg_string_to_float(str);
  if (scm_is_false (result))
    result = cljg_string_to_rational(str);
  if (scm_is_false (result))
    {
      /* Return a symbol instead of a number */
      if (opts->case_insensitive_p)
        str = scm_string_downcase_x (str);
      result = scm_string_to_symbol (str);
    }
  else if (SCM_NIMP (result))
    result = maybe_annotate_source (result, port, opts, line, column);

  scm_set_port_column_x (port,
                         scm_sum (scm_port_column (port),
                                  scm_string_length (str)));
  return result;
}

static SCM lokke_true;
static SCM lokke_false;
static SCM lokke_nil;

static SCM
scm_read_mixed_case_symbol (scm_t_wchar chr, SCM port, scm_t_read_opts *opts)
{
  SCM result;
  size_t bytes_read;
  char local_buffer[READER_BUFFER_SIZE], *buffer;
  SCM str;

  scm_ungetc (chr, port);
  buffer = read_complete_token (port, opts, local_buffer, sizeof local_buffer,
				&bytes_read);

  str = scm_from_port_stringn (buffer, bytes_read, port);

  if (opts->case_insensitive_p)
    str = scm_string_downcase_x (str);

  if (scm_is_true (scm_equal_p (str, lokke_true)))
      result = SCM_BOOL_T;
  else if (scm_is_true (scm_equal_p (str, lokke_false)))
      result = SCM_BOOL_F;
  else if (scm_is_true (scm_equal_p (str, lokke_nil)))
      result = SCM_ELISP_NIL;
  else
      result = scm_string_to_symbol (str);

  scm_set_port_column_x (port,
                         scm_sum (scm_port_column (port),
                                  scm_string_length (str)));
  return result;
}

static SCM
scm_read_quote (int chr, SCM port, scm_t_read_opts *opts)
{
  SCM p;
  long line = scm_to_long (scm_port_line (port));
  int column = scm_to_int (scm_port_column (port)) - 1;

  switch (chr)
    {
    case '`':
      p = sym_syntax_quote;
      break;

    case '\'':
      p = sym_quote;
      break;

    case '~':
      {
	scm_t_wchar c;

	c = scm_getc (port);
	if ('@' == c)
	  p = sym_uq_splicing;
	else
	  {
	    scm_ungetc (c, port);
	    p = sym_unquote;
	  }
	break;
      }

    default:
      fprintf (stderr, "%s: unhandled quote character (%i)\n",
	       "scm_read_quote", chr);
      abort ();
    }

  p = scm_cons2 (p, scm_read_expression (port, opts), SCM_EOL);
  return maybe_annotate_source (p, port, opts, line, column);
}

static SCM
scm_read_metadata (SCM port, scm_t_read_opts *opts)
{
  long line = scm_to_long (scm_port_line (port));
  int column = scm_to_int (scm_port_column (port)) - 1;
  SCM p = scm_cons2 (lokke_sym_reader_meta,
                     scm_read_expression (port, opts), SCM_EOL);
  return maybe_annotate_source (p, port, opts, line, column);
}

static SCM
scm_read_semicolon_comment (int chr, SCM port)
{
  int c;

  /* We use the get_byte here because there is no need to get the
     locale correct with comment input. This presumes that newline
     always represents itself no matter what the encoding is.  */
  for (c = scm_get_byte_or_eof (port);
       (c != EOF) && (c != '\n');
       c = scm_get_byte_or_eof (port));

  return SCM_UNSPECIFIED;
}

/* If the EXPECTED_CHARS are the next ones available from PORT, then
   consume them and return 1.  Otherwise leave the port position where
   it was and return 0.  EXPECTED_CHARS should be all lowercase, and
   will be matched case-insensitively against the characters read from
   PORT. */
/* static int */
/* try_read_ci_chars (SCM port, const char *expected_chars) */
/* { */
/*   int num_chars_wanted = strlen (expected_chars); */
/*   int num_chars_read = 0; */
/*   char *chars_read = alloca (num_chars_wanted); */
/*   int c; */

/*   while (num_chars_read < num_chars_wanted) */
/*     { */
/*       c = scm_getc (port); */
/*       if (c == EOF) */
/*         break; */
/*       else if (c_tolower (c) != expected_chars[num_chars_read]) */
/*         { */
/*           scm_ungetc (c, port); */
/*           break; */
/*         } */
/*       else */
/*         chars_read[num_chars_read++] = c; */
/*     } */

/*   if (num_chars_read == num_chars_wanted) */
/*     return 1; */
/*   else */
/*     { */
/*       while (num_chars_read > 0) */
/*         scm_ungetc (chars_read[--num_chars_read], port); */
/*       return 0; */
/*     } */
/* } */


/* Sharp readers, i.e. readers called after a `#' sign has been read.  */

static SCM
scm_read_character (scm_t_wchar chr, SCM port, scm_t_read_opts *opts)
#define FUNC_NAME "scm_lreadr"
{
  char buffer[READER_CHAR_NAME_MAX_SIZE];
  SCM charname;
  size_t charname_len, bytes_read;
  SCM cp;
  int overflow;

  overflow = read_token (port, opts, buffer, READER_CHAR_NAME_MAX_SIZE - 1,
                         &bytes_read);
  if (overflow)
    scm_i_input_error (FUNC_NAME, port, "character name too long", SCM_EOL);
  buffer[bytes_read] = '\0';

  if (bytes_read == 0)
    {
      chr = scm_getc (port);
      if (chr == EOF)
	scm_i_input_error (FUNC_NAME, port, "unexpected end of file "
			   "while reading character", SCM_EOL);

      /* CHR must be a token delimiter, like a whitespace.  */
      return (SCM_MAKE_CHAR (chr));
    }

  /* Simple ASCII characters can be processed immediately.  Also, simple
     ISO-8859-1 characters can be processed immediately if the encoding for this
     port is ISO-8859-1.  */
  if (bytes_read == 1 &&
      ((unsigned char) buffer[0] <= 127
       || scm_is_eq (scm_port_encoding(port), sym_ISO_8859_1)))
    {
      scm_set_port_column_x (port, scm_sum (scm_port_column (port), SCM_INUM1));
      return SCM_MAKE_CHAR (buffer[0]);
    }

  /* Otherwise, convert the buffer into a proper scheme string for
     processing.  */
  charname = scm_from_port_stringn (buffer, bytes_read, port);
  charname_len = scm_c_string_length (charname);
  scm_set_port_column_x (port,
                         scm_sum (scm_port_column (port),
                                  scm_from_size_t (charname_len)));
  cp = scm_c_string_ref (charname, 0);
  if (charname_len == 1)
    return cp;

  /* Ignore dotted circles, which may be used to keep combining characters from
     combining with the backslash in #\charname.  */
  if (cp == SCM_MAKE_CHAR(SCM_CODEPOINT_DOTTED_CIRCLE) && charname_len == 2)
    return scm_c_string_ref (charname, 1);

  /* oct: up \000 - \o377 */
  /* hex: \xabcde \xabcde. etc. */
  /* For now, we don't support the clj/jvm's utf-16 \uABCD \uabcd. etc. */
  if (charname_len > 1) {
    SCM p = SCM_BOOL_F;
    if (cp == SCM_MAKE_CHAR('x')) {
      /* Convert from hex, skipping the initial 'x' character in CHARNAME */
      p = scm_string_to_number (scm_c_substring (charname, 1, charname_len),
                                scm_from_uint (16));
    } else if (cp == SCM_MAKE_CHAR('o')) {
      if (charname_len > 4)
        scm_i_input_error (FUNC_NAME, port,
                           "octal escaped character too long: ~a",
                           scm_list_1 (charname));
      p = scm_string_to_number (scm_c_substring (charname, 1, charname_len),
                                scm_from_uint (16));
      if (scm_gr_p(p, SCM_I_MAKINUM(0377)))
        scm_i_input_error (FUNC_NAME, port,
                           "octal escaped character must be in range [0, 377]: ~a",
                           scm_list_1 (charname));
    }

    if (SCM_I_INUMP (p))
      {
        scm_t_wchar c = scm_to_uint32 (p);
        if (SCM_IS_UNICODE_CHAR (c))
          return SCM_MAKE_CHAR (c);
        else
          scm_i_input_error (FUNC_NAME, port,
                             "out-of-range character escape: ~a",
                             scm_list_1 (charname));
      }
  }

  /* aliased characters like \newline \return, etc. */
  static struct char_and_c_name_t *alias = aliased_characters;
  while (alias->name++) {
    if (strcmp(buffer, alias->name) == 0)
      return alias->chr;
  }
  scm_i_input_error (FUNC_NAME, port, "unknown character name ~a",
		     scm_list_1 (charname));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

// FIXME: make this stricter?
static SCM
scm_read_keyword (int chr, SCM port, scm_t_read_opts *opts)
{
  /* Read the symbol that comprises the keyword.  Doing this instead of
     invoking a specific symbol reader function allows `scm_read_keyword ()'
     to adapt to the delimiters currently valid for symbols.

     XXX: This implementation allows sloppy syntaxes like `#:  key'.  */

  SCM name = scm_read_expression (port, opts);
  if (scm_is_keyword (name))  // say via ::foo...
    // FIXME: more efficient
    {
      SCM s = scm_keyword_to_symbol (name);
      s = scm_symbol_to_string (s);
      s = scm_string_append(scm_list_2 (scm_from_latin1_string (":"), s));
      s = scm_string_to_symbol (s);
      return (scm_symbol_to_keyword (s));
    }
  if (!scm_is_symbol (name))
    scm_i_input_error ("scm_read_keyword", port,
		       "keyword prefix `~a' not followed by a symbol: ~s",
		       scm_list_2 (SCM_MAKE_CHAR (chr), name));
  return (scm_symbol_to_keyword (name));
}

static SCM
scm_read_scsh_block_comment (scm_t_wchar chr, SCM port)
{
  int bang_seen = 0;

  for (;;)
    {
      int c = scm_getc (port);

      if (c == EOF)
	scm_i_input_error ("skip_block_comment", port,
			   "unterminated `#! ... !#' comment", SCM_EOL);

      if (c == '!')
	bang_seen = 1;
      else if (c == '#' && bang_seen)
	break;
      else
	bang_seen = 0;
    }

  return SCM_UNSPECIFIED;
}

static void set_port_case_insensitive_p (SCM port, scm_t_read_opts *opts,
                                         int value);
/* static void set_port_hungry_eol_escapes_p (SCM port, scm_t_read_opts *opts, */
/*                                            int value); */

static SCM
scm_read_shebang (scm_t_wchar chr, SCM port, scm_t_read_opts *opts)
{
  char name[READER_DIRECTIVE_NAME_MAX_SIZE + 1];
  int c;
  int i = 0;

  while (i <= READER_DIRECTIVE_NAME_MAX_SIZE)
    {
      c = scm_getc (port);
      if (c == EOF)
	scm_i_input_error ("skip_block_comment", port,
			   "unterminated `#! ... !#' comment", SCM_EOL);
      else if (('a' <= c && c <= 'z') || ('0' <= c && c <= '9') || c == '-')
        name[i++] = c;
      else if (CHAR_IS_DELIMITER (c))
        {
          scm_ungetc (c, port);
          name[i] = '\0';
          if (0 == strcmp ("fold-case", name))
            set_port_case_insensitive_p (port, opts, 1);
          else if (0 == strcmp ("no-fold-case", name))
            set_port_case_insensitive_p (port, opts, 0);
          else
            break;
          return SCM_UNSPECIFIED;
        }
      else
        {
          scm_ungetc (c, port);
          break;
        }
    }
  while (i > 0)
    scm_ungetc (name[--i], port);
  return scm_read_scsh_block_comment (chr, port);
}

static SCM
scm_read_commented_expression (scm_t_wchar chr, SCM port,
                               scm_t_read_opts *opts)
{
  scm_t_wchar c;

  c = flush_ws (port, opts, (char *) NULL);
  if (EOF == c)
    scm_i_input_error ("read_commented_expression", port,
                       "no expression after #; comment", SCM_EOL);
  scm_ungetc (c, port);
  scm_read_expression (port, opts);
  return SCM_UNSPECIFIED;
}


/* Top-level token readers, i.e., dispatchers.  */

static SCM
scm_read_sharp_extension (int chr, SCM port, scm_t_read_opts *opts)
{
  SCM proc;

  proc = scm_get_hash_procedure (chr);
  if (scm_is_true (scm_procedure_p (proc)))
    {
      long line = scm_to_long (scm_port_line (port));
      int column = scm_to_int (scm_port_column (port)) - 2;
      SCM got;

      got = scm_call_2 (proc, SCM_MAKE_CHAR (chr), port);

      if (opts->record_positions_p && SCM_NIMP (got)
          && !scm_i_has_source_properties (got))
        scm_i_set_source_properties_x (got, line, column, scm_port_filename (port));

      return got;
    }

  return SCM_UNSPECIFIED;
}

static SCM
scm_read_sharp_sharp(SCM port, scm_t_read_opts *opts)
#define FUNC_NAME "read"
{
  char buffer[READER_SHARP_SHARP_NAME_MAX_SIZE];
  size_t bytes_read;
  const int overflow = read_token (port, opts,
                                   buffer, READER_CHAR_NAME_MAX_SIZE - 1,
                                   &bytes_read);
  if (overflow)
    scm_i_input_error (FUNC_NAME, port, "##NAME name too long", SCM_EOL);
  buffer[bytes_read] = '\0';
  if(strcmp("Inf", buffer) == 0)
    return scm_inf();
  if(strcmp("-Inf", buffer) == 0)
    return scm_product(SCM_I_MAKINUM(-1), scm_inf());
  if(strcmp("NaN", buffer) == 0)
    return scm_nan();
  scm_i_input_error (FUNC_NAME, port, "unknown ##NAME ~a",
		     scm_list_1(scm_from_locale_string(buffer)));
}

static SCM
scm_read_reader_conditional(SCM port, scm_t_read_opts *opts)
#define FUNC_NAME "read"
{
  // We've already read the initial #?
  const scm_t_wchar c = scm_getc (port);
  if (c == EOF)
    scm_i_input_error (FUNC_NAME, port, "end of file in reader conditional",
                       SCM_EOL);
  int splice = 0;
  if (c == '@')
    {
      splice = 1;
      const scm_t_wchar c = scm_getc(port);
      if (c == EOF)
        scm_i_input_error (FUNC_NAME, port,
                           "end of file in splicing reader conditional",
                           SCM_EOL);
      if (c != '(')
        scm_i_input_error (FUNC_NAME, port,
                           "reader conditional @ not followed by \"(\": ~a",
                           scm_list_1 (SCM_MAKE_CHAR(c)));
    }
  SCM exp = scm_read_sexp ('(', 1, port, opts);
  return scm_cons(splice ? lokke_sym_reader_cond_splice : lokke_sym_reader_cond,
                  exp);
}

#undef FUNC_NAME


/* The reader for the sharp `#' character.  It basically dispatches reads
   among the above token readers.   */
static SCM
scm_read_sharp (scm_t_wchar chr, SCM port, scm_t_read_opts *opts,
                long line, int column)
#define FUNC_NAME "scm_lreadr"
{
  SCM result;

  chr = scm_getc (port);

  // FIXME: keep extensions?
  result = scm_read_sharp_extension (chr, port, opts);
  if (!scm_is_eq (result, SCM_UNSPECIFIED))
    return result;

  switch (chr)
    {
    case '(':
      {
        SCM exp = scm_read_sexp (chr, 0, port, opts);
        return scm_list_2(lokke_sym_reader_anon_fn, exp);
      }
    case '{':
      {
        SCM exp = scm_read_sexp (chr, 1, port, opts);
        return scm_cons(lokke_sym_reader_hash_set, exp);
      }
    case '!':
      return (scm_read_shebang (chr, port, opts));
    case '_':
      return (scm_read_commented_expression (chr, port, opts));
    case '#':
      return scm_read_sharp_sharp(port, opts);
    case '?':
      return scm_read_reader_conditional(port, opts);
    default:
      scm_i_input_error (FUNC_NAME, port, "Unknown # object: ~S",
                         scm_list_1 (SCM_MAKE_CHAR (chr)));
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM
read_inner_expression (SCM port, scm_t_read_opts *opts)
#define FUNC_NAME "read_inner_expression"
{
  while (1)
    {
      scm_t_wchar chr;

      chr = scm_getc (port);

      switch (chr)
	{
	case SCM_WHITE_SPACES:
	case SCM_LINE_INCREMENTORS:
	  break;
	case ';':
	  (void) scm_read_semicolon_comment (chr, port);
	  break;
        case ':':
          return scm_read_keyword (chr, port, opts);
        case '{':
	case '[':
	case '(':
          return scm_read_sexp (chr, 0, port, opts);
	case '"':
	  return scm_read_string (chr, port, opts);
        case '|':
          return scm_read_mixed_case_symbol (chr, port, opts);
	case '\\':
          return scm_read_character (chr, port, opts);
	case '\'':
	case '`':
	case '~':
	  return (scm_read_quote (chr, port, opts));
	case '#':
	  {
            long line = scm_to_long (scm_port_line (port));
            int column = scm_to_int (scm_port_column (port)) - 1;
	    SCM result = scm_read_sharp (chr, port, opts, line, column);
	    if (scm_is_eq (result, SCM_UNSPECIFIED))
	      /* We read a comment or some such.  */
	      break;
	    else
	      return result;
	  }
        case '^':
          return scm_read_metadata (port, opts);
	case ')':
	  scm_i_input_error (FUNC_NAME, port, "unexpected \")\"", SCM_EOL);
	  break;
        case '}':
	  scm_i_input_error (FUNC_NAME, port, "unexpected \"}\"", SCM_EOL);
	  break;
	case ']':
          scm_i_input_error (FUNC_NAME, port, "unexpected \"]\"", SCM_EOL);
          break;
	case EOF:
	  return SCM_EOF_VAL;
	default:
	  {
	    if (((chr >= '0') && (chr <= '9'))
		|| (strchr ("+-", chr)))
	      return (scm_read_number (chr, port, opts));
	    else
	      return (scm_read_mixed_case_symbol (chr, port, opts));
	  }
	}
    }
}
#undef FUNC_NAME

static SCM
scm_read_expression (SCM port, scm_t_read_opts *opts)
#define FUNC_NAME "scm_read_expression"
{
  return read_inner_expression (port, opts);
}
#undef FUNC_NAME


/* Actual reader.  */

SCM_DEFINE (cljg_read, "read-primitively", 0, 1, 0,
            (SCM port),
	    "Read an s-expression from the input port @var{port}, or from\n"
	    "the current input port if @var{port} is not specified.\n"
	    "Any whitespace before the next token is discarded.")
#define FUNC_NAME s_cljg_read
{
  scm_t_read_opts opts = {.copy_source_p = 1,
                          .record_positions_p = 1,
                          .case_insensitive_p = 0,
                          .hungry_eol_escapes_p = 0 };
  if (SCM_UNBNDP (port))
    port = scm_current_input_port ();
  SCM_VALIDATE_OPINPORT (1, port);

  int c = flush_ws (port, &opts, (char *) NULL);
  if (EOF == c)
    return SCM_EOF_VAL;
  scm_ungetc (c, port);

  return (scm_read_expression (port, &opts));
}
#undef FUNC_NAME




/* Manipulate the read-hash-procedures alist.  This could be written in
   Scheme, but maybe it will also be used by C code during initialisation.  */
SCM_DEFINE (scm_read_hash_extend, "read-hash-extend", 2, 0, 0,
            (SCM chr, SCM proc),
	    "Install the procedure @var{proc} for reading expressions\n"
	    "starting with the character sequence @code{#} and @var{chr}.\n"
	    "@var{proc} will be called with two arguments:  the character\n"
	    "@var{chr} and the port to read further data from. The object\n"
	    "returned will be the return value of @code{read}. \n"
	    "Passing @code{#f} for @var{proc} will remove a previous setting. \n"
	    )
#define FUNC_NAME s_scm_read_hash_extend
{
  SCM this;
  SCM prev;

  SCM_VALIDATE_CHAR (1, chr);
  SCM_ASSERT (scm_is_false (proc)
	      || scm_is_eq (scm_procedure_p (proc), SCM_BOOL_T),
	      proc, SCM_ARG2, FUNC_NAME);

  /* Check if chr is already in the alist.  */
  this = scm_i_read_hash_procedures_ref ();
  prev = SCM_BOOL_F;
  while (1)
    {
      if (scm_is_null (this))
	{
	  /* not found, so add it to the beginning.  */
	  if (scm_is_true (proc))
	    {
              SCM new = scm_cons (scm_cons (chr, proc),
                                  scm_i_read_hash_procedures_ref ());
	      scm_i_read_hash_procedures_set_x (new);
	    }
	  break;
	}
      if (scm_is_eq (chr, SCM_CAAR (this)))
	{
	  /* already in the alist.  */
	  if (scm_is_false (proc))
	    {
	      /* remove it.  */
	      if (scm_is_false (prev))
		{
                  SCM rest = SCM_CDR (scm_i_read_hash_procedures_ref ());
		  scm_i_read_hash_procedures_set_x (rest);
		}
	      else
		scm_set_cdr_x (prev, SCM_CDR (this));
	    }
	  else
	    {
	      /* replace it.  */
	      scm_set_cdr_x (SCM_CAR (this), proc);
	    }
	  break;
	}
      prev = this;
      this = SCM_CDR (this);
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Recover the read-hash procedure corresponding to char c.  */
static SCM
scm_get_hash_procedure (int c)
{
  SCM rest = scm_i_read_hash_procedures_ref ();

  while (1)
    {
      if (scm_is_null (rest))
	return SCM_BOOL_F;

      if (SCM_CHAR (SCM_CAAR (rest)) == c)
	return SCM_CDAR (rest);

      rest = SCM_CDR (rest);
    }
}



/* Per-port read options.

   We store per-port read options in the 'port-read-options' port
   property, which is stored in the internal port structure.  The value
   stored is a single integer that contains a two-bit field for each
   read option.

   If a bit field contains READ_OPTION_INHERIT (3), that indicates that
   the applicable value should be inherited from the corresponding
   global read option.  Otherwise, the bit field contains the value of
   the read option.  For boolean read options that have been set
   per-port, the possible values are 0 or 1. */

/* Key to read options in port properties. */
SCM_SYMBOL (sym_port_read_options, "port-read-options");

/* Offsets of bit fields for each per-port override */
#define READ_OPTION_COPY_SOURCE_P          0
#define READ_OPTION_RECORD_POSITIONS_P     2
#define READ_OPTION_CASE_INSENSITIVE_P     4
#define READ_OPTION_HUNGRY_EOL_ESCAPES_P   6

/* The total width in bits of the per-port overrides */
#define READ_OPTIONS_NUM_BITS              6

#define READ_OPTIONS_INHERIT_ALL  ((1UL << READ_OPTIONS_NUM_BITS) - 1)
#define READ_OPTIONS_MAX_VALUE    READ_OPTIONS_INHERIT_ALL

#define READ_OPTION_MASK     3
#define READ_OPTION_INHERIT  3

static void
set_port_read_option (SCM port, int option, int new_value)
{
  SCM scm_read_options;
  unsigned int read_options;

  new_value &= READ_OPTION_MASK;

  scm_read_options = scm_i_port_property (port, sym_port_read_options);
  if (scm_is_unsigned_integer (scm_read_options, 0, READ_OPTIONS_MAX_VALUE))
    read_options = scm_to_uint (scm_read_options);
  else
    read_options = READ_OPTIONS_INHERIT_ALL;
  read_options &= ~(READ_OPTION_MASK << option);
  read_options |= new_value << option;
  scm_read_options = scm_from_uint (read_options);
  scm_i_set_port_property_x (port, sym_port_read_options, scm_read_options);
}

/* Set OPTS and PORT's case-insensitivity according to VALUE. */
static void
set_port_case_insensitive_p (SCM port, scm_t_read_opts *opts, int value)
{
  value = !!value;
  opts->case_insensitive_p = value;
  set_port_read_option (port, READ_OPTION_CASE_INSENSITIVE_P, value);
}

/* static void */
/* set_port_hungry_eol_escapes_p (SCM port, scm_t_read_opts *opts, int value) */
/* { */
/*   value = !!value; */
/*   opts->hungry_eol_escapes_p = value; */
/*   set_port_read_option (port, READ_OPTION_HUNGRY_EOL_ESCAPES_P, value); */
/* } */

void
init_lokke_reader ()
{
  sym_quote = scm_from_utf8_symbol("quote");
  sym_unquote = scm_from_utf8_symbol("unquote");
  sym_uq_splicing = scm_from_utf8_symbol("unquote-splicing");

  SCM read_hash_procs = scm_make_fluid_with_default (SCM_EOL);
  scm_i_read_hash_procedures =
    SCM_VARIABLE_LOC (scm_c_define ("%read-hash-procedures", read_hash_procs));

  scm_init_opts (scm_read_options, scm_read_opts);

  lokke_true = scm_permanent_object (scm_from_latin1_string ("true"));
  lokke_false = scm_permanent_object (scm_from_latin1_string ("false"));
  lokke_nil = scm_permanent_object (scm_from_latin1_string ("nil"));

#ifndef SCM_MAGIC_SNARFER
#include "lib/lokke-reader.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
