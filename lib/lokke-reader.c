/* Copyright (C) 1995-1997, 1999-2001, 2003, 2004, 2006-2012, 2014, 2015
 *   Free Software Foundation, Inc.
 * Copyright (C) 2015-2021 Rob Browning <rlb@defaultvalue.org>
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

#include <libguile.h>

SCM_SYMBOL (lokke_sym_reader_anon_fn, "/lokke/reader-anon-fn");
SCM_SYMBOL (lokke_sym_reader_cond, "/lokke/reader-cond");
SCM_SYMBOL (lokke_sym_reader_cond_splice, "/lokke/reader-cond-splice");
SCM_SYMBOL (lokke_sym_reader_meta, "/lokke/reader-meta");
SCM_SYMBOL (lokke_sym_reader_hash_map, "/lokke/reader-hash-map");
SCM_SYMBOL (lokke_sym_reader_hash_set, "/lokke/reader-hash-set");
SCM_SYMBOL (lokke_sym_reader_vector, "/lokke/reader-vector");
SCM_SYMBOL (lokke_sym_reader_tagged, "/lokke/reader-tagged");
SCM_SYMBOL (sym_core_deref, "clojure.core/deref");
SCM_SYMBOL (sym_re_pattern, "re-pattern");
SCM_SYMBOL (sym_syntax_quote, "syntax-quote");
SCM_SYMBOL (sym_var, "var");
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
   {"newline", SCM_MAKE_CHAR('\n')},
   {"space", SCM_MAKE_CHAR(' ')},
   {"tab", SCM_MAKE_CHAR('\t')},
   {"formfeed", SCM_MAKE_CHAR('\f')},
   {"backspace", SCM_MAKE_CHAR('\b')},
   {"return", SCM_MAKE_CHAR('\r')},
   {0, 0}
};

typedef struct
{
  unsigned int copy_source_p        : 1;
  unsigned int record_positions_p   : 1;
} lokke_read_opts_t;

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
#define CHAR_IS_BLANK(_chr)					\
  (((_chr) == ' ') || ((_chr) == '\t') || ((_chr) == '\n')	\
   || ((_chr) == '\f') || ((_chr) == '\r'))

#define CHAR_IS_DELIMITER(c)                                    \
  (CHAR_IS_BLANK (c)						\
   || (c) == ')' || (c) == '(' || (c) == ';' || (c) == '"'      \
   || (c) == ']' || (c) == '['                                  \
   || (c) == '}' || (c) == '{')                                 \
   || (c) == ','

#define CHAR_IS_EXPONENT_MARKER(_chr) ((_chr) == 'e')

/* Read an SCSH block comment.  */
static SCM scm_read_scsh_block_comment (scm_t_wchar, SCM);
static SCM scm_read_commented_expression (scm_t_wchar, SCM, lokke_read_opts_t *);

/* Read from PORT until a delimiter (e.g., a whitespace) is read.  Put the
   result in the pre-allocated buffer BUF.  Return zero if the whole token has
   fewer than BUF_SIZE bytes, non-zero otherwise. READ will be set the number of
   bytes actually read.  */
static int
read_token (SCM port, lokke_read_opts_t *opts,
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
read_complete_token (SCM port, lokke_read_opts_t *opts,
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
flush_ws (SCM port, lokke_read_opts_t *opts, const char *eoferr)
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
            scm_read_scsh_block_comment (c, port);
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

static SCM scm_read_expression (SCM port, lokke_read_opts_t *opts);
static SCM scm_read_sharp (int chr, SCM port, lokke_read_opts_t *opts,
                           long line, int column);


static SCM
maybe_annotate_source (SCM x, SCM port, lokke_read_opts_t *opts,
                       long line, int column)
{
  /* This condition can be caused by a user calling
     set-port-column!.  */
  if (line < 0 || column < 0)
    return x;

  // Just making SCM_COPY_SOURCE_P true for now...
  if (opts->record_positions_p)
    scm_set_source_properties_x(x, scm_make_srcprops(line, column, scm_port_filename (port),
                                                     scm_copy_tree(x),
                                                     SCM_EOL));
  return x;
}

static SCM
scm_read_sexp (scm_t_wchar chr, int list_for_curly, SCM port, lokke_read_opts_t *opts)
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
      case ']':
        return scm_list_2 (lokke_sym_reader_vector, SCM_ELISP_NIL);
        break;
      case '}':
        if (list_for_curly)
          return SCM_EOL;
        return scm_cons2(lokke_sym_reader_hash_map, SCM_ELISP_NIL, SCM_EOL);
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
    case '[':
      ans = scm_cons2(lokke_sym_reader_vector, SCM_ELISP_NIL, ans);
      break;
    case '{':
      if (!list_for_curly)
        ans = scm_cons2(lokke_sym_reader_hash_map, SCM_ELISP_NIL, ans);
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

static SCM
scm_read_string_like_syntax (int chr, SCM port, lokke_read_opts_t *opts)
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
          scm_i_input_error (FUNC_NAME, port, "end of file in string constant",
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
            case '\\':
            case '(':  /* Accept "\(" for use at the beginning of lines
			  in multiline strings to avoid confusing emacs
			  lisp modes.  */
              break;
            case '\n':
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
lokke_read_regex_literal (SCM port, lokke_read_opts_t *opts)
#define FUNC_NAME "lokke_read_regex_literal"
{
  // Assumes the leading " has already been read
  SCM str = SCM_EOL;
  size_t c_str_len = 0;
  scm_t_wchar c, c_str[READER_STRING_BUFFER_SIZE];

  /* Need to capture line and column numbers here. */
  long line = scm_to_long (scm_port_line (port));
  int column = scm_to_int (scm_port_column (port)) - 1;

  scm_t_wchar prev_c = 0;
  while ((c = scm_getc (port)) != '"' || prev_c == '\\')
    {
      if (c == EOF)
          scm_i_input_error (FUNC_NAME, port, "end of file in #"" literal",
                             SCM_EOL);
      if (c_str_len + 1 >= READER_STRING_BUFFER_SIZE)
	{
	  str = scm_cons (scm_from_utf32_stringn (c_str, c_str_len), str);
	  c_str_len = 0;
	}
      c_str[c_str_len++] = c;
      prev_c = c;
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
scm_read_string (int chr, SCM port, lokke_read_opts_t *opts)
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
scm_read_number (scm_t_wchar chr, SCM port, lokke_read_opts_t *opts)
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
    /* Return a symbol instead of a number */
    result = scm_string_to_symbol (str);
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
scm_read_mixed_case_symbol (scm_t_wchar chr, SCM port, lokke_read_opts_t *opts)
{
  SCM result;
  size_t bytes_read;
  char local_buffer[READER_BUFFER_SIZE], *buffer;
  SCM str;

  scm_ungetc (chr, port);
  buffer = read_complete_token (port, opts, local_buffer, sizeof local_buffer,
				&bytes_read);

  str = scm_from_port_stringn (buffer, bytes_read, port);

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
scm_read_quote (int chr, SCM port, lokke_read_opts_t *opts)
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
scm_read_metadata (SCM port, lokke_read_opts_t *opts)
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


/* Sharp readers, i.e. readers called after a `#' sign has been read.  */

static SCM
scm_read_character (scm_t_wchar chr, SCM port, lokke_read_opts_t *opts)
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
  struct char_and_c_name_t *alias = aliased_characters;
  while (alias->name) {
    if (strcmp(buffer, alias->name) == 0)
      return alias->chr;
    alias++;
  }
  scm_i_input_error (FUNC_NAME, port, "unknown character name ~a",
		     scm_list_1 (charname));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

// FIXME: make this stricter?
static SCM
scm_read_keyword (int chr, SCM port, lokke_read_opts_t *opts)
{
  /* Read the symbol that comprises the keyword.
     XXX: This implementation allows sloppy syntaxes like `#:  key'.  */
  chr = scm_getc (port);
  if (chr == EOF)
    scm_i_input_error ("scm_read_keyword", port,
                       "end of file at start of keyword", SCM_EOL);
  SCM name = scm_read_mixed_case_symbol (chr, port, opts);
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

static SCM
scm_read_commented_expression (scm_t_wchar chr, SCM port,
                               lokke_read_opts_t *opts)
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
scm_read_sharp_sharp(SCM port, lokke_read_opts_t *opts)
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
scm_read_reader_conditional(SCM port, lokke_read_opts_t *opts)
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
scm_read_sharp (scm_t_wchar chr, SCM port, lokke_read_opts_t *opts,
                long line, int column)
#define FUNC_NAME "scm_lreadr"
{
  chr = scm_getc (port);
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
        return scm_cons2(lokke_sym_reader_hash_set, SCM_ELISP_NIL, exp);
      }
    case '"':
      return  scm_list_2 (sym_re_pattern,
                          lokke_read_regex_literal (port, opts));
    case '!':
      return scm_read_scsh_block_comment (chr, port);
    case '_':
      return (scm_read_commented_expression (chr, port, opts));
    case '#':
      return scm_read_sharp_sharp(port, opts);
    case '?':
      return scm_read_reader_conditional(port, opts);
    case '\'':
      {
        SCM exp = scm_read_expression (port, opts);
        if (!scm_is_symbol (exp))
          scm_i_input_error ("scm_read_sharp", port,
                             "#' var syntax not followed by a symbol: #'~s",
                             scm_list_1 (exp));
        return scm_list_2(sym_var, exp);
      }
    default:
      {
        scm_unget_byte (chr, port);
        SCM tag = scm_read_expression (port, opts);
        if (!scm_is_symbol (tag))
          scm_i_input_error ("scm_read_sharp", port,
                             "reader tag is not a symbol in #~s",
                             scm_list_1 (tag));
        SCM exp = scm_read_expression (port, opts);
        // Use the available public functions to approximate what the
        // guile reader does.
        if (opts->record_positions_p && SCM_NIMP (exp)
            && scm_source_properties (exp) == SCM_EOL)
          {
            // Since scm_i_set_source_properties_x is private...
            scm_set_source_property_x(exp, scm_sym_line, scm_from_long(line));
            scm_set_source_property_x(exp, scm_sym_column, scm_from_int(column - 1));
            scm_set_source_property_x(exp, scm_sym_filename,
                                      scm_port_filename (port));
          }
        return scm_list_3 (lokke_sym_reader_tagged, tag, exp);
      }
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM
read_inner_expression (SCM port, lokke_read_opts_t *opts)
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
        case '@':
          return scm_list_2 (sym_core_deref, scm_read_expression (port, opts));
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
scm_read_expression (SCM port, lokke_read_opts_t *opts)
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
  lokke_read_opts_t opts = {
    .copy_source_p = 1,
    .record_positions_p = 1
  };
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



void
init_lokke_reader ()
{
  sym_quote = scm_from_utf8_symbol("quote");
  sym_unquote = scm_from_utf8_symbol("unquote");
  sym_uq_splicing = scm_from_utf8_symbol("unquote-splicing");

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
