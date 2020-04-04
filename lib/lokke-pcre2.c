// Copyright (C) 2019-2020 Rob Browning <rlb@defaultvalue.org>
//
// This project is free software; you can redistribute it and/or modify
// it under the terms of (at your option) either of the following two
// licences:
//
//   1) The GNU Lesser General Public License as published by the Free
//      Software Foundation; either version 2.1, or (at your option) any
//      later version.
//
//   2) The Eclipse Public License; either version 1.0 or (at your
//      option) any later version.

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <assert.h>
#include <string.h>

#include <libguile.h>

#define PCRE2_CODE_UNIT_WIDTH 0
#include <pcre2.h>


// Note: we may need a wind/free if scm_array_handle_release() changes
// to actually do anything.  Right now it doesn't.

static inline int
is_narrow_string (SCM s) {
    return scm_string_bytes_per_char (s) == SCM_INUM1;
}

static inline SCM
take_pcre2sizevector (const PCRE2_SIZE * const vec, size_t len)
{
    if (sizeof (PCRE2_SIZE) == 4)
        return scm_take_u32vector ((uint32_t *) vec, len);
    if (sizeof (PCRE2_SIZE) == 8)
        return scm_take_u64vector ((uint64_t *) vec, len);
    assert (0);  // Prevented by guards in init below...
}

static pcre2_compile_context_8 *compile_8_ctx;
static pcre2_compile_context_32 *compile_32_ctx;
static pcre2_general_context_8 *match_data_8_ctx;
static pcre2_general_context_32 *match_data_32_ctx;
static pcre2_match_context_8 *match_8_ctx;
static pcre2_match_context_32 *match_32_ctx;

static SCM code_utf8_scm_t = SCM_BOOL_F;
static SCM code_utf32_scm_t = SCM_BOOL_F;

static SCM code_u8_scm_t = SCM_BOOL_F;
static SCM code_u32_scm_t = SCM_BOOL_F;

static SCM match_data_8_scm_t = SCM_BOOL_F;
static SCM match_data_32_scm_t = SCM_BOOL_F;

// Must not allow a SCM_TICK whenever we're using
// scm_i_string... bytes, etc.  If we can't avoid it, we'll need to defer
// asyncs on on the scheme side.


SCM_DEFINE (pcre2_get_error_message, "pcre2-get-error-message",
            1, 0, 0,
            (SCM code),
"Returns an error message corresponding to a pcre2 error code, or #f if"
" the code is invalid.")
#define FUNC_NAME s_pcre2_get_error_message
{
    int c_code = scm_to_int (code);
    PCRE2_UCHAR8 msg[200];  // 120 was claimed large enough by the docs...
    int rc = pcre2_get_error_message_8 (c_code, msg,
                                        sizeof (msg) / sizeof (msg[0]));
    assert (rc != PCRE2_ERROR_NOMEMORY);
    if (rc == PCRE2_ERROR_BADDATA) {
        return SCM_BOOL_F;
    }
    return scm_from_latin1_string ((char *) msg);
}
#undef FUNC_NAME


SCM_DEFINE (pcre2_jit_compile, "pcre2-jit-compile", 2, 0, 0,
            (SCM code, SCM opts),
"Attempts JIT compilation of code as per pcre2_jit_compile(3).  Returns"
" 0 on success and a negative value otherwise.")
#define FUNC_NAME s_pcre2_jit_compile
{
    uint32_t c_opts = scm_to_int (opts);
    if (SCM_IS_A_P (code, code_utf8_scm_t)
        || SCM_IS_A_P (code, code_u8_scm_t)) {
        pcre2_code_8 * const c_code = scm_foreign_object_ref (code, 0);
        return scm_from_int (pcre2_jit_compile_8 (c_code, c_opts));
    }
    // u23
    SCM_ASSERT_TYPE ((SCM_IS_A_P (code, code_utf32_scm_t)
                      || SCM_IS_A_P (code, code_u32_scm_t)),
                     code, SCM_ARG1,
                     FUNC_NAME,
                     "<pcre2-code-utf8> <pcre2-code-utf32>"
                     " <pcre2-code-u8> or <pcre2-code-u32>");
    pcre2_code_32 * const c_code = scm_foreign_object_ref (code, 0);
    return scm_from_int (pcre2_jit_compile_32 (c_code, c_opts));
}
#undef FUNC_NAME


SCM_DEFINE (pcre2_compile_utf, "pcre2-compile-utf", 3, 0, 0,
            (SCM pattern, SCM opts, SCM width),
"Compiles the given pattern with pcre2_compile.  The width must be 8 or"
" 32.  Depending on the width, returns <pcre2-code-utf8>,"
" <pcre2-code-utf32>, or on error, the pair (error-code . error-offset)."
" The returned code instance will be a Unicode matcher, i.e. PCRE2_UTF is"
" always added to opts (see pcre2api(3).")
#define FUNC_NAME s_pcre2_compile_utf
{
    uint32_t c_opts = scm_to_int (opts);
    const int c_width = scm_to_int (width);
    SCM_ASSERT_TYPE ((c_width == 8 || c_width == 32), width, SCM_ARG3,
                     FUNC_NAME, "either 8 or 32");
    c_opts |= PCRE2_UTF;
    c_opts |= PCRE2_NO_UTF_CHECK;
    int rc;
    PCRE2_SIZE err_ofs;
    size_t len;
    if (c_width == 8) {
        pcre2_code_8 *code;
        char *c_str = scm_to_utf8_stringn (pattern, &len);
        code = pcre2_compile_8 ((PCRE2_SPTR8) c_str, len, c_opts,
                                &rc, &err_ofs, compile_8_ctx);
        free (c_str);  // No intervening scm calls, so no wind needed.
        if (code == NULL)
            return scm_cons (scm_from_int (rc), scm_from_uintmax (err_ofs));
        return scm_make_foreign_object_1 (code_utf8_scm_t, code);
    } else {  // c_width == 32
        pcre2_code_32 *code;
        if (is_narrow_string (pattern)) {
            size_t len;
            scm_t_wchar *c_str = scm_to_utf32_stringn (pattern, &len);
            code = pcre2_compile_32 ((PCRE2_SPTR32) c_str, len, c_opts,
                                     &rc, &err_ofs, compile_32_ctx);
            free (c_str);  // No intervening scm calls, so no wind needed.
        } else {
            // Add SCM_TICK warning...
            // Optimization: use buffer directly
            code = pcre2_compile_32 ((PCRE2_SPTR32) scm_i_string_wide_chars (pattern),
                                     //(PCRE2_SPTR32) scm_i_string_chars (pattern),
                                     scm_c_string_length(pattern),
                                     c_opts, &rc, &err_ofs, compile_32_ctx);
        }
        if (code == NULL)
            return scm_cons (scm_from_int (rc), scm_from_uintmax (err_ofs));
        return scm_make_foreign_object_1 (code_utf32_scm_t, code);
    }
}
#undef FUNC_NAME


static SCM
compile_u8 (const char * const chars, const size_t len, SCM opts)
{
    // Must not SCM_TICK until finished with chars
    uint32_t c_opts = scm_to_int (opts);
    if (c_opts & PCRE2_UTF || c_opts & PCRE2_MATCH_INVALID_UTF)
        c_opts &= ~(PCRE2_NO_UTF_CHECK);
    int rc;
    PCRE2_SIZE err_ofs;
    pcre2_code_8 *code = pcre2_compile_8 ((PCRE2_SPTR8) chars, len, c_opts,
                                          &rc, &err_ofs, compile_8_ctx);
    if (code == NULL)
        return scm_cons (scm_from_int (rc), scm_from_uintmax (err_ofs));
    return scm_make_foreign_object_1 (code_u8_scm_t, code);
}

static SCM
compile_u32 (const uint32_t * const chars, const size_t len, SCM opts)
{
    // Must not SCM_TICK until finished with chars
    uint32_t c_opts = scm_to_int (opts);
    if (c_opts & PCRE2_UTF || c_opts & PCRE2_MATCH_INVALID_UTF)
        c_opts &= ~(PCRE2_NO_UTF_CHECK);
    int rc;
    PCRE2_SIZE err_ofs;
    pcre2_code_32 *code = pcre2_compile_32 (chars, len, c_opts,
                                            &rc, &err_ofs, compile_32_ctx);
    if (code == NULL)
        return scm_cons (scm_from_int (rc), scm_from_uintmax (err_ofs));
    return scm_make_foreign_object_1 (code_u32_scm_t, code);
}


SCM_DEFINE (pcre2_compile_u8, "pcre2-compile-u8", 2, 0, 0,
            (SCM pattern, SCM options),
"Compiles the pattern using the given options via pcre2_compile_8().")
#define FUNC_NAME s_pcre2_compile_u8
{
    SCM_ASSERT_TYPE (scm_u8vector_p(pattern), pattern, SCM_ARG1,
                     FUNC_NAME, "u8vector");
    size_t len;
    scm_t_array_handle handle;
    const uint8_t * const chars =
        scm_u8vector_elements (pattern, &handle, &len, NULL);
    SCM result = compile_u8 ((const char *) chars, len, options);
    scm_array_handle_release (&handle);
    return result;
}
#undef FUNC_NAME


SCM_DEFINE (pcre2_compile_u32, "pcre2-compile-u32", 2, 0, 0,
            (SCM pattern, SCM opts),
"Compiles the pattern using the given options via pcre2_compile_8().")
#define FUNC_NAME s_pcre2_compile_u32
{
    SCM_ASSERT_TYPE (scm_u32vector_p(pattern), pattern, SCM_ARG1,
                     FUNC_NAME, "u32vector");
    size_t len;
    scm_t_array_handle handle;
    const uint32_t * const chars =
        scm_u32vector_elements (pattern, &handle, &len, NULL);
    SCM result = compile_u32 (chars, len, opts);
    scm_array_handle_release (&handle);
    return result;
}
#undef FUNC_NAME


SCM_DEFINE (pcre2_compile_string_units, "%pcre2-compile-string-units", 2, 0, 0,
            (SCM pattern, SCM opts),
"Compiles the pattern using the given options via pcre2_compile_8() or"
" pcre2_compile_32(), depending on the actual width of the pattern"
" characters.")
#define FUNC_NAME s_pcre2_compile_string_units
{
    if (is_narrow_string (pattern)) {
        return compile_u8 (scm_i_string_chars (pattern),
                           scm_c_string_length (pattern),
                           opts);
    }
    return compile_u32 ((uint32_t *) scm_i_string_wide_chars (pattern),
                        scm_c_string_length (pattern),
                        opts);
}
#undef FUNC_NAME


SCM_DEFINE (pcre2_make_match_data_8, "pcre2-make-match-data-8",
            1, 0, 0,
            (SCM ovecsize),
            "Returns a pcre2 <match-data-8> instance of the indicated size.")
#define FUNC_NAME s_pcre2_make_match_data_8
{
    uint32_t size = scm_to_uint32 (ovecsize);
    return scm_make_foreign_object_1 (match_data_8_scm_t,
                                      pcre2_match_data_create_8(size,
                                                                match_data_8_ctx));
}
#undef FUNC_NAME


SCM_DEFINE (pcre2_make_match_data_32, "pcre2-make-match-data-32",
            1, 0, 0,
            (SCM ovecsize),
            "Returns a pcre2 <match-data-32> instance of the indicated size.")
#define FUNC_NAME s_pcre2_make_match_data_32
{
    uint32_t size = scm_to_uint32 (ovecsize);
    return scm_make_foreign_object_1 (match_data_32_scm_t,
                                      pcre2_match_data_create_32(size,
                                                                match_data_32_ctx));
}
#undef FUNC_NAME


SCM_DEFINE (pcre2_match_data_for_code_8, "pcre2-match-data-for-code-8",
            1, 0, 0,
            (SCM code),
            "Returns a pcre2 <pcre2-match-data-8> instance suitable for a"
            " <pcre2-code-utf8> or <pcre2-code-u8>.")
#define FUNC_NAME s_pcre2_match_data_for_code_8
{
    SCM_ASSERT_TYPE ((SCM_IS_A_P (code, code_utf8_scm_t)
                      || SCM_IS_A_P (code, code_u8_scm_t)),
                     code, SCM_ARG1,
                     FUNC_NAME, "<pcre-code-utf8> or <pcre-code-8>");
    pcre2_code_8 * const c_code = scm_foreign_object_ref (code, 0);
    pcre2_match_data_8 *data =
        pcre2_match_data_create_from_pattern_8(c_code, match_data_8_ctx);
    return scm_make_foreign_object_1 (match_data_8_scm_t, data);
}
#undef FUNC_NAME


SCM_DEFINE (pcre2_match_data_for_code_32, "pcre2-match-data-for-code-32",
            1, 0, 0,
            (SCM code),
            "Returns a pcre2 <pcre2-match-data-32> instance suitable for a"
            " <pcre2-code-utf32> or <pcre2-code-u32>.")
#define FUNC_NAME s_pcre2_match_data_for_code_32
{
    SCM_ASSERT_TYPE ((SCM_IS_A_P (code, code_utf32_scm_t)
                      || SCM_IS_A_P (code, code_u32_scm_t)),
                     code, SCM_ARG1,
                     FUNC_NAME, "<pcre-code-utf32> or <pcre-code-32>");
    pcre2_code_32 * const c_code = scm_foreign_object_ref (code, 0);
    pcre2_match_data_32 *data =
        pcre2_match_data_create_from_pattern_32(c_code, match_data_32_ctx);
    return scm_make_foreign_object_1 (match_data_32_scm_t, data);
}
#undef FUNC_NAME


static SCM
scm_from_pcre2_ovector (PCRE2_SIZE *ovector, uint32_t pair_count)
{
    // FIXME: completely correct for fractional values?
    assert (pair_count <= ((UINT32_MAX / 2) / sizeof (PCRE2_SIZE)));  // FIXME: don't crash...
    const size_t ovec_size = sizeof (PCRE2_SIZE) * pair_count * 2;
    PCRE2_SIZE * const result = scm_malloc (ovec_size);
    memcpy (result, ovector, ovec_size);
    return take_pcre2sizevector (result, pair_count * 2);
}


SCM_DEFINE (pcre2_match_ovector, "pcre2-match-ovector", 1, 0, 0, (SCM match),
"Returns a homogeneous vector (array) containing the match pairs.")
#define FUNC_NAME s_pcre2_match_ovector
{
    if (SCM_IS_A_P (match, match_data_8_scm_t)) {
        pcre2_match_data_8 * const c_match = scm_foreign_object_ref (match, 0);
        return scm_from_pcre2_ovector (pcre2_get_ovector_pointer_8 (c_match),
                                       pcre2_get_ovector_count_8 (c_match));
    }
    // u23
    SCM_ASSERT_TYPE ((SCM_IS_A_P (match, match_data_32_scm_t)),
                     match, SCM_ARG1,
                     FUNC_NAME, "<pcre2-match-data-8> or <pcre2-match-data-32>");
    pcre2_match_data_32 * const c_match = scm_foreign_object_ref (match, 0);
    return scm_from_pcre2_ovector (pcre2_get_ovector_pointer_32 (c_match),
                                   pcre2_get_ovector_count_32 (c_match));
}
#undef FUNC_NAME


static int
match_8 (SCM code, const char * const string, size_t string_len,
         SCM offset, uint32_t opts, SCM match_data, int match_data_argn,
         const char * const caller_name)
{
    SCM_ASSERT_TYPE ((SCM_IS_A_P (match_data, match_data_8_scm_t)),
                     match_data, match_data_argn,
                     caller_name, "<pcre2-match-data-8>");
    const size_t c_ofs = scm_to_size_t (offset);
    // Assumes code 8 has already been checked.
    // Must not SCM_TICK (FIXME: move some scm_ funcs out of here? - is scm_gc_malloc safe?)
    pcre2_code_8 * const c_code = scm_foreign_object_ref (code, 0);
    return pcre2_match_8 (c_code,
                          (PCRE2_SPTR8) string, string_len, c_ofs, opts,
                          scm_foreign_object_ref (match_data, 0),
                          match_8_ctx);
}

static int
match_32 (SCM code, const scm_t_wchar * const string, size_t string_len,
          SCM offset, uint32_t opts, SCM match_data, int match_data_argn,
          const char * const caller_name)
{
    SCM_ASSERT_TYPE ((SCM_IS_A_P (match_data, match_data_32_scm_t)),
                     match_data, match_data_argn,
                     caller_name, "<pcre2-match-data-32>");
    const size_t c_ofs = scm_to_size_t (offset);
    // Assumes code 32 has already been checked.
    // Must not SCM_TICK (FIXME: move some scm_ funcs out of here? - is scm_gc_malloc safe?)
    pcre2_code_32 * const c_code = scm_foreign_object_ref (code, 0);
    return pcre2_match_32 (c_code,
                           (PCRE2_SPTR32) string, string_len, c_ofs, opts,
                           scm_foreign_object_ref (match_data, 0),
                           match_32_ctx);
}


SCM_DEFINE (pcre2_match_u8, "pcre2-match-u8", 5, 0, 0,
            (SCM code, SCM u8, SCM offset, SCM opts, SCM match_data),
"Attempts to find a match in the string for the pattern(s) represented"
" by the code via pcre2_match(3).  Returns 0 if no match was found."
" Returns the number of matches on success and updates the match-data."
" Returns a negative value on failure.")
#define FUNC_NAME s_pcre2_match_u8
{
    SCM_ASSERT_TYPE ((SCM_IS_A_P (code, code_u8_scm_t)), code, SCM_ARG1,
                     FUNC_NAME, "<pcre-code-8>");
    SCM_ASSERT_TYPE (scm_u8vector_p (u8), u8, SCM_ARG2,
                     FUNC_NAME, "u8vector");

    uint32_t c_opts = scm_to_int (opts);
    if (c_opts & PCRE2_UTF)
        c_opts &= ~(PCRE2_NO_UTF_CHECK);

    size_t len;
    scm_t_array_handle handle;
    const uint8_t * const chars =
        scm_u8vector_elements (u8, &handle, &len, NULL);

    int rc = match_8(code, (const char *) chars, len, offset, c_opts,
                     match_data, SCM_ARG5, FUNC_NAME);
    scm_array_handle_release (&handle);
    return scm_cons (scm_from_int (rc), match_data);
}
#undef FUNC_NAME


SCM_DEFINE (pcre2_match_u32, "pcre2-match-u32", 5, 0, 0,
            (SCM code, SCM u32, SCM offset, SCM opts, SCM match_data),
"Attempts to find a match in the string for the pattern(s) represented"
" by the code via pcre2_match(3).  Returns 0 if no match was found."
" Returns the number of matches on success and updates the match-data."
" Returns a negative value on failure.")
#define FUNC_NAME s_pcre2_match_u32
{
    SCM_ASSERT_TYPE ((SCM_IS_A_P (code, code_u32_scm_t)), code, SCM_ARG1,
                     FUNC_NAME, "<pcre-code-32>");
    SCM_ASSERT_TYPE (scm_u32vector_p (u32), u32, SCM_ARG2,
                     FUNC_NAME, "u32vector");

    uint32_t c_opts = scm_to_int (opts);
    if (c_opts & PCRE2_UTF)
        c_opts &= ~(PCRE2_NO_UTF_CHECK);

    size_t len;
    scm_t_array_handle handle;
    const uint32_t * const chars =
        scm_u32vector_elements (u32, &handle, &len, NULL);

    int rc = match_32(code, (const scm_t_wchar *) chars, len, offset, c_opts,
                      match_data, SCM_ARG5, FUNC_NAME);
    scm_array_handle_release (&handle);
    return scm_cons (scm_from_int (rc), match_data);
}
#undef FUNC_NAME



static int
ascii_only (const char * x, const size_t len)
{
    const char * const end = x + len;
    while (x < end) {
        if (*x++ < 0)
            return 0;
    }
    return 1;
}


SCM_DEFINE (pcre2_match, "pcre2-match-utf", 7, 0, 0,
            (SCM code_utf8, SCM code_utf32, SCM coerce,
             SCM string, SCM offset, SCM opts, SCM match_data),
"Attempts to find a match in the string for the pattern(s) represented"
" by the code(s) via pcre2_match(3).  Returns PCRE2_ERROR_NOMATCH if no"
" match was found.  On success, returns zero if the match was"
" successful, but the match_data isn't big enough to hold all the"
" substring offsets, otherwise returns the number of matched substrings"
" available, including the entire match, which is the first substring."
" Returns a negative value on failure.  If coerce is true and only a"
" single code is provided with a width that doesn't match the string,"
" returns #f.  Otherwise, creates a represenatation of the string in the"
" given code's width (utf-8 or utf-32) and matches that.")
#define FUNC_NAME s_pcre2_match
{
    SCM_VALIDATE_STRING (SCM_ARG1, string);
    const uint32_t c_opts = scm_to_int (opts) | PCRE2_NO_UTF_CHECK;
    if (is_narrow_string (string)) {
        if ((SCM_IS_A_P (code_utf8, code_utf8_scm_t))) {
            const size_t len = scm_c_string_length (string);
            const char * const chars = scm_i_string_chars (string);
            if (ascii_only (chars, len)) {
                int rc = match_8(code_utf8, chars, len, offset, c_opts,
                                 match_data, SCM_ARG7, FUNC_NAME);
                return scm_from_int (rc);
            }

            int rc;
            scm_dynwind_begin (0);
            {
                size_t utf8_len;
                const char * const utf8 = scm_to_utf8_stringn (string, &utf8_len);
                scm_dynwind_free ((void *) utf8);
                rc = match_8(code_utf8, utf8, len, offset, c_opts,
                             match_data, SCM_ARG7, FUNC_NAME);
            }
            scm_dynwind_end();
            return scm_from_int (rc);
        }
        SCM_ASSERT_TYPE ((scm_is_false (code_utf8)), code_utf8, SCM_ARG1,
                         FUNC_NAME, "either false or a <pcre2-code-utf8>");
        if (scm_is_false (coerce))
            return SCM_BOOL_F;
        if (scm_is_false (code_utf32))
            return SCM_BOOL_F;
        SCM_ASSERT_TYPE ((SCM_IS_A_P (code_utf32, code_utf32_scm_t)),
                         code_utf32, SCM_ARG2,
                         FUNC_NAME, "either false or a <pcre2-code-utf32>");
        // FIXME: double-check that length units are what pcre2 expects...
        int rc;
        scm_dynwind_begin (0);
        {
            size_t len;
            const scm_t_wchar * const utf32 = scm_to_utf32_stringn (string, &len);
            scm_dynwind_free ((void *) utf32);
            rc = match_32(code_utf32, utf32, len, offset, c_opts,
                          match_data, SCM_ARG7, FUNC_NAME);
        }
        scm_dynwind_end();
        return scm_from_int (rc);
    }

    // not narrow
    if ((SCM_IS_A_P (code_utf32, code_utf32_scm_t))) {
        const scm_t_wchar * const chars = scm_i_string_wide_chars (string);
        int rc = match_32(code_utf32, chars, scm_c_string_length(string),
                          offset, c_opts, match_data, SCM_ARG7, FUNC_NAME);
        return scm_from_int (rc);
    }
    SCM_ASSERT_TYPE ((scm_is_true (code_utf32)), code_utf32, SCM_ARG2,
                     FUNC_NAME, "either false or a <pcre2-code-utf32>");
    if (scm_is_false (coerce))
        return SCM_BOOL_F;
    if (scm_is_false (code_utf8))
        return SCM_BOOL_F;
    SCM_ASSERT_TYPE ((SCM_IS_A_P (code_utf8, code_utf8_scm_t)),
                     code_utf8, SCM_ARG1,
                     FUNC_NAME, "either false or a <pcre2-code-utf8>");
    // FIXME: double-check that length units are what pcre2 expects...
    int rc;
    scm_dynwind_begin (0);
    {
        size_t len;
        const char * const utf8 = scm_to_utf8_stringn (string, &len);
        scm_dynwind_free ((void *) utf8);
        rc = match_8(code_utf8, utf8, len, offset, c_opts,
                     match_data, SCM_ARG7, FUNC_NAME);
    }
    scm_dynwind_end();
    return scm_from_int (rc);
}
#undef FUNC_NAME

#define def_intmax_sym(x) scm_c_define(#x, scm_from_intmax(x))
#define def_u32_sym(x) scm_c_define(#x, scm_from_uint32(x))

static void*
gc_malloc (PCRE2_SIZE size, void *what)
{
    return scm_gc_malloc (size, what);
}

static void
gc_free (void *p, void *ignored)
{}

static void
init_contexts ()
{
    {
        pcre2_general_context_8 *gctx;
        gctx = pcre2_general_context_create_8 (gc_malloc, gc_free,
                                               "pcre2 code-8");
        compile_8_ctx = pcre2_compile_context_create_8 (gctx);
    }
    {
        pcre2_general_context_32 *gctx;
        gctx = pcre2_general_context_create_32 (gc_malloc, gc_free,
                                                "pcre2 code-32");
        compile_32_ctx = pcre2_compile_context_create_32 (gctx);
    }

    match_data_8_ctx = pcre2_general_context_create_8 (gc_malloc, gc_free,
                                                       "pcre2 match-data-8");
    match_data_32_ctx = pcre2_general_context_create_32 (gc_malloc, gc_free,
                                                         "pcre2 match-data-32");

    {
        pcre2_general_context_8 *gctx;
        gctx = pcre2_general_context_create_8 (gc_malloc, gc_free,
                                               "pcre2 match-8");
        match_8_ctx = pcre2_match_context_create_8 (gctx);
    }
    {
        pcre2_general_context_32 *gctx;
        gctx = pcre2_general_context_create_32 (gc_malloc, gc_free,
                                                "pcre2 match-32");
        match_32_ctx = pcre2_match_context_create_32 (gctx);
    }
}


static SCM
create_1_slot_type (const char *name)
{
    SCM slots = scm_list_1 (scm_from_utf8_symbol ("data"));
    return scm_make_foreign_object_type(scm_from_utf8_symbol (name),
                                        slots,
                                        NULL);
}


void
init_pcre2()
{
    init_contexts ();

    // pcre2 promises PCRE2_SIZE will be unsigned
    assert (sizeof (size_t) == sizeof(PCRE2_SIZE));
    assert (sizeof (size_t) == 4 || sizeof (size_t) == 8);  // for match-ovector

    code_utf8_scm_t = create_1_slot_type ("<pcre2-code-utf8>");
    code_utf32_scm_t = create_1_slot_type ("<pcre2-code-utf32>");
    code_u8_scm_t = create_1_slot_type ("<pcre2-code-u8>");
    code_u32_scm_t = create_1_slot_type ("<pcre2-code-u32>");

    match_data_8_scm_t = create_1_slot_type ("<pcre2-match-8>");
    match_data_32_scm_t = create_1_slot_type ("<pcre2-match-32>");

    // FIXME: store all these in a file somewhere so we can
    // automatically generate this code and the scm level export list,
    // *unless* we decide it's acceptable for the list to be invisible
    // in the scheme code (as individual exports).  In that case, just
    // store them in a structure here so that the scm side can
    // automate the exports at runtime.

    scm_c_define("pcre2-major", scm_from_uintmax(PCRE2_MAJOR));
    scm_c_define("pcre2-minor", scm_from_uintmax(PCRE2_MINOR));

    // pcre2_compile
    def_u32_sym (PCRE2_ALLOW_EMPTY_CLASS);
    def_u32_sym (PCRE2_ALT_BSUX);
    def_u32_sym (PCRE2_ALT_CIRCUMFLEX);
    def_u32_sym (PCRE2_ALT_VERBNAMES);
    def_u32_sym (PCRE2_ANCHORED);
    def_u32_sym (PCRE2_AUTO_CALLOUT);
    def_u32_sym (PCRE2_CASELESS);
    def_u32_sym (PCRE2_DOLLAR_ENDONLY);
    def_u32_sym (PCRE2_DOTALL);
    def_u32_sym (PCRE2_DUPNAMES);
    def_u32_sym (PCRE2_ENDANCHORED);
    def_u32_sym (PCRE2_EXTENDED);
    def_u32_sym (PCRE2_FIRSTLINE);
    def_u32_sym (PCRE2_JIT_COMPLETE);
    def_u32_sym (PCRE2_JIT_INVALID_UTF);
    def_u32_sym (PCRE2_JIT_PARTIAL_HARD);
    def_u32_sym (PCRE2_JIT_PARTIAL_SOFT);
    def_u32_sym (PCRE2_LITERAL);
    def_u32_sym (PCRE2_MATCH_INVALID_UTF);
    def_u32_sym (PCRE2_MATCH_UNSET_BACKREF);
    def_u32_sym (PCRE2_MULTILINE);
    def_u32_sym (PCRE2_NEVER_BACKSLASH_C);
    def_u32_sym (PCRE2_NEVER_UCP);
    def_u32_sym (PCRE2_NEVER_UTF);
    def_u32_sym (PCRE2_NO_AUTO_CAPTURE);
    def_u32_sym (PCRE2_NO_AUTO_POSSESS);
    def_u32_sym (PCRE2_NO_DOTSTAR_ANCHOR);
    def_u32_sym (PCRE2_NO_START_OPTIMIZE);
    def_u32_sym (PCRE2_NO_UTF_CHECK);
    def_u32_sym (PCRE2_UCP);
    def_u32_sym (PCRE2_UNGREEDY);
    def_u32_sym (PCRE2_USE_OFFSET_LIMIT);
    def_u32_sym (PCRE2_UTF);

    def_intmax_sym (PCRE2_ERROR_END_BACKSLASH);
    def_intmax_sym (PCRE2_ERROR_END_BACKSLASH_C);
    def_intmax_sym (PCRE2_ERROR_UNKNOWN_ESCAPE);
    def_intmax_sym (PCRE2_ERROR_QUANTIFIER_OUT_OF_ORDER);
    def_intmax_sym (PCRE2_ERROR_QUANTIFIER_TOO_BIG);
    def_intmax_sym (PCRE2_ERROR_MISSING_SQUARE_BRACKET);
    def_intmax_sym (PCRE2_ERROR_ESCAPE_INVALID_IN_CLASS);
    def_intmax_sym (PCRE2_ERROR_CLASS_RANGE_ORDER);
    def_intmax_sym (PCRE2_ERROR_QUANTIFIER_INVALID);
    def_intmax_sym (PCRE2_ERROR_INTERNAL_UNEXPECTED_REPEAT);
    def_intmax_sym (PCRE2_ERROR_INVALID_AFTER_PARENS_QUERY);
    def_intmax_sym (PCRE2_ERROR_POSIX_CLASS_NOT_IN_CLASS);
    def_intmax_sym (PCRE2_ERROR_POSIX_NO_SUPPORT_COLLATING);
    def_intmax_sym (PCRE2_ERROR_MISSING_CLOSING_PARENTHESIS);
    def_intmax_sym (PCRE2_ERROR_BAD_SUBPATTERN_REFERENCE);
    def_intmax_sym (PCRE2_ERROR_NULL_PATTERN);
    def_intmax_sym (PCRE2_ERROR_BAD_OPTIONS);
    def_intmax_sym (PCRE2_ERROR_MISSING_COMMENT_CLOSING);
    def_intmax_sym (PCRE2_ERROR_PARENTHESES_NEST_TOO_DEEP);
    def_intmax_sym (PCRE2_ERROR_PATTERN_TOO_LARGE);
    def_intmax_sym (PCRE2_ERROR_HEAP_FAILED);
    def_intmax_sym (PCRE2_ERROR_UNMATCHED_CLOSING_PARENTHESIS);
    def_intmax_sym (PCRE2_ERROR_INTERNAL_CODE_OVERFLOW);
    def_intmax_sym (PCRE2_ERROR_MISSING_CONDITION_CLOSING);
    def_intmax_sym (PCRE2_ERROR_LOOKBEHIND_NOT_FIXED_LENGTH);
    def_intmax_sym (PCRE2_ERROR_ZERO_RELATIVE_REFERENCE);
    def_intmax_sym (PCRE2_ERROR_TOO_MANY_CONDITION_BRANCHES);
    def_intmax_sym (PCRE2_ERROR_CONDITION_ASSERTION_EXPECTED);
    def_intmax_sym (PCRE2_ERROR_BAD_RELATIVE_REFERENCE);
    def_intmax_sym (PCRE2_ERROR_UNKNOWN_POSIX_CLASS);
    def_intmax_sym (PCRE2_ERROR_INTERNAL_STUDY_ERROR);
    def_intmax_sym (PCRE2_ERROR_UNICODE_NOT_SUPPORTED);
    def_intmax_sym (PCRE2_ERROR_PARENTHESES_STACK_CHECK);
    def_intmax_sym (PCRE2_ERROR_CODE_POINT_TOO_BIG);
    def_intmax_sym (PCRE2_ERROR_LOOKBEHIND_TOO_COMPLICATED);
    def_intmax_sym (PCRE2_ERROR_LOOKBEHIND_INVALID_BACKSLASH_C);
    def_intmax_sym (PCRE2_ERROR_UNSUPPORTED_ESCAPE_SEQUENCE);
    def_intmax_sym (PCRE2_ERROR_CALLOUT_NUMBER_TOO_BIG);
    def_intmax_sym (PCRE2_ERROR_MISSING_CALLOUT_CLOSING);
    def_intmax_sym (PCRE2_ERROR_ESCAPE_INVALID_IN_VERB);
    def_intmax_sym (PCRE2_ERROR_UNRECOGNIZED_AFTER_QUERY_P);
    def_intmax_sym (PCRE2_ERROR_MISSING_NAME_TERMINATOR);
    def_intmax_sym (PCRE2_ERROR_DUPLICATE_SUBPATTERN_NAME);
    def_intmax_sym (PCRE2_ERROR_INVALID_SUBPATTERN_NAME);
    def_intmax_sym (PCRE2_ERROR_UNICODE_PROPERTIES_UNAVAILABLE);
    def_intmax_sym (PCRE2_ERROR_MALFORMED_UNICODE_PROPERTY);
    def_intmax_sym (PCRE2_ERROR_UNKNOWN_UNICODE_PROPERTY);
    def_intmax_sym (PCRE2_ERROR_SUBPATTERN_NAME_TOO_LONG);
    def_intmax_sym (PCRE2_ERROR_TOO_MANY_NAMED_SUBPATTERNS);
    def_intmax_sym (PCRE2_ERROR_CLASS_INVALID_RANGE);
    def_intmax_sym (PCRE2_ERROR_OCTAL_BYTE_TOO_BIG);
    def_intmax_sym (PCRE2_ERROR_INTERNAL_OVERRAN_WORKSPACE);
    def_intmax_sym (PCRE2_ERROR_INTERNAL_MISSING_SUBPATTERN);
    def_intmax_sym (PCRE2_ERROR_DEFINE_TOO_MANY_BRANCHES);
    def_intmax_sym (PCRE2_ERROR_BACKSLASH_O_MISSING_BRACE);
    def_intmax_sym (PCRE2_ERROR_INTERNAL_UNKNOWN_NEWLINE);
    def_intmax_sym (PCRE2_ERROR_BACKSLASH_G_SYNTAX);
    def_intmax_sym (PCRE2_ERROR_PARENS_QUERY_R_MISSING_CLOSING);
    def_intmax_sym (PCRE2_ERROR_VERB_ARGUMENT_NOT_ALLOWED);
    def_intmax_sym (PCRE2_ERROR_VERB_UNKNOWN);
    def_intmax_sym (PCRE2_ERROR_SUBPATTERN_NUMBER_TOO_BIG);
    def_intmax_sym (PCRE2_ERROR_SUBPATTERN_NAME_EXPECTED);
    def_intmax_sym (PCRE2_ERROR_INTERNAL_PARSED_OVERFLOW);
    def_intmax_sym (PCRE2_ERROR_INVALID_OCTAL);
    def_intmax_sym (PCRE2_ERROR_SUBPATTERN_NAMES_MISMATCH);
    def_intmax_sym (PCRE2_ERROR_MARK_MISSING_ARGUMENT);
    def_intmax_sym (PCRE2_ERROR_INVALID_HEXADECIMAL);
    def_intmax_sym (PCRE2_ERROR_BACKSLASH_C_SYNTAX);
    def_intmax_sym (PCRE2_ERROR_BACKSLASH_K_SYNTAX);
    def_intmax_sym (PCRE2_ERROR_INTERNAL_BAD_CODE_LOOKBEHINDS);
    def_intmax_sym (PCRE2_ERROR_BACKSLASH_N_IN_CLASS);
    def_intmax_sym (PCRE2_ERROR_CALLOUT_STRING_TOO_LONG);
    def_intmax_sym (PCRE2_ERROR_UNICODE_DISALLOWED_CODE_POINT);
    def_intmax_sym (PCRE2_ERROR_UTF_IS_DISABLED);
    def_intmax_sym (PCRE2_ERROR_UCP_IS_DISABLED);
    def_intmax_sym (PCRE2_ERROR_VERB_NAME_TOO_LONG);
    def_intmax_sym (PCRE2_ERROR_BACKSLASH_U_CODE_POINT_TOO_BIG);
    def_intmax_sym (PCRE2_ERROR_MISSING_OCTAL_OR_HEX_DIGITS);
    def_intmax_sym (PCRE2_ERROR_VERSION_CONDITION_SYNTAX);
    def_intmax_sym (PCRE2_ERROR_INTERNAL_BAD_CODE_AUTO_POSSESS);
    def_intmax_sym (PCRE2_ERROR_CALLOUT_NO_STRING_DELIMITER);
    def_intmax_sym (PCRE2_ERROR_CALLOUT_BAD_STRING_DELIMITER);
    def_intmax_sym (PCRE2_ERROR_BACKSLASH_C_CALLER_DISABLED);
    def_intmax_sym (PCRE2_ERROR_QUERY_BARJX_NEST_TOO_DEEP);
    def_intmax_sym (PCRE2_ERROR_BACKSLASH_C_LIBRARY_DISABLED);
    def_intmax_sym (PCRE2_ERROR_PATTERN_TOO_COMPLICATED);
    def_intmax_sym (PCRE2_ERROR_LOOKBEHIND_TOO_LONG);
    def_intmax_sym (PCRE2_ERROR_PATTERN_STRING_TOO_LONG);
    def_intmax_sym (PCRE2_ERROR_INTERNAL_BAD_CODE);
    def_intmax_sym (PCRE2_ERROR_INTERNAL_BAD_CODE_IN_SKIP);
    def_intmax_sym (PCRE2_ERROR_NO_SURROGATES_IN_UTF16);
    def_intmax_sym (PCRE2_ERROR_BAD_LITERAL_OPTIONS);
    def_intmax_sym (PCRE2_ERROR_SUPPORTED_ONLY_IN_UNICODE);
    def_intmax_sym (PCRE2_ERROR_INVALID_HYPHEN_IN_OPTIONS);
    def_intmax_sym (PCRE2_ERROR_ALPHA_ASSERTION_UNKNOWN);
    def_intmax_sym (PCRE2_ERROR_SCRIPT_RUN_NOT_AVAILABLE);
    def_intmax_sym (PCRE2_ERROR_TOO_MANY_CAPTURES);
    def_intmax_sym (PCRE2_ERROR_CONDITION_ATOMIC_ASSERTION_EXPECTED);

    def_intmax_sym (PCRE2_ERROR_NOMATCH);
    def_intmax_sym (PCRE2_ERROR_PARTIAL);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR1);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR2);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR3);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR4);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR5);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR6);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR7);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR8);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR9);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR10);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR11);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR12);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR13);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR14);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR15);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR16);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR17);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR18);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR19);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR20);
    def_intmax_sym (PCRE2_ERROR_UTF8_ERR21);
    def_intmax_sym (PCRE2_ERROR_UTF16_ERR1);
    def_intmax_sym (PCRE2_ERROR_UTF16_ERR2);
    def_intmax_sym (PCRE2_ERROR_UTF16_ERR3);
    def_intmax_sym (PCRE2_ERROR_UTF32_ERR1);
    def_intmax_sym (PCRE2_ERROR_UTF32_ERR2);
    def_intmax_sym (PCRE2_ERROR_BADDATA);
    def_intmax_sym (PCRE2_ERROR_MIXEDTABLES);
    def_intmax_sym (PCRE2_ERROR_BADMAGIC);
    def_intmax_sym (PCRE2_ERROR_BADMODE);
    def_intmax_sym (PCRE2_ERROR_BADOFFSET);
    def_intmax_sym (PCRE2_ERROR_BADOPTION);
    def_intmax_sym (PCRE2_ERROR_BADREPLACEMENT);
    def_intmax_sym (PCRE2_ERROR_BADUTFOFFSET);
    def_intmax_sym (PCRE2_ERROR_CALLOUT);
    def_intmax_sym (PCRE2_ERROR_DFA_BADRESTART);
    def_intmax_sym (PCRE2_ERROR_DFA_RECURSE);
    def_intmax_sym (PCRE2_ERROR_DFA_UCOND);
    def_intmax_sym (PCRE2_ERROR_DFA_UFUNC);
    def_intmax_sym (PCRE2_ERROR_DFA_UITEM);
    def_intmax_sym (PCRE2_ERROR_DFA_WSSIZE);
    def_intmax_sym (PCRE2_ERROR_INTERNAL);
    def_intmax_sym (PCRE2_ERROR_JIT_BADOPTION);
    def_intmax_sym (PCRE2_ERROR_JIT_STACKLIMIT);
    def_intmax_sym (PCRE2_ERROR_MATCHLIMIT);
    def_intmax_sym (PCRE2_ERROR_NOMEMORY);
    def_intmax_sym (PCRE2_ERROR_NOSUBSTRING);
    def_intmax_sym (PCRE2_ERROR_NOUNIQUESUBSTRING);
    def_intmax_sym (PCRE2_ERROR_NULL);
    def_intmax_sym (PCRE2_ERROR_RECURSELOOP);
    def_intmax_sym (PCRE2_ERROR_DEPTHLIMIT);
    def_intmax_sym (PCRE2_ERROR_UNAVAILABLE);
    def_intmax_sym (PCRE2_ERROR_UNSET);
    def_intmax_sym (PCRE2_ERROR_BADOFFSETLIMIT);
    def_intmax_sym (PCRE2_ERROR_BADREPESCAPE);
    def_intmax_sym (PCRE2_ERROR_REPMISSINGBRACE);
    def_intmax_sym (PCRE2_ERROR_BADSUBSTITUTION);
    def_intmax_sym (PCRE2_ERROR_BADSUBSPATTERN);
    def_intmax_sym (PCRE2_ERROR_TOOMANYREPLACE);
    def_intmax_sym (PCRE2_ERROR_BADSERIALIZEDDATA);
    def_intmax_sym (PCRE2_ERROR_HEAPLIMIT);
    def_intmax_sym (PCRE2_ERROR_CONVERT_SYNTAX);
    def_intmax_sym (PCRE2_ERROR_INTERNAL_DUPMATCH);
    def_intmax_sym (PCRE2_ERROR_DFA_UINVALID_UTF);

    def_intmax_sym (PCRE2_ZERO_TERMINATED);
    def_intmax_sym (PCRE2_UNSET);

#ifndef SCM_MAGIC_SNARFER
#  include "lib/lokke-pcre2.x"
#endif
}
