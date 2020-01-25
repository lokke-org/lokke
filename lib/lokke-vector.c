// Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
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

#include <libguile.h>

/*

  Notes:

  Since vectors can only be appended to, all nodes in the tree will be
  fully packed except the one just before the tail content.

  This is not an entirely normal radix tree in that all zero prefixed
  patterns are redundant, i.e. index 000002 is of course just 2, and
  as a result, leaves[0] at the root will be NULL.

 */

// FIXME naming convenction for safe vs unsafe functions, or maybe
// anything not SCM_DEFINED is assumed to be unsafe.

typedef struct tree_node_t {
    SCM *content;  // Always len 32 since this was a tail or small that got full
    struct tree_node_t *leaves;
    uint_fast8_t leaf_count;
} tree_node_t;

typedef struct {
    tree_node_t tree;
    SCM *tail;  // No tail length because tail length = length - tail_offset
    uint32_t tail_offset;
} larger_t;

// This duplication is super ugly, and is it even entirely legal?  The
// point is to have a union type we can use to try to avoid strict
// aliasing violations, though honestly, it's probably irrelevant
// since we'll probably need -fno-strict-aliasing for guile itself
// anyway...

// The small variant of vector_t (below), for use with malloc
typedef struct {
    uint32_t length;
    SCM *small;
} small_vector_t;

// The larger variant of vector_t (below), for use with malloc
typedef struct {
    uint32_t length;
    larger_t larger;
} larger_vector_t;

// The actual union type for general use (wrt strict aliasing, etc.)
typedef struct {
  uint32_t length;
  union {
    SCM *small;
    larger_t larger;
  };
} vector_t;

static SCM empty_vector;
static SCM does_not_exist;
static SCM vector_type_scm;

SCM_DEFINE (lokke_vector_length, "lokke-vector-length", 1, 0, 0,
            (SCM vector),
	    "...@code{#} @var{index}...\n")
#define FUNC_NAME s_lokke_vector_length
{
    scm_assert_foreign_object_type (vector_type_scm, vector);
    vector_t *c_vec = scm_foreign_object_ref (vector, 0);
    return scm_from_uint32(c_vec->length);
}
#undef FUNC_NAME

static SCM
make_empty_vector ()
{
    vector_t *vec = scm_gc_malloc (sizeof (small_vector_t), "small lokke vector");
    vec->length = 0;
    // FIXME: small could be NULL here, but iirc we'd need adjustment
    // to some of the other code, and there's only one of these
    // (i.e. scm_empty_vector) -- and definitely not worth changing if
    // it would cause any extra logic on more well worn paths.
    vec->small = scm_gc_malloc (sizeof (SCM), "small lokke vector content");
    vec->small[0] = SCM_BOOL_F;
    return scm_make_foreign_object_1 (vector_type_scm, vec);
}

static vector_t *
copy_small_vector(const vector_t * const v, uint_fast8_t expand_n)
{
    // Assumes caller knows expand_n won't overflow 32.
    vector_t *result = scm_gc_malloc (sizeof (small_vector_t), "small lokke vector");
    result->length = v->length + expand_n;
    result->small = scm_gc_malloc (sizeof (SCM) * result->length, "small lokke vector content");
    memcpy (result->small, v->small, v->length * sizeof(SCM));
    return result;
}

static vector_t *
copy_vector_tail(const vector_t * const v, uint_fast8_t tail_len,
                 uint_fast8_t expand_n)
{
    // Assumes caller knows expand_n won't overflow tail.
    assert (v->length > 32);
    SCM *new_tail = scm_gc_malloc (sizeof (SCM) * (tail_len + expand_n), "lokke vector tail");
    memcpy (new_tail, v->larger.tail, tail_len * sizeof (SCM));
    vector_t *result = scm_gc_malloc (sizeof (larger_vector_t), "larger lokke vector");
    memcpy (result, v, sizeof(larger_vector_t));
    if (expand_n)
        result->length += expand_n;
    result->larger.tail = new_tail;
    return result;
}

static inline uint_fast8_t
msb_u32(uint32_t x)
{
    // Return position of the most significant bit in x.  Assume the
    // compiler will remove the irrelevant cases.
    if (x == 0)
        return 0;
    if (sizeof (unsigned int) == 4)
        return  32 - __builtin_clz(x);
    if (sizeof (unsigned long) == 4)
        return  32 - __builtin_clzl(x);
    if (sizeof (unsigned int) == 8)
        return  64 - __builtin_clz(x);
    if (sizeof (unsigned long) == 8)
        return  64 - __builtin_clzl(x);
    assert (0);
}

static inline uint_fast8_t
index_depth(uint32_t n)
{
    // Return the tree depth of the node with the value for index i.
    return (msb_u32 (n) - 1) / 5;
}

static inline uint_fast8_t
leaf_index_for_level(uint_fast8_t level, uint32_t n)
{
    // Return the fan-out index for the vector index n n at a given
    // level in the tree.
    switch (level) {
        case 0: return n & 0x0000001f;
        case 1: return (n & 0x000003e0) >> 5;
        case 2: return (n & 0x00007c00) >> 10;
        case 3: return (n & 0x000f8000) >> 15;
        case 4: return (n & 0x01f00000) >> 20;
        case 5: return (n & 0x3e000000) >> 25;
        case 6: return (n & 0xc0000000) >> 30;
        default: assert (level < 7);
    }
    // never reached...
    assert (0);
    return 0;
}

static void
tree_with_updated_value(tree_node_t * const result,
                        const tree_node_t * const n,
                        uint_fast8_t level, uint32_t index,
                        SCM value)
{
    // All vector expansions are redirected to conj, so this code
    // assumes all the relevant tree structure already exists.  Dig
    // down to the correct level and return the value.
    const uint_fast8_t radix_i = leaf_index_for_level (level, index);
    if (level == 0) {
        // It's in this node.
        result->leaves = n->leaves;
        result->leaf_count = n->leaf_count;
        result->content = scm_gc_malloc (sizeof (SCM) * 32, "lokke tree node content");
        memcpy (result->content, n->content, sizeof (SCM) * 32);
        result->content[radix_i] = value;
        return;
    }
    // Recurse to find the right place, then rewrite the spine on the way out.
    assert (radix_i < n->leaf_count);
    // FIXME: same as for tail incorporation...
    result->content = n->content;
    result->leaves = scm_gc_malloc (sizeof (tree_node_t) * n->leaf_count, "lokke tree node leaves");
    memcpy (result->leaves, n->leaves, sizeof (tree_node_t) * n->leaf_count);
    tree_with_updated_value(&(result->leaves[radix_i]),
                            &(n->leaves[radix_i]),
                            level - 1, index, value);
    result->leaf_count = n->leaf_count;
}

static void
tree_incorporating_tail(tree_node_t * const result,
                        const tree_node_t * const n,
                        uint_fast8_t level, uint32_t tail_index,
                        SCM *tail) {
    // The tail always gets appended as the highest new index/radix.
    // Assumes that tail doesn't go in the top because that content
    // always comes via promotion from a small vector.
    const uint_fast8_t radix_i = leaf_index_for_level (level, tail_index);
    if (level == 1) {  // parent of tail
        // if leaf_count is zero, radix might be 0 or 1 depending on
        // whether or not this is at the "top" of the tree.  If we're
        // at the top, then [0] isn't a thing, we start at [1].

        result->content = n->content;
        result->leaf_count = radix_i + 1;
        result->leaves = scm_gc_malloc (sizeof (tree_node_t) * result->leaf_count,
                                        "lokke tree node leaves");
        // FIXME: c99 struct init {}?
        result->leaves[radix_i].content = tail;
        result->leaves[radix_i].leaf_count = 0;
        result->leaves[radix_i].leaves = NULL;
        if (n->leaf_count != 0) {
            memcpy (result->leaves, n->leaves, sizeof (tree_node_t) * radix_i);
            return;
        }
        if (radix_i == 1) {
            // top of the tree, so there is no [0] (jumped straight to [1])
            result->leaves[0].content = NULL;
            result->leaves[0].leaf_count = 0;
            result->leaves[0].leaves = NULL;
        }
        return;
    }
    result->content = n->content;
    result->leaf_count = n->leaf_count;
    result->leaves = scm_gc_malloc (sizeof (tree_node_t) * result->leaf_count, "lokke tree node leaves");
    memcpy(result->leaves, n->leaves, sizeof (tree_node_t) * n->leaf_count);
    tree_incorporating_tail(&(result->leaves[radix_i]),
                            &(n->leaves[radix_i]),
                            level - 1, tail_index, tail);
}

static SCM
vector_ref(const vector_t * const v, uint32_t n)
{
    // Optimized under the assumption that the data structure is *correct*
    if (n >= v->length)
        return does_not_exist;

    if (v->length <= 32)
        return (n < 32) ? v->small[n] : does_not_exist;

    if (n >= v->larger.tail_offset) {
        const uint_fast8_t tail_i = n - v->larger.tail_offset;
        assert (tail_i < 32);
        return v->larger.tail[tail_i];
    }

    const tree_node_t *node = &(v->larger.tree);
    if (n > 0) {  // clz undefined at 0
        const uint_fast8_t depth = index_depth (n);
        switch (depth) {
            // Intentionally falls through cases
            case 6:
                node = &(node->leaves[(n & 0xc0000000) >> 30]);
            case 5:
                node = &(node->leaves[(n & 0x3e000000) >> 25]);
            case 4:
                node = &(node->leaves[(n & 0x01f00000) >> 20]);
            case 3:
                node = &(node->leaves[(n & 0x000f8000) >> 15]);
            case 2:
                node = &(node->leaves[(n & 0x00007c00) >> 10]);
            case 1:
                node = &(node->leaves[(n & 0x000003e0) >> 5]);
        }
    }
    return node->content[n & 0x1f];
}

SCM_DEFINE (lokke_vector_ref, "lokke-vector-ref", 2, 1, 0,
            (SCM vector, SCM index, SCM not_found),
	    "...@code{#} @var{index}...\n")
#define FUNC_NAME s_lokke_vector_ref
{
    scm_assert_foreign_object_type (vector_type_scm, vector);
    const uint32_t c_idx = scm_to_uint32 (index);
    vector_t *c_vec = scm_foreign_object_ref (vector, 0);
    SCM result = vector_ref(c_vec, c_idx);
    if (scm_is_eq (result, does_not_exist)) {
        if (SCM_UNBNDP (not_found))
            SCM_OUT_OF_RANGE (2, index);
        else
            return not_found;
    }
    return result;
}
#undef FUNC_NAME

SCM_DEFINE (lokke_vector_conj_1, "lokke-vector-conj-1", 2, 0, 0,
            (SCM vector, SCM item),
	    "...@code{#} @var{index}...\n")
#define FUNC_NAME s_lokke_vector_conj_1
{
    scm_assert_foreign_object_type (vector_type_scm, vector);
    vector_t *v = scm_foreign_object_ref (vector, 0);

    if (v->length < 32) {  // Stay small
        vector_t *result = copy_small_vector(v, 1);
        result->small[v->length] = item;
        return scm_make_foreign_object_1 (vector_type_scm, result);
    }

    if (v->length == 32) {  // Convert from small to large
        vector_t *result = scm_gc_malloc (sizeof (larger_vector_t),
                                          "larger lokke vector");
        result->length = 33;
        result->larger.tree.content = v->small;
        result->larger.tree.leaves = NULL;
        result->larger.tree.leaf_count = 0;
        result->larger.tail = scm_gc_malloc (sizeof (SCM), "lokke vector tail");
        result->larger.tail[0] = item;
        result->larger.tail_offset = 32;
        return scm_make_foreign_object_1 (vector_type_scm, result);
    }

    if (v->length == UINT32_MAX)
        scm_error (scm_out_of_range_key, FUNC_NAME,
                   "Cannot append to full vector", SCM_EOL, SCM_EOL);

    const uint32_t tail_len = v->length - v->larger.tail_offset;
    if (tail_len < 32) {
        vector_t *result = copy_vector_tail (v, tail_len, 1);
        result->larger.tail[tail_len] = item;
        return scm_make_foreign_object_1 (vector_type_scm, result);
    }
    assert (tail_len == 32);
    // Move tail into tree.
    const uint32_t new_index = v->length;
    vector_t *result = scm_gc_malloc (sizeof (larger_vector_t), "larger lokke vector");
    result->length = v->length + 1;
    tree_incorporating_tail(&(result->larger.tree),
                            &(v->larger.tree),
                            index_depth(new_index - 32),
                            new_index - 32,
                            v->larger.tail);
    result->larger.tail_offset = v->larger.tail_offset + 32;
    result->larger.tail = scm_gc_malloc (sizeof (SCM), "lokke vector tail");
    result->larger.tail[0] = item;
    return scm_make_foreign_object_1 (vector_type_scm, result);
}
#undef FUNC_NAME

SCM_DEFINE (lokke_vector_assoc_1, "lokke-vector-assoc-1", 3, 0, 0,
            (SCM vector, SCM index, SCM value),
	    "...@code{#} @var{index}...\n")
#define FUNC_NAME s_lokke_vector_assoc_1
{
    scm_assert_foreign_object_type (vector_type_scm, vector);
    vector_t *v = scm_foreign_object_ref (vector, 0);
    const uint32_t n = scm_to_uint32(index);

    if (n == v->length)
        // FIXME: avoid double-validation...
        return lokke_vector_conj_1 (vector, value);

    if (n > v->length || n == UINT32_MAX)
        SCM_OUT_OF_RANGE (2, index);

    if (v->length <= 32) {
        vector_t *result = copy_small_vector(v, 0);
        result->small[n] = value;
        return scm_make_foreign_object_1 (vector_type_scm, result);
    }

    if (n >= v->larger.tail_offset) {
        uint_fast8_t tail_len = v->length - v->larger.tail_offset;
        vector_t *result = copy_vector_tail (v, tail_len, 0);
        result->larger.tail[n - v->larger.tail_offset] = value;
        return scm_make_foreign_object_1 (vector_type_scm, result);
    }

    vector_t *result = scm_gc_malloc (sizeof (larger_vector_t), "larger lokke vector");
    memcpy (result, v, sizeof(larger_vector_t));
    tree_with_updated_value(&(result->larger.tree),
                            &(v->larger.tree),
                            index_depth(n), n, value);
    return scm_make_foreign_object_1 (vector_type_scm, result);
}
#undef FUNC_NAME

void
init_lokke_vector()
{
    assert (sizeof(unsigned long) >= sizeof (uint32_t));  // for builtin_clzl
    assert (sizeof (small_vector_t) <= sizeof (vector_t));
    assert (sizeof (larger_vector_t) <= sizeof (vector_t));

    // FIXME: OK to disallow changes like this?
    vector_type_scm = scm_variable_ref (scm_c_lookup ("<lokke-vector>"));
    empty_vector = make_empty_vector();
    does_not_exist = scm_make_symbol (scm_from_utf8_string ("lokke-vector-missing-canary"));

    scm_c_define("<lokke-vector>", vector_type_scm);
    scm_c_define("lokke-empty-vector", empty_vector);

#ifndef SCM_MAGIC_SNARFER
#  include "lib/lokke-vector.x"
#endif
}
