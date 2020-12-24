// Copyright (C) 2019-2020 Rob Browning <rlb@defaultvalue.org>
// SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <assert.h>
#include <string.h>

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

// Assumptions:
//  - OK to access small/large after checking size, i.e. -fno-strict-aliasing

typedef struct {
    uint32_t length;
    SCM meta;
    // Fields above are in common with large vecs
    SCM elt[];
} smaller_vec_t;

typedef struct tree_node_t {
    SCM *elt;  // Always len 32 since this was a tail or small that got full
    struct tree_node_t *leaves;
    uint_fast8_t leaf_count;
} tree_node_t;

typedef struct {
    uint32_t length;
    SCM meta;
    // Fields above are in common with large vecs
    uint32_t tail_offset;
    SCM *tail;  // No tail length because tail length = length - tail_offset
    tree_node_t tree;
} larger_vec_t;

// Access to the common field(s) -- exact prefix of smaller and larger.
typedef struct {
    uint32_t length;
    SCM meta;
    // ...
} vector_t;

static SCM empty_vector = SCM_BOOL_F;
static SCM does_not_exist = SCM_BOOL_F;
static SCM vector_type_scm = SCM_BOOL_F;

SCM_DEFINE (lokke_vector_length, "lokke-vector-length", 1, 0, 0,
            (SCM vector),
	    "...@code{#} @var{index}...\n")
#define FUNC_NAME s_lokke_vector_length
{
    scm_assert_foreign_object_type (vector_type_scm, vector);
    const vector_t * const c_vec = scm_foreign_object_ref (vector, 0);
    return scm_from_uint32(c_vec->length);
}
#undef FUNC_NAME

static inline size_t
sizeof_small_vec(const size_t n)
{
    return sizeof (smaller_vec_t) + n * sizeof(SCM);
}

static SCM
make_empty_vector ()
{
    smaller_vec_t * const vec = scm_gc_malloc (sizeof_small_vec(0),
                                               "small lokke vector");
    vec->length = 0;
    vec->meta = SCM_ELISP_NIL;
    return scm_make_foreign_object_1 (vector_type_scm, vec);
}

static smaller_vec_t *
smaller_vec_copy(const smaller_vec_t * const v)
{
    const size_t size = sizeof_small_vec(v->length);
    smaller_vec_t * const result = scm_gc_malloc (size, "small lokke vector");
    memcpy (result, v, size);
    return result;
}

SCM_DEFINE (lokke_vector_meta, "lokke-vector-meta", 1, 0, 0,
            (SCM vector),
	    "...@code{#} @var{index}...\n")
#define FUNC_NAME s_lokke_vector_meta
{
    scm_assert_foreign_object_type (vector_type_scm, vector);
    const vector_t * const c_vec = scm_foreign_object_ref (vector, 0);
    return c_vec->meta;
}
#undef FUNC_NAME

SCM_DEFINE (lokke_vector_with_meta, "%lokke-vector-with-meta", 2, 0, 0,
            (SCM vector, SCM meta),
	    "...@code{#} @var{index}...\n")
#define FUNC_NAME s_lokke_vector_with_meta
{
    scm_assert_foreign_object_type (vector_type_scm, vector);
    // FIXME: verify meta is map here or in parent wrapper..
    const vector_t * const c_vec = scm_foreign_object_ref (vector, 0);
    if (c_vec->length <= 32) {
        const smaller_vec_t * const vs = (smaller_vec_t *) c_vec;
        smaller_vec_t *result = smaller_vec_copy((smaller_vec_t *) vs);
        result->meta = meta;
        return scm_make_foreign_object_1 (vector_type_scm, result);
    }
    const larger_vec_t * const vl = (larger_vec_t *) c_vec;
    larger_vec_t * const result = scm_gc_malloc (sizeof (larger_vec_t),
                                                 "larger lokke vector");
    memcpy (result, vl, sizeof(larger_vec_t));
    result->meta = meta;
    return scm_make_foreign_object_1 (vector_type_scm, result);
}
#undef FUNC_NAME

static smaller_vec_t *
smaller_vec_conj(const smaller_vec_t * const v, SCM elt)
{
    // Assumes caller knows this won't overflow 32.
    smaller_vec_t * const result =
        scm_gc_malloc (sizeof_small_vec(v->length + 1), "small lokke vector");
    result->length = v->length + 1;
    result->meta = v->meta;
    for (int i = 0; i < v->length; i++) result->elt[i] = v->elt[i];
    result->elt[v->length] = elt;
    return result;
}

static larger_vec_t *
copy_vector_tail(const larger_vec_t * const v, const uint_fast8_t tail_len,
                 int_fast8_t diff_n)
{
    // Assumes caller knows diff_n won't over/underflow tail or tail_offset;
    const uint_fast8_t new_len = tail_len + diff_n;
    SCM * const new_tail = scm_gc_malloc (sizeof (SCM) * new_len,
                                          "lokke vector tail");
    if (new_len < tail_len)
        memcpy (new_tail, v->tail, new_len * sizeof (SCM));
    else
        memcpy (new_tail, v->tail, tail_len * sizeof (SCM));
    larger_vec_t * const result = scm_gc_malloc (sizeof (larger_vec_t),
                                                 "larger lokke vector");
    memcpy (result, v, sizeof (larger_vec_t));
    if (diff_n)
        result->length += diff_n;
    result->tail = new_tail;
    return result;
}

static inline uint_fast8_t
msb_u32(const uint32_t x)
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
leaf_index_for_level(const uint_fast8_t level, const uint32_t n)
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
                        const uint_fast8_t level, const uint32_t index,
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
        result->elt = scm_gc_malloc (sizeof (SCM) * 32, "lokke tree node content");
        memcpy (result->elt, n->elt, sizeof (SCM) * 32);
        result->elt[radix_i] = value;
        return;
    }
    // Recurse to find the right place, then rewrite the spine on the way out.
    assert (radix_i < n->leaf_count);
    // FIXME: same as for tail incorporation...
    result->elt = n->elt;
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
                        const uint_fast8_t level, const uint32_t tail_index,
                        SCM *tail) {
    // The tail always gets appended as the highest new index/radix.
    // Assumes that tail doesn't go in the top because that content
    // always comes via promotion from a small vector.
    const uint_fast8_t radix_i = leaf_index_for_level (level, tail_index);
    if (level == 1) {  // parent of tail
        // if leaf_count is zero, radix might be 0 or 1 depending on
        // whether or not this is at the "top" of the tree.  If we're
        // at the top, then [0] isn't a thing, we start at [1].

        result->elt = n->elt;
        result->leaf_count = radix_i + 1;
        result->leaves = scm_gc_malloc (sizeof (tree_node_t) * result->leaf_count,
                                        "lokke tree node leaves");
        // FIXME: c99 struct init {}?
        result->leaves[radix_i].elt = tail;
        result->leaves[radix_i].leaf_count = 0;
        result->leaves[radix_i].leaves = NULL;
        if (n->leaf_count != 0) {
            memcpy (result->leaves, n->leaves, sizeof (tree_node_t) * radix_i);
            return;
        }
        if (radix_i == 1) {
            // top of the tree, so there is no [0] (jumped straight to [1])
            result->leaves[0].elt = NULL;
            result->leaves[0].leaf_count = 0;
            result->leaves[0].leaves = NULL;
        }
        return;
    }
    result->elt = n->elt;
    result->leaf_count = n->leaf_count;
    result->leaves = scm_gc_malloc (sizeof (tree_node_t) * result->leaf_count, "lokke tree node leaves");
    memcpy(result->leaves, n->leaves, sizeof (tree_node_t) * n->leaf_count);
    tree_incorporating_tail(&(result->leaves[radix_i]),
                            &(n->leaves[radix_i]),
                            level - 1, tail_index, tail);
}

static void
split_rightmost_elts(tree_node_t * const result_n,
                     SCM **result_elts,
                     const tree_node_t * const n,
                     const uint_fast8_t level, const uint32_t index)
{
    // Dig down to the correct level and return both a tree without
    // the "rightmost" leaf, and the removed leaf elements.
    const uint_fast8_t radix_i = leaf_index_for_level (level, index);
    if (level == 1) {  // parent of tail
        result_n->elt = n->elt;
        result_n->leaf_count = radix_i;
        *result_elts = n->leaves[radix_i].elt;
        if (radix_i == 0) { // Removing only leaf
            result_n->leaves = NULL;
            return;
        }
        result_n->leaves = scm_gc_malloc (sizeof (tree_node_t) * radix_i,
                                             "lokke tree node leaves");
        memcpy (result_n->leaves, n->leaves, sizeof (tree_node_t) * radix_i);
        return;
    }

    result_n->elt = n->elt;
    result_n->leaf_count = n->leaf_count;
    result_n->leaves = scm_gc_malloc (sizeof (tree_node_t) * n->leaf_count,
                                      "lokke tree node leaves");
    memcpy(result_n->leaves, n->leaves, sizeof (tree_node_t) * n->leaf_count);
    split_rightmost_elts (&(result_n->leaves[radix_i]),
                          result_elts,
                          &(n->leaves[radix_i]),
                          level - 1, index);
}

static SCM
vector_ref(const vector_t * const v, const uint32_t n)
{
    // Optimized under the assumption that the data structure is *correct*
    if (n >= v->length)
        return does_not_exist;

    if (v->length <= 32) {
        if (n >= 32)
            return does_not_exist;
        return ((smaller_vec_t *) v)->elt[n];
    }

    const larger_vec_t * const vl = (larger_vec_t *) v;
    if (n >= vl->tail_offset) {
        const uint_fast8_t tail_i = n - vl->tail_offset;
        assert (tail_i < 32);
        return vl->tail[tail_i];
    }
    const tree_node_t *node = &(vl->tree);
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
    return node->elt[n & 0x1f];
}

SCM_DEFINE (lokke_vector_ref, "lokke-vector-ref", 2, 1, 0,
            (SCM vector, SCM index, SCM not_found),
	    "...@code{#} @var{index}...\n")
#define FUNC_NAME s_lokke_vector_ref
{
    scm_assert_foreign_object_type (vector_type_scm, vector);
    const uint32_t c_idx = scm_to_uint32 (index);
    const vector_t * const c_vec = scm_foreign_object_ref (vector, 0);
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
    vector_t * const v = scm_foreign_object_ref (vector, 0);

    if (v->length < 32)  // Stay small
        return scm_make_foreign_object_1 (vector_type_scm,
                                          smaller_vec_conj((smaller_vec_t *) v,
                                                           item));

    if (v->length == 32) {
        const smaller_vec_t * const vs = (smaller_vec_t *) v;
        larger_vec_t *result = scm_gc_malloc (sizeof (larger_vec_t),
                                              "larger lokke vector");
        result->length = 33;
        result->meta = vs->meta;
        SCM * const elt = scm_gc_malloc(sizeof(SCM) * 32,
                                        "lokke vector content");
        memcpy(elt, vs->elt, sizeof(SCM) * 32);
        result->tree.elt = elt;
        result->tree.leaves = NULL;
        result->tree.leaf_count = 0;
        result->tail = scm_gc_malloc (sizeof (SCM), "lokke vector tail");
        result->tail[0] = item;
        result->tail_offset = 32;
        return scm_make_foreign_object_1 (vector_type_scm, result);
    }

    if (v->length == UINT32_MAX)
        scm_error (scm_out_of_range_key, FUNC_NAME,
                   "Cannot append to full vector", SCM_EOL, SCM_EOL);

    const larger_vec_t * const vl = (larger_vec_t *) v;
    const uint32_t tail_len = vl->length - vl->tail_offset;
    if (tail_len < 32) {
        larger_vec_t * const result = copy_vector_tail (vl, tail_len, 1);
        result->tail[tail_len] = item;
        return scm_make_foreign_object_1 (vector_type_scm, result);
    }
    assert (tail_len == 32);
    // Move tail into tree.
    const uint32_t new_index = vl->length;
    larger_vec_t * const result = scm_gc_malloc (sizeof (larger_vec_t),
                                                 "larger lokke vector");
    result->length = vl->length + 1;
    result->meta = vl->meta;
    tree_incorporating_tail(&(result->tree),
                            &(vl->tree),
                            index_depth(new_index - 32),
                            new_index - 32,
                            vl->tail);
    result->tail_offset = vl->tail_offset + 32;
    result->tail = scm_gc_malloc (sizeof (SCM), "lokke vector tail");
    result->tail[0] = item;
    return scm_make_foreign_object_1 (vector_type_scm, result);
}
#undef FUNC_NAME

SCM_DEFINE (lokke_vector_assoc_1, "lokke-vector-assoc-1", 3, 0, 0,
            (SCM vector, SCM index, SCM value),
	    "...@code{#} @var{index}...\n")
#define FUNC_NAME s_lokke_vector_assoc_1
{
    scm_assert_foreign_object_type (vector_type_scm, vector);
    vector_t * const v = scm_foreign_object_ref (vector, 0);
    const uint32_t n = scm_to_uint32(index);

    if (n == v->length)
        // FIXME: avoid double-validation...
        return lokke_vector_conj_1 (vector, value);

    if (n > v->length || n == UINT32_MAX)
        SCM_OUT_OF_RANGE (2, index);

    if (v->length <= 32) {
        const smaller_vec_t * const vs = (smaller_vec_t *) v;
        smaller_vec_t *result = smaller_vec_copy((smaller_vec_t *) vs);
        result->elt[n] = value;
        return scm_make_foreign_object_1 (vector_type_scm, result);
    }

    const larger_vec_t * const vl = (larger_vec_t *) v;

    if (n >= vl->tail_offset) {
        uint_fast8_t tail_len = vl->length - vl->tail_offset;
        larger_vec_t * const result = copy_vector_tail (vl, tail_len, 0);
        result->tail[n - vl->tail_offset] = value;
        return scm_make_foreign_object_1 (vector_type_scm, result);
    }

    larger_vec_t * const result = scm_gc_malloc (sizeof (larger_vec_t),
                                                 "larger lokke vector");
    memcpy (result, vl, sizeof(larger_vec_t));
    tree_with_updated_value(&(result->tree),
                            &(vl->tree),
                            index_depth(n), n, value);
    return scm_make_foreign_object_1 (vector_type_scm, result);
}
#undef FUNC_NAME

SCM_DEFINE (lokke_vector_pop, "lokke-vector-pop", 1, 0, 0, (SCM vector),
	    "...@code{#} @var{index}...\n")
#define FUNC_NAME s_lokke_vector_pop
{
    scm_assert_foreign_object_type (vector_type_scm, vector);
    vector_t * const v = scm_foreign_object_ref (vector, 0);

    if (v->length <= 33) { // result will be smaller_vec_t
        if (v->length == 0)
            scm_error (scm_out_of_range_key, FUNC_NAME,
                       "Cannot pop empty vector", SCM_EOL, SCM_EOL);
        else if (v->length == 1)
            return empty_vector;

        const uint32_t result_len = v->length - 1;
        smaller_vec_t * const result =
            scm_gc_malloc (sizeof_small_vec(result_len), "small lokke vector");
        result->length = result_len;
        result->meta = v->meta;
        if (v->length < 33) {
            const smaller_vec_t * const vs = (smaller_vec_t *) v;
            for (uint32_t i = 0; i < result_len; i++)
                result->elt[i] = vs->elt[i];
        } else
            for (uint32_t i = 0; i < result_len; i++)
                result->elt[i] = vector_ref(v, i);
        return scm_make_foreign_object_1 (vector_type_scm, result);
    }

    const larger_vec_t * const vl = (larger_vec_t *) v;
    const uint32_t tail_len = vl->length - vl->tail_offset;

    if (tail_len > 1) {
        larger_vec_t * const result = copy_vector_tail (vl, tail_len, -1);
        return scm_make_foreign_object_1 (vector_type_scm, result);
    }

    larger_vec_t * const result = scm_gc_malloc (sizeof (larger_vec_t),
                                                 "larger lokke vector");
    result->length = vl->length - 1;
    result->meta = vl->meta;

    if (tail_len == 1) {
        result->tail_offset = vl->tail_offset;
        result->tail = NULL;
        result->tree = vl->tree;
        return scm_make_foreign_object_1 (vector_type_scm, result);
    }

    SCM *tail = NULL;
    split_rightmost_elts (&(result->tree),
                          &tail,
                          &(vl->tree),
                          index_depth(result->length),
                          result->length);
    result->tail_offset = vl->tail_offset - 32;
    result->tail = scm_gc_malloc (sizeof (SCM) * 32, "lokke vector tail");
    memcpy (result->tail, tail, sizeof (SCM) * 32);
    return scm_make_foreign_object_1 (vector_type_scm, result);
}
#undef FUNC_NAME

void
init_lokke_vector()
{
    assert (sizeof (unsigned long) >= sizeof (uint32_t));  // for builtin_clzl
    assert (sizeof (smaller_vec_t) >= sizeof (vector_t));
    assert (sizeof (larger_vec_t) > sizeof (vector_t));

    // FIXME: OK to disallow changes like this?
    vector_type_scm = scm_variable_ref (scm_c_lookup ("<lokke-vector>"));
    empty_vector = make_empty_vector();
    does_not_exist = scm_make_symbol (scm_from_utf8_string ("lokke-vector-missing-canary"));

    scm_c_define("lokke-empty-vector", empty_vector);

#ifndef SCM_MAGIC_SNARFER
#  include "lib/lokke-vector.x"
#endif
}
