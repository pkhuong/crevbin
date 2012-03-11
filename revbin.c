#include "revbin.h"
#include "revbin_bits.h"
#include <stdlib.h>
#include <assert.h>

#define likely(x) __builtin_expect((x),1)
typedef unsigned long ul;
typedef struct pair {
        ul i, j;
} pair_t;

typedef void (*block_function_t)(revbin_plan_t, v2d *);

/* a bit reversal plan is basically a closure:
 *
 * small bit reversals are implemented with fully-unrolled routines;
 *
 * medium bit reversals specialize on the shift value, but otherwise
 *   interpret pairs as usual;
 *
 * larger bit reversals are the general case which is probably easier
 *   to understand -- see block_swap_.
 */
struct revbin_plan {
        block_function_t function;
        unsigned shift; /* low bits + middle bits */
        unsigned npairs;
        pair_t pairs[];
};

void revbin_execute (revbin_plan_t plan, v2d * data)
{
        plan->function(plan, data);
}

/* We execute bit reversals as a series of swaps, and each index is
 * read and written to at most once; each SWAP_V2D executes a single
 * swap.
 *
 * Our series of swaps follows an ordering that is inconvenient to
 * compute on the fly, but well amenable to recursive decomposition.
 *
 * The basic idea is that, given a binary number a:b:c, with a, b and
 * c binary numbers and (:) bit-string contatenation, its bit-reversal
 * rev(a:b:c) = rev(c):rev(b):rev(a).
 *
 * We want to maximize locality, so the trick is to iterate over c and
 * rev(a) (for a given outer width) in Morton order.  The values for c
 * and rev(a) are independent of b/rev(b), so this inner loop can be
 * easily unrolled and nearly all the address generation
 * constant-folded.
 *
 * BLOCK_SWAP is this inner loop, given the reversed values
 * rev(a):0:0, pre-computed in rev##a.  The shift argument to
 * block_swap_ is the bit-length of (b:c).
 *
 * The same independence lets us iterate over b and rev(b) in any
 * order; in the same spirit as the inner loop, we use a Morton order
 * on the low-order bits (half) of b/rev(b).  This is what the entries
 * in revbin_plan.pairs represent.
 *
 * The one remaining subtlety is when b = rev(b).  In this case, we
 * have to be careful to perform each swap exactly once.  That's what
 * the (I < J) check is for in SWAP_SAME.
 */
#define SWAP_V2D(X, Y)                                   \
        do {                                             \
                v2d swap_one_tmp = X;                    \
                X = Y;                                   \
                Y = swap_one_tmp;                        \
        } while (0)

static __attribute__((always_inline))
void block_swap_ (unsigned shift, revbin_plan_t plan,
                  v2d * restrict data)
{
        PRELOAD_REV(shift, sizeof(v2d));
#define SWAP_DIFF(I, J) SWAP_V2D(((v2d*)((char*)base1+rev##J))[I],      \
                                 ((v2d*)((char*)base2+rev##I))[J])

#define SWAP_SAME(I, J) do {                                            \
                if (I < J)                                              \
                        SWAP_V2D(((v2d*)((char*)base+rev##J))[I],       \
                                 ((v2d*)((char*)base+rev##I))[J]);      \
        } while (0)

        const pair_t * pairs = plan->pairs;
        ul i = plan->npairs;
        do {
                ul offset1 = pairs[i-1].i,
                   offset2 = pairs[i-1].j;

                if (likely(offset1 != offset2)) {
                        v2d * restrict base1
                                = (v2d*)((char*)data+offset1),
                            * restrict base2
                                = (v2d*)((char*)data+offset2);
                        BLOCK_SWAP(SWAP_DIFF);
                } else {
                        v2d * restrict base
                                = (v2d*)((char*)data+offset1);
                        BLOCK_SWAP(SWAP_SAME);
                }
        } while (--i);
#undef SWAP_SAME
#undef SWAP_DIFF
}

/* General case: shift value is unknown until runtime.
 */
static
void block_swap (revbin_plan_t plan, v2d * restrict data)
{
        return block_swap_(plan->shift, plan, data);
}

/* Medium-size transforms: shift amount known, but we still iterate
 * over the middle bit pairs.
 */
#define BLOCK_SWAP_N(N)                                                 \
        static void block_swap_##N (revbin_plan_t plan,                 \
                                    v2d * restrict data) {              \
                return block_swap_(N-LEAF_WIDTH, plan, data);           \
        }
FOREACH_MEDIUM_REV(BLOCK_SWAP_N)
#undef BLOCK_SWAP_N

/* Small transforms: we have fully-generated routines in
 * small_revbin.inc.  The (I < J) test in SWAP is needed to execute
 * each swap exactly once.  In theory, these could be pruned in the
 * generator.  However, the routines can be re-used for out-of-place
 * bit-reverses, or a fused bit-reverse and exchange of two vectors.
 */
#define LINKAGE static
#define PREFIX(N) block_swap_##N
#define ARGLIST (revbin_plan_t plan, v2d * restrict data)
#define SWAP(I, J)                                      \
        do {                                            \
                (void)plan;                             \
                if (I < J)                              \
                        SWAP_V2D(data[I], data[J]);     \
        } while (0)
#include "small_revbin.inc"
#undef SWAP
#undef ARGLIST
#undef PREFIX
#undef LINKAGE

/* Generate dummy plans for specialised routines at compile-time.
 */
static const struct revbin_plan
small_plans[] =
{
#define MAKE_PLAN(N) {.function = block_swap_##N,\
                      .shift = 0, .npairs = 0},
        FOREACH_SMALL_REV(MAKE_PLAN)
#undef MAKE_PLAN
};

/* LUT of medium-sized shift-specialised routines.
 */
static const block_function_t 
medium_block_functions[] =
{
#define NAME_FUNCTION(N) block_swap_##N,
        FOREACH_MEDIUM_REV(NAME_FUNCTION)
#undef NAME_FUNCTION
};

/* Support code to generate the middle-bit pairs.
 *
 * unswizzle: bit-de-interleave an integer into two half-sized
 *   integers.  The idea is that, rather than interleaving values and
 *   sorting them, we simply iterate over the interleaved values in
 *   order (trivially, by incrementing a counter), and deinterleave to
 *   find the corresponding values.
 *
 * reverse: bit-reverse.
 */
static pair_t
unswizzle (ul swizzled, unsigned half_width)
{
        ul i = 0, j = 0, bit = 1;
        while (half_width --> 0) {
                if (swizzled&1)
                        i |= bit;
                if (swizzled&2)
                        j |= bit;
                bit <<= 1;
                swizzled >>= 2;
        }

        return (pair_t){i, j};
}

static unsigned
reverse (unsigned x, unsigned width)
{
        unsigned acc = 0, bit = 1ul<<(width-1);
        for (unsigned i = 0; i < width; i++) {
                if (x&1) acc |= bit;
                bit >>= 1;
                x   >>= 1;
        }

        return acc;
}

/* Generate pairs of b, rev(b) in an order that exhibits some
 * locality.
 *
 * In theory, we'd want to iterate through the pairs of low-order bits
 * of b/rev(b) in morton order.  Instead, we iterate through the
 * interleaved values in order (trivial), and unswizzle the low order
 * bits.
 */
static unsigned
fill_pairs (unsigned width, pair_t * pairs)
{
        unsigned alloc = 0,
                 half  = (width+1)/2;
        ul     max = 1ul<<width;
        /* for odd sizes, unswizzled j conceptually "borrows" its top
         * bit from i; however, it's immediately ORed with the reverse
         * of i, which will have the right top bit value in the right
         * location, so all's well that ends well.
         */
        for (ul swizzled = 0; swizzled < max; swizzled++) {
                pair_t pair = unswizzle(swizzled, half);
                ul  i = pair.i,
                    j = pair.j,
                   ri = reverse(pair.i, width), 
                   rj = reverse(pair.j, width);

                i |= rj;
                j |= ri;
                if (i <= j) {
                        pairs[alloc].i = (i<<LEAF_WIDTH)*sizeof(v2d);
                        pairs[alloc].j = (j<<LEAF_WIDTH)*sizeof(v2d);
                        alloc++;
                }
        }

        return alloc;
}
/* Generate a revbin_plan for the given middle_width (number of bits
 * in the middle number b in a:b:c).
 *
 * The task consists of allocating a plan of the right size, filling
 * the function and shift slots, and then the pair sequence.
 */
static revbin_plan_t
make_offset_pairs (unsigned middle_width, block_function_t function)
{
        assert(middle_width+2*LEAF_WIDTH <= 8*sizeof(long));
        assert(middle_width < 8*sizeof(unsigned));
        unsigned npairs = (1u<<(middle_width-1)) 
                        + (1u<<((middle_width-1)/2));
        struct revbin_plan * plan =
                malloc(sizeof(struct revbin_plan)
                       + npairs*sizeof(pair_t));
        if (!plan) return plan;

        plan->function = function;
        plan->shift = middle_width+LEAF_WIDTH;
        plan->npairs = npairs;
        assert(fill_pairs(middle_width, plan->pairs) == npairs);

        return plan;
}

/* Memoisation table for revbin plans.
 *
 * Small transforms point to the compile-time generated plans, and the
 * rest is initialized to 0, as per the standard.
 */
static revbin_plan_t
plans [8*sizeof(long)] =
{
#define SMALL_PLAN(N) small_plans+N,
        FOREACH_SMALL_REV(SMALL_PLAN)
#undef SMALL_PLAN
};

/* Return a previously-generated plan or generate one.
 */
revbin_plan_t
revbin_plan (unsigned width)
{
        assert(width < 8*sizeof(long));

        revbin_plan_t plan = plans[width];
        if (plan) return plan;

        assert((width > SMALL_WIDTH_MAX)
               && "Small width plans should be pre-filled");

        block_function_t block_fun
                = width <= MEDIUM_WIDTH_MAX
                ? medium_block_functions[width-SMALL_WIDTH_MAX-1]
                : block_swap;

        plan = make_offset_pairs(width-2*LEAF_WIDTH,
                                 block_fun);

        return plans[width] = plan;
}

#ifdef TEST_REVBIN
int main (int argc, char **argv)
{
        unsigned width = 20;
        if (argc > 1)
                width = atoi(argv[1]);

        revbin_plan_t plan = revbin_plan(width);

        pair_t * pairs = calloc(1ul<<width, sizeof(pair_t));
        for (unsigned i = 0; i < 1u<<width; i++)
                pairs[i].i = i;

        revbin_execute(plan, (v2d*)pairs);
        for (unsigned i = 0; i < 1u<<width; i++)
                assert(pairs[i].i == reverse(i, width));
        revbin_execute(plan, (v2d*)pairs);
        for (unsigned i = 0; i < 1u<<width; i++)
                assert(pairs[i].i == i);

        return 0;
}
#endif
