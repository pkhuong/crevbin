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

struct revbin_plan {
        block_function_t function;
        unsigned shift; // low bits + middle bits
        unsigned npairs;
        pair_t pairs[];
};

void revbin_execute (revbin_plan_t plan, v2d * data)
{
        plan->function(plan, data);
}

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

static
void block_swap (revbin_plan_t plan, v2d * restrict data)
{
        return block_swap_(plan->shift, plan, data);
}

#define BLOCK_SWAP_N(N)                                                 \
        static void block_swap_##N (revbin_plan_t plan,                 \
                                    v2d * restrict data) {              \
                return block_swap_(N-LEAF_WIDTH, plan, data);           \
        }
FOREACH_MEDIUM_REV(BLOCK_SWAP_N)
#undef BLOCK_SWAP_N

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

static const struct revbin_plan
small_plans[] =
{
#define MAKE_PLAN(N) {.function = block_swap_##N,\
                      .shift = 0, .npairs = 0},
        FOREACH_SMALL_REV(MAKE_PLAN)
#undef MAKE_PLAN
};

static const block_function_t 
medium_block_functions[] =
{
#define NAME_FUNCTION(N) block_swap_##N,
        FOREACH_MEDIUM_REV(NAME_FUNCTION)
#undef NAME_FUNCTION
};

static pair_t
unswizzle (unsigned swizzled, unsigned half_width)
{
        unsigned i = 0, j = 0, bit = 1;
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

static unsigned
fill_pairs (unsigned width, pair_t * pairs)
{
        unsigned alloc = 0,
                 half  = (width+1)/2;
        ul     max = 1ul<<width,
           top_bit = (width&1)?1ul<<(half-1):0ul;
        /* for odd sizes, unswizzled j "borrows" its top bit from i */
        for (ul i = 0; i < max; i++) {
                pair_t pair = unswizzle(i, half);
                pair.j |= pair.i&top_bit;

                ul ri = reverse(pair.i, width), 
                   rj = reverse(pair.j, width);

                ul i = pair.i|rj, j = pair.j|ri;
                if (i <= j) {
                        pairs[alloc].i = (i<<LEAF_WIDTH)*sizeof(v2d);
                        pairs[alloc].j = (j<<LEAF_WIDTH)*sizeof(v2d);
                        alloc++;
                }
        }

        return alloc;
}

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

static revbin_plan_t
plans [8*sizeof(long)] =
{
#define SMALL_PLAN(N) small_plans+N,
        FOREACH_SMALL_REV(SMALL_PLAN)
#undef SMALL_PLAN
};


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
