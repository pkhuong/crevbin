#include "block_swap_bits.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#define likely(x) __builtin_expect((x),1)

typedef unsigned long ul;
typedef double v2d __attribute__ ((vector_size (16)));

struct swap_plan;

typedef void (*swap_function_t)(const struct swap_plan *, v2d *);

struct offset_pair {
        ul i, j;
};

struct swap_plan {
        swap_function_t function;
        unsigned shift; // low bits + middle bits
        unsigned npairs;
        struct offset_pair pairs[];
};

void execute_plan (const struct swap_plan * plan, 
                   v2d * data)
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
void block_swap_(unsigned shift,
                 const struct swap_plan * plan,
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

        const struct offset_pair * pairs = plan->pairs;
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
void block_swap(const struct swap_plan * plan,
                v2d * restrict data)
{
        return block_swap_(plan->shift, plan, data);
}

#define BLOCK_SWAP_N(N)                                                 \
        static void block_swap_##N (const struct swap_plan * plan,      \
                                    v2d * restrict data) {              \
                return block_swap_(N-LEAF_WIDTH, plan, data);           \
        }
FOREACH_MEDIUM_REV(BLOCK_SWAP_N)
#undef BLOCK_SWAP_N

#define LINKAGE static
#define PREFIX(N) block_swap_##N
#define ARGLIST (const struct swap_plan * plan, v2d * restrict data)
#define SWAP(I, J)                                      \
        do {                                            \
                (void)plan;                             \
                if (I < J)                              \
                        SWAP_V2D(data[I], data[J]);     \
        } while (0)
#include "small_reverse.inc"
#undef SWAP
#undef ARGLIST
#undef PREFIX
#undef LINKAGE

const struct swap_plan small_plans[] =
{
#define MAKE_PLAN(N) {.function = block_swap_##N,\
                      .shift = 0, .npairs = 0},
        FOREACH_SMALL_REV(MAKE_PLAN)
#undef MAKE_PLAN
};

const swap_function_t medium_swap_functions[] =
{
#define NAME_FUNCTION(N) block_swap_##N,
        FOREACH_MEDIUM_REV(NAME_FUNCTION)
#undef NAME_FUNCTION
};

static struct offset_pair
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

        return (struct offset_pair){i, j};
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
fill_even_pairs (unsigned width, struct offset_pair * pairs)
{
        unsigned alloc = 0,
                 half  = width/2;
        ul max = 1ul<<width;
        for (ul i = 0; i < max; i++) {
                struct offset_pair pair = unswizzle(i, half);
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


static unsigned 
fill_odd_pairs (unsigned width, struct offset_pair * pairs)
{
        unsigned alloc = 0,
                 half  = (width+1)/2;

        ul max = 1ul<<width, top_bit = 1ul<<half;
        for (ul i = 0; i < max; i++) {
                struct offset_pair pair = unswizzle(i, half);
                pair.j |= (pair.i&top_bit);
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

static const struct swap_plan *
make_offset_pairs (unsigned middle_width, swap_function_t function)
{
        assert(middle_width+2*LEAF_WIDTH <= 8*sizeof(long));
        assert(middle_width < 8*sizeof(unsigned));
        unsigned npairs = (1u<<(middle_width-1)) 
                        + (1u<<((middle_width-1)/2));
        struct swap_plan * plan =
                malloc(sizeof(struct swap_plan)
                       + npairs*sizeof(struct offset_pair));
        if (!plan) return plan;

        plan->function = function;
        plan->shift = middle_width+LEAF_WIDTH;
        plan->npairs = npairs;
        if (middle_width&1) {
                assert(fill_odd_pairs(middle_width, plan->pairs)
                       == npairs);
        } else {
                assert(fill_even_pairs(middle_width, plan->pairs)
                       == npairs);
        }

        return plan;
}

const struct swap_plan * plans [8*sizeof(long)] =
{
#define SMALL_PLAN(N) small_plans+N,
        FOREACH_SMALL_REV(SMALL_PLAN)
#undef SMALL_PLAN
};

const struct
swap_plan * revbin_plan(unsigned width)
{
        assert(width < 8*sizeof(long));

        const struct swap_plan * plan = plans[width];
        if (plan) return plan;

        assert((width > SMALL_WIDTH_MAX)
               && "Small width plans should be pre-filled");

        swap_function_t block_swap_fun
                = width <= MEDIUM_WIDTH_MAX
                ? medium_swap_functions[width-SMALL_WIDTH_MAX-1]
                : block_swap;

        plan = make_offset_pairs(width-2*LEAF_WIDTH,
                                 block_swap_fun);

        return plans[width] = plan;
}

int main (int argc, char **argv)
{
        unsigned width = 20;
        if (argc > 1)
                width = atoi(argv[1]);

        const struct swap_plan * plan = revbin_plan(width);

        struct offset_pair * pairs
                = calloc(1ul<<width, sizeof(struct offset_pair));
        for (unsigned i = 0; i < 1u<<width; i++)
                pairs[i].i = i;

        execute_plan(plan, (v2d*)pairs);
        for (unsigned i = 0; i < 1u<<width; i++)
                assert(pairs[i].i == reverse(i, width));
        execute_plan(plan, (v2d*)pairs);
        for (unsigned i = 0; i < 1u<<width; i++)
                assert(pairs[i].i == i);

        return 0;
}
