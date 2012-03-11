#ifndef REVBIN_BITS_H
#define REVBIN_BITS_H

#define LEAF_WIDTH       3
#define SMALL_WIDTH_MAX  8
#define MEDIUM_WIDTH_MAX 24

#define PRELOAD_REV(SHIFT, SCALE) \
	const ul rev0 = (0UL<<(SHIFT))*(SCALE);\
	const ul rev1 = (4UL<<(SHIFT))*(SCALE);\
	const ul rev2 = (2UL<<(SHIFT))*(SCALE);\
	const ul rev3 = (6UL<<(SHIFT))*(SCALE);\
	const ul rev4 = (1UL<<(SHIFT))*(SCALE);\
	const ul rev5 = (5UL<<(SHIFT))*(SCALE);\
	const ul rev6 = (3UL<<(SHIFT))*(SCALE);\
	const ul rev7 = (7UL<<(SHIFT))*(SCALE);

#define BLOCK_SWAP(SWAPPER) do {\
	SWAPPER(0, 0);\
	SWAPPER(1, 0);\
	SWAPPER(0, 1);\
	SWAPPER(1, 1);\
	SWAPPER(2, 0);\
	SWAPPER(3, 0);\
	SWAPPER(2, 1);\
	SWAPPER(3, 1);\
	SWAPPER(0, 2);\
	SWAPPER(1, 2);\
	SWAPPER(0, 3);\
	SWAPPER(1, 3);\
	SWAPPER(2, 2);\
	SWAPPER(3, 2);\
	SWAPPER(2, 3);\
	SWAPPER(3, 3);\
	SWAPPER(4, 0);\
	SWAPPER(5, 0);\
	SWAPPER(4, 1);\
	SWAPPER(5, 1);\
	SWAPPER(6, 0);\
	SWAPPER(7, 0);\
	SWAPPER(6, 1);\
	SWAPPER(7, 1);\
	SWAPPER(4, 2);\
	SWAPPER(5, 2);\
	SWAPPER(4, 3);\
	SWAPPER(5, 3);\
	SWAPPER(6, 2);\
	SWAPPER(7, 2);\
	SWAPPER(6, 3);\
	SWAPPER(7, 3);\
	SWAPPER(0, 4);\
	SWAPPER(1, 4);\
	SWAPPER(0, 5);\
	SWAPPER(1, 5);\
	SWAPPER(2, 4);\
	SWAPPER(3, 4);\
	SWAPPER(2, 5);\
	SWAPPER(3, 5);\
	SWAPPER(0, 6);\
	SWAPPER(1, 6);\
	SWAPPER(0, 7);\
	SWAPPER(1, 7);\
	SWAPPER(2, 6);\
	SWAPPER(3, 6);\
	SWAPPER(2, 7);\
	SWAPPER(3, 7);\
	SWAPPER(4, 4);\
	SWAPPER(5, 4);\
	SWAPPER(4, 5);\
	SWAPPER(5, 5);\
	SWAPPER(6, 4);\
	SWAPPER(7, 4);\
	SWAPPER(6, 5);\
	SWAPPER(7, 5);\
	SWAPPER(4, 6);\
	SWAPPER(5, 6);\
	SWAPPER(4, 7);\
	SWAPPER(5, 7);\
	SWAPPER(6, 6);\
	SWAPPER(7, 6);\
	SWAPPER(6, 7);\
	SWAPPER(7, 7);\
} while (0)

#define FOREACH_SMALL_REV(MACRO) \
	MACRO(0)\
	MACRO(1)\
	MACRO(2)\
	MACRO(3)\
	MACRO(4)\
	MACRO(5)\
	MACRO(6)\
	MACRO(7)\
	MACRO(8)

#define FOREACH_MEDIUM_REV(MACRO) \
	MACRO(9)\
	MACRO(10)\
	MACRO(11)\
	MACRO(12)\
	MACRO(13)\
	MACRO(14)\
	MACRO(15)\
	MACRO(16)\
	MACRO(17)\
	MACRO(18)\
	MACRO(19)\
	MACRO(20)\
	MACRO(21)\
	MACRO(22)\
	MACRO(23)\
	MACRO(24)

#endif
