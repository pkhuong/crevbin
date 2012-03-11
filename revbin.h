#ifndef REVBIN_H
#define REVBIN_H

struct revbin_plan;
typedef const struct revbin_plan * revbin_plan_t;

typedef double v2d __attribute__ ((vector_size (16)));

void revbin_execute (revbin_plan_t plan, v2d * data);
revbin_plan_t revbin_plan (unsigned width);
#endif
