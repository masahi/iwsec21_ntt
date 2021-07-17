#ifndef REDUCE_H
#define REDUCE_H

#include <stdint.h>

uint16_t montgomery_reduce_newhope(uint32_t a);

uint16_t barrett_reduce_newhope(uint16_t a);

#endif
