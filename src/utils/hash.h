#pragma once

#include <stdint.h>

static inline uint64_t hash(unsigned char *str)
{
    uint64_t h = 5281;
    int c;

    while (c = *str++)
        h = ((h << 5) + h) + c;
    return h;
}