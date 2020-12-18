/*
** EPITECH PROJECT, 2020
** NWP_myftp_2019
** File description:
** array
*/

#include <stdlib.h>
#include <string.h>
#include "array.h"

void *i_array_grow(void **array, uint32_t element_size,
    uint32_t required_elements)
{
    uint32_t new_cap = (required_elements > __ARRAY_START_CAP ?
                    required_elements : __ARRAY_START_CAP) - 1;
    new_cap |= new_cap >> 1;
    new_cap |= new_cap >> 2;
    new_cap |= new_cap >> 4;
    new_cap |= new_cap >> 8;
    new_cap |= new_cap >> 16;
    new_cap += 1;

    *array = (void *)((uint32_t *)realloc(__ARRAY_RAW(*array),
        new_cap * element_size + 2 * sizeof(uint32_t)) + 2);
    if (*array == 0)
        return 0;
    *(__ARRAY_RAW(*array) + __ARRAY_CAP) = new_cap;
    return *array;
}

int i_array_insert(void **array, void *element, uint32_t element_size,
    uint32_t pos)
{
    int array_null = !*array;

    if (ARRAY_CAP(*array) < ARRAY_SIZE(*array) + 1) {
        if (i_array_grow(array, element_size, ARRAY_SIZE(*array) + 1) == 0)
            return 0;
    }
    if (array_null)
        __ARRAY_RAW(*array)[0] = 0;
    if (ARRAY_SIZE(*array) - pos > 0) {
        memmove(((char *)(*array) + (element_size * (pos + 1))),
                ((char *)(*array) + (element_size * pos)),
                element_size * (ARRAY_SIZE(*array) - pos));
    }
    memcpy(((char *)(*array)) + (element_size * pos), element, element_size);
    __ARRAY_RAW(*array)[__ARRAY_SIZE] += 1;
    return 1;
}

int i_array_concat(void **dest, void **src, uint32_t element_size)
{
    if (i_array_grow(dest, element_size,
        ARRAY_SIZE(*dest) + ARRAY_SIZE(*src)) == 0)
        return 0;
    memcpy((char *)(*dest) + element_size * ARRAY_SIZE(*dest), *src,
            element_size * ARRAY_SIZE(*src));
    __ARRAY_RAW(*dest)[__ARRAY_SIZE] += ARRAY_SIZE(*src);
    return 1;
}

int i_array_erase_shift(void **array, uint32_t element_size, uint32_t pos)
{
    if (ARRAY_SIZE(*array) > 1) {
        memmove(((char *)(*array) + (element_size * pos)),
                ((char *)(*array) + (element_size * (pos + 1))),
                element_size * (ARRAY_SIZE(*array) - pos - 1));
    }
    __ARRAY_RAW(*array)[__ARRAY_SIZE] -= 1;
    return 1;
}

int i_array_erase_swap(void **array, uint32_t element_size, uint32_t pos)
{
    if (ARRAY_SIZE(*array) > 1) {
        memcpy(((char *)(*array) + (element_size * pos)),
                ((char *)(*array) + (element_size * (ARRAY_SIZE(*array) - 1))),
                element_size);
    }
    __ARRAY_RAW(*array)[__ARRAY_SIZE] -= 1;
    return 1;
}