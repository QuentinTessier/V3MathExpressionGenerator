/*
** EPITECH PROJECT, 2020
** Array_new
** File description:
** array_io
*/

#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include "array.h"

void i_array_write_format(void **array, uint32_t element_size,
    const array_format_t *format, void *userdata)
{
    uint32_t element_count = ARRAY_SIZE(*array);
    array_ite_info_t infos = {0, element_count};
    char *ite = *array;
    const char *end = (*array) + (element_size * element_count);

    if (element_count == 0)
        return;
    while (ite != end) {
        format->printer(infos, ite, format->format_str, userdata);
        if (infos.idx != (infos.size - 1))
            printf("%c", format->sep);
        infos.idx += 1;
        ite += element_size;
    }
}

void i_array_write(int fd, void **a, uint32_t element_size,
    uint32_t element_count)
{
    void *begin = __ARRAY_RAW(*a);

    write(fd, begin, element_size * element_count);
}

void i_array_read(int fd, void **a, size_t element_size)
{
    uint32_t tmp[2] = { 0 };
    char *buffer = 0;

    read(fd, tmp, sizeof(uint32_t) * 2);
    if ((*a) == 0 || ARRAY_CAP((*a)) > tmp[__ARRAY_CAP])
        i_array_grow(a, element_size, tmp[__ARRAY_CAP]);

    memcpy(__ARRAY_RAW(*a), tmp, sizeof(uint32_t) * 2);
    buffer = *a;
    read(fd, buffer, element_size * tmp[__ARRAY_CAP]);
}