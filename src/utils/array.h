/*
** EPITECH PROJECT, 2020
** NWP_myftp_2019
** File description:
** array
*/

#pragma once

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define ARRAY(type)         type *

#define __ARRAY_START_CAP   32

#define __ARRAY_SIZE        0
#define __ARRAY_CAP         1
#define __ARRAY_RAW(a)      ((a) ? ((uint32_t *)(a)) - 2 : 0)

#define ARRAY_SIZE(a)       ((a) ? __ARRAY_RAW(a)[__ARRAY_SIZE] : 0)
#define ARRAY_CAP(a)        ((a) ? __ARRAY_RAW(a)[__ARRAY_CAP] : 0)

void *i_array_grow(void **array, uint32_t element_size,
    uint32_t required_elements);
int i_array_insert(void **array, void *element, uint32_t element_size,
    uint32_t pos);
int i_array_concat(void **array, void **src, uint32_t element_size);
int i_array_erase_swap(void **array, uint32_t element_size, uint32_t pos);
int i_array_erase_shift(void **array, uint32_t element_size, uint32_t pos);

#define ARRAY_PREALLOC(a, size)                                             \
    if (ARRAY_SIZE(a) == 0) { i_array_grow((void **)&(a), sizeof(*a), size); }

#define ARRAY_PUSH(a, e)                                                    \
    i_array_insert((void **)&(a), (void *)&(e), sizeof(e), ARRAY_SIZE(a))

#define ARRAY_INSERT(a, e, i)                                               \
    i_array_insert((void **)&(a), (void *)&(e), sizeof(e), i)

#define ARRAY_POP(a)                                                        \
    if (ARRAY_SIZE(a)) { ARRAY_RAW(a)[__ARRAY_SIZE] -= 1; }

#define ARRAY_ERASE_SWAP(a, i)                                              \
    if (ARRAY_SIZE(a)) { i_array_erase_swap((void **)&(a), sizeof((a)[0]), i); }

#define ARRAY_ERASE_SHIFT(a, i)                                             \
    if (ARRAY_SIZE(a)) {                                                    \
        i_array_erase_shift((void **)&(a), sizeof((a)[0]), i);              \
    }

#define ARRAY_ERASE         ARRAY_ERASE_SHIFT

#define ARRAY_ERASE_SLOW    ARRAY_ERASE

#define ARRAY_ERASE_FAST    ARRAY_ERASE_SHIFT

#define ARRAY_CLEAR(a)                                                      \
    if (ARRAY_SIZE(a)) { __ARRAY_RAW(a)[__ARRAY_SIZE] = 0; }

#define ARRAY_FREE(a)                                                       \
    if (a) { free(__ARRAY_RAW(a)); (a) = 0; }

#define ARRAY_CONST_FOREACH(a, ite_name, code)                              \
    for (uint32_t const_ite = 0; const_ite < ARRAY_SIZE(a); ++const_ite) {  \
        const typeof(a) ite_name = &(a)[const_ite];                         \
        code                                                                \
    }

#define ARRAY_FOREACH(a, ite_name, code)                                    \
    for (uint32_t const_ite = 0; const_ite < ARRAY_SIZE(a); ++const_ite) {  \
        typeof(a) ite_name = &(a)[const_ite];                               \
        code                                                                \
    }

typedef int (*array_search_t)(const void *ite, const void *elem,
    void *userdata);

#define ARRAY_SEARCH(a, func, element, userdata, res)                       \
    ARRAY_CONST_FOREACH(a, ite, {                                           \
        if (func(ite, element, userdata)) {                                 \
            res = ite;                                                      \
            break;                                                          \
        } else                                                              \
            res = 0;                                                        \
    })

#define ARRAY_AT(a, index, default_value)                                   \
    (ARRAY_CAP(a) != 0 && ARRAY_SIZE(a) <= index) ? (default_value) : (a[index])

#define ARRAY_END_PTR(a)                                                    \
    (ARRAY_CAP(a) != 0) ? (a + (ARRAY_SIZE(a))) : 0;

#define ARRAY_BACK_PTR(a)                                                   \
    (ARRAY_CAP(a) != 0) ? (a + ((ARRAY_SIZE(a) - 1))) : 0;

typedef struct array_format array_format_t;

typedef struct array_ite_info {
    uint32_t idx;
    uint32_t size;
} array_ite_info_t;

typedef void (*element_print_t)(array_ite_info_t infos, void *element,
    const char *format_str, void *userdata);
typedef void *(*element_read_t)(void *buffer,
    const char *str_start, const char *format_str);

typedef struct array_format {
    char sep;
    char *format_str;
    char *scan_str;
    element_print_t printer;
    element_read_t reader;
} array_format_t;

void i_array_write(int fd, void **a,
    uint32_t element_size, uint32_t element_count);
void i_array_write_format(void **array, uint32_t element_size,
    const array_format_t *format, void *userdata);
void i_array_read(int fd, void **a, size_t element_size);

#define ARRAY_WRITE_RAW(fd, a)                                              \
    if (ARRAY_SIZE(a)) {                                                    \
        i_array_write(fd, (void **)&(a), sizeof(*a), ARRAY_CAP(a));         \
    }

#define ARRAY_FORMAT_WRITE(a, format, userdata)                             \
    if (ARRAY_SIZE(a))  {                                                   \
        i_array_write_format((void **)&(a), sizeof(*a), format, userdata);  \
    }

#define ARRAY_READ_RAW(fd, a)                                               \
        i_array_read(fd, (void **)&(a), sizeof(*a));

#define ARRAY_READ_FORMAT(a, format, str)                                   \
    char *array_token = strtok(str, &format->sep);                          \
    typeof(*a) tmp_var;                                                     \
    while (array_token != 0) {                                              \
        format->reader(&tmp_var, array_token, format->scan_str);            \
        ARRAY_PUSH(a, tmp_var);                                             \
        array_token = strtok(0, &format->sep);                              \
    }
