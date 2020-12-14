#pragma once

typedef unsigned long int Hashed;

Hashed hash(unsigned char *str)
{
    Hashed h = 5281;
    int c;

    while (c = *str++)
        h = ((h << 5) + h) + c;
    return h;
}