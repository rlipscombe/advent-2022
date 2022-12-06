#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int has_dupes(const char *begin, const char *end)
{
    const char *p, *q;
    for (p = begin; p < end; ++p)
    {
        for (q = p + 1; q < end; ++q)
        {
            if (*p == *q)
            {
                return 1;
            }
        }
    }

    return 0;
}

int main(int argc, char *argv[])
{
    int count;
    char *input;
    int i;
    size_t len;

    if (argc < 3)
    {
        exit(2);
    }

    count = strtol(argv[1], NULL, 10);
    input = argv[2];

    len = strlen(input);
    for (i = 0; i < len - count; ++i)
    {
        if (!has_dupes(input + i, input + i + count))
        {
            break;
        }
    }

    printf("%d\n", i + count);

    return 0;
}
