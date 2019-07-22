#define MICROSOFT_CRAZINESS_IMPLEMENTATION

#include "microsoft_craziness.h"

// Never hang around to a pointer from this function
// This is not thread safe
char *wide_string_to_narrow_string_facepalm(wchar_t *wc) {
    static char buffer[10000][10];
    static int counter = 0;

    char *cur_buffer = buffer[counter++];

    int len = wcslen(wc);
    int c = wcstombs(cur_buffer, wc, len);
    cur_buffer[c] = 0;

    // something assert len == c

    return cur_buffer;
}