#include <stddef.h>
#include <string.h>

/* Inspect only bytes preceding the first match. This also permits a large
 * count when a matching byte occurs before an inaccessible page. */
/* C23 headers may provide a type-generic function-like macro. Parentheses
 * select the ordinary C ABI definition declared by <string.h>. */
void *(memchr)(const void *buffer, int value, size_t count) {
    const unsigned char *cursor = buffer;
    const unsigned char needle = (unsigned char)value;
    for (size_t index = 0; index < count; ++index) {
        if (cursor[index] == needle) {
            return (void *)(cursor + index);
        }
    }
    return NULL;
}
