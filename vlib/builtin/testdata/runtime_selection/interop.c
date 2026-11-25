#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include "interop.h"
#include <errno.h>
#include <netdb.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void *runtime_c_allocate(void) {
    char *pointer = malloc(64);
    if (pointer) memcpy(pointer, "C allocation", 13);
    return pointer;
}
void runtime_c_release(void *pointer) { free(pointer); }
void *runtime_c_resize(void *pointer, uintptr_t size) { return realloc(pointer, size); }
uintptr_t runtime_c_errno_address(void) {
    errno = 0;
    if (close(-1) != -1 || errno != EBADF) return 0;
    return (uintptr_t)&errno;
}
int runtime_c_interop(void) {
#ifdef TEST_HOST_LIBC
    /* These services must retain libc's private FILE and allocation ABIs. */
    char *copy = strdup("host allocation");
    if (!copy || strcmp(copy, "host allocation")) return -1;
    free(copy);
    FILE *file = tmpfile();
    if (!file) return -2;
    if (fputs("buffered input\n", file) < 0 || fseek(file, 0, SEEK_SET)) {
        fclose(file); return -3;
    }
    char *line = NULL;
    size_t capacity = 0;
    ssize_t length = getline(&line, &capacity, file);
    int valid = length == 15 && !strcmp(line, "buffered input\n");
    free(line);
    if (fclose(file) || !valid) return -4;
    struct addrinfo hints = {.ai_family = AF_INET, .ai_socktype = SOCK_STREAM,
                            .ai_flags = AI_NUMERICHOST}, *result = NULL;
    if (getaddrinfo("127.0.0.1", "80", &hints, &result)) return -5;
    freeaddrinfo(result);
    return 1;
#else
    return 0;
#endif
}
void runtime_c_buffered_output(void) {
    setvbuf(stdout, NULL, _IOFBF, 4096);
    printf("buffered C output\n");
}
