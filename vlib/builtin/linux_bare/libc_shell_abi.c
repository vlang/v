#include "libc_compat.h"
#include "libc_shell_abi.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

extern int32_t v_shell_system(uintptr_t);
extern uintptr_t v_shell_popen(uintptr_t, uintptr_t);
extern int32_t v_shell_pclose(uintptr_t);
int system(const char *command) { return v_shell_system((uintptr_t)command); }
FILE *popen(const char *command, const char *mode) {
    return (FILE *)v_shell_popen((uintptr_t)command, (uintptr_t)mode);
}
int pclose(FILE *stream) { return v_shell_pclose((uintptr_t)stream); }
uintptr_t v_shell_stream_open(int32_t fd, uintptr_t mode) { return (uintptr_t)fdopen(fd, (const char *)mode); }
int32_t v_shell_stream_close(uintptr_t stream) { return fclose((FILE *)stream); }
