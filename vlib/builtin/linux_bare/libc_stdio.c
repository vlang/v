#include "libc_compat.h"
#include "libc_cancel_abi.h"
#define V_STDIO_RETURN(type, expression) { \
    uint32_t v_stdio_saved_cancel_state = v_cancel_shield_enter(); \
    type v_stdio_return_value = (expression); \
    v_cancel_shield_leave(v_stdio_saved_cancel_state); return v_stdio_return_value; }
#include "libc_stdio_abi.h"

/* Aligned opaque storage and native public C types only. FILE state and all
 * buffering, ownership, locking and I/O policy live in stdio_d_freestanding.c.v. */
static _Alignas(long double) unsigned char v_stdio_objects[3][512];
__typeof__(stdin) stdin = (FILE *)v_stdio_objects[0];
__typeof__(stdout) stdout = (FILE *)v_stdio_objects[1];
__typeof__(stderr) stderr = (FILE *)v_stdio_objects[2];
uintptr_t v_stdio_standard(int32_t index) { return (uintptr_t)v_stdio_objects[index]; }
size_t v_stdio_standard_size(void) { return sizeof v_stdio_objects[0]; }
int64_t v_stdio_cookie_read(uintptr_t f, uintptr_t cookie, uintptr_t output, uintptr_t size) {
    return ((cookie_read_function_t *)f)((void *)cookie, (char *)output, size);
}
int64_t v_stdio_cookie_write(uintptr_t f, uintptr_t cookie, uintptr_t input, uintptr_t size) {
    return ((cookie_write_function_t *)f)((void *)cookie, (const char *)input, size);
}
int32_t v_stdio_cookie_seek(uintptr_t f, uintptr_t cookie, int64_t *offset, int32_t whence) {
    off64_t value = *offset;
    int result = ((cookie_seek_function_t *)f)((void *)cookie, &value, whence);
    *offset = value;
    return result;
}
int32_t v_stdio_cookie_close(uintptr_t f, uintptr_t cookie) {
    return ((cookie_close_function_t *)f)((void *)cookie);
}
extern void v_stdio_lock(uintptr_t);
void flockfile(FILE *file) { v_stdio_lock((uintptr_t)file); }
extern int32_t v_stdio_trylock(uintptr_t);
int ftrylockfile(FILE *file) { return (int)v_stdio_trylock((uintptr_t)file); }
extern void v_stdio_unlock(uintptr_t);
void funlockfile(FILE *file) { v_stdio_unlock((uintptr_t)file); }
extern uintptr_t v_stdio_fopen(uintptr_t, uintptr_t);
FILE * fopen(const char *path, const char *mode) V_STDIO_RETURN(FILE *, (FILE *)v_stdio_fopen((uintptr_t)path, (uintptr_t)mode))
extern uintptr_t v_stdio_fdopen(int32_t, uintptr_t);
FILE * fdopen(int fd, const char *mode) V_STDIO_RETURN(FILE *, (FILE *)v_stdio_fdopen((int32_t)fd, (uintptr_t)mode))
extern int32_t v_stdio_close(uintptr_t);
int fclose(FILE *file) V_STDIO_RETURN(int, (int)v_stdio_close((uintptr_t)file))
extern int32_t v_stdio_flush(uintptr_t);
int fflush(FILE *file) V_STDIO_RETURN(int, (int)v_stdio_flush((uintptr_t)file))
extern uintptr_t v_stdio_fread(uintptr_t, uintptr_t, uintptr_t, uintptr_t);
size_t fread(void *data, size_t size, size_t count, FILE *file) V_STDIO_RETURN(size_t, (size_t)v_stdio_fread((uintptr_t)data, (uintptr_t)size, (uintptr_t)count, (uintptr_t)file))
extern uintptr_t v_stdio_read(uintptr_t, uintptr_t, uintptr_t, uintptr_t);
size_t fread_unlocked(void *data, size_t size, size_t count, FILE *file) V_STDIO_RETURN(size_t, (size_t)v_stdio_read((uintptr_t)data, (uintptr_t)size, (uintptr_t)count, (uintptr_t)file))
extern uintptr_t v_stdio_fwrite(uintptr_t, uintptr_t, uintptr_t, uintptr_t);
size_t fwrite(const void *data, size_t size, size_t count, FILE *file) V_STDIO_RETURN(size_t, (size_t)v_stdio_fwrite((uintptr_t)data, (uintptr_t)size, (uintptr_t)count, (uintptr_t)file))
extern uintptr_t v_stdio_write(uintptr_t, uintptr_t, uintptr_t, uintptr_t);
size_t fwrite_unlocked(const void *data, size_t size, size_t count, FILE *file) V_STDIO_RETURN(size_t, (size_t)v_stdio_write((uintptr_t)data, (uintptr_t)size, (uintptr_t)count, (uintptr_t)file))
extern int32_t v_stdio_seek(uintptr_t, int64_t, int32_t);
int fseeko(FILE *file, off_t offset, int whence) V_STDIO_RETURN(int, (int)v_stdio_seek((uintptr_t)file, (int64_t)offset, (int32_t)whence))
int fseek(FILE *file, long offset, int whence) V_STDIO_RETURN(int, (int)v_stdio_seek((uintptr_t)file, (int64_t)offset, (int32_t)whence))
extern int64_t v_stdio_tell(uintptr_t);
off_t ftello(FILE *file) V_STDIO_RETURN(off_t, (off_t)v_stdio_tell((uintptr_t)file))
long ftell(FILE *file) V_STDIO_RETURN(long, (long)v_stdio_tell((uintptr_t)file))
extern void v_stdio_rewind(uintptr_t);
void rewind(FILE *file) { v_stdio_rewind((uintptr_t)file); }
extern int32_t v_stdio_setbuffer(uintptr_t, uintptr_t, int32_t, uintptr_t);
int setvbuf(FILE *file, char *buffer, int mode, size_t size) V_STDIO_RETURN(int, (int)v_stdio_setbuffer((uintptr_t)file, (uintptr_t)buffer, (int32_t)mode, (uintptr_t)size))
extern void v_stdio_setbuf(uintptr_t, uintptr_t);
void setbuf(FILE *file, char *buffer) { v_stdio_setbuf((uintptr_t)file, (uintptr_t)buffer); }
extern int32_t v_stdio_fgetc(uintptr_t);
int fgetc(FILE *file) V_STDIO_RETURN(int, (int)v_stdio_fgetc((uintptr_t)file))
int getc(FILE *file) V_STDIO_RETURN(int, (int)v_stdio_fgetc((uintptr_t)file))
int getchar(void) V_STDIO_RETURN(int, (int)v_stdio_fgetc((uintptr_t)stdin))
extern int32_t v_stdio_getchar(uintptr_t);
int fgetc_unlocked(FILE *file) V_STDIO_RETURN(int, (int)v_stdio_getchar((uintptr_t)file))
int getc_unlocked(FILE *file) V_STDIO_RETURN(int, (int)v_stdio_getchar((uintptr_t)file))
int getchar_unlocked(void) V_STDIO_RETURN(int, (int)v_stdio_getchar((uintptr_t)stdin))
extern int32_t v_stdio_fputc(int32_t, uintptr_t);
int fputc(int value, FILE *file) V_STDIO_RETURN(int, (int)v_stdio_fputc((int32_t)value, (uintptr_t)file))
int putc(int value, FILE *file) V_STDIO_RETURN(int, (int)v_stdio_fputc((int32_t)value, (uintptr_t)file))
int putchar(int value) V_STDIO_RETURN(int, (int)v_stdio_fputc((int32_t)value, (uintptr_t)stdout))
extern int32_t v_stdio_putchar(int32_t, uintptr_t);
int fputc_unlocked(int value, FILE *file) V_STDIO_RETURN(int, (int)v_stdio_putchar((int32_t)value, (uintptr_t)file))
int putc_unlocked(int value, FILE *file) V_STDIO_RETURN(int, (int)v_stdio_putchar((int32_t)value, (uintptr_t)file))
int putchar_unlocked(int value) V_STDIO_RETURN(int, (int)v_stdio_putchar((int32_t)value, (uintptr_t)stdout))
extern int32_t v_stdio_fputs(uintptr_t, uintptr_t);
int fputs(const char *text, FILE *file) V_STDIO_RETURN(int, (int)v_stdio_fputs((uintptr_t)text, (uintptr_t)file))
extern int32_t v_stdio_puts(uintptr_t);
int puts(const char *text) V_STDIO_RETURN(int, (int)v_stdio_puts((uintptr_t)text))
extern uintptr_t v_stdio_fgets(uintptr_t, int32_t, uintptr_t);
char * fgets(char *buffer, int size, FILE *file) V_STDIO_RETURN(char *, (char *)v_stdio_fgets((uintptr_t)buffer, (int32_t)size, (uintptr_t)file))
extern int64_t v_stdio_getdelim(uintptr_t, uintptr_t, int32_t, uintptr_t);
ssize_t getdelim(char **line, size_t *capacity, int delimiter, FILE *file) V_STDIO_RETURN(ssize_t, (ssize_t)v_stdio_getdelim((uintptr_t)line, (uintptr_t)capacity, (int32_t)delimiter, (uintptr_t)file))
ssize_t getline(char **line, size_t *capacity, FILE *file) V_STDIO_RETURN(ssize_t, (ssize_t)v_stdio_getdelim((uintptr_t)line, (uintptr_t)capacity, (int32_t)10, (uintptr_t)file))
extern int32_t v_stdio_ungetchar(int32_t, uintptr_t);
int ungetc(int value, FILE *file) V_STDIO_RETURN(int, (int)v_stdio_ungetchar((int32_t)value, (uintptr_t)file))
extern uintptr_t v_stdio_tmpfile(void);
FILE * tmpfile(void) V_STDIO_RETURN(FILE *, (FILE *)v_stdio_tmpfile())

extern int32_t v_stdio_state(uintptr_t, int32_t);
extern int32_t v_stdio_state_locked(uintptr_t, int32_t);
int feof_unlocked(FILE *f) V_STDIO_RETURN(int, v_stdio_state((uintptr_t)f, 0))
int feof(FILE *f) V_STDIO_RETURN(int, v_stdio_state_locked((uintptr_t)f, 0))
int ferror_unlocked(FILE *f) V_STDIO_RETURN(int, v_stdio_state((uintptr_t)f, 1))
int ferror(FILE *f) V_STDIO_RETURN(int, v_stdio_state_locked((uintptr_t)f, 1))
void clearerr_unlocked(FILE *f) { (void)v_stdio_state((uintptr_t)f, 2); }
void clearerr(FILE *f) { (void)v_stdio_state_locked((uintptr_t)f, 2); }
int fileno_unlocked(FILE *f) V_STDIO_RETURN(int, v_stdio_state((uintptr_t)f, 3))
int fileno(FILE *f) V_STDIO_RETURN(int, v_stdio_state_locked((uintptr_t)f, 3))
extern uintptr_t v_stdio_cookie(uintptr_t, uintptr_t, uintptr_t);
FILE *fopencookie(void *cookie, const char *mode, cookie_io_functions_t f) {
    uintptr_t callbacks[4] = {(uintptr_t)f.read, (uintptr_t)f.write, (uintptr_t)f.seek, (uintptr_t)f.close};
    return (FILE *)v_stdio_cookie((uintptr_t)cookie, (uintptr_t)mode, (uintptr_t)callbacks);
}
#if defined(__GLIBC__)
int __uflow(FILE *file) V_STDIO_RETURN(int, v_stdio_getchar((uintptr_t)file))
int __overflow(FILE *file, int value) {
    if (value == EOF) return fflush(file);
    V_STDIO_RETURN(int, v_stdio_putchar(value, (uintptr_t)file))
}
ssize_t __getdelim(char **line, size_t *capacity, int delimiter, FILE *file)
    V_STDIO_RETURN(ssize_t, v_stdio_getdelim((uintptr_t)line, (uintptr_t)capacity, delimiter, (uintptr_t)file))
FILE *fopen64(const char *path, const char *mode) { return (FILE *)v_stdio_fopen((uintptr_t)path, (uintptr_t)mode); }
int fseeko64(FILE *file, off64_t offset, int whence) V_STDIO_RETURN(int, v_stdio_seek((uintptr_t)file, offset, whence))
off64_t ftello64(FILE *file) V_STDIO_RETURN(off64_t, v_stdio_tell((uintptr_t)file))
#endif

#include <string.h>
_Static_assert(sizeof(wchar_t) == sizeof(uint32_t), "native wide character ABI");
_Static_assert(sizeof(mbstate_t) == 8, "native conversion-state storage");
struct v_stdio_position { int64_t offset; unsigned char state[8]; };
_Static_assert(sizeof(fpos_t) >= sizeof(struct v_stdio_position), "native opaque position storage");
void v_stdio_position_set(uintptr_t output, int64_t offset, uintptr_t state) {
    struct v_stdio_position position = {offset, {0}};
    memcpy(position.state, (void *)state, sizeof position.state);
    memset((void *)output, 0, sizeof(fpos_t));
    memcpy((void *)output, &position, sizeof position);
}
int64_t v_stdio_position_get(uintptr_t input, uintptr_t state) {
    struct v_stdio_position position;
    memcpy(&position, (void *)input, sizeof position);
    memcpy((void *)state, position.state, sizeof position.state);
    return position.offset;
}
extern int32_t v_stdio_getpos(uintptr_t, uintptr_t);
extern int32_t v_stdio_setpos(uintptr_t, uintptr_t);
extern int32_t v_stdio_orientation(uintptr_t, int32_t);
extern uint32_t v_stdio_getwide(uintptr_t);
extern uint32_t v_stdio_putwide(uint32_t, uintptr_t);
extern uint32_t v_stdio_ungetwide(uint32_t, uintptr_t);
extern uintptr_t v_stdio_getwides(uintptr_t, int32_t, uintptr_t);
extern int32_t v_stdio_putwides(uintptr_t, uintptr_t);
int fgetpos(FILE *file, fpos_t *position) V_STDIO_RETURN(int, v_stdio_getpos((uintptr_t)file, (uintptr_t)position))
int fsetpos(FILE *file, const fpos_t *position) V_STDIO_RETURN(int, v_stdio_setpos((uintptr_t)file, (uintptr_t)position))
int fwide(FILE *file, int mode) V_STDIO_RETURN(int, v_stdio_orientation((uintptr_t)file, mode))
wint_t fgetwc(FILE *file) V_STDIO_RETURN(wint_t, v_stdio_getwide((uintptr_t)file))
wint_t getwc(FILE *file) V_STDIO_RETURN(wint_t, v_stdio_getwide((uintptr_t)file))
wint_t getwchar(void) V_STDIO_RETURN(wint_t, v_stdio_getwide((uintptr_t)stdin))
wint_t fputwc(wchar_t value, FILE *file) V_STDIO_RETURN(wint_t, v_stdio_putwide((uint32_t)value, (uintptr_t)file))
wint_t putwc(wchar_t value, FILE *file) V_STDIO_RETURN(wint_t, v_stdio_putwide((uint32_t)value, (uintptr_t)file))
wint_t putwchar(wchar_t value) V_STDIO_RETURN(wint_t, v_stdio_putwide((uint32_t)value, (uintptr_t)stdout))
wint_t ungetwc(wint_t value, FILE *file) V_STDIO_RETURN(wint_t, v_stdio_ungetwide((uint32_t)value, (uintptr_t)file))
wchar_t *fgetws(wchar_t *buffer, int count, FILE *file) { return (wchar_t *)v_stdio_getwides((uintptr_t)buffer, count, (uintptr_t)file); }
int fputws(const wchar_t *text, FILE *file) V_STDIO_RETURN(int, v_stdio_putwides((uintptr_t)text, (uintptr_t)file))
#if defined(__GLIBC__)
int fgetpos64(FILE *file, fpos64_t *position) V_STDIO_RETURN(int, v_stdio_getpos((uintptr_t)file, (uintptr_t)position))
int fsetpos64(FILE *file, const fpos64_t *position) V_STDIO_RETURN(int, v_stdio_setpos((uintptr_t)file, (uintptr_t)position))
#endif
