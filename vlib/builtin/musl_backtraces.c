/* musl has no execinfo API. Strong definitions supplied by an unwinder take
 * precedence over these empty results in both hosted and freestanding links. */
__attribute__((weak)) int backtrace(void **frames, int count) {
    (void)frames;
    (void)count;
    return 0;
}
__attribute__((weak)) char **backtrace_symbols(void *const *frames, int count) {
    (void)frames;
    (void)count;
    return 0;
}
__attribute__((weak)) void backtrace_symbols_fd(void *const *frames, int count, int fd) {
    (void)frames;
    (void)count;
    (void)fd;
}
