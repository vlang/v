#ifndef __LOG_H__
#define __LOG_H__

#include <stdio.h>

#define LOGLEVEL LOGLEVEL_NONE

enum {
    LOGLEVEL_CRITICAL,
    LOGLEVEL_WARNING,
    LOGLEVEL_INFO,
    LOGLEVEL_DEBUG,
    LOGLEVEL_NONE
};

extern const char* log_level_strings[];

#define log_gc(level, fmt, ...) \
    do { if (level <= LOGLEVEL) fprintf(stderr, "[%s] %s:%s:%d: " fmt "\n", log_level_strings[level], __func__, __FILE__, __LINE__, __VA_ARGS__); } while (0)

#define LOG_CRITICAL(fmt, ...) log_gc(LOGLEVEL_CRITICAL, fmt, __VA_ARGS__)
#define LOG_WARNING(fmt, ...) log_gc(LOGLEVEL_WARNING, fmt, __VA_ARGS__)
#define LOG_INFO(fmt, ...) log_gc(LOGLEVEL_INFO, fmt, __VA_ARGS__)
#define LOG_DEBUG(fmt, ...) log_gc(LOGLEVEL_DEBUG, fmt, __VA_ARGS__)

#endif /* !__LOG_H__ */
