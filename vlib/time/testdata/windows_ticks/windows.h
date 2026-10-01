#ifndef V_TIME_TEST_WINDOWS_H
#define V_TIME_TEST_WINDOWS_H

#include <stddef.h>

#define WINAPI
typedef unsigned long long ULONGLONG;
typedef int BOOL;
typedef unsigned long DWORD;
typedef void *HMODULE;
typedef void (*FARPROC)(void);
typedef struct { long long QuadPart; } LARGE_INTEGER;

#endif
