#ifndef V_TIME_TEST_WINDOWS_H
#define V_TIME_TEST_WINDOWS_H

#include <stddef.h>

#define WINAPI
typedef unsigned long long ULONGLONG;
typedef void *HMODULE;
typedef void (*FARPROC)(void);
typedef struct { long long QuadPart; } LARGE_INTEGER;

HMODULE GetModuleHandleA(const char *name);
FARPROC GetProcAddress(HMODULE module, const char *name);
int QueryPerformanceFrequency(LARGE_INTEGER *frequency);
int QueryPerformanceCounter(LARGE_INTEGER *counter);
unsigned long GetTickCount(void);

#endif
