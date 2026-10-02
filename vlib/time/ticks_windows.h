#ifndef V_TIME_TICKS_WINDOWS_H
#define V_TIME_TICKS_WINDOWS_H

#include <windows.h>

static inline unsigned long long v_time_ticks_ms(void) {
    typedef ULONGLONG (WINAPI *tick_count_fn)(void);
    HMODULE kernel32 = GetModuleHandleA("kernel32.dll");
    tick_count_fn tick_count = kernel32
        ? (tick_count_fn)GetProcAddress(kernel32, "GetTickCount64")
        : NULL;
    if (tick_count) {
        return tick_count();
    }

    // Older Windows versions do not export GetTickCount64. QPC is also 64 bit.
    LARGE_INTEGER counter, frequency;
    if (QueryPerformanceFrequency(&frequency) && frequency.QuadPart > 0
            && QueryPerformanceCounter(&counter)) {
        unsigned long long count = (unsigned long long)counter.QuadPart;
        unsigned long long freq = (unsigned long long)frequency.QuadPart;
        return (count / freq) * 1000ULL + (count % freq) * 1000ULL / freq;
    }
    return GetTickCount();
}

#endif
