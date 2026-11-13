#ifndef V_TIME_ZONEINFO_WINDOWS_H
#define V_TIME_ZONEINFO_WINDOWS_H

#include <stdint.h>
#include <windows.h>

// Older Windows import libraries omit these APIs. Resolve them at runtime while
// retaining their native calling convention, including stdcall on 32-bit Windows.
static uint32_t v_time_get_dynamic_time_zone_information(void *info) {
    typedef DWORD (WINAPI *query_fn)(void *);
    HMODULE kernel32 = GetModuleHandleA("kernel32.dll");
    query_fn query = kernel32
        ? (query_fn)GetProcAddress(kernel32, "GetDynamicTimeZoneInformation")
        : NULL;
    return query ? (uint32_t)query(info) : UINT32_MAX;
}

static int32_t v_time_get_time_zone_information_for_year(uint16_t year,
        void *dynamic_info, void *info) {
    typedef BOOL (WINAPI *query_fn)(USHORT, void *, void *);
    HMODULE kernel32 = GetModuleHandleA("kernel32.dll");
    query_fn query = kernel32
        ? (query_fn)GetProcAddress(kernel32, "GetTimeZoneInformationForYear")
        : NULL;
    return query ? (int32_t)query((USHORT)year, dynamic_info, info) : 0;
}

#endif
