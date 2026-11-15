#include <assert.h>
#include <string.h>
#include <windows.h>

// Keep these mocks separate from the test runner's real Windows imports.
#define GetModuleHandleA v_time_zoneinfo_mock_module_handle
#define GetProcAddress v_time_zoneinfo_mock_proc_address
static HMODULE WINAPI GetModuleHandleA(const char *name);
static FARPROC WINAPI GetProcAddress(HMODULE module, const char *name);

#include "../../zoneinfo_windows.h"

static int has_module = 1;
static int has_dynamic_api = 1;
static int has_year_api = 1;
static DWORD dynamic_status = 2;
static BOOL year_status = 1;
static uint32_t dynamic_info;
static uint32_t year_info;

static DWORD WINAPI fake_dynamic_info(void *info) {
    assert(info == &dynamic_info);
    *(uint32_t *)info = 123;
    return dynamic_status;
}

static BOOL WINAPI fake_year_info(USHORT year, void *dynamic, void *info) {
    assert(year == 2026);
    assert(dynamic == &dynamic_info);
    assert(info == &year_info);
    *(uint32_t *)info = year;
    return year_status;
}

static HMODULE WINAPI GetModuleHandleA(const char *name) {
    assert(strcmp(name, "kernel32.dll") == 0);
    return has_module ? (HMODULE)1 : NULL;
}

static FARPROC WINAPI GetProcAddress(HMODULE module, const char *name) {
    assert(has_module && module == (HMODULE)1);
    if (strcmp(name, "GetDynamicTimeZoneInformation") == 0) {
        return has_dynamic_api ? (FARPROC)fake_dynamic_info : NULL;
    }
    assert(strcmp(name, "GetTimeZoneInformationForYear") == 0);
    return has_year_api ? (FARPROC)fake_year_info : NULL;
}

int32_t v_time_test_windows_zoneinfo(void) {
    assert(v_time_get_dynamic_time_zone_information(&dynamic_info) == 2);
    assert(dynamic_info == 123);
    assert(v_time_get_time_zone_information_for_year(2026,
        &dynamic_info, &year_info) == 1);
    assert(year_info == 2026);

    // Native failure results must reach the existing local-zone fallback.
    dynamic_status = (DWORD)-1;
    year_status = 0;
    assert(v_time_get_dynamic_time_zone_information(&dynamic_info) == UINT32_MAX);
    assert(v_time_get_time_zone_information_for_year(2026,
        &dynamic_info, &year_info) == 0);

    // A missing export must return failure without touching the output buffer.
    has_dynamic_api = has_year_api = 0;
    dynamic_info = year_info = 77;
    assert(v_time_get_dynamic_time_zone_information(&dynamic_info) == UINT32_MAX);
    assert(v_time_get_time_zone_information_for_year(2026,
        &dynamic_info, &year_info) == 0);
    assert(dynamic_info == 77 && year_info == 77);

    has_module = 0;
    has_dynamic_api = has_year_api = 1;
    assert(v_time_get_dynamic_time_zone_information(&dynamic_info) == UINT32_MAX);
    assert(v_time_get_time_zone_information_for_year(2026,
        &dynamic_info, &year_info) == 0);
    assert(dynamic_info == 77 && year_info == 77);
    return 0;
}
