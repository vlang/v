#include <assert.h>
#include <string.h>
#include <windows.h>

// Keep the simulated Windows APIs separate from the test runner's real APIs.
#define GetModuleHandleA v_time_mock_module_handle
#define GetProcAddress v_time_mock_proc_address
#define QueryPerformanceFrequency v_time_mock_frequency
#define QueryPerformanceCounter v_time_mock_counter
#define GetTickCount v_time_mock_tick_count

static HMODULE WINAPI GetModuleHandleA(const char *name);
static FARPROC WINAPI GetProcAddress(HMODULE module, const char *name);
static BOOL WINAPI QueryPerformanceFrequency(LARGE_INTEGER *frequency);
static BOOL WINAPI QueryPerformanceCounter(LARGE_INTEGER *counter);
static DWORD WINAPI GetTickCount(void);

// V compiles foreign C sources in MSVC's default C mode.
#ifdef _MSC_VER
#define inline __inline
#endif
#include "../../ticks_windows.h"
#ifdef _MSC_VER
#undef inline
#endif

static unsigned long long uptime;
static int has_tick_count64 = 1;

static ULONGLONG WINAPI fake_tick_count64(void) {
    return uptime;
}

static HMODULE WINAPI GetModuleHandleA(const char *name) {
    assert(strcmp(name, "kernel32.dll") == 0);
    return (HMODULE)1;
}

static FARPROC WINAPI GetProcAddress(HMODULE module, const char *name) {
    assert(module == (HMODULE)1);
    assert(strcmp(name, "GetTickCount64") == 0);
    return has_tick_count64 ? (FARPROC)fake_tick_count64 : NULL;
}

static BOOL WINAPI QueryPerformanceFrequency(LARGE_INTEGER *frequency) {
    frequency->QuadPart = 10000000;
    return 1;
}

static BOOL WINAPI QueryPerformanceCounter(LARGE_INTEGER *counter) {
    counter->QuadPart = 4611686018427387904LL;
    return 1;
}

static DWORD WINAPI GetTickCount(void) {
    assert(0 && "the wrapping 32-bit counter must not be used");
    return 0;
}

int v_time_test_windows_uptime(void) {
    uptime = 4294967295ULL;
    assert(v_time_ticks_ms() == uptime);
    uptime++;
    assert(v_time_ticks_ms() == 4294967296ULL);
    uptime++;
    assert(v_time_ticks_ms() == 4294967297ULL);
    has_tick_count64 = 0;
    assert(v_time_ticks_ms() == 461168601842738ULL);
    return 0;
}
