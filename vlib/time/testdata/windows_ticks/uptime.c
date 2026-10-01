#include <assert.h>
#include <string.h>
#include "ticks_windows.h"

static unsigned long long uptime;
static int has_tick_count64 = 1;

static ULONGLONG WINAPI fake_tick_count64(void) {
    return uptime;
}

HMODULE GetModuleHandleA(const char *name) {
    assert(strcmp(name, "kernel32.dll") == 0);
    return (HMODULE)1;
}

FARPROC GetProcAddress(HMODULE module, const char *name) {
    assert(module == (HMODULE)1);
    assert(strcmp(name, "GetTickCount64") == 0);
    return has_tick_count64 ? (FARPROC)fake_tick_count64 : NULL;
}

int QueryPerformanceFrequency(LARGE_INTEGER *frequency) {
    frequency->QuadPart = 10000000;
    return 1;
}

int QueryPerformanceCounter(LARGE_INTEGER *counter) {
    counter->QuadPart = 4611686018427387904LL;
    return 1;
}

unsigned long GetTickCount(void) {
    assert(0 && "the wrapping 32-bit counter must not be used");
    return 0;
}

int main(void) {
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
