#ifndef V_MULTIWINDOW_WIN32_MONITOR_ENUMERATION_TEST_SEAM_H
#define V_MULTIWINDOW_WIN32_MONITOR_ENUMERATION_TEST_SEAM_H

#if defined(_WIN32) && defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
#include <windows.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#define V_MULTIWINDOW_WIN32_TEST_MONITOR_NATIVE 0
#define V_MULTIWINDOW_WIN32_TEST_MONITOR_EMPTY 1
#define V_MULTIWINDOW_WIN32_TEST_MONITOR_REPLAY 2
#define V_MULTIWINDOW_WIN32_TEST_MONITOR_INFO_FAILURE 3
#define V_MULTIWINDOW_WIN32_TEST_MONITOR_GROWTH 4
#define V_MULTIWINDOW_WIN32_TEST_MONITOR_CHANGED 5
#define V_MULTIWINDOW_WIN32_TEST_MONITOR_INITIAL_CAPACITY 8
#define V_MULTIWINDOW_WIN32_TEST_MONITOR_MAX_CAPACITY 4096
#define V_MULTIWINDOW_WIN32_TEST_MONITOR_SYNTHETIC_BASE ((uintptr_t)0x57430000)

typedef struct VMultiwindowWin32TestMonitorEnumeration {
	HMONITOR *handles;
	RECT *rects;
	int count;
	int capacity;
	int captured;
	int capture_failed;
	int mode;
	int empty_calls;
	int replay_calls;
	int info_failure_calls;
	int growth_calls;
	int growth_callbacks;
	int growth_count;
	int windowposchanged_reason3_armed;
	int windowposchanged_reason3_consumed;
	int windowposchanged_reason3_uint64_max;
	int windowposchanged_reason3_move_succeeded;
	HWND windowposchanged_reason3_hwnd;
} VMultiwindowWin32TestMonitorEnumeration;

extern VMultiwindowWin32TestMonitorEnumeration
	v_multiwindow_win32_test_monitor_enumeration;

static inline void
v_multiwindow_win32_service_test_release_monitor_storage(
	VMultiwindowWin32TestMonitorEnumeration *fixture) {
	if (!fixture) {
		return;
	}
	free(fixture->handles);
	free(fixture->rects);
	fixture->handles = NULL;
	fixture->rects = NULL;
	fixture->count = 0;
	fixture->capacity = 0;
}

static inline int
v_multiwindow_win32_service_test_reserve_monitor_storage(
	VMultiwindowWin32TestMonitorEnumeration *fixture, int needed) {
	if (!fixture || needed < 0
		|| needed > V_MULTIWINDOW_WIN32_TEST_MONITOR_MAX_CAPACITY) {
		return 0;
	}
	if (needed <= fixture->capacity) {
		return 1;
	}
	int next_capacity = fixture->capacity > 0
		? fixture->capacity
		: V_MULTIWINDOW_WIN32_TEST_MONITOR_INITIAL_CAPACITY;
	while (next_capacity < needed
		&& next_capacity < V_MULTIWINDOW_WIN32_TEST_MONITOR_MAX_CAPACITY) {
		next_capacity *= 2;
	}
	if (next_capacity > V_MULTIWINDOW_WIN32_TEST_MONITOR_MAX_CAPACITY) {
		next_capacity = V_MULTIWINDOW_WIN32_TEST_MONITOR_MAX_CAPACITY;
	}
	if (next_capacity < needed) {
		return 0;
	}
	HMONITOR *handles = (HMONITOR *)calloc((size_t)next_capacity,
		sizeof(HMONITOR));
	RECT *rects = (RECT *)calloc((size_t)next_capacity, sizeof(RECT));
	if (!handles || !rects) {
		free(handles);
		free(rects);
		return 0;
	}
	if (fixture->count > 0) {
		memcpy(handles, fixture->handles,
			(size_t)fixture->count * sizeof(HMONITOR));
		memcpy(rects, fixture->rects,
			(size_t)fixture->count * sizeof(RECT));
	}
	free(fixture->handles);
	free(fixture->rects);
	fixture->handles = handles;
	fixture->rects = rects;
	fixture->capacity = next_capacity;
	return 1;
}

static inline BOOL
v_multiwindow_win32_service_test_real_get_monitor_info_w(
	HMONITOR monitor, LPMONITORINFO info) {
	return GetMonitorInfoW(monitor, info);
}

static inline int
v_multiwindow_win32_service_test_synthetic_monitor_index(HMONITOR monitor) {
	VMultiwindowWin32TestMonitorEnumeration *fixture =
		&v_multiwindow_win32_test_monitor_enumeration;
	uintptr_t value = (uintptr_t)monitor;
	uintptr_t first = V_MULTIWINDOW_WIN32_TEST_MONITOR_SYNTHETIC_BASE + 1;
	if (fixture->mode != V_MULTIWINDOW_WIN32_TEST_MONITOR_GROWTH
		|| value < first
		|| value >= first + (uintptr_t)fixture->growth_count) {
		return -1;
	}
	return (int)(value - first);
}

static inline BOOL
v_multiwindow_win32_service_test_get_monitor_info_w(
	HMONITOR monitor, LPMONITORINFO info) {
	VMultiwindowWin32TestMonitorEnumeration *fixture =
		&v_multiwindow_win32_test_monitor_enumeration;
	LPMONITORINFO monitor_info = info;
	if (fixture->mode == V_MULTIWINDOW_WIN32_TEST_MONITOR_INFO_FAILURE
		&& fixture->info_failure_calls == 0) {
		fixture->info_failure_calls++;
		SetLastError(ERROR_GEN_FAILURE);
		return FALSE;
	}
	int synthetic_index =
		v_multiwindow_win32_service_test_synthetic_monitor_index(monitor);
	if (synthetic_index < 0) {
		BOOL result = v_multiwindow_win32_service_test_real_get_monitor_info_w(
			monitor, monitor_info);
		if (result && fixture->mode == V_MULTIWINDOW_WIN32_TEST_MONITOR_CHANGED
			&& fixture->count > 0 && monitor == fixture->handles[0]) {
			monitor_info->rcMonitor.left += 13;
			monitor_info->rcMonitor.top += 17;
			monitor_info->rcMonitor.right += 50;
			monitor_info->rcMonitor.bottom += 60;
			monitor_info->rcWork.left += 5;
			monitor_info->rcWork.top += 7;
			monitor_info->rcWork.right -= 11;
			monitor_info->rcWork.bottom -= 19;
			monitor_info->dwFlags ^= MONITORINFOF_PRIMARY;
		}
		return result;
	}
	if (!monitor_info || monitor_info->cbSize < sizeof(MONITORINFO)) {
		SetLastError(ERROR_INVALID_PARAMETER);
		return FALSE;
	}
	DWORD size = monitor_info->cbSize;
	ZeroMemory(monitor_info, size);
	monitor_info->cbSize = size;
	monitor_info->rcMonitor.left = synthetic_index * 100;
	monitor_info->rcMonitor.top = 0;
	monitor_info->rcMonitor.right =
		monitor_info->rcMonitor.left + 100;
	monitor_info->rcMonitor.bottom = 100;
	monitor_info->rcWork = monitor_info->rcMonitor;
	monitor_info->dwFlags =
		synthetic_index == 0 ? MONITORINFOF_PRIMARY : 0;
	if (size >= sizeof(MONITORINFOEXW)) {
		MONITORINFOEXW *extended = (MONITORINFOEXW *)monitor_info;
		wsprintfW(extended->szDevice, L"\\\\.\\W3DISPLAY-%04d",
			synthetic_index + 1);
	}
	return TRUE;
}

#define GetMonitorInfoW v_multiwindow_win32_service_test_get_monitor_info_w

static BOOL CALLBACK v_multiwindow_test_win32_capture_monitor(
	HMONITOR monitor, HDC dc, LPRECT rect, LPARAM data) {
	(void)dc;
	VMultiwindowWin32TestMonitorEnumeration *fixture =
		(VMultiwindowWin32TestMonitorEnumeration *)(uintptr_t)data;
	if (!fixture || !rect) {
		return FALSE;
	}
	if (!v_multiwindow_win32_service_test_reserve_monitor_storage(
			fixture, fixture->count + 1)) {
		fixture->capture_failed = 1;
		return FALSE;
	}
	int index = fixture->count++;
	fixture->handles[index] = monitor;
	fixture->rects[index] = *rect;
	return TRUE;
}

static inline int
v_multiwindow_test_win32_monitor_enumeration_capture(void) {
	VMultiwindowWin32TestMonitorEnumeration *fixture =
		&v_multiwindow_win32_test_monitor_enumeration;
	v_multiwindow_win32_service_test_release_monitor_storage(fixture);
	fixture->captured = 0;
	fixture->capture_failed = 0;
	fixture->mode = V_MULTIWINDOW_WIN32_TEST_MONITOR_NATIVE;
	fixture->empty_calls = 0;
	fixture->replay_calls = 0;
	fixture->info_failure_calls = 0;
	fixture->growth_calls = 0;
	fixture->growth_callbacks = 0;
	fixture->growth_count = 0;
	BOOL enumerated = EnumDisplayMonitors(NULL, NULL,
			v_multiwindow_test_win32_capture_monitor,
			(LPARAM)(uintptr_t)fixture);
	if (!enumerated || fixture->capture_failed) {
		v_multiwindow_win32_service_test_release_monitor_storage(fixture);
		return 0;
	}
	fixture->captured = 1;
	return fixture->count;
}

static inline void
v_multiwindow_test_win32_monitor_enumeration_use_empty(void) {
	v_multiwindow_win32_test_monitor_enumeration.mode =
		V_MULTIWINDOW_WIN32_TEST_MONITOR_EMPTY;
}

static inline int
v_multiwindow_test_win32_monitor_enumeration_use_replay(void) {
	if (!v_multiwindow_win32_test_monitor_enumeration.captured) {
		return 0;
	}
	v_multiwindow_win32_test_monitor_enumeration.mode =
		V_MULTIWINDOW_WIN32_TEST_MONITOR_REPLAY;
	return 1;
}

static inline int
v_multiwindow_test_win32_monitor_enumeration_use_changed(void) {
	if (!v_multiwindow_win32_test_monitor_enumeration.captured) {
		return 0;
	}
	v_multiwindow_win32_test_monitor_enumeration.mode =
		V_MULTIWINDOW_WIN32_TEST_MONITOR_CHANGED;
	return 1;
}

static inline UINT
v_multiwindow_win32_service_test_monitor_dpi(HMONITOR monitor, UINT dpi) {
	VMultiwindowWin32TestMonitorEnumeration *fixture =
		&v_multiwindow_win32_test_monitor_enumeration;
	if (fixture->mode == V_MULTIWINDOW_WIN32_TEST_MONITOR_CHANGED
		&& fixture->count > 0 && monitor == fixture->handles[0]) {
		return dpi + 24;
	}
	return dpi;
}

static inline int
v_multiwindow_test_win32_monitor_enumeration_use_info_failure(void) {
	VMultiwindowWin32TestMonitorEnumeration *fixture =
		&v_multiwindow_win32_test_monitor_enumeration;
	if (!fixture->captured) {
		return 0;
	}
	fixture->mode = V_MULTIWINDOW_WIN32_TEST_MONITOR_INFO_FAILURE;
	fixture->info_failure_calls = 0;
	return 1;
}

static inline int
v_multiwindow_test_win32_monitor_enumeration_use_growth(int count) {
	VMultiwindowWin32TestMonitorEnumeration *fixture =
		&v_multiwindow_win32_test_monitor_enumeration;
	if (count < 33
		|| count > V_MULTIWINDOW_WIN32_TEST_MONITOR_MAX_CAPACITY) {
		return 0;
	}
	fixture->mode = V_MULTIWINDOW_WIN32_TEST_MONITOR_GROWTH;
	fixture->growth_count = count;
	fixture->growth_calls = 0;
	fixture->growth_callbacks = 0;
	return 1;
}

static inline void
v_multiwindow_test_win32_monitor_enumeration_reset(void) {
	VMultiwindowWin32TestMonitorEnumeration *fixture =
		&v_multiwindow_win32_test_monitor_enumeration;
	fixture->mode = V_MULTIWINDOW_WIN32_TEST_MONITOR_NATIVE;
	fixture->captured = 0;
	fixture->capture_failed = 0;
	fixture->growth_count = 0;
	v_multiwindow_win32_service_test_release_monitor_storage(fixture);
}

static inline int
v_multiwindow_test_win32_monitor_enumeration_empty_calls(void) {
	return v_multiwindow_win32_test_monitor_enumeration.empty_calls;
}

static inline int
v_multiwindow_test_win32_monitor_enumeration_replay_calls(void) {
	return v_multiwindow_win32_test_monitor_enumeration.replay_calls;
}

static inline int
v_multiwindow_test_win32_monitor_enumeration_info_failure_calls(void) {
	return v_multiwindow_win32_test_monitor_enumeration.info_failure_calls;
}

static inline int
v_multiwindow_test_win32_monitor_enumeration_growth_calls(void) {
	return v_multiwindow_win32_test_monitor_enumeration.growth_calls;
}

static inline int
v_multiwindow_test_win32_monitor_enumeration_growth_callbacks(void) {
	return v_multiwindow_win32_test_monitor_enumeration.growth_callbacks;
}

static inline void
v_multiwindow_test_win32_windowposchanged_reason3_reset(void) {
	VMultiwindowWin32TestMonitorEnumeration *fixture =
		&v_multiwindow_win32_test_monitor_enumeration;
	fixture->windowposchanged_reason3_armed = 0;
	fixture->windowposchanged_reason3_consumed = 0;
	fixture->windowposchanged_reason3_uint64_max = 0;
	fixture->windowposchanged_reason3_move_succeeded = 0;
	fixture->windowposchanged_reason3_hwnd = NULL;
}

static inline void
v_multiwindow_test_win32_windowposchanged_reason3_arm(void) {
	v_multiwindow_test_win32_windowposchanged_reason3_reset();
	v_multiwindow_win32_test_monitor_enumeration
		.windowposchanged_reason3_armed = 1;
}

static inline int
v_multiwindow_test_win32_windowposchanged_reason3_is_armed(void) {
	return v_multiwindow_win32_test_monitor_enumeration
		.windowposchanged_reason3_armed;
}

static inline int
v_multiwindow_test_win32_windowposchanged_reason3_consume(HWND hwnd) {
	VMultiwindowWin32TestMonitorEnumeration *fixture =
		&v_multiwindow_win32_test_monitor_enumeration;
	if (!fixture->windowposchanged_reason3_armed) {
		return 0;
	}
	fixture->windowposchanged_reason3_armed = 0;
	fixture->windowposchanged_reason3_consumed++;
	fixture->windowposchanged_reason3_hwnd = hwnd;
	return 1;
}

static inline void
v_multiwindow_test_win32_windowposchanged_reason3_record(
	uint64_t sequence) {
	if (sequence == UINT64_MAX) {
		v_multiwindow_win32_test_monitor_enumeration
			.windowposchanged_reason3_uint64_max = 1;
	}
}

static inline void
v_multiwindow_test_win32_windowposchanged_reason3_record_move_result(
	int succeeded) {
	v_multiwindow_win32_test_monitor_enumeration
		.windowposchanged_reason3_move_succeeded = succeeded != 0;
}

static inline int
v_multiwindow_test_win32_windowposchanged_reason3_consumed(void) {
	return v_multiwindow_win32_test_monitor_enumeration
		.windowposchanged_reason3_consumed;
}

static inline int
v_multiwindow_test_win32_windowposchanged_reason3_uint64_max_observed(void) {
	return v_multiwindow_win32_test_monitor_enumeration
		.windowposchanged_reason3_uint64_max;
}

static inline int
v_multiwindow_test_win32_windowposchanged_reason3_move_succeeded(void) {
	return v_multiwindow_win32_test_monitor_enumeration
		.windowposchanged_reason3_move_succeeded;
}

static inline void *
v_multiwindow_test_win32_windowposchanged_reason3_hwnd(void) {
	return (void *)v_multiwindow_win32_test_monitor_enumeration
		.windowposchanged_reason3_hwnd;
}

static inline int
v_multiwindow_test_win32_windowposchanged_reason3_hwnd_is_window(void) {
	HWND hwnd = v_multiwindow_win32_test_monitor_enumeration
		.windowposchanged_reason3_hwnd;
	return hwnd != NULL && IsWindow(hwnd);
}

/*
 * Production test-hook contract: under V_MULTIWINDOW_WIN32_SERVICE_TEST,
 * call this wrapper instead of EnumDisplayMonitors at the service monitor
 * enumeration boundary. Non-test builds continue calling EnumDisplayMonitors.
 */
static inline BOOL
v_multiwindow_win32_service_test_enum_display_monitors(HDC dc,
	LPCRECT clip, MONITORENUMPROC callback, LPARAM data) {
	VMultiwindowWin32TestMonitorEnumeration *fixture =
		&v_multiwindow_win32_test_monitor_enumeration;
	if (fixture->mode == V_MULTIWINDOW_WIN32_TEST_MONITOR_NATIVE) {
		return EnumDisplayMonitors(dc, clip, callback, data);
	}
	if (!callback) {
		return FALSE;
	}
	if (fixture->mode == V_MULTIWINDOW_WIN32_TEST_MONITOR_EMPTY) {
		fixture->empty_calls++;
		return TRUE;
	}
	if (fixture->mode == V_MULTIWINDOW_WIN32_TEST_MONITOR_GROWTH) {
		fixture->growth_calls++;
		for (int index = 0; index < fixture->growth_count; index++) {
			RECT rect = {index * 100, 0, index * 100 + 100, 100};
			HMONITOR monitor = (HMONITOR)(uintptr_t)
				(V_MULTIWINDOW_WIN32_TEST_MONITOR_SYNTHETIC_BASE
					+ (uintptr_t)index + 1);
			fixture->growth_callbacks++;
			if (!callback(monitor, dc, &rect, data)) {
				return FALSE;
			}
		}
		return TRUE;
	}
	if ((fixture->mode != V_MULTIWINDOW_WIN32_TEST_MONITOR_REPLAY
			&& fixture->mode != V_MULTIWINDOW_WIN32_TEST_MONITOR_INFO_FAILURE
			&& fixture->mode != V_MULTIWINDOW_WIN32_TEST_MONITOR_CHANGED)
		|| !fixture->captured) {
		return FALSE;
	}
	if (fixture->mode == V_MULTIWINDOW_WIN32_TEST_MONITOR_REPLAY) {
		fixture->replay_calls++;
	}
	for (int index = 0; index < fixture->count; index++) {
		RECT rect = fixture->rects[index];
		if (!callback(fixture->handles[index], dc, &rect, data)) {
			return FALSE;
		}
	}
	return TRUE;
}
#endif

#endif
