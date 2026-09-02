#ifndef V_MULTIWINDOW_WIN32_SERVICE_NATIVE_H
#define V_MULTIWINDOW_WIN32_SERVICE_NATIVE_H

#if defined(_WIN32)
#include <windows.h>
#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
#include "testdata/win32_monitor_enumeration_test_seam.h"
#endif

#define V_MULTIWINDOW_WIN32_SERVICE_OK 1
#define V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE 0
#define V_MULTIWINDOW_WIN32_SERVICE_WRONG_THREAD -1
#define V_MULTIWINDOW_WIN32_SERVICE_INVALID -2

#ifndef SIZE_MAX
#define SIZE_MAX ((size_t)-1)
#endif
#ifndef RIDEV_REMOVE
#define RIDEV_REMOVE 0x00000001
#endif
#ifndef RIDEV_PAGEONLY
#define RIDEV_PAGEONLY 0x00000020
#endif
#ifndef V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP
#define V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP L"V_x_multiwindow_mouse_lock_active"
#endif
#ifndef V_MULTIWINDOW_WIN32_MOUSE_TRACKED_PROP
#define V_MULTIWINDOW_WIN32_MOUSE_TRACKED_PROP L"V_x_multiwindow_mouse_tracked"
#endif

#define V_MULTIWINDOW_WIN32_MOUSE_LOCK_OFF 0
#define V_MULTIWINDOW_WIN32_MOUSE_LOCK_LOCKED 1
#define V_MULTIWINDOW_WIN32_MOUSE_LOCK_CLEANUP 2
#define V_MULTIWINDOW_WIN32_MOUSE_LOCK_TEARDOWN_PREPARED 3

#define V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_DIRTY -1
#define V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_BASELINE 0
#define V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_LOCKED 1

#define V_MULTIWINDOW_WIN32_CLIPBOARD_MAX_BYTES \
	((size_t)16u * (size_t)1024u * (size_t)1024u)
#define V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_FAILED -1
#define V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_RETRY 0
#define V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_READY 1
#define V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_CAPACITY -2
#define V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_CLEANED -3
#define V_MULTIWINDOW_WIN32_CLIPBOARD_CONVERT_INVALID 0
#define V_MULTIWINDOW_WIN32_CLIPBOARD_CONVERT_READY 1
#define V_MULTIWINDOW_WIN32_CLIPBOARD_CONVERT_CAPACITY -1

static inline int64_t v_multiwindow_win32_clipboard_now_ns(void) {
	LARGE_INTEGER frequency;
	LARGE_INTEGER counter;
	if (QueryPerformanceFrequency(&frequency) && frequency.QuadPart > 0
		&& QueryPerformanceCounter(&counter)) {
		int64_t seconds = counter.QuadPart / frequency.QuadPart;
		int64_t remainder = counter.QuadPart % frequency.QuadPart;
		return seconds * INT64_C(1000000000)
			+ remainder * INT64_C(1000000000) / frequency.QuadPart;
	}
	return (int64_t)GetTickCount() * INT64_C(1000000);
}

static inline int v_multiwindow_win32_parse_clipboard_utf16(
	const void *data, size_t bytes, size_t *out_utf16_units,
	size_t *out_utf8_bytes) {
	if (out_utf16_units) {
		*out_utf16_units = 0;
	}
	if (out_utf8_bytes) {
		*out_utf8_bytes = 0;
	}
	if (!data || bytes < sizeof(uint16_t)) {
		return 0;
	}
	size_t scan_bytes = bytes;
	if (scan_bytes > V_MULTIWINDOW_WIN32_CLIPBOARD_MAX_BYTES) {
		scan_bytes = V_MULTIWINDOW_WIN32_CLIPBOARD_MAX_BYTES;
	}
	size_t units = 0;
	for (size_t offset = 0;
		offset + sizeof(uint16_t) <= scan_bytes;
		offset += sizeof(uint16_t)) {
		uint16_t unit = 0;
		memcpy(&unit, (const unsigned char *)data + offset, sizeof(unit));
		if (unit == 0) {
			units = offset / sizeof(uint16_t) + 1;
			break;
		}
	}
	if (units == 0) {
		return 0;
	}
	size_t utf8_bytes = 1;
	for (size_t index = 0; index + 1 < units; index++) {
		uint16_t unit = 0;
		memcpy(&unit,
			(const unsigned char *)data + index * sizeof(uint16_t),
			sizeof(unit));
		size_t encoded = 0;
		if (unit >= 0xd800u && unit <= 0xdbffu) {
			if (index + 2 >= units) {
				return 0;
			}
			uint16_t low = 0;
			memcpy(&low,
				(const unsigned char *)data
					+ (index + 1) * sizeof(uint16_t),
				sizeof(low));
			if (low < 0xdc00u || low > 0xdfffu) {
				return 0;
			}
			encoded = 4;
			index++;
		} else if (unit >= 0xdc00u && unit <= 0xdfffu) {
			return 0;
		} else if (unit <= 0x007fu) {
			encoded = 1;
		} else if (unit <= 0x07ffu) {
			encoded = 2;
		} else {
			encoded = 3;
		}
		if (utf8_bytes >
			V_MULTIWINDOW_WIN32_CLIPBOARD_MAX_BYTES - encoded) {
			return V_MULTIWINDOW_WIN32_CLIPBOARD_CONVERT_CAPACITY;
		}
		utf8_bytes += encoded;
	}
	if (out_utf16_units) {
		*out_utf16_units = units;
	}
	if (out_utf8_bytes) {
		*out_utf8_bytes = utf8_bytes;
	}
	return 1;
}

static inline int v_multiwindow_win32_clipboard_utf8_to_utf16(
	const char *text, size_t text_bytes, uint16_t *output,
	size_t output_units, size_t *out_units) {
	if (out_units) {
		*out_units = 0;
	}
	if (text_bytes > V_MULTIWINDOW_WIN32_CLIPBOARD_MAX_BYTES - 1) {
		return V_MULTIWINDOW_WIN32_CLIPBOARD_CONVERT_CAPACITY;
	}
	if ((!text && text_bytes > 0)
		|| (text_bytes > 0 && memchr(text, '\0', text_bytes))) {
		return V_MULTIWINDOW_WIN32_CLIPBOARD_CONVERT_INVALID;
	}
	int converted_units = 0;
	if (text_bytes > 0) {
		converted_units = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
			text, (int)text_bytes, NULL, 0);
		if (converted_units <= 0) {
			return V_MULTIWINDOW_WIN32_CLIPBOARD_CONVERT_INVALID;
		}
	}
	size_t required_units = (size_t)converted_units + 1;
	if (required_units >
		V_MULTIWINDOW_WIN32_CLIPBOARD_MAX_BYTES / sizeof(uint16_t)) {
		return V_MULTIWINDOW_WIN32_CLIPBOARD_CONVERT_CAPACITY;
	}
	if (out_units) {
		*out_units = required_units;
	}
	if (!output) {
		return V_MULTIWINDOW_WIN32_CLIPBOARD_CONVERT_READY;
	}
	if (output_units < required_units) {
		return V_MULTIWINDOW_WIN32_CLIPBOARD_CONVERT_INVALID;
	}
	if (converted_units > 0
		&& MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, text,
			(int)text_bytes, (wchar_t *)output, converted_units)
			!= converted_units) {
		return V_MULTIWINDOW_WIN32_CLIPBOARD_CONVERT_INVALID;
	}
	output[converted_units] = 0;
	return V_MULTIWINDOW_WIN32_CLIPBOARD_CONVERT_READY;
}

typedef struct VMultiwindowWin32ClipboardWriteObserver {
	int *owned_globals;
	int *owned_globals_peak;
	int *global_allocations;
	int *global_transfers;
	int *global_frees;
	int *fail_before_transfer;
} VMultiwindowWin32ClipboardWriteObserver;

static inline void v_multiwindow_win32_clipboard_observe_allocation(
		VMultiwindowWin32ClipboardWriteObserver *observer) {
	if (!observer) {
		return;
	}
	if (observer->global_allocations) {
		(*observer->global_allocations)++;
	}
	if (observer->owned_globals) {
		(*observer->owned_globals)++;
		if (observer->owned_globals_peak
			&& *observer->owned_globals > *observer->owned_globals_peak) {
			*observer->owned_globals_peak = *observer->owned_globals;
		}
	}
}

static inline void v_multiwindow_win32_clipboard_observe_free(
		VMultiwindowWin32ClipboardWriteObserver *observer) {
	if (!observer) {
		return;
	}
	if (observer->global_frees) {
		(*observer->global_frees)++;
	}
	if (observer->owned_globals && *observer->owned_globals > 0) {
		(*observer->owned_globals)--;
	}
}

static inline void v_multiwindow_win32_clipboard_observe_transfer(
		VMultiwindowWin32ClipboardWriteObserver *observer) {
	if (!observer) {
		return;
	}
	if (observer->global_transfers) {
		(*observer->global_transfers)++;
	}
	if (observer->owned_globals && *observer->owned_globals > 0) {
		(*observer->owned_globals)--;
	}
}

static inline void v_multiwindow_win32_clipboard_free_owned_global(
		HGLOBAL global, VMultiwindowWin32ClipboardWriteObserver *observer) {
	if (global && !GlobalFree(global)) {
		v_multiwindow_win32_clipboard_observe_free(observer);
	}
}

static inline int v_multiwindow_win32_clipboard_take_pretransfer_failure(
		VMultiwindowWin32ClipboardWriteObserver *observer) {
	if (!observer || !observer->fail_before_transfer
		|| *observer->fail_before_transfer <= 0) {
		return 0;
	}
	(*observer->fail_before_transfer)--;
	return 1;
}

static inline int v_multiwindow_win32_clipboard_write_observed(
		void *owner_ptr, const uint16_t *text, size_t units,
		VMultiwindowWin32ClipboardWriteObserver *observer) {
	HWND owner = (HWND)owner_ptr;
	if (!owner || !IsWindow(owner) || !text || units == 0
			|| units > V_MULTIWINDOW_WIN32_CLIPBOARD_MAX_BYTES
			/ sizeof(uint16_t)
		|| text[units - 1] != 0) {
		return V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_FAILED;
	}
	if (!OpenClipboard(owner)) {
		return V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_RETRY;
	}
	size_t bytes = units * sizeof(uint16_t);
	HGLOBAL global = GlobalAlloc(GMEM_MOVEABLE, bytes);
	if (global) {
		v_multiwindow_win32_clipboard_observe_allocation(observer);
	}
	void *target = global ? GlobalLock(global) : NULL;
	if (!target) {
		if (global) {
			v_multiwindow_win32_clipboard_free_owned_global(global,
				observer);
		}
		CloseClipboard();
		return global
			? V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_CLEANED
			: V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_FAILED;
	}
	memcpy(target, text, bytes);
	GlobalUnlock(global);
	if (v_multiwindow_win32_clipboard_take_pretransfer_failure(observer)) {
		v_multiwindow_win32_clipboard_free_owned_global(global, observer);
		CloseClipboard();
		return V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_CLEANED;
	}
	if (!EmptyClipboard()) {
		v_multiwindow_win32_clipboard_free_owned_global(global, observer);
		CloseClipboard();
		return V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_CLEANED;
	}
	if (!SetClipboardData(CF_UNICODETEXT, global)) {
		v_multiwindow_win32_clipboard_free_owned_global(global, observer);
		CloseClipboard();
		return V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_CLEANED;
	}
	v_multiwindow_win32_clipboard_observe_transfer(observer);
	CloseClipboard();
	return V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_READY;
}

static inline int v_multiwindow_win32_clipboard_write(
		void *owner_ptr, const uint16_t *text, size_t units) {
	return v_multiwindow_win32_clipboard_write_observed(owner_ptr, text,
		units, NULL);
}

static inline int v_multiwindow_win32_clipboard_read(
	void *owner_ptr, void **out_text, size_t *out_text_bytes) {
	if (out_text) {
		*out_text = NULL;
	}
	if (out_text_bytes) {
		*out_text_bytes = 0;
	}
	HWND owner = (HWND)owner_ptr;
	if (!owner || !IsWindow(owner)) {
		return V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_FAILED;
	}
	if (!OpenClipboard(owner)) {
		return V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_RETRY;
	}
	HGLOBAL global = (HGLOBAL)GetClipboardData(CF_UNICODETEXT);
	size_t bytes = global ? GlobalSize(global) : 0;
	void *source = bytes >= sizeof(uint16_t) ? GlobalLock(global) : NULL;
	size_t units = 0;
	size_t utf8_bytes = 0;
	int parse_status = source
		? v_multiwindow_win32_parse_clipboard_utf16(source, bytes,
			&units, &utf8_bytes)
		: V_MULTIWINDOW_WIN32_CLIPBOARD_CONVERT_INVALID;
	if (parse_status != V_MULTIWINDOW_WIN32_CLIPBOARD_CONVERT_READY) {
		if (source) {
			GlobalUnlock(global);
		}
		CloseClipboard();
		return parse_status == V_MULTIWINDOW_WIN32_CLIPBOARD_CONVERT_CAPACITY
				|| bytes > V_MULTIWINDOW_WIN32_CLIPBOARD_MAX_BYTES
			? V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_CAPACITY
			: V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_FAILED;
	}
	char *converted = (char *)malloc(utf8_bytes);
	if (!converted) {
		GlobalUnlock(global);
		CloseClipboard();
		return V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_FAILED;
	}
	int source_units = (int)(units - 1);
	int converted_bytes = 0;
	if (source_units > 0) {
		converted_bytes = WideCharToMultiByte(CP_UTF8, 0,
			(const wchar_t *)source, source_units, converted,
			(int)(utf8_bytes - 1), NULL, NULL);
	}
	GlobalUnlock(global);
	CloseClipboard();
	if ((source_units > 0 && converted_bytes != (int)(utf8_bytes - 1))
		|| (source_units == 0 && utf8_bytes != 1)) {
		free(converted);
		return V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_FAILED;
	}
	converted[utf8_bytes - 1] = '\0';
	if (out_text) {
		*out_text = (void *)converted;
	} else {
		free(converted);
		converted = NULL;
	}
	if (out_text_bytes) {
		*out_text_bytes = utf8_bytes - 1;
	}
	return V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_READY;
}

static inline void v_multiwindow_win32_clipboard_text_free(void *text) {
	free(text);
}

#define V_MULTIWINDOW_WIN32_SERVICE_TEST_FAIL_NONE 0
#define V_MULTIWINDOW_WIN32_SERVICE_TEST_FAIL_EXIT_EXSTYLE 1
#define V_MULTIWINDOW_WIN32_SERVICE_TEST_FAIL_EXIT_PLACEMENT 2
#define V_MULTIWINDOW_WIN32_SERVICE_TEST_FAIL_EXIT_POSITION 3

#define V_MULTIWINDOW_WIN32_SERVICE_TEST_ROLLBACK_STYLE 1
#define V_MULTIWINDOW_WIN32_SERVICE_TEST_ROLLBACK_EXSTYLE 2
#define V_MULTIWINDOW_WIN32_SERVICE_TEST_ROLLBACK_PLACEMENT 4
#define V_MULTIWINDOW_WIN32_SERVICE_TEST_ROLLBACK_POSITION 8

typedef struct VMultiwindowWin32ServiceState {
	HWND hwnd;
	void *record_data;
	DWORD owner_thread;
	DWORD owner_process;
	int fullscreen;
	int fullscreen_known;
	int restore_valid;
	int windowed_visible;
	LONG_PTR windowed_style;
	LONG_PTR windowed_ex_style;
	WINDOWPLACEMENT windowed_placement;
	int requested_width;
	int requested_height;
	int resizable;
	int borderless;
	int mouse_lock_state;
	int mouse_lock_committed;
	int mouse_delivery_enabled;
	int mouse_raw_owned;
	int mouse_clip_owned;
	int mouse_baseline_valid;
	RECT mouse_baseline_clip;
	RECT mouse_locked_clip;
	RAWINPUTDEVICE *mouse_baseline_devices;
	UINT mouse_baseline_count;
} VMultiwindowWin32ServiceState;

typedef UINT(WINAPI *VMultiwindowWin32GetRegisteredRawInputDevices)(
	PRAWINPUTDEVICE, PUINT, UINT);
typedef BOOL(WINAPI *VMultiwindowWin32RegisterRawInputDevices)(
	const RAWINPUTDEVICE *, UINT, UINT);

typedef struct VMultiwindowWin32MouseApis {
	VMultiwindowWin32GetRegisteredRawInputDevices get_registered_devices;
	VMultiwindowWin32RegisterRawInputDevices register_devices;
} VMultiwindowWin32MouseApis;

typedef struct VMultiwindowWin32RawInventory {
	RAWINPUTDEVICE *items;
	UINT count;
} VMultiwindowWin32RawInventory;

static PVOID volatile v_multiwindow_win32_mouse_lock_owner = NULL;

static inline VMultiwindowWin32ServiceState *
v_multiwindow_win32_mouse_lock_owner_load(void) {
	return (VMultiwindowWin32ServiceState *)InterlockedCompareExchangePointer(
		&v_multiwindow_win32_mouse_lock_owner, NULL, NULL);
}

static inline int v_multiwindow_win32_mouse_lock_owner_claim(
	VMultiwindowWin32ServiceState *state) {
	return state && InterlockedCompareExchangePointer(
		&v_multiwindow_win32_mouse_lock_owner, (PVOID)state, NULL) == NULL;
}

static inline int v_multiwindow_win32_mouse_lock_owner_release(
	VMultiwindowWin32ServiceState *state) {
	return state && InterlockedCompareExchangePointer(
		&v_multiwindow_win32_mouse_lock_owner, NULL, (PVOID)state)
		== (PVOID)state;
}

typedef struct VMultiwindowWin32NativeWindowSnapshot {
	LONG_PTR style;
	LONG_PTR ex_style;
	WINDOWPLACEMENT placement;
	RECT rect;
	int visible;
} VMultiwindowWin32NativeWindowSnapshot;

#define V_MULTIWINDOW_WIN32_SERVICE_MONITOR_INITIAL_CAPACITY 8
#define V_MULTIWINDOW_WIN32_SERVICE_MONITOR_MAX_CAPACITY 4096

typedef struct VMultiwindowWin32ServiceMonitor {
	HMONITOR native_id;
	wchar_t name[CCHDEVICENAME];
	RECT geometry;
	RECT work;
	UINT dpi;
	int primary;
} VMultiwindowWin32ServiceMonitor;

typedef struct VMultiwindowWin32ServiceMonitorSnapshot {
	VMultiwindowWin32ServiceMonitor *monitors;
	int count;
	int capacity;
	int failed;
} VMultiwindowWin32ServiceMonitorSnapshot;

typedef HRESULT(WINAPI *VMultiwindowWin32GetDpiForMonitor)(
	HMONITOR, int, UINT *, UINT *);
typedef UINT(WINAPI *VMultiwindowWin32GetDpiForWindow)(HWND);

static inline UINT v_multiwindow_win32_service_monitor_dpi(HMONITOR monitor) {
	HMODULE shcore = LoadLibraryW(L"shcore.dll");
	if (shcore) {
		VMultiwindowWin32GetDpiForMonitor get_dpi_for_monitor =
			(VMultiwindowWin32GetDpiForMonitor)GetProcAddress(shcore,
				"GetDpiForMonitor");
		if (get_dpi_for_monitor) {
			UINT dpi_x = 0;
			UINT dpi_y = 0;
			HRESULT result = get_dpi_for_monitor(monitor, 0, &dpi_x, &dpi_y);
			FreeLibrary(shcore);
			if (SUCCEEDED(result) && dpi_x > 0) {
				return dpi_x;
			}
		} else {
			FreeLibrary(shcore);
		}
	}
	HDC dc = GetDC(NULL);
	int dpi = dc ? GetDeviceCaps(dc, LOGPIXELSX) : 96;
	if (dc) {
		ReleaseDC(NULL, dc);
	}
	return dpi > 0 ? (UINT)dpi : 96;
}

static BOOL CALLBACK v_multiwindow_win32_service_monitor_snapshot_callback(
	HMONITOR monitor, HDC dc, LPRECT rect, LPARAM data) {
	(void)dc;
	(void)rect;
	VMultiwindowWin32ServiceMonitorSnapshot *snapshot =
		(VMultiwindowWin32ServiceMonitorSnapshot *)(uintptr_t)data;
	if (!snapshot) {
		return FALSE;
	}
	if (snapshot->count >= snapshot->capacity) {
		if (snapshot->capacity >=
				V_MULTIWINDOW_WIN32_SERVICE_MONITOR_MAX_CAPACITY) {
			snapshot->failed = 1;
			return FALSE;
		}
		int next_capacity = snapshot->capacity > 0
			? snapshot->capacity * 2
			: V_MULTIWINDOW_WIN32_SERVICE_MONITOR_INITIAL_CAPACITY;
		if (next_capacity >
				V_MULTIWINDOW_WIN32_SERVICE_MONITOR_MAX_CAPACITY) {
			next_capacity =
				V_MULTIWINDOW_WIN32_SERVICE_MONITOR_MAX_CAPACITY;
		}
		VMultiwindowWin32ServiceMonitor *grown =
			(VMultiwindowWin32ServiceMonitor *)realloc(snapshot->monitors,
				(size_t)next_capacity *
					sizeof(VMultiwindowWin32ServiceMonitor));
		if (!grown) {
			snapshot->failed = 1;
			return FALSE;
		}
		snapshot->monitors = grown;
		snapshot->capacity = next_capacity;
	}
	MONITORINFOEXW info;
	ZeroMemory(&info, sizeof(info));
	LPMONITORINFO base = (LPMONITORINFO)&info;
	base->cbSize = sizeof(info);
	if (!GetMonitorInfoW(monitor, base)) {
		snapshot->failed = 1;
		return FALSE;
	}
	int index = snapshot->count++;
	VMultiwindowWin32ServiceMonitor *item = &snapshot->monitors[index];
	item->native_id = monitor;
	item->geometry = base->rcMonitor;
	item->work = base->rcWork;
	item->dpi = v_multiwindow_win32_service_monitor_dpi(monitor);
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	item->dpi = v_multiwindow_win32_service_test_monitor_dpi(monitor,
		item->dpi);
#endif
	item->primary = (base->dwFlags & MONITORINFOF_PRIMARY) != 0;
	wcsncpy(item->name, info.szDevice, CCHDEVICENAME - 1);
	item->name[CCHDEVICENAME - 1] = L'\0';
	return TRUE;
}

static inline void *v_multiwindow_win32_service_monitor_snapshot_new(void) {
	VMultiwindowWin32ServiceMonitorSnapshot *snapshot =
		(VMultiwindowWin32ServiceMonitorSnapshot *)calloc(1,
			sizeof(VMultiwindowWin32ServiceMonitorSnapshot));
	if (!snapshot) {
		return NULL;
	}
	snapshot->capacity =
		V_MULTIWINDOW_WIN32_SERVICE_MONITOR_INITIAL_CAPACITY;
	snapshot->monitors = (VMultiwindowWin32ServiceMonitor *)calloc(
		(size_t)snapshot->capacity,
		sizeof(VMultiwindowWin32ServiceMonitor));
	if (!snapshot->monitors) {
		free(snapshot);
		return NULL;
	}
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	BOOL enumerated = v_multiwindow_win32_service_test_enum_display_monitors(
		NULL, NULL, v_multiwindow_win32_service_monitor_snapshot_callback,
		(LPARAM)(uintptr_t)snapshot);
#else
	BOOL enumerated = EnumDisplayMonitors(NULL, NULL,
		v_multiwindow_win32_service_monitor_snapshot_callback,
		(LPARAM)(uintptr_t)snapshot);
#endif
	if (!enumerated || snapshot->failed) {
		free(snapshot->monitors);
		free(snapshot);
		return NULL;
	}
	return snapshot;
}

static inline void v_multiwindow_win32_service_monitor_snapshot_free(
		void *snapshot) {
	VMultiwindowWin32ServiceMonitorSnapshot *typed =
		(VMultiwindowWin32ServiceMonitorSnapshot *)snapshot;
	if (typed) {
		free(typed->monitors);
		free(typed);
	}
}

static inline int v_multiwindow_win32_service_monitor_snapshot_count(
	const VMultiwindowWin32ServiceMonitorSnapshot *snapshot) {
	return snapshot ? snapshot->count : -1;
}

static inline uint64_t v_multiwindow_win32_service_monitor_snapshot_native_id(
	const VMultiwindowWin32ServiceMonitorSnapshot *snapshot, int index) {
	if (!snapshot || index < 0 || index >= snapshot->count) {
		return 0;
	}
	return (uint64_t)(uintptr_t)snapshot->monitors[index].native_id;
}

static inline const wchar_t *
v_multiwindow_win32_service_monitor_snapshot_name(
	const VMultiwindowWin32ServiceMonitorSnapshot *snapshot, int index) {
	if (!snapshot || index < 0 || index >= snapshot->count) {
		return NULL;
	}
	return snapshot->monitors[index].name;
}

static inline int v_multiwindow_win32_service_monitor_snapshot_info(
	const VMultiwindowWin32ServiceMonitorSnapshot *snapshot, int index,
	int *x, int *y, int *width, int *height, int *work_x, int *work_y,
	int *work_width, int *work_height, UINT *dpi, int *primary) {
	if (!snapshot || index < 0 || index >= snapshot->count) {
		return 0;
	}
	const VMultiwindowWin32ServiceMonitor *item = &snapshot->monitors[index];
	if (x) *x = item->geometry.left;
	if (y) *y = item->geometry.top;
	if (width) *width = item->geometry.right - item->geometry.left;
	if (height) *height = item->geometry.bottom - item->geometry.top;
	if (work_x) *work_x = item->work.left;
	if (work_y) *work_y = item->work.top;
	if (work_width) *work_width = item->work.right - item->work.left;
	if (work_height) *work_height = item->work.bottom - item->work.top;
	if (dpi) *dpi = item->dpi;
	if (primary) *primary = item->primary;
	return 1;
}

static inline uint64_t v_multiwindow_win32_service_window_monitor(
	void *hwnd_ptr) {
	HWND hwnd = (HWND)hwnd_ptr;
	if (!hwnd || !IsWindow(hwnd)) {
		return 0;
	}
	return (uint64_t)(uintptr_t)MonitorFromWindow(hwnd,
		MONITOR_DEFAULTTONEAREST);
}

static inline UINT v_multiwindow_win32_service_window_dpi(void *hwnd_ptr) {
	HWND hwnd = (HWND)hwnd_ptr;
	if (!hwnd || !IsWindow(hwnd)) {
		return 0;
	}
	HMODULE user32 = GetModuleHandleW(L"user32.dll");
	VMultiwindowWin32GetDpiForWindow get_dpi_for_window = user32 ?
		(VMultiwindowWin32GetDpiForWindow)GetProcAddress(user32,
			"GetDpiForWindow") : NULL;
	UINT dpi = get_dpi_for_window ? get_dpi_for_window(hwnd) : 0;
	if (dpi > 0) {
		return dpi;
	}
	HDC dc = GetDC(hwnd);
	int fallback = dc ? GetDeviceCaps(dc, LOGPIXELSX) : 96;
	if (dc) {
		ReleaseDC(hwnd, dc);
	}
	return fallback > 0 ? (UINT)fallback : 96;
}

#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
static int v_multiwindow_win32_service_test_focus_refused;
static int v_multiwindow_win32_service_test_show_failure;
static int v_multiwindow_win32_service_test_fullscreen_exit_failure;
static int v_multiwindow_win32_service_test_fullscreen_rollback_failure_mask;
static int v_multiwindow_win32_service_test_fullscreen_rollback_attempt_mask;

static inline void v_multiwindow_win32_service_test_set_focus_refused(int refused) {
	v_multiwindow_win32_service_test_focus_refused = refused != 0;
}

static inline void v_multiwindow_win32_service_test_set_show_failure(int fail) {
	v_multiwindow_win32_service_test_show_failure = fail != 0;
}

static inline void v_multiwindow_win32_service_test_set_fullscreen_exit_failure(
	int failure) {
	v_multiwindow_win32_service_test_fullscreen_exit_failure = failure;
}

static inline void v_multiwindow_win32_service_test_set_fullscreen_rollback_failure(
	int failure_mask) {
	v_multiwindow_win32_service_test_fullscreen_rollback_failure_mask = failure_mask;
	v_multiwindow_win32_service_test_fullscreen_rollback_attempt_mask = 0;
}

static inline int v_multiwindow_win32_service_test_fullscreen_rollback_attempts(void) {
	return v_multiwindow_win32_service_test_fullscreen_rollback_attempt_mask;
}
#endif

static inline DWORD v_multiwindow_win32_service_windowed_style(int resizable,
	int borderless) {
	if (borderless) {
		return WS_POPUP | WS_CLIPSIBLINGS | WS_CLIPCHILDREN;
	}
	DWORD style = WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX | WS_CLIPSIBLINGS
		| WS_CLIPCHILDREN;
	if (resizable) {
		style |= WS_SIZEBOX | WS_MAXIMIZEBOX;
	}
	return style;
}

static inline DWORD v_multiwindow_win32_service_windowed_ex_style(int borderless) {
	return borderless ? WS_EX_APPWINDOW : WS_EX_APPWINDOW | WS_EX_WINDOWEDGE;
}

static inline int v_multiwindow_win32_service_set_long_ptr(HWND hwnd, int index,
	LONG_PTR value) {
	SetLastError(ERROR_SUCCESS);
	LONG_PTR previous = SetWindowLongPtrW(hwnd, index, value);
	return previous != 0 || GetLastError() == ERROR_SUCCESS;
}

static inline int v_multiwindow_win32_service_capture_native_snapshot(HWND hwnd,
	VMultiwindowWin32NativeWindowSnapshot *snapshot) {
	if (!hwnd || !snapshot || !IsWindow(hwnd)) {
		return 0;
	}
	ZeroMemory(snapshot, sizeof(*snapshot));
	snapshot->placement.length = sizeof(snapshot->placement);
	if (!GetWindowPlacement(hwnd, &snapshot->placement)
		|| !GetWindowRect(hwnd, &snapshot->rect)) {
		return 0;
	}
	snapshot->style = GetWindowLongPtrW(hwnd, GWL_STYLE);
	snapshot->ex_style = GetWindowLongPtrW(hwnd, GWL_EXSTYLE);
	snapshot->visible = IsWindowVisible(hwnd) != 0;
	return 1;
}

static inline int v_multiwindow_win32_service_native_snapshot_matches(HWND hwnd,
	const VMultiwindowWin32NativeWindowSnapshot *snapshot) {
	if (!hwnd || !snapshot || !IsWindow(hwnd)
		|| GetWindowLongPtrW(hwnd, GWL_STYLE) != snapshot->style
		|| GetWindowLongPtrW(hwnd, GWL_EXSTYLE) != snapshot->ex_style) {
		return 0;
	}
	WINDOWPLACEMENT placement;
	RECT rect;
	ZeroMemory(&placement, sizeof(placement));
	ZeroMemory(&rect, sizeof(rect));
	placement.length = sizeof(placement);
	if (!GetWindowPlacement(hwnd, &placement) || !GetWindowRect(hwnd, &rect)) {
		return 0;
	}
	return placement.flags == snapshot->placement.flags
		&& placement.showCmd == snapshot->placement.showCmd
		&& placement.ptMinPosition.x == snapshot->placement.ptMinPosition.x
		&& placement.ptMinPosition.y == snapshot->placement.ptMinPosition.y
		&& placement.ptMaxPosition.x == snapshot->placement.ptMaxPosition.x
		&& placement.ptMaxPosition.y == snapshot->placement.ptMaxPosition.y
		&& EqualRect(&placement.rcNormalPosition,
			&snapshot->placement.rcNormalPosition)
		&& EqualRect(&rect, &snapshot->rect)
		&& (IsWindowVisible(hwnd) != 0) == snapshot->visible;
}

static inline int v_multiwindow_win32_service_restore_visibility(HWND hwnd,
	int visible, UINT show_command) {
	if (visible) {
		if (!IsWindowVisible(hwnd)) {
			ShowWindow(hwnd,
				show_command == SW_HIDE ? SW_SHOWNOACTIVATE : (int)show_command);
		}
	} else if (IsWindowVisible(hwnd)) {
		ShowWindow(hwnd, SW_HIDE);
	}
	return (IsWindowVisible(hwnd) != 0) == (visible != 0);
}

static inline int v_multiwindow_win32_service_restore_native_snapshot(HWND hwnd,
	const VMultiwindowWin32NativeWindowSnapshot *snapshot) {
	if (!hwnd || !snapshot) {
		return 0;
	}
	int restored = 1;
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	v_multiwindow_win32_service_test_fullscreen_rollback_attempt_mask
		|= V_MULTIWINDOW_WIN32_SERVICE_TEST_ROLLBACK_STYLE;
	if ((v_multiwindow_win32_service_test_fullscreen_rollback_failure_mask
		& V_MULTIWINDOW_WIN32_SERVICE_TEST_ROLLBACK_STYLE)
		|| !v_multiwindow_win32_service_set_long_ptr(hwnd, GWL_STYLE,
			snapshot->style)) {
		restored = 0;
	}
	v_multiwindow_win32_service_test_fullscreen_rollback_attempt_mask
		|= V_MULTIWINDOW_WIN32_SERVICE_TEST_ROLLBACK_EXSTYLE;
	if ((v_multiwindow_win32_service_test_fullscreen_rollback_failure_mask
		& V_MULTIWINDOW_WIN32_SERVICE_TEST_ROLLBACK_EXSTYLE)
		|| !v_multiwindow_win32_service_set_long_ptr(hwnd, GWL_EXSTYLE,
			snapshot->ex_style)) {
		restored = 0;
	}
	v_multiwindow_win32_service_test_fullscreen_rollback_attempt_mask
		|= V_MULTIWINDOW_WIN32_SERVICE_TEST_ROLLBACK_PLACEMENT;
	if ((v_multiwindow_win32_service_test_fullscreen_rollback_failure_mask
		& V_MULTIWINDOW_WIN32_SERVICE_TEST_ROLLBACK_PLACEMENT)
		|| !SetWindowPlacement(hwnd, &snapshot->placement)) {
		restored = 0;
	}
	v_multiwindow_win32_service_test_fullscreen_rollback_attempt_mask
		|= V_MULTIWINDOW_WIN32_SERVICE_TEST_ROLLBACK_POSITION;
	if ((v_multiwindow_win32_service_test_fullscreen_rollback_failure_mask
		& V_MULTIWINDOW_WIN32_SERVICE_TEST_ROLLBACK_POSITION)
		|| !SetWindowPos(hwnd, NULL, snapshot->rect.left, snapshot->rect.top,
			snapshot->rect.right - snapshot->rect.left,
			snapshot->rect.bottom - snapshot->rect.top,
			SWP_NOZORDER | SWP_NOACTIVATE | SWP_NOOWNERZORDER
				| SWP_FRAMECHANGED)) {
		restored = 0;
	}
#else
	if (!v_multiwindow_win32_service_set_long_ptr(hwnd, GWL_STYLE,
		snapshot->style)) {
		restored = 0;
	}
	if (!v_multiwindow_win32_service_set_long_ptr(hwnd, GWL_EXSTYLE,
		snapshot->ex_style)) {
		restored = 0;
	}
	if (!SetWindowPlacement(hwnd, &snapshot->placement)) {
		restored = 0;
	}
	if (!SetWindowPos(hwnd, NULL, snapshot->rect.left, snapshot->rect.top,
		snapshot->rect.right - snapshot->rect.left,
		snapshot->rect.bottom - snapshot->rect.top,
		SWP_NOZORDER | SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_FRAMECHANGED)) {
		restored = 0;
	}
#endif
	if (!v_multiwindow_win32_service_restore_visibility(hwnd, snapshot->visible,
		snapshot->placement.showCmd)) {
		restored = 0;
	}
	return restored
		&& v_multiwindow_win32_service_native_snapshot_matches(hwnd, snapshot);
}

static inline int v_multiwindow_win32_service_authority(void *state_ptr) {
	const VMultiwindowWin32ServiceState *state =
		(const VMultiwindowWin32ServiceState *)state_ptr;
	if (!state || !state->hwnd || !IsWindow(state->hwnd)) {
		return V_MULTIWINDOW_WIN32_SERVICE_INVALID;
	}
	DWORD window_process = 0;
	DWORD window_thread = GetWindowThreadProcessId(state->hwnd, &window_process);
	if (!window_thread || window_thread != state->owner_thread || !window_process
		|| window_process != state->owner_process
		|| state->owner_process != GetCurrentProcessId()
		|| (void *)GetWindowLongPtrW(state->hwnd, GWLP_USERDATA) != state->record_data) {
		return V_MULTIWINDOW_WIN32_SERVICE_INVALID;
	}
	if (GetCurrentThreadId() != state->owner_thread) {
		return V_MULTIWINDOW_WIN32_SERVICE_WRONG_THREAD;
	}
	return V_MULTIWINDOW_WIN32_SERVICE_OK;
}

static inline int v_multiwindow_win32_mouse_resolve_proc(HMODULE module,
	const char *name, void *destination, size_t destination_size) {
	FARPROC procedure;
	if (!module || !name || !destination
		|| destination_size != sizeof(procedure)) {
		return 0;
	}
	memset(destination, 0, destination_size);
	procedure = GetProcAddress(module, name);
	if (!procedure) {
		return 0;
	}
	memcpy(destination, &procedure, sizeof(procedure));
	return 1;
}

static inline int v_multiwindow_win32_mouse_resolve_apis(
	VMultiwindowWin32MouseApis *apis) {
	HMODULE user32;
	VMultiwindowWin32MouseApis resolved;
	if (!apis) {
		return 0;
	}
	memset(&resolved, 0, sizeof(resolved));
	user32 = GetModuleHandleW(L"user32.dll");
	if (!user32
		|| !v_multiwindow_win32_mouse_resolve_proc(user32,
			"GetRegisteredRawInputDevices", &resolved.get_registered_devices,
			sizeof(resolved.get_registered_devices))
		|| !v_multiwindow_win32_mouse_resolve_proc(user32,
			"RegisterRawInputDevices", &resolved.register_devices,
			sizeof(resolved.register_devices))) {
		return 0;
	}
	*apis = resolved;
	return 1;
}

static inline void v_multiwindow_win32_raw_inventory_free(
	VMultiwindowWin32RawInventory *inventory) {
	if (!inventory) {
		return;
	}
	free(inventory->items);
	inventory->items = NULL;
	inventory->count = 0;
}

static inline int v_multiwindow_win32_raw_inventory_query(
	VMultiwindowWin32GetRegisteredRawInputDevices query,
	VMultiwindowWin32RawInventory *inventory) {
	UINT count = 0;
	UINT copied;
	UINT result;
	RAWINPUTDEVICE *items;
	if (!query || !inventory) {
		return 0;
	}
	memset(inventory, 0, sizeof(*inventory));
	result = query(NULL, &count, sizeof(RAWINPUTDEVICE));
	if (result != 0) {
		return 0;
	}
	if (count == 0) {
		return 1;
	}
	if ((size_t)count > SIZE_MAX / sizeof(RAWINPUTDEVICE)) {
		return 0;
	}
	items = (RAWINPUTDEVICE *)calloc((size_t)count, sizeof(RAWINPUTDEVICE));
	if (!items) {
		return 0;
	}
	copied = count;
	result = query(items, &copied, sizeof(RAWINPUTDEVICE));
	if (result == (UINT)-1 || result != copied || copied > count) {
		free(items);
		return 0;
	}
	inventory->items = items;
	inventory->count = copied;
	return 1;
}

static inline int v_multiwindow_win32_raw_device_equal(
	const RAWINPUTDEVICE *left, const RAWINPUTDEVICE *right) {
	return left && right && left->usUsagePage == right->usUsagePage
		&& left->usUsage == right->usUsage && left->dwFlags == right->dwFlags
		&& left->hwndTarget == right->hwndTarget;
}

static inline int v_multiwindow_win32_raw_inventory_equal(
	const RAWINPUTDEVICE *left, UINT left_count,
	const RAWINPUTDEVICE *right, UINT right_count) {
	unsigned char *matched;
	UINT left_index;
	if (left_count != right_count || (left_count > 0 && (!left || !right))) {
		return 0;
	}
	if (left_count == 0) {
		return 1;
	}
	if ((size_t)right_count > SIZE_MAX / sizeof(unsigned char)) {
		return 0;
	}
	matched = (unsigned char *)calloc((size_t)right_count,
		sizeof(unsigned char));
	if (!matched) {
		return 0;
	}
	for (left_index = 0; left_index < left_count; left_index++) {
		UINT right_index;
		int found = 0;
		for (right_index = 0; right_index < right_count; right_index++) {
			if (!matched[right_index]
				&& v_multiwindow_win32_raw_device_equal(&left[left_index],
					&right[right_index])) {
				matched[right_index] = 1;
				found = 1;
				break;
			}
		}
		if (!found) {
			free(matched);
			return 0;
		}
	}
	free(matched);
	return 1;
}

static inline int v_multiwindow_win32_raw_inventory_mouse_free(
	const VMultiwindowWin32RawInventory *inventory) {
	UINT index;
	if (!inventory) {
		return 0;
	}
	for (index = 0; index < inventory->count; index++) {
		const RAWINPUTDEVICE *item = &inventory->items[index];
		if (item->usUsagePage == 0x01
			&& (item->usUsage == 0x02
				|| (item->usUsage == 0
					&& (item->dwFlags & RIDEV_PAGEONLY) != 0))) {
			return 0;
		}
	}
	return 1;
}

static inline int v_multiwindow_win32_raw_inventory_locked_exact(
	const VMultiwindowWin32ServiceState *state,
	const VMultiwindowWin32RawInventory *current) {
	RAWINPUTDEVICE *remainder;
	UINT source_index;
	UINT remainder_index = 0;
	int exact_count = 0;
	int equal;
	if (!state || !state->mouse_baseline_valid || !current
		|| state->mouse_baseline_count == UINT_MAX
		|| current->count != state->mouse_baseline_count + 1) {
		return 0;
	}
	remainder = NULL;
	if (state->mouse_baseline_count > 0) {
		if ((size_t)state->mouse_baseline_count
			> SIZE_MAX / sizeof(RAWINPUTDEVICE)) {
			return 0;
		}
		remainder = (RAWINPUTDEVICE *)calloc(
			(size_t)state->mouse_baseline_count, sizeof(RAWINPUTDEVICE));
		if (!remainder) {
			return 0;
		}
	}
	for (source_index = 0; source_index < current->count; source_index++) {
		const RAWINPUTDEVICE *item = &current->items[source_index];
		if (item->usUsagePage == 0x01 && item->usUsage == 0x02
			&& item->dwFlags == 0 && item->hwndTarget == state->hwnd) {
			exact_count++;
			continue;
		}
		if (remainder_index >= state->mouse_baseline_count) {
			free(remainder);
			return 0;
		}
		remainder[remainder_index++] = *item;
	}
	equal = exact_count == 1
		&& remainder_index == state->mouse_baseline_count
		&& v_multiwindow_win32_raw_inventory_equal(
			state->mouse_baseline_devices, state->mouse_baseline_count,
			remainder, remainder_index);
	free(remainder);
	return equal;
}

static inline int v_multiwindow_win32_rect_equal(
	const RECT *left, const RECT *right) {
	return left && right && left->left == right->left
		&& left->top == right->top && left->right == right->right
		&& left->bottom == right->bottom;
}

static inline int v_multiwindow_win32_virtual_screen_rect(RECT *out_rect) {
	int width;
	int height;
	if (!out_rect) {
		return 0;
	}
	width = GetSystemMetrics(SM_CXVIRTUALSCREEN);
	height = GetSystemMetrics(SM_CYVIRTUALSCREEN);
	if (width <= 0 || height <= 0) {
		return 0;
	}
	out_rect->left = GetSystemMetrics(SM_XVIRTUALSCREEN);
	out_rect->top = GetSystemMetrics(SM_YVIRTUALSCREEN);
	out_rect->right = out_rect->left + width;
	out_rect->bottom = out_rect->top + height;
	return out_rect->right > out_rect->left
		&& out_rect->bottom > out_rect->top;
}

static inline int v_multiwindow_win32_client_screen_rect(
	HWND hwnd, RECT *out_rect) {
	RECT client;
	POINT points[2];
	int mapped;
	if (!hwnd || !out_rect || !GetClientRect(hwnd, &client)
		|| client.right <= client.left || client.bottom <= client.top) {
		return 0;
	}
	points[0].x = client.left;
	points[0].y = client.top;
	points[1].x = client.right;
	points[1].y = client.bottom;
	SetLastError(ERROR_SUCCESS);
	mapped = MapWindowPoints(hwnd, NULL, points, 2);
	if (mapped == 0 && GetLastError() != ERROR_SUCCESS) {
		return 0;
	}
	out_rect->left = points[0].x;
	out_rect->top = points[0].y;
	out_rect->right = points[1].x;
	out_rect->bottom = points[1].y;
	return out_rect->right > out_rect->left
		&& out_rect->bottom > out_rect->top;
}

static inline void v_multiwindow_win32_mouse_baseline_free(
	VMultiwindowWin32ServiceState *state) {
	if (!state) {
		return;
	}
	free(state->mouse_baseline_devices);
	state->mouse_baseline_devices = NULL;
	state->mouse_baseline_count = 0;
	state->mouse_baseline_valid = 0;
}

static inline int v_multiwindow_win32_mouse_raw_resource_state(
	const VMultiwindowWin32ServiceState *state,
	const VMultiwindowWin32MouseApis *apis) {
	VMultiwindowWin32RawInventory current;
	int resource_state = V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_DIRTY;
	if (!state || !apis || !state->mouse_baseline_valid
		|| !apis->get_registered_devices
		|| (state->mouse_baseline_count > 0
			&& !state->mouse_baseline_devices)) {
		return V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_DIRTY;
	}
	memset(&current, 0, sizeof(current));
	if (!v_multiwindow_win32_raw_inventory_query(
		apis->get_registered_devices, &current)) {
		return V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_DIRTY;
	}
	if (v_multiwindow_win32_raw_inventory_equal(
			state->mouse_baseline_devices, state->mouse_baseline_count,
			current.items, current.count)) {
		resource_state = V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_BASELINE;
	} else if (v_multiwindow_win32_raw_inventory_locked_exact(state,
		&current)) {
		resource_state = V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_LOCKED;
	}
	v_multiwindow_win32_raw_inventory_free(&current);
	return resource_state;
}

static inline int v_multiwindow_win32_mouse_clip_resource_state(
	const VMultiwindowWin32ServiceState *state) {
	RECT current_clip;
	if (!state || !state->mouse_baseline_valid
		|| !GetClipCursor(&current_clip)) {
		return V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_DIRTY;
	}
	if (v_multiwindow_win32_rect_equal(&current_clip,
		&state->mouse_baseline_clip)) {
		return V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_BASELINE;
	}
	if (v_multiwindow_win32_rect_equal(&current_clip,
		&state->mouse_locked_clip)) {
		return V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_LOCKED;
	}
	return V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_DIRTY;
}

static inline int v_multiwindow_win32_mouse_cancel_tracking(
	VMultiwindowWin32ServiceState *state, int live_window) {
	TRACKMOUSEEVENT tracking;
	if (!state) {
		return 0;
	}
	if (!live_window) {
		return 1;
	}
	if (GetPropW(state->hwnd, V_MULTIWINDOW_WIN32_MOUSE_TRACKED_PROP)
		== NULL) {
		return 1;
	}
	memset(&tracking, 0, sizeof(tracking));
	tracking.cbSize = sizeof(tracking);
	tracking.dwFlags = TME_CANCEL | TME_LEAVE;
	tracking.hwndTrack = state->hwnd;
	if (!TrackMouseEvent(&tracking)) {
		return 0;
	}
	RemovePropW(state->hwnd, V_MULTIWINDOW_WIN32_MOUSE_TRACKED_PROP);
	return GetPropW(state->hwnd,
		V_MULTIWINDOW_WIN32_MOUSE_TRACKED_PROP) == NULL;
}

static inline int v_multiwindow_win32_mouse_off_exact(
	const VMultiwindowWin32ServiceState *state, int live_window) {
	return state
		&& state->mouse_lock_state == V_MULTIWINDOW_WIN32_MOUSE_LOCK_OFF
		&& !state->mouse_lock_committed && !state->mouse_delivery_enabled
		&& !state->mouse_raw_owned && !state->mouse_clip_owned
		&& !state->mouse_baseline_valid && !state->mouse_baseline_devices
		&& state->mouse_baseline_count == 0
		&& v_multiwindow_win32_mouse_lock_owner_load() != state
		&& (!live_window || GetPropW(state->hwnd,
			V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP) == NULL);
}

static inline int v_multiwindow_win32_mouse_prepared_exact(
	const VMultiwindowWin32ServiceState *state) {
	return state
		&& state->mouse_lock_state
			== V_MULTIWINDOW_WIN32_MOUSE_LOCK_TEARDOWN_PREPARED
		&& !state->mouse_lock_committed && !state->mouse_delivery_enabled
		&& !state->mouse_raw_owned && !state->mouse_clip_owned
		&& !state->mouse_baseline_valid && !state->mouse_baseline_devices
		&& state->mouse_baseline_count == 0
		&& v_multiwindow_win32_mouse_lock_owner_load() != state;
}

static inline int v_multiwindow_win32_service_teardown_prepared(
	void *service_state) {
	return v_multiwindow_win32_mouse_prepared_exact(
		(const VMultiwindowWin32ServiceState *)service_state);
}

static inline int v_multiwindow_win32_mouse_live_environment_exact(
	const VMultiwindowWin32ServiceState *state) {
	return state
		&& v_multiwindow_win32_service_authority((void *)state)
			== V_MULTIWINDOW_WIN32_SERVICE_OK
		&& IsWindowVisible(state->hwnd)
		&& GetForegroundWindow() == state->hwnd
		&& GetFocus() == state->hwnd && GetCapture() == NULL;
}

static inline int v_multiwindow_win32_mouse_client_clip_exact(
	const VMultiwindowWin32ServiceState *state) {
	RECT client_clip;
	return state
		&& v_multiwindow_win32_client_screen_rect(state->hwnd, &client_clip)
		&& v_multiwindow_win32_rect_equal(&client_clip,
			&state->mouse_locked_clip)
		&& v_multiwindow_win32_mouse_clip_resource_state(state)
			== V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_LOCKED;
}

static inline int v_multiwindow_win32_mouse_locked_exact(
	VMultiwindowWin32ServiceState *state,
	const VMultiwindowWin32MouseApis *apis) {
	return state && apis
		&& state->mouse_lock_state == V_MULTIWINDOW_WIN32_MOUSE_LOCK_LOCKED
		&& state->mouse_lock_committed && state->mouse_delivery_enabled
		&& state->mouse_raw_owned && state->mouse_clip_owned
		&& state->mouse_baseline_valid
		&& v_multiwindow_win32_mouse_lock_owner_load() == state
		&& GetPropW(state->hwnd, V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP)
			== (HANDLE)state->record_data
		&& v_multiwindow_win32_mouse_live_environment_exact(state)
		&& v_multiwindow_win32_mouse_raw_resource_state(state, apis)
			== V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_LOCKED
		&& v_multiwindow_win32_mouse_client_clip_exact(state);
}

static inline int v_multiwindow_win32_mouse_acquire_candidate_exact(
	VMultiwindowWin32ServiceState *state,
	const VMultiwindowWin32MouseApis *apis) {
	return state && apis
		&& state->mouse_lock_state == V_MULTIWINDOW_WIN32_MOUSE_LOCK_CLEANUP
		&& !state->mouse_lock_committed && !state->mouse_delivery_enabled
		&& state->mouse_raw_owned && state->mouse_clip_owned
		&& state->mouse_baseline_valid
		&& v_multiwindow_win32_mouse_lock_owner_load() == state
		&& GetPropW(state->hwnd, V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP)
			== (HANDLE)state->record_data
		&& v_multiwindow_win32_mouse_live_environment_exact(state)
		&& v_multiwindow_win32_mouse_raw_resource_state(state, apis)
			== V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_LOCKED
		&& v_multiwindow_win32_mouse_client_clip_exact(state);
}

static inline int v_multiwindow_win32_service_mouse_cleanup_raw(
	VMultiwindowWin32ServiceState *state,
	const VMultiwindowWin32MouseApis *apis) {
	RAWINPUTDEVICE remove_device;
	int resource_state;
	if (!state || !apis || !state->mouse_baseline_valid
		|| !apis->get_registered_devices || !apis->register_devices) {
		return 0;
	}
	resource_state = v_multiwindow_win32_mouse_raw_resource_state(state,
		apis);
	if (resource_state == V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_BASELINE) {
		state->mouse_raw_owned = 0;
		return 1;
	}
	if (resource_state != V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_LOCKED
		|| !state->mouse_raw_owned) {
		return 0;
	}
	memset(&remove_device, 0, sizeof(remove_device));
	remove_device.usUsagePage = 0x01;
	remove_device.usUsage = 0x02;
	remove_device.dwFlags = RIDEV_REMOVE;
	remove_device.hwndTarget = NULL;
	(void)apis->register_devices(&remove_device, 1, sizeof(RAWINPUTDEVICE));
	if (v_multiwindow_win32_mouse_raw_resource_state(state, apis)
		!= V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_BASELINE) {
		return 0;
	}
	state->mouse_raw_owned = 0;
	return 1;
}

static inline int v_multiwindow_win32_service_mouse_cleanup_clip(
	VMultiwindowWin32ServiceState *state) {
	int resource_state;
	if (!state || !state->mouse_baseline_valid) {
		return 0;
	}
	resource_state = v_multiwindow_win32_mouse_clip_resource_state(state);
	if (resource_state == V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_BASELINE) {
		state->mouse_clip_owned = 0;
		return 1;
	}
	if (resource_state != V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_LOCKED
		|| !state->mouse_clip_owned) {
		return 0;
	}
	(void)ClipCursor(NULL);
	if (v_multiwindow_win32_mouse_clip_resource_state(state)
		!= V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_BASELINE) {
		return 0;
	}
	state->mouse_clip_owned = 0;
	return 1;
}

static inline int v_multiwindow_win32_service_mouse_restore_raw(
	VMultiwindowWin32ServiceState *state,
	const VMultiwindowWin32MouseApis *apis) {
	RAWINPUTDEVICE device;
	int resource_state;
	if (!state || !apis || !state->mouse_baseline_valid
		|| !apis->get_registered_devices || !apis->register_devices) {
		return 0;
	}
	resource_state = v_multiwindow_win32_mouse_raw_resource_state(state,
		apis);
	if (resource_state == V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_LOCKED) {
		return state->mouse_raw_owned;
	}
	if (resource_state != V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_BASELINE) {
		return 0;
	}
	memset(&device, 0, sizeof(device));
	device.usUsagePage = 0x01;
	device.usUsage = 0x02;
	device.dwFlags = 0;
	device.hwndTarget = state->hwnd;
	if (!apis->register_devices(&device, 1, sizeof(RAWINPUTDEVICE))) {
		return 0;
	}
	state->mouse_raw_owned = 1;
	return v_multiwindow_win32_mouse_raw_resource_state(state, apis)
		== V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_LOCKED;
}

static inline int v_multiwindow_win32_service_mouse_restore_clip(
	VMultiwindowWin32ServiceState *state) {
	int resource_state;
	if (!state || !state->mouse_baseline_valid) {
		return 0;
	}
	resource_state = v_multiwindow_win32_mouse_clip_resource_state(state);
	if (resource_state == V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_LOCKED) {
		return state->mouse_clip_owned;
	}
	if (resource_state != V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_BASELINE
		|| !ClipCursor(&state->mouse_locked_clip)) {
		return 0;
	}
	state->mouse_clip_owned = 1;
	return v_multiwindow_win32_mouse_clip_resource_state(state)
		== V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_LOCKED;
}

static inline int v_multiwindow_win32_service_mouse_complete_cleanup(
	VMultiwindowWin32ServiceState *state, int teardown) {
	if (!state || !v_multiwindow_win32_mouse_lock_owner_release(state)) {
		return 0;
	}
	state->mouse_lock_committed = 0;
	state->mouse_delivery_enabled = 0;
	state->mouse_raw_owned = 0;
	state->mouse_clip_owned = 0;
	v_multiwindow_win32_mouse_baseline_free(state);
	memset(&state->mouse_baseline_clip, 0, sizeof(state->mouse_baseline_clip));
	memset(&state->mouse_locked_clip, 0, sizeof(state->mouse_locked_clip));
	state->mouse_lock_state = teardown
		? V_MULTIWINDOW_WIN32_MOUSE_LOCK_TEARDOWN_PREPARED
		: V_MULTIWINDOW_WIN32_MOUSE_LOCK_OFF;
	return 1;
}

static inline int v_multiwindow_win32_service_mouse_restore_locked(
	VMultiwindowWin32ServiceState *state,
	const VMultiwindowWin32MouseApis *apis) {
	HANDLE property;
	int raw_restored;
	int clip_restored;
	if (!state || !apis || !state->mouse_baseline_valid
		|| v_multiwindow_win32_mouse_lock_owner_load() != state
		|| v_multiwindow_win32_service_authority(state)
			!= V_MULTIWINDOW_WIN32_SERVICE_OK) {
		return 0;
	}
	property = GetPropW(state->hwnd, V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP);
	if (!v_multiwindow_win32_mouse_live_environment_exact(state)
		|| (property != NULL && property != (HANDLE)state->record_data)) {
		return 0;
	}
	raw_restored = v_multiwindow_win32_service_mouse_restore_raw(state, apis);
	clip_restored = v_multiwindow_win32_service_mouse_restore_clip(state);
	if (!raw_restored || !clip_restored) {
		return 0;
	}
	property = GetPropW(state->hwnd, V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP);
	if (property == NULL) {
		if (!SetPropW(state->hwnd, V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP,
			(HANDLE)state->record_data)) {
			return 0;
		}
	} else if (property != (HANDLE)state->record_data) {
		return 0;
	}
	if (!v_multiwindow_win32_mouse_live_environment_exact(state)
		|| GetPropW(state->hwnd, V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP)
			!= (HANDLE)state->record_data
		|| v_multiwindow_win32_mouse_raw_resource_state(state, apis)
			!= V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_LOCKED
		|| !v_multiwindow_win32_mouse_client_clip_exact(state)) {
		return 0;
	}
	state->mouse_lock_committed = 1;
	state->mouse_lock_state = V_MULTIWINDOW_WIN32_MOUSE_LOCK_LOCKED;
	state->mouse_delivery_enabled = 1;
	if (!v_multiwindow_win32_mouse_locked_exact(state, apis)) {
		state->mouse_delivery_enabled = 0;
		state->mouse_lock_state = V_MULTIWINDOW_WIN32_MOUSE_LOCK_CLEANUP;
		return 0;
	}
	return 1;
}

static inline int v_multiwindow_win32_service_mouse_cleanup(
	VMultiwindowWin32ServiceState *state, int teardown, int rollback) {
	VMultiwindowWin32MouseApis apis;
	int authority;
	int apis_ready;
	int live_window;
	int restore_allowed = 0;
	int tracking_clean = 0;
	int raw_clean = 0;
	int clip_clean = 0;
	int raw_baseline = 0;
	int clip_baseline = 0;
	int property_clean = 0;
	int authority_clean = 0;
	int capture_clean = 0;
	int clean;
	HANDLE property;
	if (!state) {
		return V_MULTIWINDOW_WIN32_SERVICE_INVALID;
	}
	if (GetCurrentThreadId() != state->owner_thread) {
		return V_MULTIWINDOW_WIN32_SERVICE_WRONG_THREAD;
	}
	if (GetCurrentProcessId() != state->owner_process) {
		return V_MULTIWINDOW_WIN32_SERVICE_INVALID;
	}
	if (v_multiwindow_win32_mouse_prepared_exact(state)) {
		return teardown ? V_MULTIWINDOW_WIN32_SERVICE_OK
			: V_MULTIWINDOW_WIN32_SERVICE_INVALID;
	}
	live_window = state->hwnd && IsWindow(state->hwnd);
	authority = v_multiwindow_win32_service_authority(state);
	if (live_window && authority != V_MULTIWINDOW_WIN32_SERVICE_OK) {
		return authority;
	}
	if (!live_window && !teardown) {
		return V_MULTIWINDOW_WIN32_SERVICE_INVALID;
	}
	if (v_multiwindow_win32_mouse_off_exact(state, live_window)) {
		if (teardown) {
			if (!v_multiwindow_win32_mouse_cancel_tracking(state,
				live_window)) {
				return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
			}
			state->mouse_lock_state =
				V_MULTIWINDOW_WIN32_MOUSE_LOCK_TEARDOWN_PREPARED;
		}
		return V_MULTIWINDOW_WIN32_SERVICE_OK;
	}
	if (!live_window
		&& (!state->hwnd || !state->record_data
			|| v_multiwindow_win32_mouse_lock_owner_load() != state
			|| !state->mouse_baseline_valid
			|| (state->mouse_lock_state != V_MULTIWINDOW_WIN32_MOUSE_LOCK_LOCKED
				&& state->mouse_lock_state
					!= V_MULTIWINDOW_WIN32_MOUSE_LOCK_CLEANUP))) {
		return V_MULTIWINDOW_WIN32_SERVICE_INVALID;
	}
	if (!state->record_data || !state->mouse_baseline_valid
		|| v_multiwindow_win32_mouse_lock_owner_load() != state
		|| (state->mouse_lock_state != V_MULTIWINDOW_WIN32_MOUSE_LOCK_LOCKED
			&& state->mouse_lock_state != V_MULTIWINDOW_WIN32_MOUSE_LOCK_CLEANUP)) {
		return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
	}
	memset(&apis, 0, sizeof(apis));
	apis_ready = v_multiwindow_win32_mouse_resolve_apis(&apis);
	if (!teardown && !rollback && live_window && apis_ready) {
		restore_allowed = v_multiwindow_win32_mouse_locked_exact(state, &apis);
	}
	state->mouse_lock_state = V_MULTIWINDOW_WIN32_MOUSE_LOCK_CLEANUP;
	state->mouse_delivery_enabled = 0;
	tracking_clean = v_multiwindow_win32_mouse_cancel_tracking(state,
		live_window);
	if (rollback) {
		if (live_window) {
			property = GetPropW(state->hwnd,
				V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP);
			if (property == (HANDLE)state->record_data) {
				RemovePropW(state->hwnd,
					V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP);
			}
			property_clean = GetPropW(state->hwnd,
				V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP) == NULL;
		}
		clip_clean = v_multiwindow_win32_service_mouse_cleanup_clip(state);
		raw_clean = apis_ready
			? v_multiwindow_win32_service_mouse_cleanup_raw(state, &apis) : 0;
	} else {
		raw_clean = apis_ready
			? v_multiwindow_win32_service_mouse_cleanup_raw(state, &apis) : 0;
		clip_clean = v_multiwindow_win32_service_mouse_cleanup_clip(state);
	}
	if (apis_ready) {
		raw_baseline = v_multiwindow_win32_mouse_raw_resource_state(state,
			&apis) == V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_BASELINE;
	}
	clip_baseline = v_multiwindow_win32_mouse_clip_resource_state(state)
		== V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_BASELINE;
	if (live_window) {
		authority_clean = v_multiwindow_win32_service_authority(state)
			== V_MULTIWINDOW_WIN32_SERVICE_OK;
		capture_clean = GetCapture() == NULL;
		property = GetPropW(state->hwnd, V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP);
		if (!rollback && tracking_clean && raw_clean && clip_clean
			&& raw_baseline && clip_baseline && authority_clean
			&& capture_clean
			&& (property == NULL || property == (HANDLE)state->record_data)) {
			if (property == (HANDLE)state->record_data) {
				RemovePropW(state->hwnd,
					V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP);
			}
			property_clean = GetPropW(state->hwnd,
				V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP) == NULL;
		}
	} else {
		authority_clean = GetCurrentThreadId() == state->owner_thread
			&& GetCurrentProcessId() == state->owner_process
			&& v_multiwindow_win32_mouse_lock_owner_load() == state;
		capture_clean = GetCapture() == NULL;
		property_clean = 1;
	}
	clean = tracking_clean && raw_clean && clip_clean
		&& raw_baseline && clip_baseline && property_clean
		&& authority_clean && capture_clean
		&& !state->mouse_raw_owned && !state->mouse_clip_owned
		&& v_multiwindow_win32_mouse_lock_owner_load() == state;
	if (!clean) {
		if (!teardown && !rollback && restore_allowed && live_window
			&& apis_ready
			&& v_multiwindow_win32_service_mouse_restore_locked(state,
				&apis)) {
			return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
		}
		state->mouse_lock_state = V_MULTIWINDOW_WIN32_MOUSE_LOCK_CLEANUP;
		state->mouse_delivery_enabled = 0;
		return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
	}
	if (!v_multiwindow_win32_service_mouse_complete_cleanup(state, teardown)) {
		state->mouse_lock_state = V_MULTIWINDOW_WIN32_MOUSE_LOCK_CLEANUP;
		state->mouse_delivery_enabled = 0;
		return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
	}
	return V_MULTIWINDOW_WIN32_SERVICE_OK;
}

static inline int v_multiwindow_win32_service_mouse_acquire(
	VMultiwindowWin32ServiceState *state) {
	VMultiwindowWin32MouseApis apis;
	VMultiwindowWin32RawInventory baseline;
	RAWINPUTDEVICE device;
	RECT virtual_screen;
	RECT current_clip;
	RECT client_clip;
	int authority;
	int result = V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
	if (!state) {
		return V_MULTIWINDOW_WIN32_SERVICE_INVALID;
	}
	authority = v_multiwindow_win32_service_authority(state);
	if (authority != V_MULTIWINDOW_WIN32_SERVICE_OK) {
		return authority;
	}
	if (!v_multiwindow_win32_mouse_off_exact(state, 1)
		|| v_multiwindow_win32_mouse_lock_owner_load() != NULL
		|| !state->record_data
		|| !IsWindowVisible(state->hwnd)
		|| GetForegroundWindow() != state->hwnd
		|| GetFocus() != state->hwnd || GetCapture() != NULL) {
		return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
	}
	memset(&apis, 0, sizeof(apis));
	memset(&baseline, 0, sizeof(baseline));
	if (!v_multiwindow_win32_mouse_resolve_apis(&apis)
		|| !v_multiwindow_win32_raw_inventory_query(
			apis.get_registered_devices, &baseline)
		|| !v_multiwindow_win32_raw_inventory_mouse_free(&baseline)
		|| !v_multiwindow_win32_virtual_screen_rect(&virtual_screen)
		|| !GetClipCursor(&current_clip)
		|| !v_multiwindow_win32_rect_equal(&current_clip, &virtual_screen)
		|| !v_multiwindow_win32_client_screen_rect(state->hwnd, &client_clip)) {
		v_multiwindow_win32_raw_inventory_free(&baseline);
		return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
	}
	if (!v_multiwindow_win32_mouse_lock_owner_claim(state)) {
		v_multiwindow_win32_raw_inventory_free(&baseline);
		return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
	}
	state->mouse_baseline_devices = baseline.items;
	state->mouse_baseline_count = baseline.count;
	state->mouse_baseline_clip = current_clip;
	state->mouse_locked_clip = client_clip;
	state->mouse_baseline_valid = 1;
	state->mouse_lock_state = V_MULTIWINDOW_WIN32_MOUSE_LOCK_CLEANUP;
	memset(&device, 0, sizeof(device));
	device.usUsagePage = 0x01;
	device.usUsage = 0x02;
	device.dwFlags = 0;
	device.hwndTarget = state->hwnd;
	if (!apis.register_devices(&device, 1, sizeof(RAWINPUTDEVICE))) {
		goto rollback;
	}
	state->mouse_raw_owned = 1;
	if (v_multiwindow_win32_mouse_raw_resource_state(state, &apis)
		!= V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_LOCKED) {
		goto rollback;
	}
	if (!ClipCursor(&client_clip)) {
		goto rollback;
	}
	state->mouse_clip_owned = 1;
	if (v_multiwindow_win32_mouse_clip_resource_state(state)
		!= V_MULTIWINDOW_WIN32_MOUSE_RESOURCE_LOCKED) {
		goto rollback;
	}
	if (!SetPropW(state->hwnd, V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP,
		(HANDLE)state->record_data)
		|| !v_multiwindow_win32_mouse_acquire_candidate_exact(state,
			&apis)) {
		goto rollback;
	}
	state->mouse_lock_committed = 1;
	state->mouse_delivery_enabled = 1;
	state->mouse_lock_state = V_MULTIWINDOW_WIN32_MOUSE_LOCK_LOCKED;
	return V_MULTIWINDOW_WIN32_SERVICE_OK;

rollback:
	(void)v_multiwindow_win32_service_mouse_cleanup(state, 0, 1);
	return result;
}

static inline int v_multiwindow_win32_service_set_mouse_lock(
	void *state_ptr, int enabled) {
	VMultiwindowWin32ServiceState *state =
		(VMultiwindowWin32ServiceState *)state_ptr;
	if (enabled) {
		return v_multiwindow_win32_service_mouse_acquire(state);
	}
	return v_multiwindow_win32_service_mouse_cleanup(state, 0, 0);
}

#if defined(V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_BACKEND_IMPLEMENTATION)
static volatile LONG v_multiwindow_win32_focus_cleanup_failures_for_test = 0;

static inline void v_multiwindow_win32_service_test_focus_cleanup_failures(
	int count) {
	InterlockedExchange(&v_multiwindow_win32_focus_cleanup_failures_for_test,
		count > 0 ? (LONG)count : 0);
}
#endif

static inline int v_multiwindow_win32_service_focus_lost(void *state_ptr) {
#if defined(V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_BACKEND_IMPLEMENTATION)
	LONG failures = InterlockedCompareExchange(
		&v_multiwindow_win32_focus_cleanup_failures_for_test, 0, 0);
	if (failures > 0) {
		InterlockedDecrement(
			&v_multiwindow_win32_focus_cleanup_failures_for_test);
		return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
	}
#endif
	return v_multiwindow_win32_service_mouse_cleanup(
		(VMultiwindowWin32ServiceState *)state_ptr, 0, 0);
}

static inline int v_multiwindow_win32_service_mouse_delivery_active(
	void *state_ptr) {
	VMultiwindowWin32ServiceState *state =
		(VMultiwindowWin32ServiceState *)state_ptr;
	VMultiwindowWin32MouseApis apis;
	memset(&apis, 0, sizeof(apis));
	return v_multiwindow_win32_mouse_resolve_apis(&apis)
		&& v_multiwindow_win32_mouse_locked_exact(state, &apis);
}

static inline int v_multiwindow_win32_service_disable_mouse_delivery(
	void *state_ptr) {
	VMultiwindowWin32ServiceState *state =
		(VMultiwindowWin32ServiceState *)state_ptr;
	int authority = v_multiwindow_win32_service_authority(state);
	if (authority != V_MULTIWINDOW_WIN32_SERVICE_OK) {
		return authority;
	}
	if (state->mouse_lock_state != V_MULTIWINDOW_WIN32_MOUSE_LOCK_LOCKED
		|| !state->mouse_lock_committed || !state->mouse_delivery_enabled) {
		return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
	}
	state->mouse_delivery_enabled = 0;
	return V_MULTIWINDOW_WIN32_SERVICE_OK;
}

static inline int v_multiwindow_win32_service_prepare_window_teardown(
	void *state_ptr) {
	return v_multiwindow_win32_service_mouse_cleanup(
		(VMultiwindowWin32ServiceState *)state_ptr, 1, 0);
}

static inline int v_multiwindow_win32_service_mouse_observation(
	VMultiwindowWin32ServiceState *state, int *out_locked) {
	VMultiwindowWin32MouseApis apis;
	int authority;
	if (out_locked) {
		*out_locked = 0;
	}
	authority = v_multiwindow_win32_service_authority(state);
	if (authority != V_MULTIWINDOW_WIN32_SERVICE_OK) {
		return authority;
	}
	if (v_multiwindow_win32_mouse_off_exact(state, 1)) {
		return V_MULTIWINDOW_WIN32_SERVICE_OK;
	}
	memset(&apis, 0, sizeof(apis));
	if (!v_multiwindow_win32_mouse_resolve_apis(&apis)
		|| !v_multiwindow_win32_mouse_locked_exact(state, &apis)) {
		return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
	}
	if (out_locked) {
		*out_locked = 1;
	}
	return V_MULTIWINDOW_WIN32_SERVICE_OK;
}

static inline int v_multiwindow_win32_service_capture_restore(
	VMultiwindowWin32ServiceState *state) {
	if (v_multiwindow_win32_service_authority(state)
		!= V_MULTIWINDOW_WIN32_SERVICE_OK) {
		return 0;
	}
	WINDOWPLACEMENT placement;
	ZeroMemory(&placement, sizeof(placement));
	placement.length = sizeof(placement);
	if (!GetWindowPlacement(state->hwnd, &placement)) {
		return 0;
	}
	state->windowed_style = GetWindowLongPtrW(state->hwnd, GWL_STYLE);
	state->windowed_ex_style = GetWindowLongPtrW(state->hwnd, GWL_EXSTYLE);
	state->windowed_placement = placement;
	state->windowed_visible = IsWindowVisible(state->hwnd) != 0;
	state->restore_valid = 1;
	return 1;
}

static inline int v_multiwindow_win32_service_synthesize_restore(
	VMultiwindowWin32ServiceState *state) {
	if (v_multiwindow_win32_service_authority(state)
		!= V_MULTIWINDOW_WIN32_SERVICE_OK) {
		return 0;
	}
	DWORD style = v_multiwindow_win32_service_windowed_style(state->resizable,
		state->borderless);
	DWORD ex_style = v_multiwindow_win32_service_windowed_ex_style(state->borderless);
	RECT frame = {0, 0, state->requested_width > 0 ? state->requested_width : 1,
		state->requested_height > 0 ? state->requested_height : 1};
	if (!AdjustWindowRectEx(&frame, style, FALSE, ex_style)) {
		return 0;
	}
	HMONITOR monitor = MonitorFromWindow(state->hwnd, MONITOR_DEFAULTTONEAREST);
	MONITORINFO monitor_info;
	ZeroMemory(&monitor_info, sizeof(monitor_info));
	monitor_info.cbSize = sizeof(monitor_info);
	if (!monitor || !GetMonitorInfoW(monitor, &monitor_info)) {
		return 0;
	}
	WINDOWPLACEMENT current;
	ZeroMemory(&current, sizeof(current));
	current.length = sizeof(current);
	if (!GetWindowPlacement(state->hwnd, &current)) {
		return 0;
	}
	int width = frame.right - frame.left;
	int height = frame.bottom - frame.top;
	int work_width = monitor_info.rcWork.right - monitor_info.rcWork.left;
	int work_height = monitor_info.rcWork.bottom - monitor_info.rcWork.top;
	int screen_x = monitor_info.rcWork.left + (work_width - width) / 2;
	int screen_y = monitor_info.rcWork.top + (work_height - height) / 2;
	int workspace_x =
		screen_x + monitor_info.rcMonitor.left - monitor_info.rcWork.left;
	int workspace_y =
		screen_y + monitor_info.rcMonitor.top - monitor_info.rcWork.top;
	WINDOWPLACEMENT placement;
	ZeroMemory(&placement, sizeof(placement));
	placement.length = sizeof(placement);
	placement.showCmd = current.showCmd;
	placement.rcNormalPosition.left = workspace_x;
	placement.rcNormalPosition.top = workspace_y;
	placement.rcNormalPosition.right = workspace_x + width;
	placement.rcNormalPosition.bottom = workspace_y + height;
	state->windowed_style = (LONG_PTR)style;
	state->windowed_ex_style = (LONG_PTR)ex_style;
	state->windowed_placement = placement;
	state->windowed_visible = IsWindowVisible(state->hwnd) != 0;
	state->restore_valid = 1;
	return 1;
}

static inline void *v_multiwindow_win32_service_create(void *hwnd_ptr,
	void *record_data, int initial_fullscreen, int width, int height, int resizable,
	int borderless) {
	HWND hwnd = (HWND)hwnd_ptr;
	if (!hwnd || !IsWindow(hwnd)) {
		return NULL;
	}
	DWORD owner_process = 0;
	DWORD owner_thread = GetWindowThreadProcessId(hwnd, &owner_process);
	if (!owner_thread || owner_thread != GetCurrentThreadId() || !owner_process
		|| owner_process != GetCurrentProcessId()
		|| (void *)GetWindowLongPtrW(hwnd, GWLP_USERDATA) != record_data) {
		return NULL;
	}
	VMultiwindowWin32ServiceState *state =
		(VMultiwindowWin32ServiceState *)calloc(1, sizeof(*state));
	if (!state) {
		return NULL;
	}
	state->hwnd = hwnd;
	state->record_data = record_data;
	state->owner_thread = owner_thread;
	state->owner_process = owner_process;
	state->fullscreen = initial_fullscreen != 0;
	state->fullscreen_known = 1;
	state->requested_width = width;
	state->requested_height = height;
	state->resizable = resizable != 0;
	state->borderless = borderless != 0;
	int captured = state->fullscreen ? v_multiwindow_win32_service_synthesize_restore(state)
		: v_multiwindow_win32_service_capture_restore(state);
	if (!captured) {
		free(state);
		return NULL;
	}
	return state;
}

static inline int v_multiwindow_win32_service_release(void *state_ptr) {
	VMultiwindowWin32ServiceState *state =
		(VMultiwindowWin32ServiceState *)state_ptr;
	if (!state) {
		return V_MULTIWINDOW_WIN32_SERVICE_OK;
	}
	if (GetCurrentThreadId() != state->owner_thread) {
		return V_MULTIWINDOW_WIN32_SERVICE_WRONG_THREAD;
	}
	if (GetCurrentProcessId() != state->owner_process) {
		return V_MULTIWINDOW_WIN32_SERVICE_INVALID;
	}
	if (!v_multiwindow_win32_mouse_prepared_exact(state)) {
		return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
	}
	state->hwnd = NULL;
	free(state);
	return V_MULTIWINDOW_WIN32_SERVICE_OK;
}

static inline int v_multiwindow_win32_service_window_state_with_mouse_lock(void *state_ptr,
	int *out_mapping, int *out_visibility, int *out_active, int *out_focused,
	int *out_minimized, int *out_maximized, int *out_fullscreen,
	int *out_mouse_locked, int *out_position_known, int *out_x, int *out_y) {
	VMultiwindowWin32ServiceState *state =
		(VMultiwindowWin32ServiceState *)state_ptr;
	RECT rect;
	int mouse_locked = 0;
	int observation;
	int position_known;
	int visible;
	int active;
	observation = v_multiwindow_win32_service_mouse_observation(state,
		&mouse_locked);
	if (observation != V_MULTIWINDOW_WIN32_SERVICE_OK) {
		return observation;
	}
	ZeroMemory(&rect, sizeof(rect));
	position_known = GetWindowRect(state->hwnd, &rect) != 0;
	visible = IsWindowVisible(state->hwnd) != 0;
	active = GetForegroundWindow() == state->hwnd;
	if (out_mapping) *out_mapping = visible ? 2 : 1;
	if (out_visibility) *out_visibility = visible ? 2 : 1;
	if (out_active) *out_active = active ? 2 : 1;
	if (out_focused) *out_focused = active && GetFocus() == state->hwnd ? 2 : 1;
	if (out_minimized) *out_minimized = IsIconic(state->hwnd) ? 2 : 1;
	if (out_maximized) *out_maximized = IsZoomed(state->hwnd) ? 2 : 1;
	if (out_fullscreen) {
		*out_fullscreen = state->fullscreen_known ? (state->fullscreen ? 2 : 1) : 0;
	}
	if (out_mouse_locked) {
		*out_mouse_locked = mouse_locked ? 2 : 1;
	}
	if (out_position_known) *out_position_known = position_known;
	if (out_x) *out_x = position_known ? rect.left : 0;
	if (out_y) *out_y = position_known ? rect.top : 0;
	return V_MULTIWINDOW_WIN32_SERVICE_OK;
}

static inline int v_multiwindow_win32_service_window_state(void *state_ptr,
	int *out_mapping, int *out_visibility, int *out_active, int *out_focused,
	int *out_minimized, int *out_maximized, int *out_fullscreen,
	int *out_position_known, int *out_x, int *out_y) {
	return v_multiwindow_win32_service_window_state_with_mouse_lock(state_ptr,
		out_mapping, out_visibility, out_active, out_focused, out_minimized,
		out_maximized, out_fullscreen, NULL, out_position_known, out_x, out_y);
}

static inline int v_multiwindow_win32_service_show_window(void *state_ptr) {
	VMultiwindowWin32ServiceState *state =
		(VMultiwindowWin32ServiceState *)state_ptr;
	int authority = v_multiwindow_win32_service_authority(state);
	if (authority != V_MULTIWINDOW_WIN32_SERVICE_OK) return authority;
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	if (v_multiwindow_win32_service_test_show_failure) {
		return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
	}
#endif
	ShowWindow(state->hwnd, SW_SHOWNOACTIVATE);
	UpdateWindow(state->hwnd);
	return IsWindowVisible(state->hwnd) ? V_MULTIWINDOW_WIN32_SERVICE_OK
		: V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
}

static inline int v_multiwindow_win32_service_hide_window(void *state_ptr) {
	VMultiwindowWin32ServiceState *state =
		(VMultiwindowWin32ServiceState *)state_ptr;
	int authority = v_multiwindow_win32_service_authority(state);
	if (authority != V_MULTIWINDOW_WIN32_SERVICE_OK) return authority;
	ShowWindow(state->hwnd, SW_HIDE);
	return !IsWindowVisible(state->hwnd) ? V_MULTIWINDOW_WIN32_SERVICE_OK
		: V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
}

static inline int v_multiwindow_win32_service_focus_window(void *state_ptr) {
	VMultiwindowWin32ServiceState *state =
		(VMultiwindowWin32ServiceState *)state_ptr;
	int authority = v_multiwindow_win32_service_authority(state);
	if (authority != V_MULTIWINDOW_WIN32_SERVICE_OK) return authority;
	if (IsIconic(state->hwnd)) {
		ShowWindow(state->hwnd, SW_RESTORE);
	}
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	if (!v_multiwindow_win32_service_test_focus_refused) {
		SetForegroundWindow(state->hwnd);
	}
#else
	SetForegroundWindow(state->hwnd);
#endif
	if (GetForegroundWindow() == state->hwnd) {
		SetFocus(state->hwnd);
	}
	return V_MULTIWINDOW_WIN32_SERVICE_OK;
}

static inline int v_multiwindow_win32_service_raise_window(void *state_ptr) {
	VMultiwindowWin32ServiceState *state =
		(VMultiwindowWin32ServiceState *)state_ptr;
	int authority = v_multiwindow_win32_service_authority(state);
	if (authority != V_MULTIWINDOW_WIN32_SERVICE_OK) return authority;
	return SetWindowPos(state->hwnd, HWND_TOP, 0, 0, 0, 0,
		SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE | SWP_NOOWNERZORDER)
		? V_MULTIWINDOW_WIN32_SERVICE_OK : V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
}

static inline int v_multiwindow_win32_service_set_window_position(void *state_ptr,
	int x, int y) {
	VMultiwindowWin32ServiceState *state =
		(VMultiwindowWin32ServiceState *)state_ptr;
	int authority = v_multiwindow_win32_service_authority(state);
	if (authority != V_MULTIWINDOW_WIN32_SERVICE_OK) return authority;
	return SetWindowPos(state->hwnd, NULL, x, y, 0, 0,
		SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE | SWP_NOOWNERZORDER)
		? V_MULTIWINDOW_WIN32_SERVICE_OK : V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
}

static inline int v_multiwindow_win32_service_minimize_window(void *state_ptr) {
	VMultiwindowWin32ServiceState *state =
		(VMultiwindowWin32ServiceState *)state_ptr;
	int authority = v_multiwindow_win32_service_authority(state);
	if (authority != V_MULTIWINDOW_WIN32_SERVICE_OK) return authority;
	ShowWindow(state->hwnd, SW_MINIMIZE);
	return IsIconic(state->hwnd) ? V_MULTIWINDOW_WIN32_SERVICE_OK
		: V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
}

static inline int v_multiwindow_win32_service_maximize_window(void *state_ptr) {
	VMultiwindowWin32ServiceState *state =
		(VMultiwindowWin32ServiceState *)state_ptr;
	int authority = v_multiwindow_win32_service_authority(state);
	if (authority != V_MULTIWINDOW_WIN32_SERVICE_OK) return authority;
	ShowWindow(state->hwnd, SW_MAXIMIZE);
	return IsZoomed(state->hwnd) ? V_MULTIWINDOW_WIN32_SERVICE_OK
		: V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
}

static inline int v_multiwindow_win32_service_set_fullscreen(void *state_ptr,
	int enabled) {
	VMultiwindowWin32ServiceState *state =
		(VMultiwindowWin32ServiceState *)state_ptr;
	int authority = v_multiwindow_win32_service_authority(state);
	if (authority != V_MULTIWINDOW_WIN32_SERVICE_OK) return authority;
	enabled = enabled != 0;
	if (!state->fullscreen_known) {
		return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
	}
	if (state->fullscreen == enabled) {
		return V_MULTIWINDOW_WIN32_SERVICE_OK;
	}
	if (enabled) {
		if (!v_multiwindow_win32_service_capture_restore(state)) {
			return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
		}
		HMONITOR monitor = MonitorFromWindow(state->hwnd, MONITOR_DEFAULTTONEAREST);
		MONITORINFO monitor_info;
		ZeroMemory(&monitor_info, sizeof(monitor_info));
		monitor_info.cbSize = sizeof(monitor_info);
		if (!monitor || !GetMonitorInfoW(monitor, &monitor_info)) {
			return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
		}
		LONG_PTR fullscreen_style =
			(state->windowed_style & ~(LONG_PTR)WS_OVERLAPPEDWINDOW) | WS_POPUP;
		LONG_PTR fullscreen_ex_style =
			state->windowed_ex_style & ~(LONG_PTR)WS_EX_WINDOWEDGE;
		if (!v_multiwindow_win32_service_set_long_ptr(state->hwnd, GWL_STYLE,
			fullscreen_style)
			|| !v_multiwindow_win32_service_set_long_ptr(state->hwnd, GWL_EXSTYLE,
				fullscreen_ex_style)
			|| !SetWindowPos(state->hwnd, HWND_TOP, monitor_info.rcMonitor.left,
				monitor_info.rcMonitor.top,
				monitor_info.rcMonitor.right - monitor_info.rcMonitor.left,
				monitor_info.rcMonitor.bottom - monitor_info.rcMonitor.top,
				SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_FRAMECHANGED)) {
			v_multiwindow_win32_service_set_long_ptr(state->hwnd, GWL_STYLE,
				state->windowed_style);
			v_multiwindow_win32_service_set_long_ptr(state->hwnd, GWL_EXSTYLE,
				state->windowed_ex_style);
			SetWindowPlacement(state->hwnd, &state->windowed_placement);
			SetWindowPos(state->hwnd, NULL, 0, 0, 0, 0,
				SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE
					| SWP_NOOWNERZORDER | SWP_FRAMECHANGED);
			return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
		}
			state->fullscreen = 1;
			state->fullscreen_known = 1;
			return V_MULTIWINDOW_WIN32_SERVICE_OK;
	}
	if (!state->restore_valid) {
		return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
	}
	VMultiwindowWin32NativeWindowSnapshot fullscreen_snapshot;
	if (!v_multiwindow_win32_service_capture_native_snapshot(state->hwnd,
		&fullscreen_snapshot)) {
		return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
	}
	int restored =
		v_multiwindow_win32_service_set_long_ptr(state->hwnd, GWL_STYLE,
			state->windowed_style);
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	if (restored
		&& v_multiwindow_win32_service_test_fullscreen_exit_failure
			== V_MULTIWINDOW_WIN32_SERVICE_TEST_FAIL_EXIT_EXSTYLE) {
		restored = 0;
	}
#endif
	if (restored) {
		restored = v_multiwindow_win32_service_set_long_ptr(state->hwnd,
			GWL_EXSTYLE, state->windowed_ex_style);
	}
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	if (restored
		&& v_multiwindow_win32_service_test_fullscreen_exit_failure
			== V_MULTIWINDOW_WIN32_SERVICE_TEST_FAIL_EXIT_PLACEMENT) {
		restored = 0;
	}
#endif
	if (restored) {
		restored = SetWindowPlacement(state->hwnd, &state->windowed_placement);
	}
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	if (restored
		&& v_multiwindow_win32_service_test_fullscreen_exit_failure
			== V_MULTIWINDOW_WIN32_SERVICE_TEST_FAIL_EXIT_POSITION) {
		restored = 0;
	}
#endif
	if (restored) {
		restored = SetWindowPos(state->hwnd, NULL, 0, 0, 0, 0,
			SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE
				| SWP_NOOWNERZORDER | SWP_FRAMECHANGED);
	}
	if (restored) {
		restored = v_multiwindow_win32_service_restore_visibility(state->hwnd,
			state->windowed_visible, state->windowed_placement.showCmd);
	}
	if (!restored) {
		if (!v_multiwindow_win32_service_restore_native_snapshot(state->hwnd,
			&fullscreen_snapshot)) {
			state->fullscreen_known = 0;
		}
		return V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
	}
	state->fullscreen = 0;
	state->fullscreen_known = 1;
	return V_MULTIWINDOW_WIN32_SERVICE_OK;
}

static inline int v_multiwindow_win32_service_fullscreen_known(void *state_ptr) {
	VMultiwindowWin32ServiceState *state =
		(VMultiwindowWin32ServiceState *)state_ptr;
	return v_multiwindow_win32_service_authority(state)
		== V_MULTIWINDOW_WIN32_SERVICE_OK && state->fullscreen_known;
}

static inline int v_multiwindow_win32_service_restore_window(void *state_ptr) {
	VMultiwindowWin32ServiceState *state =
		(VMultiwindowWin32ServiceState *)state_ptr;
	int authority = v_multiwindow_win32_service_authority(state);
	if (authority != V_MULTIWINDOW_WIN32_SERVICE_OK) return authority;
	if (state->fullscreen) {
		return v_multiwindow_win32_service_set_fullscreen(state, 0);
	}
	ShowWindow(state->hwnd, SW_RESTORE);
	return !IsIconic(state->hwnd) && !IsZoomed(state->hwnd)
		? V_MULTIWINDOW_WIN32_SERVICE_OK : V_MULTIWINDOW_WIN32_SERVICE_UNAVAILABLE;
}

static inline void *v_multiwindow_win32_service_native_window(void *state_ptr) {
	VMultiwindowWin32ServiceState *state =
		(VMultiwindowWin32ServiceState *)state_ptr;
	return v_multiwindow_win32_service_authority(state)
		== V_MULTIWINDOW_WIN32_SERVICE_OK ? (void *)state->hwnd : NULL;
}
#endif

#endif
