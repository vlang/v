#ifndef V_MULTIWINDOW_WIN32_NONREADBACK_TEST_ORACLE_H
#define V_MULTIWINDOW_WIN32_NONREADBACK_TEST_ORACLE_H

#if defined(_WIN32)
#include <windows.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#include "../win32_service_native.h"
#include "win32_monitor_enumeration_test_seam.h"

typedef HRESULT(WINAPI *VMultiwindowTestDwmGetWindowAttribute)(HWND, DWORD, PVOID, DWORD);
typedef UINT(WINAPI *VMultiwindowTestGetDpiForWindow)(HWND);
typedef HANDLE (WINAPI *VMultiwindowTestGetWindowDpiAwarenessContext)(HWND);
typedef int (WINAPI *VMultiwindowTestGetAwarenessFromDpiAwarenessContext)(HANDLE);
typedef HANDLE (WINAPI *VMultiwindowTestGetThreadDpiAwarenessContext)(void);
typedef BOOL (WINAPI *VMultiwindowTestAreDpiAwarenessContextsEqual)(
	HANDLE, HANDLE);
typedef UINT(WINAPI *VMultiwindowTestGetRegisteredRawInputDevices)(
	PRAWINPUTDEVICE, PUINT, UINT);

typedef struct VMultiwindowTestMonitorSnapshotItem {
	HMONITOR handle;
	wchar_t name[CCHDEVICENAME];
	RECT geometry;
	RECT work;
	int primary;
} VMultiwindowTestMonitorSnapshotItem;

typedef struct VMultiwindowTestMonitorSnapshot {
	VMultiwindowTestMonitorSnapshotItem *items;
	int count;
	int capacity;
	int failed;
} VMultiwindowTestMonitorSnapshot;

typedef struct VMultiwindowTestWin32WindowSnapshot {
	LONG_PTR style;
	LONG_PTR ex_style;
	WINDOWPLACEMENT placement;
	RECT rect;
	int visible;
} VMultiwindowTestWin32WindowSnapshot;

typedef struct VMultiwindowTestWin32WrongThreadProbe {
	void *service_state;
	int authority;
	int window_state;
	void *native_window;
	volatile LONG references;
} VMultiwindowTestWin32WrongThreadProbe;

static volatile LONG v_multiwindow_test_win32_wrong_thread_active;
static DWORD v_multiwindow_test_win32_wrong_thread_worker_delay;
static DWORD v_multiwindow_test_win32_wrong_thread_wait_timeout = 5000;

static inline int v_multiwindow_test_win32_is_window(void *hwnd) {
	return hwnd && IsWindow((HWND)hwnd) ? 1 : 0;
}

static inline int v_multiwindow_test_win32_is_visible(void *hwnd) {
	return hwnd && IsWindowVisible((HWND)hwnd) ? 1 : 0;
}

static inline int v_multiwindow_test_win32_is_enabled(void *hwnd) {
	return hwnd && IsWindowEnabled((HWND)hwnd) ? 1 : 0;
}

static inline int v_multiwindow_test_win32_set_enabled(void *hwnd_ptr,
	int enabled) {
	HWND hwnd = (HWND)hwnd_ptr;
	if (!hwnd || !IsWindow(hwnd)) {
		return 0;
	}
	int target = enabled != 0;
	if ((IsWindowEnabled(hwnd) != 0) != target) {
		(void)EnableWindow(hwnd, target ? TRUE : FALSE);
	}
	return (IsWindowEnabled(hwnd) != 0) == target;
}

static inline int v_multiwindow_test_win32_is_iconic(void *hwnd) {
	return hwnd && IsIconic((HWND)hwnd) ? 1 : 0;
}

static inline int v_multiwindow_test_win32_is_zoomed(void *hwnd) {
	return hwnd && IsZoomed((HWND)hwnd) ? 1 : 0;
}

static inline void *v_multiwindow_test_win32_foreground(void) {
	return (void *)GetForegroundWindow();
}

static inline void *v_multiwindow_test_win32_focus(void) {
	return (void *)GetFocus();
}

static inline int v_multiwindow_test_win32_establish_foreground_focus(
	void *hwnd_ptr) {
	HWND hwnd = (HWND)hwnd_ptr;
	if (!hwnd || !IsWindow(hwnd)) {
		return 0;
	}
	ShowWindow(hwnd, SW_SHOW);
	SetForegroundWindow(hwnd);
	if (GetForegroundWindow() != hwnd) {
		return 0;
	}
	SetFocus(hwnd);
	return GetForegroundWindow() == hwnd && GetFocus() == hwnd;
}

static inline void *v_multiwindow_test_win32_swap_user_data(void *hwnd_ptr,
	void *replacement) {
	HWND hwnd = (HWND)hwnd_ptr;
	if (!hwnd || !IsWindow(hwnd)) {
		return NULL;
	}
	return (void *)SetWindowLongPtrW(hwnd, GWLP_USERDATA,
		(LONG_PTR)replacement);
}

static inline void *v_multiwindow_test_win32_owner(void *hwnd) {
	return hwnd ? (void *)GetWindow((HWND)hwnd, GW_OWNER) : NULL;
}

static inline uint64_t v_multiwindow_test_win32_style(void *hwnd) {
	return hwnd ? (uint64_t)(uintptr_t)GetWindowLongPtrW((HWND)hwnd, GWL_STYLE) : 0;
}

static inline uint64_t v_multiwindow_test_win32_ex_style(void *hwnd) {
	return hwnd ? (uint64_t)(uintptr_t)GetWindowLongPtrW((HWND)hwnd, GWL_EXSTYLE) : 0;
}

static inline int v_multiwindow_test_win32_rect(void *hwnd, int *left, int *top,
	int *right, int *bottom) {
	RECT rect = {0};
	if (!hwnd || !GetWindowRect((HWND)hwnd, &rect)) {
		return 0;
	}
	if (left) *left = rect.left;
	if (top) *top = rect.top;
	if (right) *right = rect.right;
	if (bottom) *bottom = rect.bottom;
	return 1;
}

static inline int v_multiwindow_test_win32_is_above(void *upper, void *lower) {
	if (!upper || !lower || upper == lower) {
		return 0;
	}
	HWND cursor = (HWND)upper;
	while ((cursor = GetWindow(cursor, GW_HWNDNEXT)) != NULL) {
		if (cursor == (HWND)lower) {
			return 1;
		}
	}
	return 0;
}

static inline void *v_multiwindow_test_win32_window_snapshot_new(void *hwnd_ptr) {
	HWND hwnd = (HWND)hwnd_ptr;
	VMultiwindowTestWin32WindowSnapshot *snapshot =
		(VMultiwindowTestWin32WindowSnapshot *)calloc(1, sizeof(*snapshot));
	if (!snapshot || !hwnd || !IsWindow(hwnd)) {
		free(snapshot);
		return NULL;
	}
	snapshot->placement.length = sizeof(snapshot->placement);
	if (!GetWindowPlacement(hwnd, &snapshot->placement)
		|| !GetWindowRect(hwnd, &snapshot->rect)) {
		free(snapshot);
		return NULL;
	}
	snapshot->style = GetWindowLongPtrW(hwnd, GWL_STYLE);
	snapshot->ex_style = GetWindowLongPtrW(hwnd, GWL_EXSTYLE);
	snapshot->visible = IsWindowVisible(hwnd) != 0;
	return snapshot;
}

static inline void v_multiwindow_test_win32_window_snapshot_free(
	void *snapshot_ptr) {
	free(snapshot_ptr);
}

static inline int v_multiwindow_test_win32_window_snapshot_matches(
	void *snapshot_ptr, void *hwnd_ptr) {
	const VMultiwindowTestWin32WindowSnapshot *snapshot =
		(const VMultiwindowTestWin32WindowSnapshot *)snapshot_ptr;
	HWND hwnd = (HWND)hwnd_ptr;
	if (!snapshot || !hwnd || !IsWindow(hwnd)
		|| GetWindowLongPtrW(hwnd, GWL_STYLE) != snapshot->style
		|| GetWindowLongPtrW(hwnd, GWL_EXSTYLE) != snapshot->ex_style) {
		return 0;
	}
	WINDOWPLACEMENT current;
	RECT rect;
	ZeroMemory(&current, sizeof(current));
	ZeroMemory(&rect, sizeof(rect));
	current.length = sizeof(current);
	if (!GetWindowPlacement(hwnd, &current) || !GetWindowRect(hwnd, &rect)) {
		return 0;
	}
	return current.flags == snapshot->placement.flags
		&& current.showCmd == snapshot->placement.showCmd
		&& current.ptMinPosition.x == snapshot->placement.ptMinPosition.x
		&& current.ptMinPosition.y == snapshot->placement.ptMinPosition.y
		&& current.ptMaxPosition.x == snapshot->placement.ptMaxPosition.x
		&& current.ptMaxPosition.y == snapshot->placement.ptMaxPosition.y
		&& EqualRect(&current.rcNormalPosition,
			&snapshot->placement.rcNormalPosition)
		&& EqualRect(&rect, &snapshot->rect)
		&& (IsWindowVisible(hwnd) != 0) == snapshot->visible;
}

static inline int v_multiwindow_test_win32_synthesized_windowed_matches(
	void *hwnd_ptr, int resizable, int borderless, int requested_width,
	int requested_height, int expected_visible, UINT expected_show_command) {
	HWND hwnd = (HWND)hwnd_ptr;
	if (!hwnd || !IsWindow(hwnd)) {
		return 0;
	}
	LONG_PTR expected_style = (LONG_PTR)v_multiwindow_win32_service_windowed_style(
		resizable, borderless);
	LONG_PTR expected_ex_style =
		(LONG_PTR)v_multiwindow_win32_service_windowed_ex_style(borderless);
	LONG_PTR style_mask = WS_POPUP | WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX
		| WS_SIZEBOX | WS_MAXIMIZEBOX | WS_CLIPSIBLINGS | WS_CLIPCHILDREN;
	LONG_PTR ex_style_mask = WS_EX_APPWINDOW | WS_EX_WINDOWEDGE;
	LONG_PTR style = GetWindowLongPtrW(hwnd, GWL_STYLE);
	LONG_PTR ex_style = GetWindowLongPtrW(hwnd, GWL_EXSTYLE);
	WINDOWPLACEMENT placement;
	RECT rect;
	RECT frame = {0, 0, requested_width > 0 ? requested_width : 1,
		requested_height > 0 ? requested_height : 1};
	HMONITOR monitor = MonitorFromWindow(hwnd, MONITOR_DEFAULTTONEAREST);
	MONITORINFO monitor_info;
	ZeroMemory(&placement, sizeof(placement));
	ZeroMemory(&rect, sizeof(rect));
	ZeroMemory(&monitor_info, sizeof(monitor_info));
	placement.length = sizeof(placement);
	monitor_info.cbSize = sizeof(monitor_info);
	if (!AdjustWindowRectEx(&frame, (DWORD)expected_style, FALSE,
		(DWORD)expected_ex_style) || !monitor
		|| !GetMonitorInfoW(monitor, &monitor_info)) {
		return 0;
	}
	int width = frame.right - frame.left;
	int height = frame.bottom - frame.top;
	int screen_x = monitor_info.rcWork.left
		+ ((monitor_info.rcWork.right - monitor_info.rcWork.left) - width) / 2;
	int screen_y = monitor_info.rcWork.top
		+ ((monitor_info.rcWork.bottom - monitor_info.rcWork.top) - height) / 2;
	int workspace_x =
		screen_x + monitor_info.rcMonitor.left - monitor_info.rcWork.left;
	int workspace_y =
		screen_y + monitor_info.rcMonitor.top - monitor_info.rcWork.top;
	return (style & style_mask) == (expected_style & style_mask)
		&& (ex_style & ex_style_mask) == (expected_ex_style & ex_style_mask)
		&& GetWindowPlacement(hwnd, &placement)
		&& placement.showCmd == expected_show_command
		&& placement.rcNormalPosition.left == workspace_x
		&& placement.rcNormalPosition.top == workspace_y
		&& placement.rcNormalPosition.right == workspace_x + width
		&& placement.rcNormalPosition.bottom == workspace_y + height
		&& (IsWindowVisible(hwnd) != 0) == (expected_visible != 0)
		&& GetWindowRect(hwnd, &rect) && rect.right > rect.left
		&& rect.bottom > rect.top;
}

static inline void v_multiwindow_test_win32_wrong_thread_release(
	VMultiwindowTestWin32WrongThreadProbe *context) {
	if (context && InterlockedDecrement(&context->references) == 0) {
		InterlockedDecrement(&v_multiwindow_test_win32_wrong_thread_active);
		free(context);
	}
}

static DWORD WINAPI v_multiwindow_test_win32_service_wrong_thread_worker(
	void *context_ptr) {
	VMultiwindowTestWin32WrongThreadProbe *context =
		(VMultiwindowTestWin32WrongThreadProbe *)context_ptr;
	if (v_multiwindow_test_win32_wrong_thread_worker_delay > 0) {
		Sleep(v_multiwindow_test_win32_wrong_thread_worker_delay);
	}
	context->authority =
		v_multiwindow_win32_service_authority(context->service_state);
	context->window_state = v_multiwindow_win32_service_window_state(
		context->service_state, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
		NULL, NULL);
	context->native_window =
		v_multiwindow_win32_service_native_window(context->service_state);
	v_multiwindow_test_win32_wrong_thread_release(context);
	return 0;
}

static inline void v_multiwindow_test_win32_service_wrong_thread_timing(
	DWORD worker_delay, DWORD wait_timeout) {
	v_multiwindow_test_win32_wrong_thread_worker_delay = worker_delay;
	v_multiwindow_test_win32_wrong_thread_wait_timeout = wait_timeout;
}

static inline int v_multiwindow_test_win32_service_wrong_thread_active_count(void) {
	return (int)InterlockedCompareExchange(
		&v_multiwindow_test_win32_wrong_thread_active, 0, 0);
}

static inline int v_multiwindow_test_win32_service_wrong_thread_wait_cleanup(
	DWORD timeout) {
	DWORD waited = 0;
	while (v_multiwindow_test_win32_service_wrong_thread_active_count() != 0
		&& waited < timeout) {
		Sleep(1);
		waited++;
	}
	return v_multiwindow_test_win32_service_wrong_thread_active_count() == 0;
}

static inline int v_multiwindow_test_win32_service_wrong_thread_rejected(
	void *service_state) {
	VMultiwindowTestWin32WrongThreadProbe *context =
		(VMultiwindowTestWin32WrongThreadProbe *)calloc(1, sizeof(*context));
	if (!context) {
		return 0;
	}
	context->service_state = service_state;
	context->references = 2;
	InterlockedIncrement(&v_multiwindow_test_win32_wrong_thread_active);
	HANDLE thread = CreateThread(NULL, 0,
		v_multiwindow_test_win32_service_wrong_thread_worker, context, 0, NULL);
	if (!thread) {
		InterlockedDecrement(&v_multiwindow_test_win32_wrong_thread_active);
		free(context);
		return 0;
	}
	DWORD wait = WaitForSingleObject(thread,
		v_multiwindow_test_win32_wrong_thread_wait_timeout);
	int rejected = wait == WAIT_OBJECT_0
		&& context->authority == V_MULTIWINDOW_WIN32_SERVICE_WRONG_THREAD
		&& context->window_state == V_MULTIWINDOW_WIN32_SERVICE_WRONG_THREAD
		&& context->native_window == NULL;
	CloseHandle(thread);
	v_multiwindow_test_win32_wrong_thread_release(context);
	return rejected;
}

static inline UINT v_multiwindow_test_win32_dpi(void *hwnd) {
	HMODULE user32 = GetModuleHandleW(L"user32.dll");
	VMultiwindowTestGetDpiForWindow get_dpi = user32 ?
		(VMultiwindowTestGetDpiForWindow)GetProcAddress(user32, "GetDpiForWindow") : NULL;
	if (get_dpi && hwnd) {
		UINT dpi = get_dpi((HWND)hwnd);
		if (dpi) {
			return dpi;
		}
	}
	HDC dc = GetDC((HWND)hwnd);
	int dpi = dc ? GetDeviceCaps(dc, LOGPIXELSX) : 96;
	if (dc) {
		ReleaseDC((HWND)hwnd, dc);
	}
	return dpi > 0 ? (UINT)dpi : 96;
}

static inline int
v_multiwindow_test_win32_window_dpi_awareness(void *hwnd_ptr) {
	HWND hwnd = (HWND)hwnd_ptr;
	if (!hwnd || !IsWindow(hwnd)) {
		return -2;
	}
	HMODULE user32 = GetModuleHandleW(L"user32.dll");
	VMultiwindowTestGetWindowDpiAwarenessContext get_window_context = user32
		? (VMultiwindowTestGetWindowDpiAwarenessContext)GetProcAddress(
			user32, "GetWindowDpiAwarenessContext")
		: NULL;
	VMultiwindowTestGetAwarenessFromDpiAwarenessContext get_awareness = user32
		? (VMultiwindowTestGetAwarenessFromDpiAwarenessContext)GetProcAddress(
			user32, "GetAwarenessFromDpiAwarenessContext")
		: NULL;
	if (!get_window_context || !get_awareness) {
		return -1;
	}
	HANDLE context = get_window_context(hwnd);
	return context ? get_awareness(context) : -2;
}

static inline void *
v_multiwindow_test_win32_thread_dpi_awareness_context(void) {
	HMODULE user32 = GetModuleHandleW(L"user32.dll");
	VMultiwindowTestGetThreadDpiAwarenessContext get_thread_context = user32
		? (VMultiwindowTestGetThreadDpiAwarenessContext)GetProcAddress(
			user32, "GetThreadDpiAwarenessContext")
		: NULL;
	return get_thread_context ? (void *)get_thread_context() : NULL;
}

static inline int
v_multiwindow_test_win32_dpi_awareness_contexts_equal(
	void *first, void *second) {
	if (!first || !second) {
		return 0;
	}
	HMODULE user32 = GetModuleHandleW(L"user32.dll");
	VMultiwindowTestAreDpiAwarenessContextsEqual contexts_equal = user32
		? (VMultiwindowTestAreDpiAwarenessContextsEqual)GetProcAddress(
			user32, "AreDpiAwarenessContextsEqual")
		: NULL;
	if (!contexts_equal) {
		return -1;
	}
	return contexts_equal((HANDLE)first, (HANDLE)second) ? 1 : 0;
}

static inline void
v_multiwindow_test_win32_monitor_snapshot_release_items(
	VMultiwindowTestMonitorSnapshot *snapshot) {
	if (!snapshot) {
		return;
	}
	free(snapshot->items);
	snapshot->items = NULL;
	snapshot->count = 0;
	snapshot->capacity = 0;
}

static inline int
v_multiwindow_test_win32_monitor_snapshot_reserve(
	VMultiwindowTestMonitorSnapshot *snapshot, int needed) {
	if (!snapshot || needed < 0
		|| needed > V_MULTIWINDOW_WIN32_SERVICE_MONITOR_MAX_CAPACITY) {
		return 0;
	}
	if (needed <= snapshot->capacity) {
		return 1;
	}
	int next_capacity = snapshot->capacity > 0
		? snapshot->capacity
		: V_MULTIWINDOW_WIN32_SERVICE_MONITOR_INITIAL_CAPACITY;
	while (next_capacity < needed
		&& next_capacity < V_MULTIWINDOW_WIN32_SERVICE_MONITOR_MAX_CAPACITY) {
		next_capacity *= 2;
	}
	if (next_capacity > V_MULTIWINDOW_WIN32_SERVICE_MONITOR_MAX_CAPACITY) {
		next_capacity = V_MULTIWINDOW_WIN32_SERVICE_MONITOR_MAX_CAPACITY;
	}
	if (next_capacity < needed) {
		return 0;
	}
	VMultiwindowTestMonitorSnapshotItem *items =
		(VMultiwindowTestMonitorSnapshotItem *)calloc(
			(size_t)next_capacity,
			sizeof(VMultiwindowTestMonitorSnapshotItem));
	if (!items) {
		return 0;
	}
	if (snapshot->count > 0) {
		memcpy(items, snapshot->items,
			(size_t)snapshot->count *
				sizeof(VMultiwindowTestMonitorSnapshotItem));
	}
	free(snapshot->items);
	snapshot->items = items;
	snapshot->capacity = next_capacity;
	return 1;
}

static BOOL CALLBACK v_multiwindow_test_win32_monitor_callback(HMONITOR monitor,
	HDC dc, LPRECT rect, LPARAM data) {
	(void)dc;
	(void)rect;
	VMultiwindowTestMonitorSnapshot *snapshot =
		(VMultiwindowTestMonitorSnapshot *)(uintptr_t)data;
	if (!snapshot) {
		return FALSE;
	}
	if (!v_multiwindow_test_win32_monitor_snapshot_reserve(
			snapshot, snapshot->count + 1)) {
		snapshot->failed = 1;
		return FALSE;
	}
	MONITORINFOEXW info = {0};
	LPMONITORINFO monitor_info = (LPMONITORINFO)&info;
	monitor_info->cbSize = sizeof(info);
	if (!GetMonitorInfoW(monitor, monitor_info)) {
		snapshot->failed = 1;
		return FALSE;
	}
	int index = snapshot->count++;
	VMultiwindowTestMonitorSnapshotItem *item = &snapshot->items[index];
	item->handle = monitor;
	item->geometry = monitor_info->rcMonitor;
	item->work = monitor_info->rcWork;
	item->primary =
		(monitor_info->dwFlags & MONITORINFOF_PRIMARY) != 0;
	wcsncpy(item->name, info.szDevice, CCHDEVICENAME - 1);
	item->name[CCHDEVICENAME - 1] = L'\0';
	return TRUE;
}

static inline int v_multiwindow_test_win32_monitor_snapshot(
	VMultiwindowTestMonitorSnapshot *snapshot) {
	if (!snapshot) {
		return -1;
	}
	v_multiwindow_test_win32_monitor_snapshot_release_items(snapshot);
	snapshot->failed = 0;
	BOOL enumerated = EnumDisplayMonitors(NULL, NULL,
		v_multiwindow_test_win32_monitor_callback,
		(LPARAM)(uintptr_t)snapshot);
	if (!enumerated || snapshot->failed) {
		v_multiwindow_test_win32_monitor_snapshot_release_items(snapshot);
		snapshot->failed = 1;
		return -1;
	}
	return snapshot->count;
}

static inline void *v_multiwindow_test_win32_monitor_snapshot_new(void) {
	VMultiwindowTestMonitorSnapshot *snapshot =
		(VMultiwindowTestMonitorSnapshot *)calloc(1,
			sizeof(VMultiwindowTestMonitorSnapshot));
	if (!snapshot) {
		return NULL;
	}
	if (v_multiwindow_test_win32_monitor_snapshot(snapshot) < 0) {
		v_multiwindow_test_win32_monitor_snapshot_release_items(snapshot);
		free(snapshot);
		return NULL;
	}
	return snapshot;
}

static inline void
v_multiwindow_test_win32_monitor_snapshot_free(void *snapshot_ptr) {
	VMultiwindowTestMonitorSnapshot *snapshot =
		(VMultiwindowTestMonitorSnapshot *)snapshot_ptr;
	if (!snapshot) {
		return;
	}
	v_multiwindow_test_win32_monitor_snapshot_release_items(snapshot);
	free(snapshot);
}

static inline uint64_t v_multiwindow_test_win32_monitor_identity(
	VMultiwindowTestMonitorSnapshot *snapshot, int index) {
	if (!snapshot || index < 0 || index >= snapshot->count) {
		return 0;
	}
	return (uint64_t)(uintptr_t)snapshot->items[index].handle;
}

static inline const wchar_t *v_multiwindow_test_win32_monitor_name(
	VMultiwindowTestMonitorSnapshot *snapshot, int index) {
	if (!snapshot || index < 0 || index >= snapshot->count) {
		return NULL;
	}
	return snapshot->items[index].name;
}

static inline int v_multiwindow_test_win32_monitor_info(
	VMultiwindowTestMonitorSnapshot *snapshot, int index, int *x, int *y,
	int *width, int *height, int *work_x, int *work_y, int *work_width,
	int *work_height, int *primary) {
	if (!snapshot || index < 0 || index >= snapshot->count) {
		return 0;
	}
	VMultiwindowTestMonitorSnapshotItem *item = &snapshot->items[index];
	RECT geometry = item->geometry;
	RECT work = item->work;
	if (x) *x = geometry.left;
	if (y) *y = geometry.top;
	if (width) *width = geometry.right - geometry.left;
	if (height) *height = geometry.bottom - geometry.top;
	if (work_x) *work_x = work.left;
	if (work_y) *work_y = work.top;
	if (work_width) *work_width = work.right - work.left;
	if (work_height) *work_height = work.bottom - work.top;
	if (primary) *primary = item->primary;
	return 1;
}

static inline int v_multiwindow_test_win32_emit_display_change(void *hwnd) {
	DWORD_PTR result = 0;
	return hwnd && SendMessageTimeoutW((HWND)hwnd, WM_DISPLAYCHANGE, 32, 0,
			SMTO_ABORTIFHUNG, 1000, &result) != 0;
}

static inline int v_multiwindow_test_win32_emit_display_changes(void *hwnd,
	int count) {
	if (!hwnd || count <= 0) {
		return 0;
	}
	for (int index = 0; index < count; index++) {
		if (!v_multiwindow_test_win32_emit_display_change(hwnd)) {
			return 0;
		}
	}
	return 1;
}

static inline int v_multiwindow_test_win32_emit_dpi_change(void *hwnd,
	UINT dpi, int left, int top, int width, int height) {
	if (!hwnd || !dpi || width <= 0 || height <= 0) {
		return 0;
	}
	RECT suggested = {
		left,
		top,
		left + width,
		top + height
	};
	DWORD_PTR result = 0;
	return SendMessageTimeoutW((HWND)hwnd, WM_DPICHANGED,
			(WPARAM)MAKELONG(dpi, dpi), (LPARAM)&suggested,
			SMTO_ABORTIFHUNG, 1000, &result) != 0;
}

#define V_MULTIWINDOW_TEST_WIN32_CLIPBOARD_MAX_BYTES (16u * 1024u * 1024u)

static inline int v_multiwindow_test_win32_clipboard_equals(
	const wchar_t *expected, size_t expected_units) {
	if (!expected || expected_units == 0 ||
		expected_units > V_MULTIWINDOW_TEST_WIN32_CLIPBOARD_MAX_BYTES /
			sizeof(wchar_t) ||
		expected[expected_units - 1] != L'\0' ||
		!OpenClipboard(NULL)) {
		return 0;
	}
	HGLOBAL data = (HGLOBAL)GetClipboardData(CF_UNICODETEXT);
	size_t bytes = data ? GlobalSize(data) : 0;
	if (!data || bytes < sizeof(wchar_t)) {
		CloseClipboard();
		return 0;
	}
	size_t scan_bytes = bytes;
	if (scan_bytes > V_MULTIWINDOW_TEST_WIN32_CLIPBOARD_MAX_BYTES) {
		scan_bytes = V_MULTIWINDOW_TEST_WIN32_CLIPBOARD_MAX_BYTES;
	}
	const wchar_t *actual = (const wchar_t *)GlobalLock(data);
	size_t actual_units = 0;
	if (actual) {
		for (size_t offset = 0; offset + sizeof(wchar_t) <= scan_bytes;
			offset += sizeof(wchar_t)) {
			wchar_t unit = 0;
			memcpy(&unit, (const unsigned char *)actual + offset,
				sizeof(unit));
			if (unit == L'\0') {
				actual_units = offset / sizeof(wchar_t) + 1;
				break;
			}
		}
	}
	int equal = actual && actual_units == expected_units &&
		memcmp(actual, expected, expected_units * sizeof(wchar_t)) == 0;
	if (actual) {
		GlobalUnlock(data);
	}
	CloseClipboard();
	return equal;
}

static inline int v_multiwindow_test_win32_set_clipboard_raw(void *owner_ptr,
	const void *payload, size_t bytes) {
	HWND owner = (HWND)owner_ptr;
	if (!owner || !IsWindow(owner) || !payload || bytes == 0 ||
		!OpenClipboard(owner)) {
		return 0;
	}
	HGLOBAL data = GlobalAlloc(GMEM_MOVEABLE, bytes);
	void *target = data ? GlobalLock(data) : NULL;
	if (!target) {
		if (data) GlobalFree(data);
		CloseClipboard();
		return 0;
	}
	memcpy(target, payload, bytes);
	GlobalUnlock(data);
	if (!EmptyClipboard()) {
		GlobalFree(data);
		CloseClipboard();
		return 0;
	}
	if (!SetClipboardData(CF_UNICODETEXT, data)) {
		GlobalFree(data);
		CloseClipboard();
		return 0;
	}
	return CloseClipboard() != 0;
}

static inline int v_multiwindow_test_win32_set_clipboard(void *owner_ptr,
	const wchar_t *text, size_t units) {
	if (!text || units == 0 ||
		units > ((size_t)-1) / sizeof(wchar_t)) {
		return 0;
	}
	return v_multiwindow_test_win32_set_clipboard_raw(owner_ptr, text,
		units * sizeof(wchar_t));
}

static inline int
v_multiwindow_test_win32_clipboard_unterminated_parser_probe(void) {
	HGLOBAL data = GlobalAlloc(GMEM_MOVEABLE, sizeof(uint16_t));
	if (!data) {
		return -1;
	}
	size_t actual_bytes = GlobalSize(data);
	if (actual_bytes < sizeof(uint16_t)) {
		(void)GlobalFree(data);
		return -1;
	}
	void *target = GlobalLock(data);
	if (!target) {
		(void)GlobalFree(data);
		return -1;
	}
	memset(target, 0x41, actual_bytes);
	size_t units = (size_t)-1;
	size_t utf8_bytes = (size_t)-1;
	int parse_status = v_multiwindow_win32_parse_clipboard_utf16(target,
		actual_bytes, &units, &utf8_bytes);
	int passed = parse_status == V_MULTIWINDOW_WIN32_CLIPBOARD_CONVERT_INVALID
		&& units == 0 && utf8_bytes == 0;
	(void)GlobalUnlock(data);
	if (GlobalFree(data) != NULL) {
		return -1;
	}
	return passed ? 1 : 0;
}

static inline int v_multiwindow_test_win32_set_clipboard_malformed(
	void *owner_ptr, int kind) {
	HWND owner = (HWND)owner_ptr;
	if (!owner || !IsWindow(owner)) {
		return 0;
	}
	if (kind == 1) {
		const uint16_t invalid_low_surrogate[] = {
			0xdc00u, 0u
		};
		return v_multiwindow_test_win32_set_clipboard_raw(owner,
			invalid_low_surrogate, sizeof(invalid_low_surrogate));
	}
	if (kind == 2) {
		const uint16_t invalid_high_surrogate[] = {
			0xd800u, 0x0041u, 0u
		};
		return v_multiwindow_test_win32_set_clipboard_raw(owner,
			invalid_high_surrogate, sizeof(invalid_high_surrogate));
	}
	return 0;
}

static inline VMultiwindowTestGetRegisteredRawInputDevices
v_multiwindow_test_win32_get_registered_raw_input_devices(void) {
	HMODULE user32 = GetModuleHandleW(L"user32.dll");
	if (!user32) {
		return NULL;
	}
	return (VMultiwindowTestGetRegisteredRawInputDevices)GetProcAddress(
		user32, "GetRegisteredRawInputDevices");
}

static inline void *v_multiwindow_test_win32_raw_mouse_target(void) {
	VMultiwindowTestGetRegisteredRawInputDevices get_registered_raw_input_devices =
		v_multiwindow_test_win32_get_registered_raw_input_devices();
	if (!get_registered_raw_input_devices) {
		return NULL;
	}
	UINT count = 0;
	if (get_registered_raw_input_devices(NULL, &count, sizeof(RAWINPUTDEVICE)) != 0
		|| count == 0) {
		return NULL;
	}
	RAWINPUTDEVICE *devices = (RAWINPUTDEVICE *)calloc(count, sizeof(RAWINPUTDEVICE));
	if (!devices) {
		return NULL;
	}
	UINT copied = count;
	if (get_registered_raw_input_devices(devices, &copied,
		sizeof(RAWINPUTDEVICE)) == (UINT)-1) {
		free(devices);
		return NULL;
	}
	HWND target = NULL;
	for (UINT index = 0; index < copied; index++) {
		if (devices[index].usUsagePage == 0x01 && devices[index].usUsage == 0x02) {
			target = devices[index].hwndTarget;
			break;
		}
	}
	free(devices);
	return (void *)target;
}

static inline int v_multiwindow_test_win32_raw_mouse_registered_for(void *hwnd) {
	VMultiwindowTestGetRegisteredRawInputDevices get_registered_raw_input_devices =
		v_multiwindow_test_win32_get_registered_raw_input_devices();
	UINT count = 0;
	if (!hwnd || !get_registered_raw_input_devices
		|| get_registered_raw_input_devices(NULL, &count, sizeof(RAWINPUTDEVICE)) != 0
		|| count == 0) {
		return 0;
	}
	RAWINPUTDEVICE *devices = (RAWINPUTDEVICE *)calloc(count, sizeof(RAWINPUTDEVICE));
	if (!devices) {
		return 0;
	}
	UINT copied = count;
	if (get_registered_raw_input_devices(devices, &copied, sizeof(RAWINPUTDEVICE))
		== (UINT)-1) {
		free(devices);
		return 0;
	}
	int registered = 0;
	for (UINT index = 0; index < copied; index++) {
		if (devices[index].usUsagePage == 0x01
			&& devices[index].usUsage == 0x02
			&& devices[index].hwndTarget == (HWND)hwnd) {
			registered = 1;
			break;
		}
	}
	free(devices);
	return registered;
}

static inline int v_multiwindow_test_win32_emit_focus_loss(void *hwnd,
	void *next_hwnd) {
	DWORD_PTR result = 0;
	return hwnd && SendMessageTimeoutW((HWND)hwnd, WM_KILLFOCUS,
		(WPARAM)(HWND)next_hwnd, 0, SMTO_ABORTIFHUNG, 1000, &result) != 0;
}

static inline int v_multiwindow_test_win32_clip_matches_client(void *hwnd) {
	RECT client = {0};
	RECT clip = {0};
	if (!hwnd || !GetClientRect((HWND)hwnd, &client)
		|| !GetClipCursor(&clip)) {
		return 0;
	}
	MapWindowPoints((HWND)hwnd, NULL, (POINT *)&client, 2);
	return EqualRect(&client, &clip) ? 1 : 0;
}

static inline int v_multiwindow_test_win32_clip_is_virtual_screen(void) {
	RECT clip = {0};
	RECT screen = {
		GetSystemMetrics(SM_XVIRTUALSCREEN),
		GetSystemMetrics(SM_YVIRTUALSCREEN),
		GetSystemMetrics(SM_XVIRTUALSCREEN) + GetSystemMetrics(SM_CXVIRTUALSCREEN),
		GetSystemMetrics(SM_YVIRTUALSCREEN) + GetSystemMetrics(SM_CYVIRTUALSCREEN)
	};
	return GetClipCursor(&clip) && EqualRect(&clip, &screen) ? 1 : 0;
}

static inline void *v_multiwindow_test_win32_capture(void) {
	return (void *)GetCapture();
}

static inline int v_multiwindow_test_win32_dwm_dark(void *hwnd, int *value) {
	HMODULE dwmapi = LoadLibraryW(L"dwmapi.dll");
	VMultiwindowTestDwmGetWindowAttribute get_attribute = dwmapi ?
		(VMultiwindowTestDwmGetWindowAttribute)GetProcAddress(dwmapi,
			"DwmGetWindowAttribute") : NULL;
	if (!get_attribute || !hwnd || !value) {
		if (dwmapi) FreeLibrary(dwmapi);
		return 0;
	}
	BOOL dark = FALSE;
	HRESULT result = get_attribute((HWND)hwnd, 20, &dark, sizeof(dark));
	FreeLibrary(dwmapi);
	if (FAILED(result)) {
		return 0;
	}
	*value = dark ? 1 : 0;
	return 1;
}

#if (defined(V_MULTIWINDOW_WIN32_SERVICE_TEST) \
	|| defined(V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_BACKEND_IMPLEMENTATION)) \
	&& !defined(V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_ADAPTERS)
#define V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_ADAPTERS

#define V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_ROUTE_UNAVAILABLE (-2147483000)

#if defined(V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_BACKEND_IMPLEMENTATION)

#define V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_MAX_BACKENDS 32
#define V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_MAX_REQUESTS 128

typedef struct VMultiwindowWin32ClipboardTestRequest {
	uint64_t app;
	uint64_t serial;
	int attempts;
} VMultiwindowWin32ClipboardTestRequest;

typedef struct VMultiwindowWin32ClipboardTestState {
	void *backend;
	int use_injected_clock;
	int64_t now_ns;
	int fail_open_attempts;
	int attempts;
	void *last_open_owner;
	int owned_globals;
	int owned_globals_peak;
	int global_allocations;
	int global_transfers;
	int global_frees;
	int fail_before_transfer;
	int sequence_allocations;
	VMultiwindowWin32ClipboardTestRequest
		requests[V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_MAX_REQUESTS];
	int request_count;
} VMultiwindowWin32ClipboardTestState;

static VMultiwindowWin32ClipboardTestState
	v_multiwindow_win32_clipboard_test_states[
		V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_MAX_BACKENDS];

static inline VMultiwindowWin32ClipboardTestState *
v_multiwindow_win32_clipboard_test_state(void *backend, int create) {
	if (!backend) {
		return NULL;
	}
	VMultiwindowWin32ClipboardTestState *available = NULL;
	for (int index = 0;
		index < V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_MAX_BACKENDS;
		index++) {
		VMultiwindowWin32ClipboardTestState *state =
			&v_multiwindow_win32_clipboard_test_states[index];
		if (state->backend == backend) {
			return state;
		}
		if (!available && !state->backend) {
			available = state;
		}
	}
	if (!create || !available) {
		return NULL;
	}
	available->backend = backend;
	return available;
}

static inline VMultiwindowWin32ClipboardTestRequest *
v_multiwindow_win32_clipboard_test_request(
		VMultiwindowWin32ClipboardTestState *state, uint64_t app,
		uint64_t serial, int create) {
	if (!state) {
		return NULL;
	}
	for (int index = 0; index < state->request_count; index++) {
		VMultiwindowWin32ClipboardTestRequest *request =
			&state->requests[index];
		if (request->app == app && request->serial == serial) {
			return request;
		}
	}
	if (!create || state->request_count >=
		V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_MAX_REQUESTS) {
		return NULL;
	}
	VMultiwindowWin32ClipboardTestRequest *request =
		&state->requests[state->request_count++];
	request->app = app;
	request->serial = serial;
	return request;
}

static inline void v_multiwindow_win32_service_test_clipboard_configure(
		void *backend, int64_t now_ns, int fail_open_attempts) {
	VMultiwindowWin32ClipboardTestState *state =
		v_multiwindow_win32_clipboard_test_state(backend, 1);
	if (!state) {
		return;
	}
	memset(state, 0, sizeof(*state));
	state->backend = backend;
	state->use_injected_clock = 1;
	state->now_ns = now_ns;
	state->fail_open_attempts =
		fail_open_attempts > 0 ? fail_open_attempts : 0;
}

static inline void
v_multiwindow_win32_service_test_clipboard_set_now_ns(
		void *backend, int64_t now_ns) {
	VMultiwindowWin32ClipboardTestState *state =
		v_multiwindow_win32_clipboard_test_state(backend, 1);
	if (!state) {
		return;
	}
	state->use_injected_clock = 1;
	state->now_ns = now_ns;
}

static inline void
v_multiwindow_win32_service_test_clipboard_use_real_clock(void *backend) {
	VMultiwindowWin32ClipboardTestState *state =
		v_multiwindow_win32_clipboard_test_state(backend, 0);
	if (state) {
		state->use_injected_clock = 0;
	}
}

static inline void
v_multiwindow_win32_service_test_clipboard_fail_before_transfer(
		void *backend, int count) {
	VMultiwindowWin32ClipboardTestState *state =
		v_multiwindow_win32_clipboard_test_state(backend, 1);
	if (state) {
		state->fail_before_transfer = count > 0 ? count : 0;
	}
}

static inline int64_t v_multiwindow_win32_clipboard_now_for_test(
		void *backend, int64_t real_now_ns) {
	VMultiwindowWin32ClipboardTestState *state =
		v_multiwindow_win32_clipboard_test_state(backend, 0);
	return state && state->use_injected_clock
		? state->now_ns : real_now_ns;
}

static inline int v_multiwindow_win32_clipboard_take_open_failure_for_test(
		VMultiwindowWin32ClipboardTestState *state) {
	if (!state || state->fail_open_attempts <= 0) {
		return 0;
	}
	state->fail_open_attempts--;
	return 1;
}

static inline void v_multiwindow_win32_clipboard_record_attempt_for_test(
		VMultiwindowWin32ClipboardTestState *state, uint64_t request_app,
		uint64_t request_serial, void *owner) {
	if (!state) {
		return;
	}
	state->attempts++;
	state->last_open_owner = owner;
	VMultiwindowWin32ClipboardTestRequest *request =
		v_multiwindow_win32_clipboard_test_request(state, request_app,
			request_serial, 1);
	if (request) {
		request->attempts++;
	}
}

static inline int v_multiwindow_win32_clipboard_write_for_test(
		void *backend, uint64_t request_app, uint64_t request_serial,
		void *owner, const uint16_t *text, size_t units) {
	VMultiwindowWin32ClipboardTestState *state =
		v_multiwindow_win32_clipboard_test_state(backend, 1);
	v_multiwindow_win32_clipboard_record_attempt_for_test(state,
		request_app, request_serial, owner);
	if (v_multiwindow_win32_clipboard_take_open_failure_for_test(state)) {
		return V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_RETRY;
	}
	VMultiwindowWin32ClipboardWriteObserver observer = {0};
	if (state) {
		observer.owned_globals = &state->owned_globals;
		observer.owned_globals_peak = &state->owned_globals_peak;
		observer.global_allocations = &state->global_allocations;
		observer.global_transfers = &state->global_transfers;
		observer.global_frees = &state->global_frees;
		observer.fail_before_transfer = &state->fail_before_transfer;
	}
	int status = v_multiwindow_win32_clipboard_write_observed(owner, text,
		units, state ? &observer : NULL);
	if (status == V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_CLEANED) {
		return V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_FAILED;
	}
	return status;
}

static inline int v_multiwindow_win32_clipboard_read_for_test(
		void *backend, uint64_t request_app, uint64_t request_serial,
		void *owner, void **out_text, size_t *out_text_bytes) {
	VMultiwindowWin32ClipboardTestState *state =
		v_multiwindow_win32_clipboard_test_state(backend, 1);
	v_multiwindow_win32_clipboard_record_attempt_for_test(state,
		request_app, request_serial, owner);
	if (v_multiwindow_win32_clipboard_take_open_failure_for_test(state)) {
		if (out_text) {
			*out_text = NULL;
		}
		if (out_text_bytes) {
			*out_text_bytes = 0;
		}
		return V_MULTIWINDOW_WIN32_CLIPBOARD_ATTEMPT_RETRY;
	}
	return v_multiwindow_win32_clipboard_read(owner, out_text,
		out_text_bytes);
}

static inline void
v_multiwindow_win32_clipboard_record_sequence_for_test(void *backend) {
	VMultiwindowWin32ClipboardTestState *state =
		v_multiwindow_win32_clipboard_test_state(backend, 1);
	if (state) {
		state->sequence_allocations++;
	}
}

static inline int v_multiwindow_win32_service_test_clipboard_attempts(
		void *backend) {
	VMultiwindowWin32ClipboardTestState *state =
		v_multiwindow_win32_clipboard_test_state(backend, 0);
	return state ? state->attempts : 0;
}

static inline int
v_multiwindow_win32_service_test_clipboard_request_attempts(
		void *backend, uint64_t request_app, uint64_t request_serial) {
	VMultiwindowWin32ClipboardTestState *state =
		v_multiwindow_win32_clipboard_test_state(backend, 0);
	VMultiwindowWin32ClipboardTestRequest *request =
		v_multiwindow_win32_clipboard_test_request(state, request_app,
			request_serial, 0);
	return request ? request->attempts : 0;
}

static inline void *
v_multiwindow_win32_service_test_clipboard_last_open_owner(
		void *backend) {
	VMultiwindowWin32ClipboardTestState *state =
		v_multiwindow_win32_clipboard_test_state(backend, 0);
	return state ? state->last_open_owner : NULL;
}

static inline int
v_multiwindow_win32_service_test_clipboard_owned_globals(void *backend) {
	VMultiwindowWin32ClipboardTestState *state =
		v_multiwindow_win32_clipboard_test_state(backend, 0);
	return state ? state->owned_globals : 0;
}

static inline int
v_multiwindow_win32_service_test_clipboard_owned_globals_peak(
		void *backend) {
	VMultiwindowWin32ClipboardTestState *state =
		v_multiwindow_win32_clipboard_test_state(backend, 0);
	return state ? state->owned_globals_peak : 0;
}

static inline int
v_multiwindow_win32_service_test_clipboard_global_allocations(
		void *backend) {
	VMultiwindowWin32ClipboardTestState *state =
		v_multiwindow_win32_clipboard_test_state(backend, 0);
	return state ? state->global_allocations : 0;
}

static inline int
v_multiwindow_win32_service_test_clipboard_global_transfers(
		void *backend) {
	VMultiwindowWin32ClipboardTestState *state =
		v_multiwindow_win32_clipboard_test_state(backend, 0);
	return state ? state->global_transfers : 0;
}

static inline int
v_multiwindow_win32_service_test_clipboard_global_frees(void *backend) {
	VMultiwindowWin32ClipboardTestState *state =
		v_multiwindow_win32_clipboard_test_state(backend, 0);
	return state ? state->global_frees : 0;
}

static inline int
v_multiwindow_win32_service_test_clipboard_sequence_allocations(
		void *backend) {
	VMultiwindowWin32ClipboardTestState *state =
		v_multiwindow_win32_clipboard_test_state(backend, 0);
	return state ? state->sequence_allocations : 0;
}

static inline int64_t
v_multiwindow_win32_service_test_clipboard_timeout_ns(void *backend) {
	(void)backend;
	return INT64_C(2000000000);
}

static int x__multiwindow__win32_service_test_clipboard_pending_count(
	void *backend);
static int64_t
x__multiwindow__win32_service_test_clipboard_pending_deadline_ns(
	void *backend, int index);
static int
x__multiwindow__win32_service_test_clipboard_pending_write_matches(
	void *backend, int index, uint64_t request_app,
	uint64_t request_serial, uint64_t window_app, int window_slot,
	uint32_t window_generation, uint16_t *text, size_t units);

static inline int
v_multiwindow_win32_service_test_clipboard_pending_count(void *backend) {
	return x__multiwindow__win32_service_test_clipboard_pending_count(
		backend);
}

static inline int64_t
v_multiwindow_win32_service_test_clipboard_pending_deadline_ns(
		void *backend, int index) {
	return
		x__multiwindow__win32_service_test_clipboard_pending_deadline_ns(
			backend, index);
}

static inline int
v_multiwindow_win32_service_test_clipboard_pending_write_matches(
		void *backend, int index, uint64_t request_app,
		uint64_t request_serial, uint64_t window_app, int window_slot,
		uint32_t window_generation, uint16_t *text, size_t units) {
	return
		x__multiwindow__win32_service_test_clipboard_pending_write_matches(
			backend, index, request_app, request_serial, window_app,
			window_slot, window_generation, text, units);
}

#else

static inline void v_multiwindow_win32_service_test_clipboard_configure(
		void *backend, int64_t now_ns, int fail_open_attempts) {
	(void)backend;
	(void)now_ns;
	(void)fail_open_attempts;
}

static inline void
v_multiwindow_win32_service_test_clipboard_set_now_ns(
		void *backend, int64_t now_ns) {
	(void)backend;
	(void)now_ns;
}

static inline void
v_multiwindow_win32_service_test_clipboard_use_real_clock(void *backend) {
	(void)backend;
}

static inline void
v_multiwindow_win32_service_test_clipboard_fail_before_transfer(
		void *backend, int count) {
	(void)backend;
	(void)count;
}

static inline int64_t v_multiwindow_win32_clipboard_now_for_test(
		void *backend, int64_t real_now_ns) {
	(void)backend;
	return real_now_ns;
}

static inline int v_multiwindow_win32_clipboard_write_for_test(
		void *backend, uint64_t request_app, uint64_t request_serial,
		void *owner, const uint16_t *text, size_t units) {
	(void)backend;
	(void)request_app;
	(void)request_serial;
	(void)owner;
	(void)text;
	(void)units;
	return V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_ROUTE_UNAVAILABLE;
}

static inline int v_multiwindow_win32_clipboard_read_for_test(
		void *backend, uint64_t request_app, uint64_t request_serial,
		void *owner, void **out_text, size_t *out_text_bytes) {
	(void)backend;
	(void)request_app;
	(void)request_serial;
	(void)owner;
	if (out_text) {
		*out_text = NULL;
	}
	if (out_text_bytes) {
		*out_text_bytes = 0;
	}
	return V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_ROUTE_UNAVAILABLE;
}

static inline void
v_multiwindow_win32_clipboard_record_sequence_for_test(void *backend) {
	(void)backend;
}

static inline int v_multiwindow_win32_service_test_clipboard_attempts(
		void *backend) {
	(void)backend;
	return V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_ROUTE_UNAVAILABLE;
}

static inline int
v_multiwindow_win32_service_test_clipboard_request_attempts(
		void *backend, uint64_t request_app, uint64_t request_serial) {
	(void)backend;
	(void)request_app;
	(void)request_serial;
	return V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_ROUTE_UNAVAILABLE;
}

static inline void *
v_multiwindow_win32_service_test_clipboard_last_open_owner(
		void *backend) {
	(void)backend;
	return NULL;
}

static inline int
v_multiwindow_win32_service_test_clipboard_owned_globals(void *backend) {
	(void)backend;
	return V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_ROUTE_UNAVAILABLE;
}

static inline int
v_multiwindow_win32_service_test_clipboard_owned_globals_peak(
		void *backend) {
	(void)backend;
	return V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_ROUTE_UNAVAILABLE;
}

static inline int
v_multiwindow_win32_service_test_clipboard_global_allocations(
		void *backend) {
	(void)backend;
	return V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_ROUTE_UNAVAILABLE;
}

static inline int
v_multiwindow_win32_service_test_clipboard_global_transfers(
		void *backend) {
	(void)backend;
	return V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_ROUTE_UNAVAILABLE;
}

static inline int
v_multiwindow_win32_service_test_clipboard_global_frees(void *backend) {
	(void)backend;
	return V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_ROUTE_UNAVAILABLE;
}

static inline int
v_multiwindow_win32_service_test_clipboard_sequence_allocations(
		void *backend) {
	(void)backend;
	return V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_ROUTE_UNAVAILABLE;
}

static inline int64_t
v_multiwindow_win32_service_test_clipboard_timeout_ns(void *backend) {
	(void)backend;
	return INT64_C(-1);
}

static inline int
v_multiwindow_win32_service_test_clipboard_pending_count(void *backend) {
	(void)backend;
	return V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_ROUTE_UNAVAILABLE;
}

static inline int64_t
v_multiwindow_win32_service_test_clipboard_pending_deadline_ns(
		void *backend, int index) {
	(void)backend;
	(void)index;
	return INT64_C(-1);
}

static inline int
v_multiwindow_win32_service_test_clipboard_pending_write_matches(
		void *backend, int index, uint64_t request_app,
		uint64_t request_serial, uint64_t window_app, int window_slot,
		uint32_t window_generation, uint16_t *text, size_t units) {
	(void)backend;
	(void)index;
	(void)request_app;
	(void)request_serial;
	(void)window_app;
	(void)window_slot;
	(void)window_generation;
	(void)text;
	(void)units;
	return V_MULTIWINDOW_WIN32_CLIPBOARD_TEST_ROUTE_UNAVAILABLE;
}

#endif
#endif
#endif

#endif
