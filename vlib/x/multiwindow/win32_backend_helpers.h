#pragma once

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include <windows.h>
#include <shellapi.h>

#ifndef WM_DPICHANGED
#define WM_DPICHANGED 0x02E0
#endif

#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
#include "testdata/win32_monitor_enumeration_test_seam.h"
#endif

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
#include "native_render_result.h"
#define V_MULTIWINDOW_TEST_WIN32_PROCESS_COMMIT UINT64_C(17)
static inline void v_multiwindow_test_win32_oracle_record(uint64_t kind,
	uint64_t identity, uint64_t parent_identity,
	const VMultiwindowNativePrimitive *raw);
#endif

#ifndef WM_TOUCH
#define WM_TOUCH 0x0240
typedef HANDLE HTOUCHINPUT;
typedef struct tagTOUCHINPUT {
	LONG x;
	LONG y;
	HANDLE hSource;
	DWORD dwID;
	DWORD dwFlags;
	DWORD dwMask;
	DWORD dwTime;
	ULONG_PTR dwExtraInfo;
	DWORD cxContact;
	DWORD cyContact;
} TOUCHINPUT, *PTOUCHINPUT;
#endif
#ifndef RIM_INPUT
#define RIM_INPUT 0
#endif
#ifndef RIM_TYPEMOUSE
#define RIM_TYPEMOUSE 0
#endif
#ifndef MOUSE_MOVE_ABSOLUTE
#define MOUSE_MOVE_ABSOLUTE 0x0001
#endif
#ifndef V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP
#define V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP L"V_x_multiwindow_mouse_lock_active"
#endif
#ifndef V_MULTIWINDOW_WIN32_MOUSE_TRACKED_PROP
#define V_MULTIWINDOW_WIN32_MOUSE_TRACKED_PROP L"V_x_multiwindow_mouse_tracked"
#endif

#define V_MULTIWINDOW_WIN32_RAW_INPUT_MAX_BYTES (64u * 1024u)
#define V_MULTIWINDOW_WIN32_RAW_INPUT_ERROR -1
#define V_MULTIWINDOW_WIN32_RAW_INPUT_VALID_IGNORED 0
#define V_MULTIWINDOW_WIN32_RAW_INPUT_DELIVERED 1
#ifndef TOUCHEVENTF_MOVE
#define TOUCHEVENTF_MOVE 0x0001
#endif
#ifndef TOUCHEVENTF_DOWN
#define TOUCHEVENTF_DOWN 0x0002
#endif
#ifndef TOUCHEVENTF_UP
#define TOUCHEVENTF_UP 0x0004
#endif
#ifndef TOUCH_COORD_TO_PIXEL
#define TOUCH_COORD_TO_PIXEL(l) ((l) / 100)
#endif

#ifdef __cplusplus
extern "C" {
#endif
#ifndef VV_EXP
#define V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE extern
#else
#define V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE VV_EXP
#endif
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_close_requested(void *data, uint64_t sequence);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_destroyed(void *data, uint64_t sequence);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_resized(void *data, uint64_t sequence, int width, int height);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_service_refresh(void *data, uint64_t sequence, int reason);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_input_event(void *data, uint64_t sequence, int kind, int key_code, uint32_t char_code, int key_repeat, uint32_t modifiers, int mouse_button, int mouse_x, int mouse_y, int wheel_delta_x, int wheel_delta_y);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_drop_begin(void *data, uint64_t sequence, int mouse_x, int mouse_y, uint32_t modifiers);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_drop_file(void *data, uint64_t sequence, char *path);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_drop_end(void *data, uint64_t sequence);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_touch_event(void *data, uint64_t sequence, int kind, uint32_t modifiers, int count, uint64_t *ids, int *xs, int *ys, int *changed);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE int v_multiwindow_win32_window_mouse_lock_active(void *data);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_focus_lost(void *data);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_raw_mouse_event(void *data, uint64_t sequence, int mouse_x, int mouse_y, int mouse_dx, int mouse_dy, uint32_t modifiers);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE int v_multiwindow_win32_window_suppress_legacy_mouse_tail(void *data, int mouse_x, int mouse_y);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_raw_input_error(void *data);
#undef V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE
#ifdef __cplusplus
}
#endif

#define V_MULTIWINDOW_WIN32_INPUT_KEY_DOWN 1
#define V_MULTIWINDOW_WIN32_INPUT_KEY_UP 2
#define V_MULTIWINDOW_WIN32_INPUT_CHAR 3
#define V_MULTIWINDOW_WIN32_INPUT_MOUSE_DOWN 4
#define V_MULTIWINDOW_WIN32_INPUT_MOUSE_UP 5
#define V_MULTIWINDOW_WIN32_INPUT_MOUSE_SCROLL 6
#define V_MULTIWINDOW_WIN32_INPUT_MOUSE_MOVE 7
#define V_MULTIWINDOW_WIN32_INPUT_MOUSE_ENTER 8
#define V_MULTIWINDOW_WIN32_INPUT_MOUSE_LEAVE 9
#define V_MULTIWINDOW_WIN32_INPUT_FOCUSED 10
#define V_MULTIWINDOW_WIN32_INPUT_UNFOCUSED 11
#define V_MULTIWINDOW_WIN32_INPUT_ICONIFIED 12
#define V_MULTIWINDOW_WIN32_INPUT_RESTORED 13
#define V_MULTIWINDOW_WIN32_INPUT_CLIPBOARD_PASTED 14
#define V_MULTIWINDOW_WIN32_INPUT_TOUCHES_BEGAN 15
#define V_MULTIWINDOW_WIN32_INPUT_TOUCHES_MOVED 16
#define V_MULTIWINDOW_WIN32_INPUT_TOUCHES_ENDED 17
#define V_MULTIWINDOW_WIN32_MOUSE_BUTTON_LEFT 0
#define V_MULTIWINDOW_WIN32_MOUSE_BUTTON_RIGHT 1
#define V_MULTIWINDOW_WIN32_MOUSE_BUTTON_MIDDLE 2
#define V_MULTIWINDOW_WIN32_MOUSE_BUTTON_INVALID 256
#define V_MULTIWINDOW_WIN32_MAX_TOUCH_POINTS 8
#define V_MULTIWINDOW_WIN32_MODIFIER_LMB 0x100
#define V_MULTIWINDOW_WIN32_MODIFIER_RMB 0x200
#define V_MULTIWINDOW_WIN32_MODIFIER_MMB 0x400
#ifndef WM_MOUSEHWHEEL
#define WM_MOUSEHWHEEL 0x020e
#endif
#ifndef MAPVK_VSC_TO_VK_EX
#define MAPVK_VSC_TO_VK_EX 3
#endif

#define V_MULTIWINDOW_CURSOR_SHAPE_DEFAULT 0
#define V_MULTIWINDOW_CURSOR_SHAPE_POINTER 1
#define V_MULTIWINDOW_CURSOR_SHAPE_MOVE 2
#define V_MULTIWINDOW_CURSOR_SHAPE_N_RESIZE 3
#define V_MULTIWINDOW_CURSOR_SHAPE_S_RESIZE 4
#define V_MULTIWINDOW_CURSOR_SHAPE_E_RESIZE 5
#define V_MULTIWINDOW_CURSOR_SHAPE_W_RESIZE 6
#define V_MULTIWINDOW_CURSOR_SHAPE_NE_RESIZE 7
#define V_MULTIWINDOW_CURSOR_SHAPE_NW_RESIZE 8
#define V_MULTIWINDOW_CURSOR_SHAPE_SE_RESIZE 9
#define V_MULTIWINDOW_CURSOR_SHAPE_SW_RESIZE 10
#define V_MULTIWINDOW_CURSOR_SHAPE_EW_RESIZE 11
#define V_MULTIWINDOW_CURSOR_SHAPE_NS_RESIZE 12
#define V_MULTIWINDOW_CURSOR_SHAPE_NESW_RESIZE 13
#define V_MULTIWINDOW_CURSOR_SHAPE_NWSE_RESIZE 14
#define V_MULTIWINDOW_CURSOR_SHAPE_GRAB 15
#define V_MULTIWINDOW_CURSOR_SHAPE_GRABBING 16
#define V_MULTIWINDOW_CURSOR_SHAPE_TEXT 17
#define V_MULTIWINDOW_CURSOR_SHAPE_CROSSHAIR 18
#define V_MULTIWINDOW_CURSOR_SHAPE_NOT_ALLOWED 19
#define V_MULTIWINDOW_CURSOR_SHAPE_RESIZE_ALL 20

static const wchar_t *v_multiwindow_win32_class_name = L"V_x_multiwindow_win32";
static const wchar_t *v_multiwindow_win32_min_width_prop = L"V_x_multiwindow_min_width";
static const wchar_t *v_multiwindow_win32_min_height_prop = L"V_x_multiwindow_min_height";
static const wchar_t *v_multiwindow_win32_mouse_tracked_prop =
	V_MULTIWINDOW_WIN32_MOUSE_TRACKED_PROP;
static const wchar_t *v_multiwindow_win32_cursor_shape_prop = L"V_x_multiwindow_cursor_shape";

static inline int v_multiwindow_win32_max_int(int a, int b) {
	return a > b ? a : b;
}

static inline int v_multiwindow_win32_hwnd_int_prop(HWND hwnd, const wchar_t *name) {
	return (int)(INT_PTR)GetPropW(hwnd, name);
}

static inline void v_multiwindow_win32_set_hwnd_int_prop(HWND hwnd, const wchar_t *name, int value) {
	if (value > 0) {
		SetPropW(hwnd, name, (HANDLE)(INT_PTR)value);
	} else {
		RemovePropW(hwnd, name);
	}
}

static inline LPCWSTR v_multiwindow_win32_cursor_id_for_shape(int shape) {
	switch (shape) {
	case V_MULTIWINDOW_CURSOR_SHAPE_POINTER:
		return IDC_HAND;
	case V_MULTIWINDOW_CURSOR_SHAPE_MOVE:
	case V_MULTIWINDOW_CURSOR_SHAPE_GRAB:
	case V_MULTIWINDOW_CURSOR_SHAPE_GRABBING:
	case V_MULTIWINDOW_CURSOR_SHAPE_RESIZE_ALL:
		return IDC_SIZEALL;
	case V_MULTIWINDOW_CURSOR_SHAPE_TEXT:
		return IDC_IBEAM;
	case V_MULTIWINDOW_CURSOR_SHAPE_CROSSHAIR:
		return IDC_CROSS;
	case V_MULTIWINDOW_CURSOR_SHAPE_NOT_ALLOWED:
		return IDC_NO;
	case V_MULTIWINDOW_CURSOR_SHAPE_N_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_S_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_NS_RESIZE:
		return IDC_SIZENS;
	case V_MULTIWINDOW_CURSOR_SHAPE_E_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_W_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_EW_RESIZE:
		return IDC_SIZEWE;
	case V_MULTIWINDOW_CURSOR_SHAPE_NE_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_SW_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_NESW_RESIZE:
		return IDC_SIZENESW;
	case V_MULTIWINDOW_CURSOR_SHAPE_NW_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_SE_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_NWSE_RESIZE:
		return IDC_SIZENWSE;
	default:
		return IDC_ARROW;
	}
}

static inline int v_multiwindow_win32_apply_cursor_shape(HWND hwnd) {
	int shape = v_multiwindow_win32_hwnd_int_prop(hwnd, v_multiwindow_win32_cursor_shape_prop);
	HCURSOR cursor = LoadCursorW(NULL, v_multiwindow_win32_cursor_id_for_shape(shape));
	if (cursor == NULL) {
		return 0;
	}
	SetCursor(cursor);
	return 1;
}

typedef BOOL (WINAPI *VMultiwindowWin32BackendAdjustWindowRectExForDpi)(LPRECT,
	DWORD, BOOL, DWORD, UINT);
typedef UINT (WINAPI *VMultiwindowWin32BackendGetDpiForWindow)(HWND);
typedef HANDLE (WINAPI *VMultiwindowWin32BackendSetThreadDpiAwarenessContext)(
	HANDLE);

#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
#define V_MULTIWINDOW_WIN32_TEST_DPI_CONTEXT_NORMAL 0
#define V_MULTIWINDOW_WIN32_TEST_DPI_CONTEXT_UNAVAILABLE 1
#define V_MULTIWINDOW_WIN32_TEST_DPI_CONTEXT_REJECTED 2

static int v_multiwindow_win32_test_dpi_context_mode;
static int v_multiwindow_win32_test_dpi_frame_bias_width;
static int v_multiwindow_win32_test_dpi_frame_bias_height;
static int v_multiwindow_win32_test_dpi_context_attempts;
static int v_multiwindow_win32_test_dpi_context_fallbacks;
static int v_multiwindow_win32_test_dpi_exact_resizes;

static inline void v_multiwindow_win32_test_dpi_creation_configure(
		int context_mode, int frame_bias_width, int frame_bias_height) {
	v_multiwindow_win32_test_dpi_context_mode =
		context_mode >= V_MULTIWINDOW_WIN32_TEST_DPI_CONTEXT_NORMAL
			&& context_mode <= V_MULTIWINDOW_WIN32_TEST_DPI_CONTEXT_REJECTED
		? context_mode : V_MULTIWINDOW_WIN32_TEST_DPI_CONTEXT_NORMAL;
	v_multiwindow_win32_test_dpi_frame_bias_width =
		frame_bias_width > 0 ? frame_bias_width : 0;
	v_multiwindow_win32_test_dpi_frame_bias_height =
		frame_bias_height > 0 ? frame_bias_height : 0;
	v_multiwindow_win32_test_dpi_context_attempts = 0;
	v_multiwindow_win32_test_dpi_context_fallbacks = 0;
	v_multiwindow_win32_test_dpi_exact_resizes = 0;
}

static inline void v_multiwindow_win32_test_dpi_creation_reset(void) {
	v_multiwindow_win32_test_dpi_creation_configure(
		V_MULTIWINDOW_WIN32_TEST_DPI_CONTEXT_NORMAL, 0, 0);
}

static inline int v_multiwindow_win32_test_dpi_context_attempt_count(void) {
	return v_multiwindow_win32_test_dpi_context_attempts;
}

static inline int v_multiwindow_win32_test_dpi_context_fallback_count(void) {
	return v_multiwindow_win32_test_dpi_context_fallbacks;
}

static inline int v_multiwindow_win32_test_dpi_exact_resize_count(void) {
	return v_multiwindow_win32_test_dpi_exact_resizes;
}

static inline int v_multiwindow_win32_test_client_size_matches(
		void *hwnd_ptr, int width, int height) {
	RECT rect = {0, 0, 0, 0};
	HWND hwnd = (HWND)hwnd_ptr;
	return hwnd && IsWindow(hwnd) && GetClientRect(hwnd, &rect)
		&& rect.right - rect.left == width
		&& rect.bottom - rect.top == height;
}
#endif

static inline int v_multiwindow_win32_adjusted_size(int width, int height,
		DWORD style, DWORD ex_style, int *out_width, int *out_height) {
	RECT rect = {0, 0, width, height};
	if (!AdjustWindowRectEx(&rect, style, FALSE, ex_style)) {
		return 0;
	}
	*out_width = rect.right - rect.left;
	*out_height = rect.bottom - rect.top;
	return 1;
}

static inline int v_multiwindow_win32_adjusted_size_for_window(HWND hwnd,
		int width, int height, DWORD style, DWORD ex_style,
		int *out_width, int *out_height) {
	if (!out_width || !out_height || width < 0 || height < 0) {
		return 0;
	}
	if (hwnd && IsWindow(hwnd)) {
		HMODULE user32 = GetModuleHandleW(L"user32.dll");
		VMultiwindowWin32BackendGetDpiForWindow get_dpi_for_window = user32
			? (VMultiwindowWin32BackendGetDpiForWindow)GetProcAddress(
				user32, "GetDpiForWindow")
			: NULL;
		VMultiwindowWin32BackendAdjustWindowRectExForDpi adjust_for_dpi = user32
			? (VMultiwindowWin32BackendAdjustWindowRectExForDpi)GetProcAddress(
				user32, "AdjustWindowRectExForDpi")
			: NULL;
		UINT dpi = get_dpi_for_window ? get_dpi_for_window(hwnd) : 0;
		if (adjust_for_dpi && dpi) {
			RECT rect = {0, 0, width, height};
			if (adjust_for_dpi(&rect, style, FALSE, ex_style, dpi)) {
				*out_width = rect.right - rect.left;
				*out_height = rect.bottom - rect.top;
				return 1;
			}
		}

		RECT client_rect = {0, 0, 0, 0};
		RECT window_rect = {0, 0, 0, 0};
		if (GetClientRect(hwnd, &client_rect)
				&& GetWindowRect(hwnd, &window_rect)) {
			int client_width = client_rect.right - client_rect.left;
			int client_height = client_rect.bottom - client_rect.top;
			int window_width = window_rect.right - window_rect.left;
			int window_height = window_rect.bottom - window_rect.top;
			int extra_width = window_width - client_width;
			int extra_height = window_height - client_height;
			if (client_width >= 0 && client_height >= 0
					&& extra_width >= 0 && extra_height >= 0
					&& width <= INT_MAX - extra_width
					&& height <= INT_MAX - extra_height) {
				*out_width = width + extra_width;
				*out_height = height + extra_height;
				return 1;
			}
		}
	}
	return v_multiwindow_win32_adjusted_size(width, height, style, ex_style,
		out_width, out_height);
}

static inline void v_multiwindow_win32_record_exact_resize_for_test(void) {
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	v_multiwindow_win32_test_dpi_exact_resizes++;
#endif
}

static inline int v_multiwindow_win32_set_exact_client_size(HWND hwnd,
		int width, int height, DWORD style, DWORD ex_style) {
	if (!hwnd || !IsWindow(hwnd) || width < 0 || height < 0) {
		return 0;
	}
	RECT client_rect = {0, 0, 0, 0};
	if (!GetClientRect(hwnd, &client_rect)) {
		return 0;
	}
	int actual_width = client_rect.right - client_rect.left;
	int actual_height = client_rect.bottom - client_rect.top;
	if (actual_width == width && actual_height == height) {
		return 1;
	}
	int frame_width = width;
	int frame_height = height;
	if (!v_multiwindow_win32_adjusted_size_for_window(hwnd, width, height,
			style, ex_style, &frame_width, &frame_height)) {
		return 0;
	}
	UINT flags = SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE
		| SWP_NOOWNERZORDER | SWP_NOSENDCHANGING;
	v_multiwindow_win32_record_exact_resize_for_test();
	if (!SetWindowPos(hwnd, NULL, 0, 0, frame_width, frame_height, flags)
			|| !GetClientRect(hwnd, &client_rect)) {
		return 0;
	}
	actual_width = client_rect.right - client_rect.left;
	actual_height = client_rect.bottom - client_rect.top;
	if (actual_width == width && actual_height == height) {
		return 1;
	}

	RECT window_rect = {0, 0, 0, 0};
	if (!GetWindowRect(hwnd, &window_rect)) {
		return 0;
	}
	int64_t corrected_width = (int64_t)(window_rect.right - window_rect.left)
		+ (int64_t)width - (int64_t)actual_width;
	int64_t corrected_height = (int64_t)(window_rect.bottom - window_rect.top)
		+ (int64_t)height - (int64_t)actual_height;
	if (corrected_width <= 0 || corrected_width > INT_MAX
			|| corrected_height <= 0 || corrected_height > INT_MAX) {
		return 0;
	}
	v_multiwindow_win32_record_exact_resize_for_test();
	if (!SetWindowPos(hwnd, NULL, 0, 0, (int)corrected_width,
			(int)corrected_height, flags)
			|| !GetClientRect(hwnd, &client_rect)) {
		return 0;
	}
	return client_rect.right - client_rect.left == width
		&& client_rect.bottom - client_rect.top == height;
}

static uint64_t v_multiwindow_win32_event_sequence = 1;
static int v_multiwindow_win32_event_sequence_exhausted_flag = 0;

static inline uint64_t v_multiwindow_win32_next_event_sequence(void) {
	if (v_multiwindow_win32_event_sequence_exhausted_flag) {
		return 0;
	}
	uint64_t sequence = v_multiwindow_win32_event_sequence;
	if (sequence == UINT64_MAX) {
		v_multiwindow_win32_event_sequence = 0;
		v_multiwindow_win32_event_sequence_exhausted_flag = 1;
	} else {
		v_multiwindow_win32_event_sequence++;
	}
	return sequence;
}

static inline int v_multiwindow_win32_event_sequence_exhausted(void) {
	return v_multiwindow_win32_event_sequence_exhausted_flag;
}

static inline int v_multiwindow_win32_lparam_x(LPARAM lparam) {
	return (int)(int16_t)(lparam & 0xffff);
}

static inline int v_multiwindow_win32_lparam_y(LPARAM lparam) {
	return (int)(int16_t)((lparam >> 16) & 0xffff);
}

static inline int v_multiwindow_win32_wheel_delta(WPARAM wparam) {
	return (int)(int16_t)((wparam >> 16) & 0xffff);
}

static inline void v_multiwindow_win32_client_pos_from_lparam(HWND hwnd, LPARAM lparam, int *out_x, int *out_y) {
	*out_x = v_multiwindow_win32_lparam_x(lparam);
	*out_y = v_multiwindow_win32_lparam_y(lparam);
	if (hwnd) {
		POINT point = {*out_x, *out_y};
		if (ScreenToClient(hwnd, &point)) {
			*out_x = point.x;
			*out_y = point.y;
		}
	}
}

static inline void v_multiwindow_win32_cursor_client_pos(HWND hwnd, int *out_x, int *out_y) {
	POINT point = {0, 0};
	if (GetCursorPos(&point) && hwnd && ScreenToClient(hwnd, &point)) {
		*out_x = point.x;
		*out_y = point.y;
		return;
	}
	*out_x = 0;
	*out_y = 0;
}

static inline uint32_t v_multiwindow_win32_modifiers(void) {
	uint32_t modifiers = 0;
	if (GetKeyState(VK_SHIFT) & 0x8000) {
		modifiers |= 1;
	}
	if (GetKeyState(VK_CONTROL) & 0x8000) {
		modifiers |= 2;
	}
	if (GetKeyState(VK_MENU) & 0x8000) {
		modifiers |= 4;
	}
	if ((GetKeyState(VK_LWIN) | GetKeyState(VK_RWIN)) & 0x8000) {
		modifiers |= 8;
	}
	int swapped = (TRUE == GetSystemMetrics(SM_SWAPBUTTON));
	if (GetAsyncKeyState(VK_LBUTTON) & 0x8000) {
		modifiers |= swapped ? V_MULTIWINDOW_WIN32_MODIFIER_RMB : V_MULTIWINDOW_WIN32_MODIFIER_LMB;
	}
	if (GetAsyncKeyState(VK_RBUTTON) & 0x8000) {
		modifiers |= swapped ? V_MULTIWINDOW_WIN32_MODIFIER_LMB : V_MULTIWINDOW_WIN32_MODIFIER_RMB;
	}
	if (GetAsyncKeyState(VK_MBUTTON) & 0x8000) {
		modifiers |= V_MULTIWINDOW_WIN32_MODIFIER_MMB;
	}
	return modifiers;
}

static inline int v_multiwindow_win32_is_char_code(WPARAM c) {
	return c >= 32 || c == 8 || c == 9 || c == 13 || c == 127;
}

static inline int v_multiwindow_win32_normalized_vk(WPARAM wparam, LPARAM lparam) {
	UINT vk = (UINT)wparam;
	if (vk == VK_SHIFT || vk == VK_CONTROL || vk == VK_MENU) {
		UINT scancode = (UINT)((lparam >> 16) & 0xff);
		if (lparam & 0x01000000) {
			scancode |= 0xe000;
		}
		vk = MapVirtualKeyW(scancode, MAPVK_VSC_TO_VK_EX);
	}
	return (int)vk;
}

static inline int v_multiwindow_win32_scancode(LPARAM lparam) {
	return (int)(HIWORD(lparam) & 0x1FF);
}

static inline const int *v_multiwindow_win32_keycodes(void) {
	static int keycodes[512];
	static int initialized = 0;
	if (initialized) {
		return keycodes;
	}
	initialized = 1;
	/* Same physical scancode table used by sokol_app.h/GLFW. */
	keycodes[0x00B] = 48;
	keycodes[0x002] = 49;
	keycodes[0x003] = 50;
	keycodes[0x004] = 51;
	keycodes[0x005] = 52;
	keycodes[0x006] = 53;
	keycodes[0x007] = 54;
	keycodes[0x008] = 55;
	keycodes[0x009] = 56;
	keycodes[0x00A] = 57;
	keycodes[0x01E] = 65;
	keycodes[0x030] = 66;
	keycodes[0x02E] = 67;
	keycodes[0x020] = 68;
	keycodes[0x012] = 69;
	keycodes[0x021] = 70;
	keycodes[0x022] = 71;
	keycodes[0x023] = 72;
	keycodes[0x017] = 73;
	keycodes[0x024] = 74;
	keycodes[0x025] = 75;
	keycodes[0x026] = 76;
	keycodes[0x032] = 77;
	keycodes[0x031] = 78;
	keycodes[0x018] = 79;
	keycodes[0x019] = 80;
	keycodes[0x010] = 81;
	keycodes[0x013] = 82;
	keycodes[0x01F] = 83;
	keycodes[0x014] = 84;
	keycodes[0x016] = 85;
	keycodes[0x02F] = 86;
	keycodes[0x011] = 87;
	keycodes[0x02D] = 88;
	keycodes[0x015] = 89;
	keycodes[0x02C] = 90;
	keycodes[0x028] = 39;
	keycodes[0x02B] = 92;
	keycodes[0x033] = 44;
	keycodes[0x00D] = 61;
	keycodes[0x029] = 96;
	keycodes[0x01A] = 91;
	keycodes[0x00C] = 45;
	keycodes[0x034] = 46;
	keycodes[0x01B] = 93;
	keycodes[0x027] = 59;
	keycodes[0x035] = 47;
	keycodes[0x056] = 162; /* VK_OEM_102 / non-US #2 */
	keycodes[0x00E] = 259;
	keycodes[0x153] = 261;
	keycodes[0x14F] = 269;
	keycodes[0x01C] = 257;
	keycodes[0x001] = 256;
	keycodes[0x147] = 268;
	keycodes[0x152] = 260;
	keycodes[0x15D] = 348;
	keycodes[0x151] = 267;
	keycodes[0x149] = 266;
	keycodes[0x045] = 284;
	keycodes[0x146] = 284;
	keycodes[0x039] = 32;
	keycodes[0x00F] = 258;
	keycodes[0x03A] = 280;
	keycodes[0x145] = 282;
	keycodes[0x046] = 281;
	keycodes[0x03B] = 290;
	keycodes[0x03C] = 291;
	keycodes[0x03D] = 292;
	keycodes[0x03E] = 293;
	keycodes[0x03F] = 294;
	keycodes[0x040] = 295;
	keycodes[0x041] = 296;
	keycodes[0x042] = 297;
	keycodes[0x043] = 298;
	keycodes[0x044] = 299;
	keycodes[0x057] = 300;
	keycodes[0x058] = 301;
	keycodes[0x064] = 302;
	keycodes[0x065] = 303;
	keycodes[0x066] = 304;
	keycodes[0x067] = 305;
	keycodes[0x068] = 306;
	keycodes[0x069] = 307;
	keycodes[0x06A] = 308;
	keycodes[0x06B] = 309;
	keycodes[0x06C] = 310;
	keycodes[0x06D] = 311;
	keycodes[0x06E] = 312;
	keycodes[0x076] = 313;
	keycodes[0x038] = 342;
	keycodes[0x01D] = 341;
	keycodes[0x02A] = 340;
	keycodes[0x15B] = 343;
	keycodes[0x137] = 283;
	keycodes[0x138] = 346;
	keycodes[0x11D] = 345;
	keycodes[0x036] = 344;
	keycodes[0x136] = 344;
	keycodes[0x15C] = 347;
	keycodes[0x150] = 264;
	keycodes[0x14B] = 263;
	keycodes[0x14D] = 262;
	keycodes[0x148] = 265;
	/* Physical numpad scancodes stay keypad keys even when NumLock is off. */
	keycodes[0x052] = 320;
	keycodes[0x04F] = 321;
	keycodes[0x050] = 322;
	keycodes[0x051] = 323;
	keycodes[0x04B] = 324;
	keycodes[0x04C] = 325;
	keycodes[0x04D] = 326;
	keycodes[0x047] = 327;
	keycodes[0x048] = 328;
	keycodes[0x049] = 329;
	keycodes[0x04E] = 334;
	keycodes[0x053] = 330;
	keycodes[0x135] = 331;
	keycodes[0x11C] = 335;
	keycodes[0x037] = 332;
	keycodes[0x04A] = 333;
	return keycodes;
}

static inline int v_multiwindow_win32_key_code_from_vk(WPARAM wparam, LPARAM lparam) {
	int vk = v_multiwindow_win32_normalized_vk(wparam, lparam);
	switch (vk) {
	case VK_RETURN:
		return (lparam & 0x01000000) ? 335 : 257;
	case VK_LSHIFT:
		return 340;
	case VK_LCONTROL:
		return 341;
	case VK_LMENU:
		return 342;
	case VK_LWIN:
		return 343;
	case VK_RSHIFT:
		return 344;
	case VK_RCONTROL:
		return 345;
	case VK_RMENU:
		return 346;
	case VK_RWIN:
		return 347;
	case VK_APPS:
		return 348;
	default:
		return 0;
	}
}

static inline int v_multiwindow_win32_key_code(WPARAM wparam, LPARAM lparam) {
	int scancode = v_multiwindow_win32_scancode(lparam);
	const int *keycodes = v_multiwindow_win32_keycodes();
	if (scancode >= 0 && scancode < 512 && keycodes[scancode] != 0) {
		return keycodes[scancode];
	}
	return v_multiwindow_win32_key_code_from_vk(wparam, lparam);
}

static inline int v_multiwindow_win32_key_repeat(LPARAM lparam) {
	return (lparam & 0x40000000) != 0;
}

typedef BOOL(WINAPI *v_multiwindow_win32_register_touch_window_proc)(HWND, ULONG);
typedef BOOL(WINAPI *v_multiwindow_win32_unregister_touch_window_proc)(HWND);
typedef BOOL(WINAPI *v_multiwindow_win32_get_touch_input_info_proc)(HTOUCHINPUT, UINT, PTOUCHINPUT, int);
typedef BOOL(WINAPI *v_multiwindow_win32_close_touch_input_handle_proc)(HTOUCHINPUT);

static inline FARPROC v_multiwindow_win32_user32_proc(const char *name) {
	HMODULE user32 = GetModuleHandleW(L"user32.dll");
	if (!user32) {
		return NULL;
	}
	return GetProcAddress(user32, name);
}

static inline int v_multiwindow_win32_register_touch_window(HWND hwnd) {
	v_multiwindow_win32_register_touch_window_proc fn = (v_multiwindow_win32_register_touch_window_proc)v_multiwindow_win32_user32_proc("RegisterTouchWindow");
	return fn ? (fn(hwnd, 0) != 0) : 0;
}

static inline void v_multiwindow_win32_unregister_touch_window(HWND hwnd) {
	v_multiwindow_win32_unregister_touch_window_proc fn = (v_multiwindow_win32_unregister_touch_window_proc)v_multiwindow_win32_user32_proc("UnregisterTouchWindow");
	if (fn) {
		fn(hwnd);
	}
}

static inline int v_multiwindow_win32_get_touch_input_info(HTOUCHINPUT handle, UINT count, PTOUCHINPUT inputs) {
	v_multiwindow_win32_get_touch_input_info_proc fn = (v_multiwindow_win32_get_touch_input_info_proc)v_multiwindow_win32_user32_proc("GetTouchInputInfo");
	return fn ? (fn(handle, count, inputs, sizeof(TOUCHINPUT)) != 0) : 0;
}

static inline void v_multiwindow_win32_close_touch_input_handle(HTOUCHINPUT handle) {
	v_multiwindow_win32_close_touch_input_handle_proc fn = (v_multiwindow_win32_close_touch_input_handle_proc)v_multiwindow_win32_user32_proc("CloseTouchInputHandle");
	if (fn) {
		fn(handle);
	}
}

static inline char *v_multiwindow_win32_wide_to_utf8_alloc(const wchar_t *value) {
	if (!value) {
		return NULL;
	}
	int required = WideCharToMultiByte(CP_UTF8, 0, value, -1, NULL, 0, NULL, NULL);
	if (required <= 0) {
		return NULL;
	}
	char *utf8 = (char *)malloc((size_t)required);
	if (!utf8) {
		return NULL;
	}
	if (WideCharToMultiByte(CP_UTF8, 0, value, -1, utf8, required, NULL, NULL) <= 0) {
		free(utf8);
		return NULL;
	}
	return utf8;
}

static inline int v_multiwindow_win32_begin_mouse_tracking(HWND hwnd) {
	if (GetPropW(hwnd, v_multiwindow_win32_mouse_tracked_prop)) {
		return 0;
	}
	TRACKMOUSEEVENT tme;
	ZeroMemory(&tme, sizeof(tme));
	tme.cbSize = sizeof(tme);
	tme.dwFlags = TME_LEAVE;
	tme.hwndTrack = hwnd;
	if (!TrackMouseEvent(&tme)) {
		return 0;
	}
	SetPropW(hwnd, v_multiwindow_win32_mouse_tracked_prop, (HANDLE)(INT_PTR)1);
	return 1;
}

static inline void v_multiwindow_win32_end_mouse_tracking(HWND hwnd) {
	RemovePropW(hwnd, v_multiwindow_win32_mouse_tracked_prop);
}

static inline void v_multiwindow_win32_emit_drop_files(HWND hwnd, void *data, HDROP hdrop) {
	if (!data || !hdrop) {
		if (hdrop) {
			DragFinish(hdrop);
		}
		return;
	}
	POINT point = {0, 0};
	DragQueryPoint(hdrop, &point);
	uint64_t sequence = v_multiwindow_win32_next_event_sequence();
	v_multiwindow_win32_window_drop_begin(data, sequence, point.x, point.y, v_multiwindow_win32_modifiers());
	UINT count = DragQueryFileW(hdrop, 0xFFFFFFFFu, NULL, 0);
	for (UINT i = 0; i < count; i++) {
		UINT chars = DragQueryFileW(hdrop, i, NULL, 0);
		if (chars == 0) {
			continue;
		}
		wchar_t *wide_path = (wchar_t *)calloc((size_t)chars + 1, sizeof(wchar_t));
		if (!wide_path) {
			continue;
		}
		if (DragQueryFileW(hdrop, i, wide_path, chars + 1) != 0) {
			char *path = v_multiwindow_win32_wide_to_utf8_alloc(wide_path);
			if (path) {
				v_multiwindow_win32_window_drop_file(data, sequence, path);
				free(path);
			}
		}
		free(wide_path);
	}
	v_multiwindow_win32_window_drop_end(data, sequence);
	DragFinish(hdrop);
	(void)hwnd;
}

static inline void v_multiwindow_win32_emit_touch_group(HWND hwnd, void *data, const TOUCHINPUT *inputs, UINT count, DWORD flag, int kind, uint64_t sequence, uint32_t modifiers) {
	uint64_t ids[V_MULTIWINDOW_WIN32_MAX_TOUCH_POINTS];
	int xs[V_MULTIWINDOW_WIN32_MAX_TOUCH_POINTS];
	int ys[V_MULTIWINDOW_WIN32_MAX_TOUCH_POINTS];
	int changed[V_MULTIWINDOW_WIN32_MAX_TOUCH_POINTS];
	int out_count = 0;
	for (UINT i = 0; i < count; i++) {
		if ((inputs[i].dwFlags & flag) == 0) {
			continue;
		}
		POINT point = {TOUCH_COORD_TO_PIXEL(inputs[i].x), TOUCH_COORD_TO_PIXEL(inputs[i].y)};
		ScreenToClient(hwnd, &point);
		ids[out_count] = (uint64_t)inputs[i].dwID;
		xs[out_count] = point.x;
		ys[out_count] = point.y;
		changed[out_count] = 1;
		out_count++;
		if (out_count == V_MULTIWINDOW_WIN32_MAX_TOUCH_POINTS) {
			v_multiwindow_win32_window_touch_event(data, sequence, kind, modifiers, out_count, ids, xs, ys, changed);
			out_count = 0;
		}
	}
	if (out_count > 0) {
		v_multiwindow_win32_window_touch_event(data, sequence, kind, modifiers, out_count, ids, xs, ys, changed);
	}
}

static inline int v_multiwindow_win32_emit_touch_event(HWND hwnd, void *data, WPARAM wparam, LPARAM lparam) {
	UINT count = LOWORD(wparam);
	HTOUCHINPUT handle = (HTOUCHINPUT)lparam;
	if (!data || count == 0 || !handle) {
		return 0;
	}
	TOUCHINPUT *inputs = (TOUCHINPUT *)calloc((size_t)count, sizeof(TOUCHINPUT));
	if (!inputs) {
		return 0;
	}
	if (!v_multiwindow_win32_get_touch_input_info(handle, count, inputs)) {
		free(inputs);
		return 0;
	}
	v_multiwindow_win32_close_touch_input_handle(handle);
	uint64_t sequence = v_multiwindow_win32_next_event_sequence();
	uint32_t modifiers = v_multiwindow_win32_modifiers();
	v_multiwindow_win32_emit_touch_group(hwnd, data, inputs, count, TOUCHEVENTF_DOWN, V_MULTIWINDOW_WIN32_INPUT_TOUCHES_BEGAN, sequence, modifiers);
	v_multiwindow_win32_emit_touch_group(hwnd, data, inputs, count, TOUCHEVENTF_MOVE, V_MULTIWINDOW_WIN32_INPUT_TOUCHES_MOVED, sequence, modifiers);
	v_multiwindow_win32_emit_touch_group(hwnd, data, inputs, count, TOUCHEVENTF_UP, V_MULTIWINDOW_WIN32_INPUT_TOUCHES_ENDED, sequence, modifiers);
	free(inputs);
	return 1;
}

typedef UINT(WINAPI *v_multiwindow_win32_get_raw_input_data_proc)(
	HRAWINPUT, UINT, LPVOID, PUINT, UINT);

static inline int v_multiwindow_win32_resolve_raw_input_proc(
	v_multiwindow_win32_get_raw_input_data_proc *out_proc) {
	HMODULE user32;
	FARPROC procedure;
	if (!out_proc || sizeof(*out_proc) != sizeof(procedure)) {
		return 0;
	}
	memset(out_proc, 0, sizeof(*out_proc));
	user32 = GetModuleHandleW(L"user32.dll");
	if (!user32) {
		return 0;
	}
	procedure = GetProcAddress(user32, "GetRawInputData");
	if (!procedure) {
		return 0;
	}
	memcpy(out_proc, &procedure, sizeof(procedure));
	return 1;
}

static inline int v_multiwindow_win32_emit_raw_mouse_event(
	HWND hwnd, void *data, LPARAM lparam) {
	v_multiwindow_win32_get_raw_input_data_proc get_raw_input_data;
	UINT size = 0;
	UINT copied;
	UINT read;
	unsigned char *storage;
	RAWINPUT *raw;
	POINT cursor;
	LONG dx;
	LONG dy;
	uint64_t sequence;
	if (!hwnd || !data || !lparam
		|| !v_multiwindow_win32_resolve_raw_input_proc(&get_raw_input_data)) {
		return V_MULTIWINDOW_WIN32_RAW_INPUT_ERROR;
	}
	read = get_raw_input_data((HRAWINPUT)lparam, RID_INPUT, NULL, &size,
		sizeof(RAWINPUTHEADER));
	if (read == (UINT)-1 || read != 0 || size < sizeof(RAWINPUTHEADER)
		|| size > V_MULTIWINDOW_WIN32_RAW_INPUT_MAX_BYTES) {
		return V_MULTIWINDOW_WIN32_RAW_INPUT_ERROR;
	}
	storage = (unsigned char *)malloc((size_t)size);
	if (!storage) {
		return V_MULTIWINDOW_WIN32_RAW_INPUT_ERROR;
	}
	copied = size;
	read = get_raw_input_data((HRAWINPUT)lparam, RID_INPUT, storage, &copied,
		sizeof(RAWINPUTHEADER));
	if (read == (UINT)-1 || read != size || copied != size) {
		free(storage);
		return V_MULTIWINDOW_WIN32_RAW_INPUT_ERROR;
	}
	raw = (RAWINPUT *)storage;
	if (raw->header.dwSize != size) {
		free(storage);
		return V_MULTIWINDOW_WIN32_RAW_INPUT_ERROR;
	}
	if (raw->header.dwType != RIM_TYPEMOUSE) {
		free(storage);
		return V_MULTIWINDOW_WIN32_RAW_INPUT_VALID_IGNORED;
	}
	if (size < sizeof(RAWINPUT)) {
		free(storage);
		return V_MULTIWINDOW_WIN32_RAW_INPUT_ERROR;
	}
	if ((raw->data.mouse.usFlags & MOUSE_MOVE_ABSOLUTE) != 0) {
		free(storage);
		return V_MULTIWINDOW_WIN32_RAW_INPUT_VALID_IGNORED;
	}
	dx = raw->data.mouse.lLastX;
	dy = raw->data.mouse.lLastY;
	if (dx == 0 && dy == 0) {
		free(storage);
		return V_MULTIWINDOW_WIN32_RAW_INPUT_VALID_IGNORED;
	}
	if (!GetCursorPos(&cursor) || !ScreenToClient(hwnd, &cursor)) {
		free(storage);
		return V_MULTIWINDOW_WIN32_RAW_INPUT_ERROR;
	}
	free(storage);
	if (!v_multiwindow_win32_window_mouse_lock_active(data)) {
		return V_MULTIWINDOW_WIN32_RAW_INPUT_ERROR;
	}
	sequence = v_multiwindow_win32_next_event_sequence();
	if (sequence == 0) {
		return V_MULTIWINDOW_WIN32_RAW_INPUT_ERROR;
	}
	v_multiwindow_win32_window_raw_mouse_event(data, sequence, cursor.x,
		cursor.y, dx, dy, v_multiwindow_win32_modifiers());
	return V_MULTIWINDOW_WIN32_RAW_INPUT_DELIVERED;
}

static LRESULT CALLBACK v_multiwindow_win32_wnd_proc(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam) {
	void *data = (void *)GetWindowLongPtrW(hwnd, GWLP_USERDATA);
	if (msg == WM_NCCREATE) {
		CREATESTRUCTW *create = (CREATESTRUCTW *)lparam;
		data = create ? create->lpCreateParams : 0;
		SetWindowLongPtrW(hwnd, GWLP_USERDATA, (LONG_PTR)data);
		return TRUE;
	}
	switch (msg) {
	case WM_CLOSE:
		if (data) {
			uint64_t sequence = v_multiwindow_win32_next_event_sequence();
			v_multiwindow_win32_window_close_requested(data, sequence);
			return 0;
		}
		break;
	case WM_DESTROY:
		if (data) {
			RemovePropW(hwnd, V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP);
			uint64_t sequence = v_multiwindow_win32_next_event_sequence();
			v_multiwindow_win32_window_destroyed(data, sequence);
			SetWindowLongPtrW(hwnd, GWLP_USERDATA, 0);
			RemovePropW(hwnd, v_multiwindow_win32_min_width_prop);
			RemovePropW(hwnd, v_multiwindow_win32_min_height_prop);
			RemovePropW(hwnd, v_multiwindow_win32_cursor_shape_prop);
			v_multiwindow_win32_end_mouse_tracking(hwnd);
			v_multiwindow_win32_unregister_touch_window(hwnd);
			DragAcceptFiles(hwnd, FALSE);
			return 0;
		}
		break;
	case WM_SETFOCUS:
		if (data) {
			uint64_t sequence = v_multiwindow_win32_next_event_sequence();
			v_multiwindow_win32_window_input_event(data, sequence, V_MULTIWINDOW_WIN32_INPUT_FOCUSED, 0, 0, 0, v_multiwindow_win32_modifiers(), V_MULTIWINDOW_WIN32_MOUSE_BUTTON_INVALID, 0, 0, 0, 0);
			return 0;
		}
		break;
	case WM_KILLFOCUS:
		if (data) {
			v_multiwindow_win32_window_focus_lost(data);
			uint64_t sequence = v_multiwindow_win32_next_event_sequence();
			v_multiwindow_win32_window_input_event(data, sequence, V_MULTIWINDOW_WIN32_INPUT_UNFOCUSED, 0, 0, 0, v_multiwindow_win32_modifiers(), V_MULTIWINDOW_WIN32_MOUSE_BUTTON_INVALID, 0, 0, 0, 0);
			return 0;
		}
		break;
	case WM_SETCURSOR:
		if (LOWORD(lparam) == HTCLIENT && v_multiwindow_win32_apply_cursor_shape(hwnd)) {
			return TRUE;
		}
		break;
	case WM_GETMINMAXINFO:
		{
			int min_width = v_multiwindow_win32_hwnd_int_prop(hwnd, v_multiwindow_win32_min_width_prop);
			int min_height = v_multiwindow_win32_hwnd_int_prop(hwnd, v_multiwindow_win32_min_height_prop);
			if (min_width > 0 || min_height > 0) {
				MINMAXINFO *mmi = (MINMAXINFO *)lparam;
				DWORD style = (DWORD)GetWindowLongPtrW(hwnd, GWL_STYLE);
				DWORD ex_style = (DWORD)GetWindowLongPtrW(hwnd, GWL_EXSTYLE);
				int frame_width = v_multiwindow_win32_max_int(min_width, 1);
				int frame_height = v_multiwindow_win32_max_int(min_height, 1);
				if (v_multiwindow_win32_adjusted_size_for_window(hwnd,
						frame_width, frame_height, style, ex_style,
						&frame_width, &frame_height)) {
					if (min_width > 0) {
						mmi->ptMinTrackSize.x = frame_width;
					}
					if (min_height > 0) {
						mmi->ptMinTrackSize.y = frame_height;
					}
					return 0;
				}
			}
		}
		break;
	case WM_SIZE:
		if (data) {
			uint64_t state_sequence = v_multiwindow_win32_next_event_sequence();
			if (wparam == SIZE_MINIMIZED) {
				v_multiwindow_win32_window_input_event(data, state_sequence, V_MULTIWINDOW_WIN32_INPUT_ICONIFIED, 0, 0, 0, v_multiwindow_win32_modifiers(), V_MULTIWINDOW_WIN32_MOUSE_BUTTON_INVALID, 0, 0, 0, 0);
				break;
			}
			v_multiwindow_win32_window_input_event(data, state_sequence, V_MULTIWINDOW_WIN32_INPUT_RESTORED, 0, 0, 0, v_multiwindow_win32_modifiers(), V_MULTIWINDOW_WIN32_MOUSE_BUTTON_INVALID, 0, 0, 0, 0);
			RECT rect = {0, 0, 0, 0};
			if (GetClientRect(hwnd, &rect)) {
				int width = rect.right - rect.left;
				int height = rect.bottom - rect.top;
				uint64_t sequence = v_multiwindow_win32_next_event_sequence();
				v_multiwindow_win32_window_resized(data, sequence, width, height);
			}
		}
		break;
	case WM_DISPLAYCHANGE:
		if (data) {
			uint64_t sequence = v_multiwindow_win32_next_event_sequence();
			v_multiwindow_win32_window_service_refresh(data, sequence, 1);
			return 0;
		}
		break;
	case WM_DPICHANGED:
		if (data && lparam) {
			RECT *suggested = (RECT *)lparam;
			if (SetWindowPos(hwnd, NULL, suggested->left, suggested->top,
				suggested->right - suggested->left,
				suggested->bottom - suggested->top,
				SWP_NOZORDER | SWP_NOACTIVATE | SWP_NOOWNERZORDER)) {
				uint64_t sequence = v_multiwindow_win32_next_event_sequence();
				v_multiwindow_win32_window_service_refresh(data, sequence, 2);
			}
			return 0;
		}
		break;
	case WM_WINDOWPOSCHANGED:
		if (data) {
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
			if (v_multiwindow_test_win32_windowposchanged_reason3_consume(hwnd)) {
				v_multiwindow_win32_event_sequence = UINT64_MAX;
				v_multiwindow_win32_event_sequence_exhausted_flag = 0;
			}
#endif
			uint64_t sequence = v_multiwindow_win32_next_event_sequence();
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
			v_multiwindow_test_win32_windowposchanged_reason3_record(sequence);
#endif
			v_multiwindow_win32_window_service_refresh(data, sequence, 3);
		}
		break;
	case WM_MOUSEMOVE:
		if (data) {
			if (GetPropW(hwnd, V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP)
				== (HANDLE)data) {
				return 0;
			}
			int x = v_multiwindow_win32_lparam_x(lparam);
			int y = v_multiwindow_win32_lparam_y(lparam);
			int suppress_legacy_tail =
				v_multiwindow_win32_window_suppress_legacy_mouse_tail(
					data, x, y);
			uint32_t modifiers = v_multiwindow_win32_modifiers();
			if (v_multiwindow_win32_begin_mouse_tracking(hwnd)) {
				uint64_t enter_sequence = v_multiwindow_win32_next_event_sequence();
				v_multiwindow_win32_window_input_event(data, enter_sequence, V_MULTIWINDOW_WIN32_INPUT_MOUSE_ENTER, 0, 0, 0, modifiers, V_MULTIWINDOW_WIN32_MOUSE_BUTTON_INVALID, x, y, 0, 0);
			}
			if (suppress_legacy_tail) {
				return 0;
			}
			uint64_t sequence = v_multiwindow_win32_next_event_sequence();
			v_multiwindow_win32_window_input_event(data, sequence, V_MULTIWINDOW_WIN32_INPUT_MOUSE_MOVE, 0, 0, 0, modifiers, V_MULTIWINDOW_WIN32_MOUSE_BUTTON_INVALID, x, y, 0, 0);
			return 0;
		}
		break;
	case WM_INPUT:
		{
			int raw_result = V_MULTIWINDOW_WIN32_RAW_INPUT_VALID_IGNORED;
			if (data
				&& GetPropW(hwnd, V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP)
					== (HANDLE)data
				&& (UINT)(wparam & 0xffu) == RIM_INPUT) {
				raw_result = v_multiwindow_win32_window_mouse_lock_active(data)
					? v_multiwindow_win32_emit_raw_mouse_event(hwnd, data,
						lparam)
					: V_MULTIWINDOW_WIN32_RAW_INPUT_ERROR;
			}
			if (raw_result == V_MULTIWINDOW_WIN32_RAW_INPUT_ERROR) {
				v_multiwindow_win32_window_raw_input_error(data);
			}
			return DefWindowProcW(hwnd, msg, wparam, lparam);
		}
	case WM_MOUSELEAVE:
		if (data) {
			if (GetPropW(hwnd, V_MULTIWINDOW_WIN32_MOUSE_LOCK_PROP)
				== (HANDLE)data) {
				v_multiwindow_win32_end_mouse_tracking(hwnd);
				return 0;
			}
			int x = 0;
			int y = 0;
			v_multiwindow_win32_end_mouse_tracking(hwnd);
			v_multiwindow_win32_cursor_client_pos(hwnd, &x, &y);
			uint64_t sequence = v_multiwindow_win32_next_event_sequence();
			v_multiwindow_win32_window_input_event(data, sequence, V_MULTIWINDOW_WIN32_INPUT_MOUSE_LEAVE, 0, 0, 0, v_multiwindow_win32_modifiers(), V_MULTIWINDOW_WIN32_MOUSE_BUTTON_INVALID, x, y, 0, 0);
			return 0;
		}
		break;
	case WM_LBUTTONDOWN:
	case WM_RBUTTONDOWN:
	case WM_MBUTTONDOWN:
		if (data) {
			int button = msg == WM_LBUTTONDOWN ? V_MULTIWINDOW_WIN32_MOUSE_BUTTON_LEFT : (msg == WM_RBUTTONDOWN ? V_MULTIWINDOW_WIN32_MOUSE_BUTTON_RIGHT : V_MULTIWINDOW_WIN32_MOUSE_BUTTON_MIDDLE);
			int x = v_multiwindow_win32_lparam_x(lparam);
			int y = v_multiwindow_win32_lparam_y(lparam);
			SetFocus(hwnd);
			SetCapture(hwnd);
			uint64_t sequence = v_multiwindow_win32_next_event_sequence();
			v_multiwindow_win32_window_input_event(data, sequence, V_MULTIWINDOW_WIN32_INPUT_MOUSE_DOWN, 0, 0, 0, v_multiwindow_win32_modifiers(), button, x, y, 0, 0);
			return 0;
		}
		break;
	case WM_LBUTTONUP:
	case WM_RBUTTONUP:
	case WM_MBUTTONUP:
		if (data) {
			int button = msg == WM_LBUTTONUP ? V_MULTIWINDOW_WIN32_MOUSE_BUTTON_LEFT : (msg == WM_RBUTTONUP ? V_MULTIWINDOW_WIN32_MOUSE_BUTTON_RIGHT : V_MULTIWINDOW_WIN32_MOUSE_BUTTON_MIDDLE);
			int x = v_multiwindow_win32_lparam_x(lparam);
			int y = v_multiwindow_win32_lparam_y(lparam);
			uint64_t sequence = v_multiwindow_win32_next_event_sequence();
			v_multiwindow_win32_window_input_event(data, sequence, V_MULTIWINDOW_WIN32_INPUT_MOUSE_UP, 0, 0, 0, v_multiwindow_win32_modifiers(), button, x, y, 0, 0);
			if ((wparam & (MK_LBUTTON | MK_RBUTTON | MK_MBUTTON)) == 0) {
				ReleaseCapture();
			}
			return 0;
		}
		break;
	case WM_MOUSEWHEEL:
	case WM_MOUSEHWHEEL:
		if (data) {
			int x = 0;
			int y = 0;
			int delta = v_multiwindow_win32_wheel_delta(wparam);
			v_multiwindow_win32_client_pos_from_lparam(hwnd, lparam, &x, &y);
			uint64_t sequence = v_multiwindow_win32_next_event_sequence();
			v_multiwindow_win32_window_input_event(data, sequence, V_MULTIWINDOW_WIN32_INPUT_MOUSE_SCROLL, 0, 0, 0, v_multiwindow_win32_modifiers(), V_MULTIWINDOW_WIN32_MOUSE_BUTTON_INVALID, x, y, msg == WM_MOUSEHWHEEL ? delta : 0, msg == WM_MOUSEWHEEL ? delta : 0);
			return 0;
		}
		break;
	case WM_KEYDOWN:
		if (data) {
			int key_code = v_multiwindow_win32_key_code(wparam, lparam);
			if (key_code != 0) {
				uint64_t sequence = v_multiwindow_win32_next_event_sequence();
				uint32_t modifiers = v_multiwindow_win32_modifiers();
				v_multiwindow_win32_window_input_event(data, sequence, V_MULTIWINDOW_WIN32_INPUT_KEY_DOWN, key_code, 0, v_multiwindow_win32_key_repeat(lparam), modifiers, V_MULTIWINDOW_WIN32_MOUSE_BUTTON_INVALID, 0, 0, 0, 0);
				if (key_code == 86 && modifiers == 2) {
					v_multiwindow_win32_window_input_event(data, sequence, V_MULTIWINDOW_WIN32_INPUT_CLIPBOARD_PASTED, 0, 0, 0, modifiers, V_MULTIWINDOW_WIN32_MOUSE_BUTTON_INVALID, 0, 0, 0, 0);
				}
				return 0;
			}
		}
		break;
	case WM_SYSKEYDOWN:
		if (data) {
			int key_code = v_multiwindow_win32_key_code(wparam, lparam);
			if (key_code != 0) {
				uint64_t sequence = v_multiwindow_win32_next_event_sequence();
				v_multiwindow_win32_window_input_event(data, sequence, V_MULTIWINDOW_WIN32_INPUT_KEY_DOWN, key_code, 0, v_multiwindow_win32_key_repeat(lparam), v_multiwindow_win32_modifiers(), V_MULTIWINDOW_WIN32_MOUSE_BUTTON_INVALID, 0, 0, 0, 0);
			}
		}
		break;
	case WM_KEYUP:
		if (data) {
			int key_code = v_multiwindow_win32_key_code(wparam, lparam);
			if (key_code != 0) {
				uint64_t sequence = v_multiwindow_win32_next_event_sequence();
				v_multiwindow_win32_window_input_event(data, sequence, V_MULTIWINDOW_WIN32_INPUT_KEY_UP, key_code, 0, 0, v_multiwindow_win32_modifiers(), V_MULTIWINDOW_WIN32_MOUSE_BUTTON_INVALID, 0, 0, 0, 0);
				return 0;
			}
		}
		break;
	case WM_SYSKEYUP:
		if (data) {
			int key_code = v_multiwindow_win32_key_code(wparam, lparam);
			if (key_code != 0) {
				uint64_t sequence = v_multiwindow_win32_next_event_sequence();
				v_multiwindow_win32_window_input_event(data, sequence, V_MULTIWINDOW_WIN32_INPUT_KEY_UP, key_code, 0, 0, v_multiwindow_win32_modifiers(), V_MULTIWINDOW_WIN32_MOUSE_BUTTON_INVALID, 0, 0, 0, 0);
			}
		}
		break;
	case WM_CHAR:
		if (data && v_multiwindow_win32_is_char_code(wparam)) {
			uint64_t sequence = v_multiwindow_win32_next_event_sequence();
			v_multiwindow_win32_window_input_event(data, sequence, V_MULTIWINDOW_WIN32_INPUT_CHAR, 0, (uint32_t)wparam, v_multiwindow_win32_key_repeat(lparam), v_multiwindow_win32_modifiers(), V_MULTIWINDOW_WIN32_MOUSE_BUTTON_INVALID, 0, 0, 0, 0);
			return 0;
		}
		break;
	case WM_DROPFILES:
		if (data) {
			v_multiwindow_win32_emit_drop_files(hwnd, data, (HDROP)wparam);
			return 0;
		}
		break;
	case WM_TOUCH:
		if (v_multiwindow_win32_emit_touch_event(hwnd, data, wparam, lparam)) {
			return 0;
		}
		break;
	default:
		break;
	}
	return DefWindowProcW(hwnd, msg, wparam, lparam);
}

static inline DWORD v_multiwindow_win32_window_style(int resizable, int borderless, int fullscreen) {
	if (borderless || fullscreen) {
		return WS_POPUP | WS_CLIPSIBLINGS | WS_CLIPCHILDREN;
	}
	DWORD style = WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX | WS_CLIPSIBLINGS | WS_CLIPCHILDREN;
	if (resizable) {
		style |= WS_SIZEBOX | WS_MAXIMIZEBOX;
	}
	return style;
}

static inline DWORD v_multiwindow_win32_window_ex_style(int borderless, int fullscreen) {
	if (borderless || fullscreen) {
		return WS_EX_APPWINDOW;
	}
	return WS_EX_APPWINDOW | WS_EX_WINDOWEDGE;
}

static inline int v_multiwindow_win32_register_class(void) {
	WNDCLASSEXW wndclass;
	ZeroMemory(&wndclass, sizeof(wndclass));
	wndclass.cbSize = sizeof(wndclass);
	wndclass.style = CS_HREDRAW | CS_VREDRAW;
	wndclass.lpfnWndProc = v_multiwindow_win32_wnd_proc;
	wndclass.hInstance = GetModuleHandleW(NULL);
	wndclass.hCursor = LoadCursorW(NULL, IDC_ARROW);
	wndclass.hIcon = LoadIconW(NULL, IDI_APPLICATION);
	wndclass.lpszClassName = v_multiwindow_win32_class_name;
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
	ATOM registration = RegisterClassExW(&wndclass);
	int registered = registration != 0;
	if (!registered) {
		registered = GetLastError() == ERROR_CLASS_ALREADY_EXISTS;
	}
	if (registered) {
		v_multiwindow_test_win32_oracle_record(V_MULTIWINDOW_TEST_WIN32_PROCESS_COMMIT,
			(uint64_t)(uintptr_t)wndclass.hInstance, UINT64_C(0), NULL);
	}
	return registered;
#else
	if (RegisterClassExW(&wndclass) != 0) {
		return 1;
	}
	return GetLastError() == ERROR_CLASS_ALREADY_EXISTS;
#endif
}

static inline int v_multiwindow_win32_owner_matches(void *hwnd_ptr, void *owner_ptr) {
	HWND hwnd = (HWND)hwnd_ptr;
	HWND owner = (HWND)owner_ptr;
	return hwnd && owner && IsWindow(hwnd) && IsWindow(owner)
		&& GetWindow(hwnd, GW_OWNER) == owner;
}

static inline int v_multiwindow_win32_is_window_enabled(void *hwnd_ptr) {
	HWND hwnd = (HWND)hwnd_ptr;
	return hwnd && IsWindow(hwnd) && IsWindowEnabled(hwnd);
}

#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
static HWND v_multiwindow_win32_test_modal_trace_owner;
static HWND v_multiwindow_win32_test_modal_trace_window;
static uint64_t v_multiwindow_win32_test_modal_trace_sequence;
static uint64_t v_multiwindow_win32_test_modal_owner_disable_sequence;
static uint64_t v_multiwindow_win32_test_modal_owner_enable_sequence;
static uint64_t v_multiwindow_win32_test_modal_show_sequence;
static uint64_t v_multiwindow_win32_test_modal_destroy_sequence;
static uint64_t v_multiwindow_win32_test_modal_owner_destroy_sequence;
static int v_multiwindow_win32_test_modal_owner_disable_count;
static int v_multiwindow_win32_test_modal_owner_enable_count;
static int v_multiwindow_win32_test_modal_show_count;
static int v_multiwindow_win32_test_modal_destroy_count;
static int v_multiwindow_win32_test_modal_owner_destroy_count;
static int v_multiwindow_win32_test_modal_destroy_attempt_count;
static int v_multiwindow_win32_test_modal_owner_destroy_attempt_count;
static int v_multiwindow_win32_test_modal_fail_enable;
static int v_multiwindow_win32_test_modal_enable_failures_remaining;
static int v_multiwindow_win32_test_modal_show_failures_remaining;
static int v_multiwindow_win32_test_modal_destroy_failures_remaining;

static inline void v_multiwindow_win32_test_modal_trace_reset(
	void *owner_ptr, void *window_ptr) {
	v_multiwindow_win32_test_modal_trace_owner = (HWND)owner_ptr;
	v_multiwindow_win32_test_modal_trace_window = (HWND)window_ptr;
	v_multiwindow_win32_test_modal_trace_sequence = 0;
	v_multiwindow_win32_test_modal_owner_disable_sequence = 0;
	v_multiwindow_win32_test_modal_owner_enable_sequence = 0;
	v_multiwindow_win32_test_modal_show_sequence = 0;
	v_multiwindow_win32_test_modal_destroy_sequence = 0;
	v_multiwindow_win32_test_modal_owner_destroy_sequence = 0;
	v_multiwindow_win32_test_modal_owner_disable_count = 0;
	v_multiwindow_win32_test_modal_owner_enable_count = 0;
	v_multiwindow_win32_test_modal_show_count = 0;
	v_multiwindow_win32_test_modal_destroy_count = 0;
	v_multiwindow_win32_test_modal_owner_destroy_count = 0;
	v_multiwindow_win32_test_modal_destroy_attempt_count = 0;
	v_multiwindow_win32_test_modal_owner_destroy_attempt_count = 0;
}

static inline void v_multiwindow_win32_test_modal_set_enable_failure(int fail) {
	v_multiwindow_win32_test_modal_fail_enable = fail != 0;
}

static inline void v_multiwindow_win32_test_modal_set_enable_failures(
		int count) {
	v_multiwindow_win32_test_modal_enable_failures_remaining =
		count > 0 ? count : 0;
}

static inline void v_multiwindow_win32_test_modal_set_show_created_failures(
		int count) {
	v_multiwindow_win32_test_modal_show_failures_remaining =
		count > 0 ? count : 0;
}

static inline void v_multiwindow_win32_test_modal_set_destroy_failures(
		int count) {
	v_multiwindow_win32_test_modal_destroy_failures_remaining =
		count > 0 ? count : 0;
}

static inline void *v_multiwindow_win32_test_modal_trace_window_value(void) {
	return (void *)v_multiwindow_win32_test_modal_trace_window;
}

static inline int v_multiwindow_win32_test_modal_owner_disable_count_value(void) {
	return v_multiwindow_win32_test_modal_owner_disable_count;
}

static inline int v_multiwindow_win32_test_modal_owner_enable_count_value(void) {
	return v_multiwindow_win32_test_modal_owner_enable_count;
}

static inline int v_multiwindow_win32_test_modal_show_count_value(void) {
	return v_multiwindow_win32_test_modal_show_count;
}

static inline int v_multiwindow_win32_test_modal_destroy_count_value(void) {
	return v_multiwindow_win32_test_modal_destroy_count;
}

static inline int v_multiwindow_win32_test_modal_owner_destroy_count_value(void) {
	return v_multiwindow_win32_test_modal_owner_destroy_count;
}

static inline int v_multiwindow_win32_test_modal_destroy_attempt_count_value(void) {
	return v_multiwindow_win32_test_modal_destroy_attempt_count;
}

static inline int v_multiwindow_win32_test_modal_owner_destroy_attempt_count_value(void) {
	return v_multiwindow_win32_test_modal_owner_destroy_attempt_count;
}

static inline uint64_t v_multiwindow_win32_test_modal_owner_disable_sequence_value(void) {
	return v_multiwindow_win32_test_modal_owner_disable_sequence;
}

static inline uint64_t v_multiwindow_win32_test_modal_owner_enable_sequence_value(void) {
	return v_multiwindow_win32_test_modal_owner_enable_sequence;
}

static inline uint64_t v_multiwindow_win32_test_modal_show_sequence_value(void) {
	return v_multiwindow_win32_test_modal_show_sequence;
}

static inline uint64_t v_multiwindow_win32_test_modal_destroy_sequence_value(void) {
	return v_multiwindow_win32_test_modal_destroy_sequence;
}

static inline uint64_t v_multiwindow_win32_test_modal_owner_destroy_sequence_value(void) {
	return v_multiwindow_win32_test_modal_owner_destroy_sequence;
}

static inline void v_multiwindow_win32_test_modal_record_enabled(
	HWND hwnd, int before, int target) {
	if (hwnd != v_multiwindow_win32_test_modal_trace_owner || before == target) {
		return;
	}
	uint64_t sequence = ++v_multiwindow_win32_test_modal_trace_sequence;
	if (target) {
		v_multiwindow_win32_test_modal_owner_enable_count++;
		v_multiwindow_win32_test_modal_owner_enable_sequence = sequence;
	} else {
		v_multiwindow_win32_test_modal_owner_disable_count++;
		v_multiwindow_win32_test_modal_owner_disable_sequence = sequence;
	}
}

static inline void v_multiwindow_win32_test_modal_record_show(HWND hwnd) {
	if (v_multiwindow_win32_test_modal_trace_window == NULL) {
		v_multiwindow_win32_test_modal_trace_window = hwnd;
	}
	if (hwnd != v_multiwindow_win32_test_modal_trace_window) {
		return;
	}
	v_multiwindow_win32_test_modal_show_count++;
	v_multiwindow_win32_test_modal_show_sequence =
		++v_multiwindow_win32_test_modal_trace_sequence;
}

static inline void v_multiwindow_win32_test_modal_record_destroy(HWND hwnd) {
	if (hwnd != v_multiwindow_win32_test_modal_trace_window
			&& hwnd != v_multiwindow_win32_test_modal_trace_owner) {
		return;
	}
	uint64_t sequence = ++v_multiwindow_win32_test_modal_trace_sequence;
	if (hwnd == v_multiwindow_win32_test_modal_trace_window) {
		v_multiwindow_win32_test_modal_destroy_count++;
		v_multiwindow_win32_test_modal_destroy_sequence = sequence;
	}
	if (hwnd == v_multiwindow_win32_test_modal_trace_owner) {
		v_multiwindow_win32_test_modal_owner_destroy_count++;
		v_multiwindow_win32_test_modal_owner_destroy_sequence = sequence;
	}
}
#endif

static inline int v_multiwindow_win32_set_window_enabled(void *hwnd_ptr, int enabled) {
	HWND hwnd = (HWND)hwnd_ptr;
	if (!hwnd || !IsWindow(hwnd)) {
		return 0;
	}
	int target = enabled != 0;
	int before = IsWindowEnabled(hwnd) != 0;
	if (before != target) {
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
		if (target
				&& v_multiwindow_win32_test_modal_enable_failures_remaining > 0) {
			v_multiwindow_win32_test_modal_enable_failures_remaining--;
			return 0;
		}
		if (target && v_multiwindow_win32_test_modal_fail_enable) {
			return 0;
		}
#endif
		(void)EnableWindow(hwnd, target ? TRUE : FALSE);
	}
	int matched = (IsWindowEnabled(hwnd) != 0) == target;
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	if (matched) {
		v_multiwindow_win32_test_modal_record_enabled(hwnd, before, target);
	}
#endif
	return matched;
}

static inline VMultiwindowWin32BackendSetThreadDpiAwarenessContext
v_multiwindow_win32_resolve_set_thread_dpi_context(void) {
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	if (v_multiwindow_win32_test_dpi_context_mode
			== V_MULTIWINDOW_WIN32_TEST_DPI_CONTEXT_UNAVAILABLE) {
		return NULL;
	}
#endif
	HMODULE user32 = GetModuleHandleW(L"user32.dll");
	return user32
		? (VMultiwindowWin32BackendSetThreadDpiAwarenessContext)GetProcAddress(
			user32, "SetThreadDpiAwarenessContext")
		: NULL;
}

static inline HANDLE v_multiwindow_win32_try_per_monitor_v2_context(
		VMultiwindowWin32BackendSetThreadDpiAwarenessContext set_thread_dpi_context) {
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	v_multiwindow_win32_test_dpi_context_attempts++;
	if (v_multiwindow_win32_test_dpi_context_mode
			== V_MULTIWINDOW_WIN32_TEST_DPI_CONTEXT_REJECTED) {
		v_multiwindow_win32_test_dpi_context_fallbacks++;
		return NULL;
	}
#endif
	if (!set_thread_dpi_context) {
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
		v_multiwindow_win32_test_dpi_context_fallbacks++;
#endif
		return NULL;
	}
	HANDLE previous = set_thread_dpi_context((HANDLE)(INT_PTR)-4);
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	if (!previous) {
		v_multiwindow_win32_test_dpi_context_fallbacks++;
	}
#endif
	return previous;
}

static inline void v_multiwindow_win32_apply_creation_frame_bias_for_test(
		int *frame_width, int *frame_height) {
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	if (frame_width && v_multiwindow_win32_test_dpi_frame_bias_width > 0
			&& *frame_width <= INT_MAX
				- v_multiwindow_win32_test_dpi_frame_bias_width) {
		*frame_width += v_multiwindow_win32_test_dpi_frame_bias_width;
	}
	if (frame_height && v_multiwindow_win32_test_dpi_frame_bias_height > 0
			&& *frame_height <= INT_MAX
				- v_multiwindow_win32_test_dpi_frame_bias_height) {
		*frame_height += v_multiwindow_win32_test_dpi_frame_bias_height;
	}
#else
	(void)frame_width;
	(void)frame_height;
#endif
}

static inline void *v_multiwindow_win32_create_window(const wchar_t *title, int width, int height, int min_width, int min_height, int resizable, int high_dpi, int borderless, int fullscreen, int visible, void *owner_ptr, void *data) {
	DWORD style = v_multiwindow_win32_window_style(resizable, borderless, fullscreen);
	DWORD ex_style = v_multiwindow_win32_window_ex_style(borderless, fullscreen);
	HWND owner = (HWND)owner_ptr;
	if (owner && !IsWindow(owner)) {
		return NULL;
	}
	int client_width = v_multiwindow_win32_max_int(width, min_width);
	int client_height = v_multiwindow_win32_max_int(height, min_height);
	VMultiwindowWin32BackendSetThreadDpiAwarenessContext set_thread_dpi_context =
		NULL;
	HANDLE previous_dpi_context = NULL;
	if (high_dpi) {
		set_thread_dpi_context =
			v_multiwindow_win32_resolve_set_thread_dpi_context();
		previous_dpi_context = v_multiwindow_win32_try_per_monitor_v2_context(
			set_thread_dpi_context);
	}
	int frame_width = client_width;
	int frame_height = client_height;
	if (!v_multiwindow_win32_adjusted_size_for_window(owner, client_width,
			client_height, style, ex_style, &frame_width, &frame_height)) {
		if (previous_dpi_context) {
			(void)set_thread_dpi_context(previous_dpi_context);
		}
		return NULL;
	}
	v_multiwindow_win32_apply_creation_frame_bias_for_test(&frame_width,
		&frame_height);
	HWND hwnd = CreateWindowExW(
		ex_style,
		v_multiwindow_win32_class_name,
		title,
		style,
		CW_USEDEFAULT,
		CW_USEDEFAULT,
		frame_width,
		frame_height,
		owner,
		NULL,
		GetModuleHandleW(NULL),
		data);
	if (hwnd) {
		v_multiwindow_win32_set_hwnd_int_prop(hwnd,
			v_multiwindow_win32_min_width_prop, min_width);
		v_multiwindow_win32_set_hwnd_int_prop(hwnd,
			v_multiwindow_win32_min_height_prop, min_height);
		if (!v_multiwindow_win32_set_exact_client_size(hwnd, client_width,
				client_height, style, ex_style)) {
			DestroyWindow(hwnd);
			hwnd = NULL;
		}
	}
	if (previous_dpi_context
		&& !set_thread_dpi_context(previous_dpi_context)) {
		if (hwnd) {
			DestroyWindow(hwnd);
		}
		return NULL;
	}
	if (hwnd) {
		DragAcceptFiles(hwnd, TRUE);
		v_multiwindow_win32_register_touch_window(hwnd);
	}
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	if (hwnd && v_multiwindow_test_win32_windowposchanged_reason3_is_armed()) {
		RECT test_rect = {0, 0, 0, 0};
		int moved = GetWindowRect(hwnd, &test_rect)
			&& MoveWindow(hwnd, test_rect.left + 1, test_rect.top,
				test_rect.right - test_rect.left,
				test_rect.bottom - test_rect.top, FALSE);
		v_multiwindow_test_win32_windowposchanged_reason3_record_move_result(moved);
	}
#endif
	if (hwnd && visible) {
		ShowWindow(hwnd, fullscreen ? SW_MAXIMIZE : SW_SHOW);
		UpdateWindow(hwnd);
	}
	return (void *)hwnd;
}

static inline int v_multiwindow_win32_show_created_window(void *hwnd_ptr,
	int fullscreen) {
	HWND hwnd = (HWND)hwnd_ptr;
	if (!hwnd || !IsWindow(hwnd)) {
		return 0;
	}
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	if (v_multiwindow_win32_test_modal_trace_window == NULL) {
		v_multiwindow_win32_test_modal_trace_window = hwnd;
	}
	if (v_multiwindow_win32_test_modal_show_failures_remaining > 0) {
		v_multiwindow_win32_test_modal_show_failures_remaining--;
		return 0;
	}
#endif
	ShowWindow(hwnd, fullscreen ? SW_MAXIMIZE : SW_SHOW);
	UpdateWindow(hwnd);
	if (!IsWindowVisible(hwnd)) {
		return 0;
	}
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	v_multiwindow_win32_test_modal_record_show(hwnd);
#endif
	return 1;
}

static inline int v_multiwindow_win32_destroy_window(void *hwnd) {
	if (!hwnd) {
		return 1;
	}
	HWND native_window = (HWND)hwnd;
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	if (native_window == v_multiwindow_win32_test_modal_trace_window) {
		v_multiwindow_win32_test_modal_destroy_attempt_count++;
	}
	if (native_window == v_multiwindow_win32_test_modal_trace_owner) {
		v_multiwindow_win32_test_modal_owner_destroy_attempt_count++;
	}
	if (v_multiwindow_win32_test_modal_destroy_failures_remaining > 0) {
		v_multiwindow_win32_test_modal_destroy_failures_remaining--;
		return 0;
	}
#endif
	int destroyed = DestroyWindow(native_window) != 0;
#if defined(V_MULTIWINDOW_WIN32_SERVICE_TEST)
	if (destroyed) {
		v_multiwindow_win32_test_modal_record_destroy(native_window);
	}
#endif
	return destroyed;
}

static inline int v_multiwindow_win32_set_window_text(void *hwnd, const wchar_t *title) {
	return SetWindowTextW((HWND)hwnd, title) != 0;
}

static inline int v_multiwindow_win32_set_cursor_shape(void *hwnd, int shape) {
	if (!hwnd) {
		return 0;
	}
	v_multiwindow_win32_set_hwnd_int_prop((HWND)hwnd, v_multiwindow_win32_cursor_shape_prop, shape);
	return v_multiwindow_win32_apply_cursor_shape((HWND)hwnd);
}

static inline int v_multiwindow_win32_set_client_size(void *hwnd, int width, int height, int min_width, int min_height, int resizable, int borderless, int fullscreen) {
	HWND native_window = (HWND)hwnd;
	DWORD style = v_multiwindow_win32_window_style(resizable, borderless, fullscreen);
	DWORD ex_style = v_multiwindow_win32_window_ex_style(borderless, fullscreen);
	int client_width = v_multiwindow_win32_max_int(width, min_width);
	int client_height = v_multiwindow_win32_max_int(height, min_height);
	return v_multiwindow_win32_set_exact_client_size(native_window, client_width,
		client_height, style, ex_style);
}

static inline int v_multiwindow_win32_client_width(void *hwnd) {
	RECT rect = {0, 0, 0, 0};
	if (!GetClientRect((HWND)hwnd, &rect)) {
		return 0;
	}
	return rect.right - rect.left;
}

static inline int v_multiwindow_win32_client_height(void *hwnd) {
	RECT rect = {0, 0, 0, 0};
	if (!GetClientRect((HWND)hwnd, &rect)) {
		return 0;
	}
	return rect.bottom - rect.top;
}

static inline int v_multiwindow_win32_pump_messages(void) {
	MSG msg;
	int count = 0;
	while (PeekMessageW(&msg, NULL, 0, 0, PM_REMOVE)) {
		if (msg.message != WM_QUIT) {
			TranslateMessage(&msg);
			DispatchMessageW(&msg);
		}
		count++;
	}
	return count;
}

typedef BOOL (WINAPI *VMultiwindowLogicalToPhysicalPointForPerMonitorDPI)(HWND, LPPOINT);
typedef BOOL (WINAPI *VMultiwindowPhysicalToLogicalPointForPerMonitorDPI)(HWND, LPPOINT);
typedef UINT (WINAPI *VMultiwindowGetDpiForWindow)(HWND);

#if defined(V_MULTIWINDOW_WIN32_RENDER_METRICS_TEST)
typedef struct VMultiwindowWin32RenderMetricsTestState {
	int enabled;
	int client_width;
	int client_height;
	int visible;
	int minimized;
	UINT dpi;
	int conversion_mode;
} VMultiwindowWin32RenderMetricsTestState;

static VMultiwindowWin32RenderMetricsTestState v_multiwindow_win32_render_metrics_test_state = {0};

static BOOL WINAPI v_multiwindow_test_win32_conversion_failure(HWND hwnd, LPPOINT point) {
	(void)hwnd;
	(void)point;
	return FALSE;
}

static inline void v_multiwindow_test_win32_configure_render_fixture(int client_width, int client_height, int visible, int minimized, UINT dpi, int conversion_mode) {
	v_multiwindow_win32_render_metrics_test_state.enabled = 1;
	v_multiwindow_win32_render_metrics_test_state.client_width = client_width;
	v_multiwindow_win32_render_metrics_test_state.client_height = client_height;
	v_multiwindow_win32_render_metrics_test_state.visible = visible;
	v_multiwindow_win32_render_metrics_test_state.minimized = minimized;
	v_multiwindow_win32_render_metrics_test_state.dpi = dpi;
	v_multiwindow_win32_render_metrics_test_state.conversion_mode = conversion_mode;
}

static inline void v_multiwindow_test_win32_reset_render_fixture(void) {
	v_multiwindow_win32_render_metrics_test_state.enabled = 0;
	v_multiwindow_win32_render_metrics_test_state.client_width = 0;
	v_multiwindow_win32_render_metrics_test_state.client_height = 0;
	v_multiwindow_win32_render_metrics_test_state.visible = 0;
	v_multiwindow_win32_render_metrics_test_state.minimized = 0;
	v_multiwindow_win32_render_metrics_test_state.dpi = 0;
	v_multiwindow_win32_render_metrics_test_state.conversion_mode = 0;
}
#endif

static inline int v_multiwindow_win32_render_client_rect(HWND hwnd, RECT *rect) {
#if defined(V_MULTIWINDOW_WIN32_RENDER_METRICS_TEST)
	if (v_multiwindow_win32_render_metrics_test_state.enabled) {
		rect->left = 0;
		rect->top = 0;
		rect->right = v_multiwindow_win32_render_metrics_test_state.client_width;
		rect->bottom = v_multiwindow_win32_render_metrics_test_state.client_height;
		return 1;
	}
#endif
	return GetClientRect(hwnd, rect) != 0;
}

static inline int v_multiwindow_win32_render_window_visible(HWND hwnd) {
#if defined(V_MULTIWINDOW_WIN32_RENDER_METRICS_TEST)
	if (v_multiwindow_win32_render_metrics_test_state.enabled) {
		return v_multiwindow_win32_render_metrics_test_state.visible;
	}
#endif
	return IsWindowVisible(hwnd) ? 1 : 0;
}

static inline int v_multiwindow_win32_render_window_minimized(HWND hwnd) {
#if defined(V_MULTIWINDOW_WIN32_RENDER_METRICS_TEST)
	if (v_multiwindow_win32_render_metrics_test_state.enabled) {
		return v_multiwindow_win32_render_metrics_test_state.minimized;
	}
#endif
	return IsIconic(hwnd) ? 1 : 0;
}

static inline UINT v_multiwindow_win32_render_window_dpi(HWND hwnd) {
#if defined(V_MULTIWINDOW_WIN32_RENDER_METRICS_TEST)
	if (v_multiwindow_win32_render_metrics_test_state.enabled) {
		return v_multiwindow_win32_render_metrics_test_state.dpi;
	}
#endif
	HMODULE user32 = GetModuleHandleW(L"user32.dll");
	VMultiwindowGetDpiForWindow get_dpi_for_window = user32 ?
		(VMultiwindowGetDpiForWindow)GetProcAddress(user32, "GetDpiForWindow") : NULL;
	UINT dpi = get_dpi_for_window ? get_dpi_for_window(hwnd) : 0;
	if (dpi != 0) {
		return dpi;
	}

	// GetDeviceCaps(LOGPIXELSX) is the documented compatibility path for
	// systems predating GetDpiForWindow.
	HDC dc = GetDC(hwnd);
	if (dc) {
		int fallback_dpi = GetDeviceCaps(dc, LOGPIXELSX);
		ReleaseDC(hwnd, dc);
		if (fallback_dpi > 0) {
			return (UINT)fallback_dpi;
		}
	}
	return 96;
}

static inline void v_multiwindow_win32_resolve_conversion_apis(VMultiwindowLogicalToPhysicalPointForPerMonitorDPI *out_logical_to_physical, VMultiwindowPhysicalToLogicalPointForPerMonitorDPI *out_physical_to_logical) {
	*out_logical_to_physical = NULL;
	*out_physical_to_logical = NULL;
#if defined(V_MULTIWINDOW_WIN32_RENDER_METRICS_TEST)
	if (v_multiwindow_win32_render_metrics_test_state.enabled) {
		if (v_multiwindow_win32_render_metrics_test_state.conversion_mode == 2) {
			*out_logical_to_physical = v_multiwindow_test_win32_conversion_failure;
			*out_physical_to_logical = v_multiwindow_test_win32_conversion_failure;
		}
		return;
	}
#endif
	HMODULE user32 = GetModuleHandleW(L"user32.dll");
	if (!user32) {
		return;
	}
	*out_logical_to_physical =
		(VMultiwindowLogicalToPhysicalPointForPerMonitorDPI)GetProcAddress(user32, "LogicalToPhysicalPointForPerMonitorDPI");
	*out_physical_to_logical =
		(VMultiwindowPhysicalToLogicalPointForPerMonitorDPI)GetProcAddress(user32, "PhysicalToLogicalPointForPerMonitorDPI");
}

static inline int v_multiwindow_win32_render_snapshot(void *hwnd_ptr, int *out_visible, int *out_minimized, int *out_logical_width, int *out_logical_height, int *out_framebuffer_width, int *out_framebuffer_height, float *out_scale, int *out_conversion_available) {
	HWND hwnd = (HWND)hwnd_ptr;
	RECT rect = {0, 0, 0, 0};
	if (!hwnd || !v_multiwindow_win32_render_client_rect(hwnd, &rect)) {
		return 0;
	}
	int framebuffer_width = rect.right - rect.left;
	int framebuffer_height = rect.bottom - rect.top;
	UINT dpi = v_multiwindow_win32_render_window_dpi(hwnd);
	float scale = dpi > 0 ? (float)dpi / 96.0f : 1.0f;
	if (!(scale > 0.0f)) {
		scale = 1.0f;
	}
	int logical_width = framebuffer_width > 0 ?
		(int)floorf(((float)framebuffer_width / scale) + 0.5f) : framebuffer_width;
	int logical_height = framebuffer_height > 0 ?
		(int)floorf(((float)framebuffer_height / scale) + 0.5f) : framebuffer_height;
	VMultiwindowLogicalToPhysicalPointForPerMonitorDPI logical_to_physical = NULL;
	VMultiwindowPhysicalToLogicalPointForPerMonitorDPI physical_to_logical = NULL;
	v_multiwindow_win32_resolve_conversion_apis(&logical_to_physical, &physical_to_logical);
	int conversion_available = logical_to_physical && physical_to_logical;
	if (out_visible) *out_visible = v_multiwindow_win32_render_window_visible(hwnd);
	if (out_minimized) *out_minimized = v_multiwindow_win32_render_window_minimized(hwnd);
	if (out_logical_width) *out_logical_width = logical_width;
	if (out_logical_height) *out_logical_height = logical_height;
	if (out_framebuffer_width) *out_framebuffer_width = framebuffer_width;
	if (out_framebuffer_height) *out_framebuffer_height = framebuffer_height;
	if (out_scale) *out_scale = scale;
	if (out_conversion_available) *out_conversion_available = conversion_available;
	return 1;
}

static inline int v_multiwindow_win32_logical_to_pixel_rect(void *hwnd_ptr, float x, float y, float width, float height, int *out_x, int *out_y, int *out_width, int *out_height) {
	HWND hwnd = (HWND)hwnd_ptr;
	VMultiwindowLogicalToPhysicalPointForPerMonitorDPI convert = NULL;
	VMultiwindowPhysicalToLogicalPointForPerMonitorDPI reverse = NULL;
	v_multiwindow_win32_resolve_conversion_apis(&convert, &reverse);
	if (!hwnd || !convert || !reverse) return 0;
	POINT first = {(LONG)floorf(x), (LONG)floorf(y)};
	POINT last = {(LONG)ceilf(x + width), (LONG)ceilf(y + height)};
	if (!convert(hwnd, &first) || !convert(hwnd, &last)) return 0;
	if (out_x) *out_x = first.x;
	if (out_y) *out_y = first.y;
	if (out_width) *out_width = last.x - first.x;
	if (out_height) *out_height = last.y - first.y;
	return 1;
}

static inline int v_multiwindow_win32_pixel_to_logical_rect(void *hwnd_ptr, int x, int y, int width, int height, float *out_x, float *out_y, float *out_width, float *out_height) {
	HWND hwnd = (HWND)hwnd_ptr;
	VMultiwindowLogicalToPhysicalPointForPerMonitorDPI reverse = NULL;
	VMultiwindowPhysicalToLogicalPointForPerMonitorDPI convert = NULL;
	v_multiwindow_win32_resolve_conversion_apis(&reverse, &convert);
	if (!hwnd || !reverse || !convert) return 0;
	POINT first = {x, y};
	POINT last = {x + width, y + height};
	if (!convert(hwnd, &first) || !convert(hwnd, &last)) return 0;
	if (out_x) *out_x = (float)first.x;
	if (out_y) *out_y = (float)first.y;
	if (out_width) *out_width = (float)(last.x - first.x);
	if (out_height) *out_height = (float)(last.y - first.y);
	return 1;
}
