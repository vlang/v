#pragma once

#include <stdint.h>
#include <stdlib.h>
#include <windows.h>
#include <shellapi.h>

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
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_input_event(void *data, uint64_t sequence, int kind, int key_code, uint32_t char_code, int key_repeat, uint32_t modifiers, int mouse_button, int mouse_x, int mouse_y, int wheel_delta_x, int wheel_delta_y);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_drop_begin(void *data, uint64_t sequence, int mouse_x, int mouse_y, uint32_t modifiers);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_drop_file(void *data, uint64_t sequence, char *path);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_drop_end(void *data, uint64_t sequence);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_touch_event(void *data, uint64_t sequence, int kind, uint32_t modifiers, int count, uint64_t *ids, int *xs, int *ys, int *changed);
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

static const wchar_t *v_multiwindow_win32_class_name = L"V_x_multiwindow_win32";
static const wchar_t *v_multiwindow_win32_min_width_prop = L"V_x_multiwindow_min_width";
static const wchar_t *v_multiwindow_win32_min_height_prop = L"V_x_multiwindow_min_height";
static const wchar_t *v_multiwindow_win32_mouse_tracked_prop = L"V_x_multiwindow_mouse_tracked";
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
		return IDC_SIZEALL;
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

static inline int v_multiwindow_win32_adjusted_size(int width, int height, DWORD style, DWORD ex_style, int *out_width, int *out_height) {
	RECT rect = {0, 0, width, height};
	if (!AdjustWindowRectEx(&rect, style, FALSE, ex_style)) {
		return 0;
	}
	*out_width = rect.right - rect.left;
	*out_height = rect.bottom - rect.top;
	return 1;
}

static inline uint64_t v_multiwindow_win32_next_event_sequence(void) {
	static uint64_t sequence = 1;
	return sequence++;
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
				if (v_multiwindow_win32_adjusted_size(frame_width, frame_height, style, ex_style, &frame_width, &frame_height)) {
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
	case WM_MOUSEMOVE:
		if (data) {
			int x = v_multiwindow_win32_lparam_x(lparam);
			int y = v_multiwindow_win32_lparam_y(lparam);
			uint32_t modifiers = v_multiwindow_win32_modifiers();
			if (v_multiwindow_win32_begin_mouse_tracking(hwnd)) {
				uint64_t enter_sequence = v_multiwindow_win32_next_event_sequence();
				v_multiwindow_win32_window_input_event(data, enter_sequence, V_MULTIWINDOW_WIN32_INPUT_MOUSE_ENTER, 0, 0, 0, modifiers, V_MULTIWINDOW_WIN32_MOUSE_BUTTON_INVALID, x, y, 0, 0);
			}
			uint64_t sequence = v_multiwindow_win32_next_event_sequence();
			v_multiwindow_win32_window_input_event(data, sequence, V_MULTIWINDOW_WIN32_INPUT_MOUSE_MOVE, 0, 0, 0, modifiers, V_MULTIWINDOW_WIN32_MOUSE_BUTTON_INVALID, x, y, 0, 0);
			return 0;
		}
		break;
	case WM_MOUSELEAVE:
		if (data) {
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
	if (RegisterClassExW(&wndclass) != 0) {
		return 1;
	}
	return GetLastError() == ERROR_CLASS_ALREADY_EXISTS;
}

static inline void *v_multiwindow_win32_create_window(const wchar_t *title, int width, int height, int min_width, int min_height, int resizable, int borderless, int fullscreen, int visible, void *data) {
	DWORD style = v_multiwindow_win32_window_style(resizable, borderless, fullscreen);
	DWORD ex_style = v_multiwindow_win32_window_ex_style(borderless, fullscreen);
	int client_width = v_multiwindow_win32_max_int(width, min_width);
	int client_height = v_multiwindow_win32_max_int(height, min_height);
	int frame_width = client_width;
	int frame_height = client_height;
	if (!v_multiwindow_win32_adjusted_size(client_width, client_height, style, ex_style, &frame_width, &frame_height)) {
		return NULL;
	}
	HWND hwnd = CreateWindowExW(
		ex_style,
		v_multiwindow_win32_class_name,
		title,
		style,
		CW_USEDEFAULT,
		CW_USEDEFAULT,
		frame_width,
		frame_height,
		NULL,
		NULL,
		GetModuleHandleW(NULL),
		data);
	if (hwnd) {
		v_multiwindow_win32_set_hwnd_int_prop(hwnd, v_multiwindow_win32_min_width_prop, min_width);
		v_multiwindow_win32_set_hwnd_int_prop(hwnd, v_multiwindow_win32_min_height_prop, min_height);
		DragAcceptFiles(hwnd, TRUE);
		v_multiwindow_win32_register_touch_window(hwnd);
	}
	if (hwnd && visible) {
		ShowWindow(hwnd, fullscreen ? SW_MAXIMIZE : SW_SHOW);
		UpdateWindow(hwnd);
	}
	return (void *)hwnd;
}

static inline int v_multiwindow_win32_destroy_window(void *hwnd) {
	if (!hwnd) {
		return 1;
	}
	return DestroyWindow((HWND)hwnd) != 0;
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
	DWORD style = v_multiwindow_win32_window_style(resizable, borderless, fullscreen);
	DWORD ex_style = v_multiwindow_win32_window_ex_style(borderless, fullscreen);
	int client_width = v_multiwindow_win32_max_int(width, min_width);
	int client_height = v_multiwindow_win32_max_int(height, min_height);
	int frame_width = client_width;
	int frame_height = client_height;
	if (!v_multiwindow_win32_adjusted_size(client_width, client_height, style, ex_style, &frame_width, &frame_height)) {
		return 0;
	}
	return SetWindowPos((HWND)hwnd, NULL, 0, 0, frame_width, frame_height, SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE) != 0;
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
