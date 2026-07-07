#pragma once

#include <stdint.h>
#include <windows.h>

#ifdef __cplusplus
extern "C" {
#endif
#ifndef VV_EXP
#define V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE extern
#else
#define V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE VV_EXP
#endif
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_close_requested(void *data);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_destroyed(void *data);
V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE void v_multiwindow_win32_window_resized(void *data, int width, int height);
#undef V_MULTIWINDOW_WIN32_CALLBACK_LINKAGE
#ifdef __cplusplus
}
#endif

static const wchar_t *v_multiwindow_win32_class_name = L"V_x_multiwindow_win32";
static const wchar_t *v_multiwindow_win32_min_width_prop = L"V_x_multiwindow_min_width";
static const wchar_t *v_multiwindow_win32_min_height_prop = L"V_x_multiwindow_min_height";

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

static inline int v_multiwindow_win32_adjusted_size(int width, int height, DWORD style, DWORD ex_style, int *out_width, int *out_height) {
	RECT rect = {0, 0, width, height};
	if (!AdjustWindowRectEx(&rect, style, FALSE, ex_style)) {
		return 0;
	}
	*out_width = rect.right - rect.left;
	*out_height = rect.bottom - rect.top;
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
			v_multiwindow_win32_window_close_requested(data);
			return 0;
		}
		break;
	case WM_DESTROY:
		if (data) {
			v_multiwindow_win32_window_destroyed(data);
			SetWindowLongPtrW(hwnd, GWLP_USERDATA, 0);
			RemovePropW(hwnd, v_multiwindow_win32_min_width_prop);
			RemovePropW(hwnd, v_multiwindow_win32_min_height_prop);
			return 0;
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
		if (data && wparam != SIZE_MINIMIZED) {
			RECT rect = {0, 0, 0, 0};
			if (GetClientRect(hwnd, &rect)) {
				int width = rect.right - rect.left;
				int height = rect.bottom - rect.top;
				v_multiwindow_win32_window_resized(data, width, height);
			}
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
