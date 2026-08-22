#ifndef V_MULTIWINDOW_WIN32_RAW_INPUT_W5_A1_ORACLE_H
#define V_MULTIWINDOW_WIN32_RAW_INPUT_W5_A1_ORACLE_H

#if defined(_WIN32)

#include <windows.h>
#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifndef SIZE_MAX
#define SIZE_MAX ((size_t)-1)
#endif

#ifndef MOUSEEVENTF_MOVE_NOCOALESCE
#define MOUSEEVENTF_MOVE_NOCOALESCE 0x2000
#endif

#ifndef RIDEV_PAGEONLY
#define RIDEV_PAGEONLY 0x00000020
#endif

#define V_MULTIWINDOW_W5_A1_WH_MOUSE_LL 14
#define V_MULTIWINDOW_W5_A1_HC_ACTION 0
#define V_MULTIWINDOW_W5_A1_LLMHF_INJECTED 0x00000001u

enum VMultiwindowW5A1BaselineMask {
	V_MULTIWINDOW_W5_A1_BASELINE_INVENTORY = 1u << 0,
	V_MULTIWINDOW_W5_A1_BASELINE_MOUSE_FREE = 1u << 1,
	V_MULTIWINDOW_W5_A1_BASELINE_CLIP_VIRTUAL = 1u << 2,
	V_MULTIWINDOW_W5_A1_BASELINE_CAPTURE_FREE = 1u << 3,
	V_MULTIWINDOW_W5_A1_BASELINE_CURSOR = 1u << 4,
	V_MULTIWINDOW_W5_A1_BASELINE_ALL = 0x1fu
};

enum VMultiwindowW5A1LockedMask {
	V_MULTIWINDOW_W5_A1_LOCKED_INVENTORY = 1u << 0,
	V_MULTIWINDOW_W5_A1_LOCKED_RAW_TARGET = 1u << 1,
	V_MULTIWINDOW_W5_A1_LOCKED_CLIP_CLIENT = 1u << 2,
	V_MULTIWINDOW_W5_A1_LOCKED_ALL = 0x07u
};

typedef struct VMultiwindowW5A1LowLevelMouseInput {
	POINT point;
	DWORD mouse_data;
	DWORD flags;
	DWORD time;
	ULONG_PTR extra_info;
} VMultiwindowW5A1LowLevelMouseInput;

typedef LRESULT(CALLBACK *VMultiwindowW5A1LowLevelMouseProc)(
	int, WPARAM, LPARAM);
typedef UINT(WINAPI *VMultiwindowW5A1GetRegisteredDevices)(
	PRAWINPUTDEVICE, PUINT, UINT);
typedef UINT(WINAPI *VMultiwindowW5A1SendOneInput)(UINT, LPINPUT, int);
typedef HHOOK(WINAPI *VMultiwindowW5A1InstallHook)(
	int, VMultiwindowW5A1LowLevelMouseProc, HINSTANCE, DWORD);
typedef LRESULT(WINAPI *VMultiwindowW5A1CallNext)(
	HHOOK, int, WPARAM, LPARAM);
typedef BOOL(WINAPI *VMultiwindowW5A1RemoveHook)(HHOOK);

typedef struct VMultiwindowW5A1Apis {
	VMultiwindowW5A1GetRegisteredDevices get_registered_devices;
	VMultiwindowW5A1SendOneInput send_one_input;
	VMultiwindowW5A1InstallHook install_hook;
	VMultiwindowW5A1CallNext call_next;
	VMultiwindowW5A1RemoveHook remove_hook;
} VMultiwindowW5A1Apis;

typedef struct VMultiwindowW5A1Inventory {
	RAWINPUTDEVICE *items;
	UINT count;
} VMultiwindowW5A1Inventory;

typedef struct VMultiwindowW5A1Oracle {
	VMultiwindowW5A1Apis apis;
	VMultiwindowW5A1Inventory baseline;
	POINT cursor_before;
	POINT edge_screen;
	POINT edge_client;
	HHOOK hook;
	ULONG_PTR expected_tag;
	DWORD last_error;
	int baseline_valid;
	int product_attempted;
	int cursor_valid;
	int cursor_moved;
	int armed;
	int exact_hook_inputs;
	int unexpected_hook_inputs;
	int rescue_used;
} VMultiwindowW5A1Oracle;

static VMultiwindowW5A1Oracle *v_multiwindow_w5_a1_active_oracle = NULL;
static VMultiwindowW5A1CallNext v_multiwindow_w5_a1_call_next = NULL;

static int v_multiwindow_w5_a1_resolve(HMODULE module, const char *name,
	void *destination, size_t destination_size) {
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

static int v_multiwindow_w5_a1_resolve_apis(
	VMultiwindowW5A1Apis *apis) {
	HMODULE user32;
	VMultiwindowW5A1Apis resolved;
	if (!apis) {
		return 0;
	}
	memset(&resolved, 0, sizeof(resolved));
	user32 = GetModuleHandleW(L"user32.dll");
	if (!user32
		|| !v_multiwindow_w5_a1_resolve(user32,
			"GetRegisteredRawInputDevices", &resolved.get_registered_devices,
			sizeof(resolved.get_registered_devices))
		|| !v_multiwindow_w5_a1_resolve(user32, "SendInput",
			&resolved.send_one_input, sizeof(resolved.send_one_input))
		|| !v_multiwindow_w5_a1_resolve(user32, "SetWindowsHookExW",
			&resolved.install_hook, sizeof(resolved.install_hook))
		|| !v_multiwindow_w5_a1_resolve(user32, "CallNextHookEx",
			&resolved.call_next, sizeof(resolved.call_next))
		|| !v_multiwindow_w5_a1_resolve(user32, "UnhookWindowsHookEx",
			&resolved.remove_hook, sizeof(resolved.remove_hook))) {
		return 0;
	}
	*apis = resolved;
	return 1;
}

static void v_multiwindow_w5_a1_free_inventory(
	VMultiwindowW5A1Inventory *inventory) {
	if (!inventory) {
		return;
	}
	free(inventory->items);
	inventory->items = NULL;
	inventory->count = 0;
}

static int v_multiwindow_w5_a1_query_inventory(
	VMultiwindowW5A1Oracle *oracle,
	VMultiwindowW5A1Inventory *inventory) {
	UINT count = 0;
	UINT copied;
	UINT result;
	RAWINPUTDEVICE *items;
	if (!oracle || !oracle->apis.get_registered_devices || !inventory) {
		return -1;
	}
	memset(inventory, 0, sizeof(*inventory));
	SetLastError(ERROR_SUCCESS);
	result = oracle->apis.get_registered_devices(NULL, &count,
		sizeof(RAWINPUTDEVICE));
	if (result != 0) {
		oracle->last_error = GetLastError() ? GetLastError()
			: ERROR_INVALID_DATA;
		return -1;
	}
	if (count == 0) {
		return 1;
	}
	if ((size_t)count > SIZE_MAX / sizeof(RAWINPUTDEVICE)) {
		oracle->last_error = ERROR_INVALID_DATA;
		return -1;
	}
	items = (RAWINPUTDEVICE *)calloc((size_t)count,
		sizeof(RAWINPUTDEVICE));
	if (!items) {
		oracle->last_error = ERROR_NOT_ENOUGH_MEMORY;
		return -1;
	}
	copied = count;
	SetLastError(ERROR_SUCCESS);
	result = oracle->apis.get_registered_devices(items, &copied,
		sizeof(RAWINPUTDEVICE));
	if (result == (UINT)-1 || result != copied || copied > count) {
		oracle->last_error = GetLastError() ? GetLastError()
			: ERROR_INVALID_DATA;
		free(items);
		return -1;
	}
	inventory->items = items;
	inventory->count = copied;
	return 1;
}

static int v_multiwindow_w5_a1_registration_equal(
	const RAWINPUTDEVICE *left, const RAWINPUTDEVICE *right) {
	return left && right && left->usUsagePage == right->usUsagePage
		&& left->usUsage == right->usUsage
		&& left->dwFlags == right->dwFlags
		&& left->hwndTarget == right->hwndTarget;
}

static int v_multiwindow_w5_a1_inventory_equal(
	VMultiwindowW5A1Oracle *oracle,
	const VMultiwindowW5A1Inventory *left,
	const VMultiwindowW5A1Inventory *right) {
	unsigned char *matched;
	UINT left_index;
	if (!oracle || !left || !right || left->count != right->count) {
		return 0;
	}
	if (left->count == 0) {
		return 1;
	}
	if ((size_t)right->count > SIZE_MAX / sizeof(unsigned char)) {
		oracle->last_error = ERROR_INVALID_DATA;
		return -1;
	}
	matched = (unsigned char *)calloc((size_t)right->count,
		sizeof(unsigned char));
	if (!matched) {
		oracle->last_error = ERROR_NOT_ENOUGH_MEMORY;
		return -1;
	}
	for (left_index = 0; left_index < left->count; left_index++) {
		UINT right_index;
		int found = 0;
		for (right_index = 0; right_index < right->count; right_index++) {
			if (!matched[right_index]
				&& v_multiwindow_w5_a1_registration_equal(
					&left->items[left_index], &right->items[right_index])) {
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

static int v_multiwindow_w5_a1_forbidden_baseline_device(
	const RAWINPUTDEVICE *item) {
	if (!item || item->usUsagePage != 0x01) {
		return 0;
	}
	return item->usUsage == 0x02
		|| (item->usUsage == 0
			&& (item->dwFlags & RIDEV_PAGEONLY) != 0);
}

static int v_multiwindow_w5_a1_baseline_is_mouse_free(
	const VMultiwindowW5A1Inventory *inventory) {
	UINT index;
	if (!inventory) {
		return 0;
	}
	for (index = 0; index < inventory->count; index++) {
		if (v_multiwindow_w5_a1_forbidden_baseline_device(
			&inventory->items[index])) {
			return 0;
		}
	}
	return 1;
}

static int v_multiwindow_w5_a1_inventory_has_exact_target(
	VMultiwindowW5A1Oracle *oracle,
	const VMultiwindowW5A1Inventory *current, HWND target) {
	VMultiwindowW5A1Inventory remainder;
	UINT source_index;
	UINT remainder_index = 0;
	int exact_count = 0;
	int equal;
	if (!oracle || !current || !target || !oracle->baseline_valid) {
		return 0;
	}
	if (oracle->baseline.count == UINT_MAX) {
		oracle->last_error = ERROR_INVALID_DATA;
		return -1;
	}
	if (current->count != oracle->baseline.count + 1) {
		return 0;
	}
	memset(&remainder, 0, sizeof(remainder));
	if (oracle->baseline.count > 0) {
		if ((size_t)oracle->baseline.count
			> SIZE_MAX / sizeof(RAWINPUTDEVICE)) {
			oracle->last_error = ERROR_INVALID_DATA;
			return -1;
		}
		remainder.items = (RAWINPUTDEVICE *)calloc(
			(size_t)oracle->baseline.count, sizeof(RAWINPUTDEVICE));
		if (!remainder.items) {
			oracle->last_error = ERROR_NOT_ENOUGH_MEMORY;
			return -1;
		}
	}
	remainder.count = oracle->baseline.count;
	for (source_index = 0; source_index < current->count; source_index++) {
		const RAWINPUTDEVICE *item = &current->items[source_index];
		if (item->usUsagePage == 0x01 && item->usUsage == 0x02
			&& item->dwFlags == 0 && item->hwndTarget == target) {
			exact_count++;
			continue;
		}
		if (remainder_index >= remainder.count) {
			v_multiwindow_w5_a1_free_inventory(&remainder);
			return 0;
		}
		remainder.items[remainder_index++] = *item;
	}
	equal = exact_count == 1 && remainder_index == remainder.count
		? v_multiwindow_w5_a1_inventory_equal(oracle,
			&oracle->baseline, &remainder)
		: 0;
	v_multiwindow_w5_a1_free_inventory(&remainder);
	return equal;
}

static RECT v_multiwindow_w5_a1_virtual_screen(void) {
	RECT screen;
	screen.left = GetSystemMetrics(SM_XVIRTUALSCREEN);
	screen.top = GetSystemMetrics(SM_YVIRTUALSCREEN);
	screen.right = screen.left + GetSystemMetrics(SM_CXVIRTUALSCREEN);
	screen.bottom = screen.top + GetSystemMetrics(SM_CYVIRTUALSCREEN);
	return screen;
}

static int v_multiwindow_w5_a1_rect_equal(
	const RECT *left, const RECT *right) {
	return left && right && left->left == right->left
		&& left->top == right->top && left->right == right->right
		&& left->bottom == right->bottom;
}

static int v_multiwindow_w5_a1_client_screen_rect(
	VMultiwindowW5A1Oracle *oracle, HWND target, RECT *out_rect) {
	RECT client;
	POINT points[2];
	int mapped;
	if (!oracle || !target || !out_rect || !IsWindow(target)
		|| !GetClientRect(target, &client)
		|| client.right <= client.left || client.bottom <= client.top) {
		if (oracle) {
			oracle->last_error = GetLastError() ? GetLastError()
				: ERROR_INVALID_WINDOW_HANDLE;
		}
		return -1;
	}
	points[0].x = client.left;
	points[0].y = client.top;
	points[1].x = client.right;
	points[1].y = client.bottom;
	SetLastError(ERROR_SUCCESS);
	mapped = MapWindowPoints(target, NULL, points, 2);
	if (mapped == 0 && GetLastError() != ERROR_SUCCESS) {
		oracle->last_error = GetLastError();
		return -1;
	}
	out_rect->left = points[0].x;
	out_rect->top = points[0].y;
	out_rect->right = points[1].x;
	out_rect->bottom = points[1].y;
	return 1;
}

static LRESULT CALLBACK v_multiwindow_w5_a1_mouse_hook(int code,
	WPARAM wparam, LPARAM lparam) {
	VMultiwindowW5A1Oracle *oracle = v_multiwindow_w5_a1_active_oracle;
	HHOOK hook = oracle ? oracle->hook : NULL;
	if (oracle && oracle->armed && code >= 0) {
		const VMultiwindowW5A1LowLevelMouseInput *mouse =
			(const VMultiwindowW5A1LowLevelMouseInput *)lparam;
		if (code != V_MULTIWINDOW_W5_A1_HC_ACTION
			|| wparam != (WPARAM)WM_MOUSEMOVE || !mouse
			|| oracle->exact_hook_inputs != 0
			|| (mouse->flags & V_MULTIWINDOW_W5_A1_LLMHF_INJECTED) == 0
			|| mouse->extra_info != oracle->expected_tag) {
			oracle->unexpected_hook_inputs++;
		} else {
			oracle->exact_hook_inputs = 1;
		}
	}
	return v_multiwindow_w5_a1_call_next(hook, code, wparam, lparam);
}

static inline void *v_multiwindow_test_win32_raw_input_w5_a1_new(
	uint32_t *out_error) {
	VMultiwindowW5A1Oracle *oracle =
		(VMultiwindowW5A1Oracle *)calloc(1, sizeof(VMultiwindowW5A1Oracle));
	if (out_error) {
		*out_error = 0;
	}
	if (!oracle) {
		if (out_error) {
			*out_error = ERROR_NOT_ENOUGH_MEMORY;
		}
		return NULL;
	}
	if (!v_multiwindow_w5_a1_resolve_apis(&oracle->apis)) {
		if (out_error) {
			*out_error = ERROR_PROC_NOT_FOUND;
		}
		free(oracle);
		return NULL;
	}
	v_multiwindow_w5_a1_call_next = oracle->apis.call_next;
	return oracle;
}

static inline uint32_t
v_multiwindow_test_win32_raw_input_w5_a1_last_error(void *oracle_ptr) {
	VMultiwindowW5A1Oracle *oracle =
		(VMultiwindowW5A1Oracle *)oracle_ptr;
	return oracle ? (uint32_t)oracle->last_error : ERROR_INVALID_PARAMETER;
}

static inline int v_multiwindow_test_win32_raw_input_w5_a1_baseline(
	void *oracle_ptr, void *target_ptr, uint32_t *out_mask) {
	VMultiwindowW5A1Oracle *oracle =
		(VMultiwindowW5A1Oracle *)oracle_ptr;
	VMultiwindowW5A1Inventory inventory;
	RECT clip;
	RECT screen = v_multiwindow_w5_a1_virtual_screen();
	POINT cursor;
	uint32_t mask = 0;
	int query;
	UINT index;
	if (out_mask) {
		*out_mask = 0;
	}
	if (!oracle || !target_ptr || oracle->baseline_valid
		|| !IsWindow((HWND)target_ptr)) {
		if (oracle) {
			oracle->last_error = ERROR_INVALID_PARAMETER;
		}
		return -1;
	}
	memset(&inventory, 0, sizeof(inventory));
	query = v_multiwindow_w5_a1_query_inventory(oracle, &inventory);
	if (query < 0) {
		return -1;
	}
	mask |= V_MULTIWINDOW_W5_A1_BASELINE_INVENTORY;
	if (v_multiwindow_w5_a1_baseline_is_mouse_free(&inventory)) {
		mask |= V_MULTIWINDOW_W5_A1_BASELINE_MOUSE_FREE;
	}
	if (!GetClipCursor(&clip)) {
		oracle->last_error = GetLastError() ? GetLastError()
			: ERROR_INVALID_DATA;
		v_multiwindow_w5_a1_free_inventory(&inventory);
		return -1;
	}
	if (v_multiwindow_w5_a1_rect_equal(&clip, &screen)) {
		mask |= V_MULTIWINDOW_W5_A1_BASELINE_CLIP_VIRTUAL;
	}
	if (GetCapture() == NULL) {
		mask |= V_MULTIWINDOW_W5_A1_BASELINE_CAPTURE_FREE;
	}
	if (!GetCursorPos(&cursor)) {
		oracle->last_error = GetLastError() ? GetLastError()
			: ERROR_INVALID_DATA;
		v_multiwindow_w5_a1_free_inventory(&inventory);
		return -1;
	}
	mask |= V_MULTIWINDOW_W5_A1_BASELINE_CURSOR;
	if (out_mask) {
		*out_mask = mask;
	}
	if (mask != V_MULTIWINDOW_W5_A1_BASELINE_ALL) {
		v_multiwindow_w5_a1_free_inventory(&inventory);
		return 0;
	}
	for (index = 0; index < inventory.count; index++) {
		if (v_multiwindow_w5_a1_forbidden_baseline_device(
			&inventory.items[index])) {
			v_multiwindow_w5_a1_free_inventory(&inventory);
			return 0;
		}
	}
	oracle->baseline = inventory;
	oracle->baseline_valid = 1;
	oracle->cursor_before = cursor;
	oracle->cursor_valid = 1;
	return 1;
}

static inline int v_multiwindow_test_win32_raw_input_w5_a1_target_ready(
	void *oracle_ptr, void *target_ptr) {
	VMultiwindowW5A1Oracle *oracle =
		(VMultiwindowW5A1Oracle *)oracle_ptr;
	HWND target = (HWND)target_ptr;
	if (!oracle || !target) {
		return -1;
	}
	return IsWindow(target) && IsWindowVisible(target)
		&& GetForegroundWindow() == target && GetFocus() == target ? 1 : 0;
}

static inline int
v_multiwindow_test_win32_raw_input_w5_a1_mark_product_attempted(
	void *oracle_ptr) {
	VMultiwindowW5A1Oracle *oracle =
		(VMultiwindowW5A1Oracle *)oracle_ptr;
	if (!oracle || !oracle->baseline_valid || oracle->product_attempted
		|| oracle->hook || oracle->armed || oracle->cursor_moved
		|| v_multiwindow_w5_a1_active_oracle) {
		if (oracle) {
			oracle->last_error = ERROR_INVALID_DATA;
		}
		return -1;
	}
	oracle->product_attempted = 1;
	return 1;
}

static inline int v_multiwindow_test_win32_raw_input_w5_a1_locked(
	void *oracle_ptr, void *target_ptr, uint32_t *out_mask) {
	VMultiwindowW5A1Oracle *oracle =
		(VMultiwindowW5A1Oracle *)oracle_ptr;
	HWND target = (HWND)target_ptr;
	VMultiwindowW5A1Inventory current;
	RECT expected_clip;
	RECT actual_clip;
	uint32_t mask = 0;
	int query;
	int target_inventory;
	if (out_mask) {
		*out_mask = 0;
	}
	if (!oracle || !target || !oracle->baseline_valid) {
		return -1;
	}
	memset(&current, 0, sizeof(current));
	query = v_multiwindow_w5_a1_query_inventory(oracle, &current);
	if (query < 0) {
		return -1;
	}
	mask |= V_MULTIWINDOW_W5_A1_LOCKED_INVENTORY;
	target_inventory = v_multiwindow_w5_a1_inventory_has_exact_target(
		oracle, &current, target);
	if (target_inventory < 0) {
		v_multiwindow_w5_a1_free_inventory(&current);
		return -1;
	}
	if (target_inventory == 1) {
		mask |= V_MULTIWINDOW_W5_A1_LOCKED_RAW_TARGET;
	}
	v_multiwindow_w5_a1_free_inventory(&current);
	if (v_multiwindow_w5_a1_client_screen_rect(oracle, target,
		&expected_clip) < 0 || !GetClipCursor(&actual_clip)) {
		oracle->last_error = GetLastError() ? GetLastError()
			: ERROR_INVALID_DATA;
		return -1;
	}
	if (v_multiwindow_w5_a1_rect_equal(&expected_clip, &actual_clip)) {
		mask |= V_MULTIWINDOW_W5_A1_LOCKED_CLIP_CLIENT;
	}
	if (out_mask) {
		*out_mask = mask;
	}
	return mask == V_MULTIWINDOW_W5_A1_LOCKED_ALL ? 1 : 0;
}

static inline int v_multiwindow_test_win32_raw_input_w5_a1_position_edge(
	void *oracle_ptr, void *target_ptr, int *out_client_x,
	int *out_client_y) {
	VMultiwindowW5A1Oracle *oracle =
		(VMultiwindowW5A1Oracle *)oracle_ptr;
	HWND target = (HWND)target_ptr;
	RECT client;
	RECT clip;
	POINT current;
	POINT client_point;
	if (out_client_x) {
		*out_client_x = 0;
	}
	if (out_client_y) {
		*out_client_y = 0;
	}
	if (!oracle || !target || !oracle->cursor_valid
		|| v_multiwindow_w5_a1_client_screen_rect(oracle, target, &client) < 0
		|| !GetClipCursor(&clip)) {
		if (oracle && oracle->last_error == ERROR_SUCCESS) {
			oracle->last_error = GetLastError() ? GetLastError()
				: ERROR_INVALID_DATA;
		}
		return -1;
	}
	if (!v_multiwindow_w5_a1_rect_equal(&client, &clip)
		|| clip.right - clip.left < 4 || clip.bottom - clip.top < 4) {
		return 0;
	}
	oracle->edge_screen.x = clip.right - 1;
	oracle->edge_screen.y = clip.top + (clip.bottom - clip.top) / 2;
	if (!SetCursorPos(oracle->edge_screen.x, oracle->edge_screen.y)) {
		oracle->last_error = GetLastError() ? GetLastError()
			: ERROR_ACCESS_DENIED;
		return -1;
	}
	oracle->cursor_moved = 1;
	if (!GetCursorPos(&current)) {
		oracle->last_error = GetLastError() ? GetLastError()
			: ERROR_INVALID_DATA;
		return -1;
	}
	if (current.x != oracle->edge_screen.x
		|| current.y != oracle->edge_screen.y) {
		return 0;
	}
	client_point = current;
	SetLastError(ERROR_SUCCESS);
	if (ScreenToClient(target, &client_point) == 0
		&& GetLastError() != ERROR_SUCCESS) {
		oracle->last_error = GetLastError();
		return -1;
	}
	oracle->edge_client = client_point;
	if (out_client_x) {
		*out_client_x = client_point.x;
	}
	if (out_client_y) {
		*out_client_y = client_point.y;
	}
	return 1;
}

static inline int v_multiwindow_test_win32_raw_input_w5_a1_arm_send(
	void *oracle_ptr, uint32_t tag, int dx, int dy) {
	VMultiwindowW5A1Oracle *oracle =
		(VMultiwindowW5A1Oracle *)oracle_ptr;
	INPUT input;
	if (!oracle || !tag || dx <= 0 || dy != 0 || oracle->hook
		|| v_multiwindow_w5_a1_active_oracle) {
		if (oracle) {
			oracle->last_error = ERROR_INVALID_PARAMETER;
		}
		return -1;
	}
	oracle->expected_tag = (ULONG_PTR)tag;
	oracle->exact_hook_inputs = 0;
	oracle->unexpected_hook_inputs = 0;
	SetLastError(ERROR_SUCCESS);
	oracle->hook = oracle->apis.install_hook(V_MULTIWINDOW_W5_A1_WH_MOUSE_LL,
		v_multiwindow_w5_a1_mouse_hook, GetModuleHandleW(NULL), 0);
	if (!oracle->hook) {
		oracle->last_error = GetLastError() ? GetLastError()
			: ERROR_INVALID_HANDLE;
		return -1;
	}
	v_multiwindow_w5_a1_active_oracle = oracle;
	oracle->armed = 1;
	memset(&input, 0, sizeof(input));
	input.type = INPUT_MOUSE;
	input.mi.dx = dx;
	input.mi.dy = dy;
	input.mi.dwFlags = MOUSEEVENTF_MOVE | MOUSEEVENTF_MOVE_NOCOALESCE;
	input.mi.dwExtraInfo = oracle->expected_tag;
	SetLastError(ERROR_SUCCESS);
	if (oracle->apis.send_one_input(1, &input, sizeof(INPUT)) != 1) {
		oracle->last_error = GetLastError() ? GetLastError()
			: ERROR_ACCESS_DENIED;
		return -1;
	}
	return 1;
}

static inline int v_multiwindow_test_win32_raw_input_w5_a1_hook_result(
	void *oracle_ptr, int *out_exact, int *out_unexpected) {
	VMultiwindowW5A1Oracle *oracle =
		(VMultiwindowW5A1Oracle *)oracle_ptr;
	if (out_exact) {
		*out_exact = oracle ? oracle->exact_hook_inputs : 0;
	}
	if (out_unexpected) {
		*out_unexpected = oracle ? oracle->unexpected_hook_inputs : 0;
	}
	if (!oracle || !oracle->hook) {
		return -1;
	}
	if (oracle->unexpected_hook_inputs != 0
		|| oracle->exact_hook_inputs > 1) {
		return -1;
	}
	return oracle->exact_hook_inputs == 1 ? 1 : 0;
}

static inline int v_multiwindow_test_win32_raw_input_w5_a1_cursor_at_edge(
	void *oracle_ptr) {
	VMultiwindowW5A1Oracle *oracle =
		(VMultiwindowW5A1Oracle *)oracle_ptr;
	POINT current;
	if (!oracle || !oracle->cursor_moved || !GetCursorPos(&current)) {
		if (oracle) {
			oracle->last_error = GetLastError() ? GetLastError()
				: ERROR_INVALID_DATA;
		}
		return -1;
	}
	return current.x == oracle->edge_screen.x
		&& current.y == oracle->edge_screen.y ? 1 : 0;
}

static inline int v_multiwindow_test_win32_raw_input_w5_a1_unhook(
	void *oracle_ptr) {
	VMultiwindowW5A1Oracle *oracle =
		(VMultiwindowW5A1Oracle *)oracle_ptr;
	if (!oracle) {
		return -1;
	}
	oracle->armed = 0;
	if (!oracle->hook) {
		if (v_multiwindow_w5_a1_active_oracle == oracle) {
			v_multiwindow_w5_a1_active_oracle = NULL;
		}
		return 1;
	}
	SetLastError(ERROR_SUCCESS);
	if (!oracle->apis.remove_hook(oracle->hook)) {
		oracle->last_error = GetLastError() ? GetLastError()
			: ERROR_INVALID_HANDLE;
		return -1;
	}
	oracle->hook = NULL;
	if (v_multiwindow_w5_a1_active_oracle == oracle) {
		v_multiwindow_w5_a1_active_oracle = NULL;
	}
	return 1;
}

static inline int v_multiwindow_test_win32_raw_input_w5_a1_restore_cursor(
	void *oracle_ptr) {
	VMultiwindowW5A1Oracle *oracle =
		(VMultiwindowW5A1Oracle *)oracle_ptr;
	POINT current;
	if (!oracle || !oracle->cursor_valid || !oracle->product_attempted) {
		return -1;
	}
	if (!GetCursorPos(&current)) {
		oracle->last_error = GetLastError() ? GetLastError()
			: ERROR_INVALID_DATA;
		return -1;
	}
	if (current.x != oracle->cursor_before.x
		|| current.y != oracle->cursor_before.y) {
		if (!SetCursorPos(oracle->cursor_before.x, oracle->cursor_before.y)
			|| !GetCursorPos(&current)) {
			oracle->last_error = GetLastError() ? GetLastError()
				: ERROR_ACCESS_DENIED;
			return -1;
		}
	}
	if (current.x != oracle->cursor_before.x
		|| current.y != oracle->cursor_before.y) {
		return 0;
	}
	oracle->cursor_moved = 0;
	return 1;
}

static inline int v_multiwindow_test_win32_raw_input_w5_a1_unlocked(
	void *oracle_ptr, void *target_ptr, uint32_t *out_mask) {
	VMultiwindowW5A1Oracle *oracle =
		(VMultiwindowW5A1Oracle *)oracle_ptr;
	VMultiwindowW5A1Inventory current;
	RECT clip;
	RECT screen = v_multiwindow_w5_a1_virtual_screen();
	POINT cursor;
	uint32_t mask = 0;
	int query;
	int equal;
	if (out_mask) {
		*out_mask = 0;
	}
	if (!oracle || !target_ptr || !IsWindow((HWND)target_ptr)
		|| !oracle->baseline_valid) {
		return -1;
	}
	memset(&current, 0, sizeof(current));
	query = v_multiwindow_w5_a1_query_inventory(oracle, &current);
	if (query < 0) {
		return -1;
	}
	equal = v_multiwindow_w5_a1_inventory_equal(oracle,
		&oracle->baseline, &current);
	v_multiwindow_w5_a1_free_inventory(&current);
	if (equal < 0) {
		return -1;
	}
	if (equal == 1) {
		mask |= V_MULTIWINDOW_W5_A1_BASELINE_INVENTORY
			| V_MULTIWINDOW_W5_A1_BASELINE_MOUSE_FREE;
	}
	if (!GetClipCursor(&clip)) {
		oracle->last_error = GetLastError() ? GetLastError()
			: ERROR_INVALID_DATA;
		return -1;
	}
	if (v_multiwindow_w5_a1_rect_equal(&clip, &screen)) {
		mask |= V_MULTIWINDOW_W5_A1_BASELINE_CLIP_VIRTUAL;
	}
	if (GetCapture() == NULL) {
		mask |= V_MULTIWINDOW_W5_A1_BASELINE_CAPTURE_FREE;
	}
	if (!GetCursorPos(&cursor)) {
		oracle->last_error = GetLastError() ? GetLastError()
			: ERROR_INVALID_DATA;
		return -1;
	}
	if (!oracle->cursor_moved && cursor.x == oracle->cursor_before.x
		&& cursor.y == oracle->cursor_before.y) {
		mask |= V_MULTIWINDOW_W5_A1_BASELINE_CURSOR;
	}
	if (out_mask) {
		*out_mask = mask;
	}
	return mask == V_MULTIWINDOW_W5_A1_BASELINE_ALL ? 1 : 0;
}

static inline int
v_multiwindow_test_win32_raw_input_w5_a1_rescue_after_failure(
	void *oracle_ptr) {
	VMultiwindowW5A1Oracle *oracle =
		(VMultiwindowW5A1Oracle *)oracle_ptr;
	RECT clip;
	RECT screen = v_multiwindow_w5_a1_virtual_screen();
	int ok = 1;
	int need_unclip = 0;
	if (!oracle) {
		return -1;
	}
	if (!oracle->product_attempted) {
		return !oracle->hook && !oracle->armed && !oracle->cursor_moved
			&& !v_multiwindow_w5_a1_active_oracle ? 1 : -1;
	}
	if (!oracle->baseline_valid) {
		return -1;
	}
	oracle->rescue_used = 1;
	if (v_multiwindow_test_win32_raw_input_w5_a1_unhook(oracle) < 0) {
		ok = 0;
	}
	if (!GetClipCursor(&clip)) {
		need_unclip = 1;
	} else if (!v_multiwindow_w5_a1_rect_equal(&clip, &screen)) {
		need_unclip = 1;
	}
	if (need_unclip) {
		if (!ClipCursor(NULL) || !GetClipCursor(&clip)
			|| !v_multiwindow_w5_a1_rect_equal(&clip, &screen)) {
			ok = 0;
		}
	}
	if (oracle->cursor_valid
		&& v_multiwindow_test_win32_raw_input_w5_a1_restore_cursor(oracle) != 1) {
		ok = 0;
	}
	return ok ? 1 : -1;
}

static inline int v_multiwindow_test_win32_raw_input_w5_a1_rescue_used(
	void *oracle_ptr) {
	VMultiwindowW5A1Oracle *oracle =
		(VMultiwindowW5A1Oracle *)oracle_ptr;
	return oracle ? oracle->rescue_used : 1;
}

static inline int v_multiwindow_test_win32_raw_input_w5_a1_dispose(
	void *oracle_ptr) {
	VMultiwindowW5A1Oracle *oracle =
		(VMultiwindowW5A1Oracle *)oracle_ptr;
	if (!oracle || oracle->hook || oracle->armed || oracle->cursor_moved
		|| v_multiwindow_w5_a1_active_oracle == oracle) {
		return -1;
	}
	v_multiwindow_w5_a1_free_inventory(&oracle->baseline);
	free(oracle);
	return 1;
}

#endif

#endif
