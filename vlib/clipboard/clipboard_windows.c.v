module clipboard

import time

#include <windows.h>
#flag -lUser32
struct WndClassEx {
	cb_size         u32
	style           u32
	lpfn_wnd_proc   voidptr
	cb_cls_extra    int
	cb_wnd_extra    int
	h_instance      C.HINSTANCE
	h_icon          C.HICON
	h_cursor        C.HCURSOR
	hbr_background  C.HBRUSH
	lpsz_menu_name  &u16 // LPCWSTR
	lpsz_class_name &u16
	h_icon_sm       &u16
}

fn C.RegisterClassEx(class WndClassEx) int

fn C.GetClipboardOwner() &C.HWND

fn C.CreateWindowEx(dwExStyle i64, lpClassName &u16, lpWindowName &u16, dwStyle i64, x int, y int, nWidth int, nHeight int, hWndParent i64, hMenu voidptr, h_instance voidptr, lpParam voidptr) &C.HWND

// fn C.MultiByteToWideChar(CodePage u32, dw_flags u16, lpMultiByteStr byteptr, cbMultiByte int, lpWideCharStr u16, cchWideChar int) int
fn C.EmptyClipboard()

fn C.CloseClipboard()

fn C.GlobalAlloc(uFlag u32, size i64) C.HGLOBAL

fn C.GlobalFree(buf C.HGLOBAL)

fn C.GlobalLock(buf C.HGLOBAL)

fn C.GlobalUnlock(buf C.HGLOBAL)

fn C.SetClipboardData(uFormat u32, data voidptr) C.HANDLE

fn C.GetClipboardData(uFormat u32) C.HANDLE

fn C.DefWindowProc(hwnd C.HWND, msg u32, wParam C.WPARAM, lParam C.LPARAM) C.LRESULT

fn C.SetLastError(error i64)

fn C.OpenClipboard(hwnd C.HWND) int

fn C.DestroyWindow(hwnd C.HWND)

struct Clipboard {
	max_retries int
	retry_delay int
mut:
	hwnd C.HWND
	foo  int // TODO remove
}

fn (cb &Clipboard) get_clipboard_lock() bool {
	mut retries := cb.max_retries
	mut last_error := u32(0)
	for {
		retries--
		if retries < 0 {
			break
		}
		last_error = C.GetLastError()
		if C.OpenClipboard(cb.hwnd) > 0 {
			return true
		} else if last_error != u32(C.ERROR_ACCESS_DENIED) {
			return false
		}
		time.sleep(cb.retry_delay)
	}
	C.SetLastError(last_error)
	return false
}

fn new_clipboard() &Clipboard {
	mut cb := &Clipboard{
		max_retries: 5
		retry_delay: 5
	}
	class_name := 'clipboard'
	wndclass := WndClassEx{
		cb_size: sizeof(WndClassEx)
		lpfn_wnd_proc: voidptr(&C.DefWindowProc)
		lpsz_class_name: class_name.to_wide()
		lpsz_menu_name: 0
		h_icon_sm: 0
	}
	if C.RegisterClassEx(&wndclass) == 0 && C.GetLastError() != u32(C.ERROR_CLASS_ALREADY_EXISTS) {
		println('Failed registering class.')
	}
	hwnd := C.CreateWindowEx(0, wndclass.lpsz_class_name, wndclass.lpsz_class_name, 0,
		0, 0, 0, 0, C.HWND_MESSAGE, C.NULL, C.NULL, C.NULL)
	if hwnd == C.NULL {
		println('Error creating window!')
	}
	cb.hwnd = hwnd
	return cb
}

fn (cb &Clipboard) check_availability() bool {
	return cb.hwnd != C.HWND(C.NULL)
}

fn (cb &Clipboard) has_ownership() bool {
	return C.GetClipboardOwner() == cb.hwnd
}

fn (mut cb Clipboard) clear() {
	if !cb.get_clipboard_lock() {
		return
	}
	C.EmptyClipboard()
	C.CloseClipboard()
	cb.foo = 0
}

fn (mut cb Clipboard) free() {
	C.DestroyWindow(cb.hwnd)
	cb.foo = 0
}

// the string.to_wide doesn't work with SetClipboardData, don't know why
fn to_wide(text string) C.HGLOBAL {
	len_required := C.MultiByteToWideChar(C.CP_UTF8, C.MB_ERR_INVALID_CHARS, text.str,
		text.len + 1, C.NULL, 0)
	buf := C.GlobalAlloc(C.GMEM_MOVEABLE, i64(sizeof(u16)) * len_required)
	if buf != C.HGLOBAL(C.NULL) {
		mut locked := &u16(C.GlobalLock(buf))
		C.MultiByteToWideChar(C.CP_UTF8, C.MB_ERR_INVALID_CHARS, text.str, text.len + 1,
			locked, len_required)
		unsafe {
			locked[len_required - 1] = u16(0)
		}
		C.GlobalUnlock(buf)
	}
	return buf
}

fn (mut cb Clipboard) set_text(text string) bool {
	cb.foo = 0
	buf := to_wide(text)
	if !cb.get_clipboard_lock() {
		C.GlobalFree(buf)
		return false
	} else {
		// EmptyClipboard must be called to properly update clipboard ownership
		C.EmptyClipboard()
		if C.SetClipboardData(C.CF_UNICODETEXT, buf) == C.HANDLE(C.NULL) {
			println('SetClipboardData: Failed.')
			C.CloseClipboard()
			C.GlobalFree(buf)
			return false
		}
	}
	// CloseClipboard appears to change the sequence number...
	C.CloseClipboard()
	return true
}

fn (mut cb Clipboard) get_text() string {
	cb.foo = 0
	if !cb.get_clipboard_lock() {
		return ''
	}
	h_data := C.GetClipboardData(C.CF_UNICODETEXT)
	if h_data == C.HANDLE(C.NULL) {
		C.CloseClipboard()
		return ''
	}
	str := string_from_wide(&u16(C.GlobalLock(h_data)))
	C.GlobalUnlock(h_data)
	return str
}

// new_primary returns a new X11 `PRIMARY` type `Clipboard` instance allocated on the heap.
// Please note: new_primary only works on X11 based systems.
pub fn new_primary() &Clipboard {
	panic('Primary clipboard is not supported on non-Linux systems.')
}
