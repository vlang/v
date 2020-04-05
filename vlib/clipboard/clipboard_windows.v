module clipboard

import time

struct WndClassEx {
	cbSize u32
    style u32
    lpfnWndProc voidptr
    cbClsExtra int
    cbWndExtra int
  	hInstance C.HINSTANCE
    hIcon C.HICON
    hCursor C.HCURSOR
    hbrBackground C.HBRUSH
    lpszMenuName &u16 // LPCWSTR
    lpszClassName &u16
    hIconSm &u16
}

fn C.RegisterClassEx(class WndClassEx) int
fn C.GetClipboardOwner() &C.HWND
fn C.CreateWindowEx(dwExStyle i64, lpClassName &u16, lpWindowName &u16, dwStyle i64, x int, y int, nWidth int, nHeight int, hWndParent i64, hMenu voidptr, hInstance voidptr, lpParam voidptr) &C.HWND
//fn C.MultiByteToWideChar(CodePage u32, dwFlags u16, lpMultiByteStr byteptr, cbMultiByte int, lpWideCharStr u16, cchWideChar int) int
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
    foo int // TODO remove
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
    mut cb := &Clipboard {
        max_retries: 5
        retry_delay: 5
    }
	class_name := "clipboard"
    wndclass := WndClassEx {
        cbSize: sizeof(WndClassEx)
        lpfnWndProc: voidptr(&C.DefWindowProc)
        lpszClassName: class_name.to_wide()
		lpszMenuName: 0
		hIconSm: 0
    }
    if C.RegisterClassEx(&wndclass) == 0 && C.GetLastError() != u32(C.ERROR_CLASS_ALREADY_EXISTS) {
        println("Failed registering class.")
    }
    hwnd := C.CreateWindowEx(0, wndclass.lpszClassName, wndclass.lpszClassName, 0, 0, 0, 0, 0, C.HWND_MESSAGE, C.NULL, C.NULL, C.NULL)
    if hwnd == C.NULL {
        println("Error creating window!")
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

fn (cb mut Clipboard) clear() {
    if !cb.get_clipboard_lock() {return}
    C.EmptyClipboard()
    C.CloseClipboard()
	cb.foo = 0
}

fn (cb mut Clipboard) free(){
    C.DestroyWindow(cb.hwnd)
	cb.foo = 0
}

// the string.to_wide doesn't work with SetClipboardData, don't know why
fn to_wide(text string) &C.HGLOBAL {
    len_required := C.MultiByteToWideChar(C.CP_UTF8, C.MB_ERR_INVALID_CHARS, text.str, text.len + 1, C.NULL, 0)
    buf := C.GlobalAlloc(C.GMEM_MOVEABLE, sizeof(u16) * len_required)
    if buf != C.HGLOBAL(C.NULL) {
        mut locked := &u16(C.GlobalLock(buf))
        C.MultiByteToWideChar(C.CP_UTF8, C.MB_ERR_INVALID_CHARS, text.str, text.len + 1, locked, len_required)
        locked[len_required - 1] = u16(0)
        C.GlobalUnlock(buf)
    }
    return buf
}

fn (cb mut Clipboard) set_text(text string) bool {
	cb.foo = 0
    buf := to_wide(text)
    if !cb.get_clipboard_lock() {
        C.GlobalFree(buf)
        return false
    } else {
        // EmptyClipboard must be called to properly update clipboard ownership
        C.EmptyClipboard()
        if C.SetClipboardData(C.CF_UNICODETEXT, buf) == C.HANDLE(C.NULL) {
            println("SetClipboardData: Failed.")
            C.CloseClipboard()
            C.GlobalFree(buf)
            return false
        }
    }
    // CloseClipboard appears to change the sequence number...
    C.CloseClipboard()
    return true
}

fn (cb mut Clipboard) get_text() string {
	cb.foo = 0
    if !cb.get_clipboard_lock() {
        return ""
    }
    h_data := C.GetClipboardData(C.CF_UNICODETEXT)
    if h_data == C.HANDLE(C.NULL) {
        C.CloseClipboard()
        return ""
    }
    str := string_from_wide(&u16(C.GlobalLock(h_data)))
    C.GlobalUnlock(h_data)
    return str
}

pub fn new_primary() &Clipboard {
	panic('Primary clipboard is not supported on non-Linux systems.')
}
