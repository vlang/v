module clipboard

import time

#include <windows.h>

type HANDLE voidptr

struct C.HWND
struct C.WPARAM
struct C.LPARAM
struct C.LRESULT
struct C.HGLOBAL
//struct C.HANDLE

struct C.WNDCLASSEX {
    cbSize int
    lpfnWndProc voidptr
    lpszClassName &u16
}

fn C.RegisterClassEx(class WNDCLASSEX) int
fn C.GetClipboardOwner() &HWND
fn C.CreateWindowEx(dwExStyle i64, lpClassName &u16, lpWindowName &u16, dwStyle i64, x int, y int, nWidth int, nHeight int, hWndParent i64, hMenu voidptr, hInstance voidptr, lpParam voidptr) &HWND
//fn C.MultiByteToWideChar(CodePage u32, dwFlags u16, lpMultiByteStr byteptr, cbMultiByte int, lpWideCharStr u16, cchWideChar int) int
fn C.EmptyClipboard()
fn C.CloseClipboard()
fn C.GlobalAlloc(uFlag u32, size i64) HGLOBAL
fn C.GlobalFree(buf HGLOBAL)
fn C.GlobalLock(buf HGLOBAL)
fn C.GlobalUnlock(buf HGLOBAL)
fn C.SetClipboardData(uFormat u32, data voidptr) C.HANDLE
fn C.GetClipboardData(uFormat u32) C.HANDLE
fn C.DefWindowProc(hwnd HWND, msg u32, wParam WPARAM, lParam LPARAM) LRESULT
fn C.SetLastError(error i64)
fn C.OpenClipboard(hwnd HWND) int
fn C.DestroyWindow(hwnd HWND)

pub struct Clipboard {
    max_retries int
    retry_delay int
    mut:
    hwnd HWND
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
        last_error = GetLastError()
        if OpenClipboard(cb.hwnd) > 0 {
            return true
        } else if last_error != u32(C.ERROR_ACCESS_DENIED) {
            return false
        }

		time.sleep(cb.retry_delay)
	}
    SetLastError(last_error)
    return false
}

fn new_clipboard() &Clipboard {
    mut cb := &Clipboard {
        max_retries: 5
        retry_delay: 5
    }
    wndclass := WNDCLASSEX{
        cbSize: sizeof(WNDCLASSEX)
        lpfnWndProc: voidptr(&DefWindowProc)
        lpszClassName: "clipboard".to_wide()
    }
    if RegisterClassEx(&wndclass) <= 0 && GetLastError() != u32(C.ERROR_CLASS_ALREADY_EXISTS) {
        println("Failed registering class.")
    }
    hwnd := CreateWindowEx(0, wndclass.lpszClassName, wndclass.lpszClassName, 0, 0, 0, 0, 0, C.HWND_MESSAGE, C.NULL, C.NULL, C.NULL)
    if hwnd == C.NULL {
        println("Error creating window!")
    }
    cb.hwnd = hwnd
    return cb
}

fn (cb &Clipboard) check_availability() bool {
	return cb.hwnd != HWND(C.NULL)
}

fn (cb &Clipboard) has_ownership() bool {
    return GetClipboardOwner() == cb.hwnd
}

fn (cb mut Clipboard) clear() {
    if !cb.get_clipboard_lock() {return}
    EmptyClipboard()
    CloseClipboard()
	cb.foo = 0
}

fn (cb mut Clipboard) free(){
    DestroyWindow(cb.hwnd)
	cb.foo = 0
}

// the string.to_wide doesn't work with SetClipboardData, don't know why
fn to_wide(text string) &HGLOBAL {
    len_required := MultiByteToWideChar(C.CP_UTF8, C.MB_ERR_INVALID_CHARS, text.str, text.len + 1, C.NULL, 0)
    buf := GlobalAlloc(C.GMEM_MOVEABLE, sizeof(u16) * len_required)
    if buf != HGLOBAL(C.NULL) {
        mut locked := &u16(GlobalLock(buf))
        MultiByteToWideChar(C.CP_UTF8, C.MB_ERR_INVALID_CHARS, text.str, text.len + 1, locked, len_required)
        locked[len_required - 1] = u16(0)
        GlobalUnlock(buf)
    }
    return &buf
}

fn (cb mut Clipboard) set_text(text string) bool {
	cb.foo = 0
    buf := to_wide(text)
    if !cb.get_clipboard_lock() {
        GlobalFree(buf)
        return false
    } else {
        // EmptyClipboard must be called to properly update clipboard ownership
        EmptyClipboard()
        if SetClipboardData(C.CF_UNICODETEXT, buf) == HANDLE(C.NULL) {
            println("SetClipboardData: Failed.")
            CloseClipboard()
            GlobalFree(buf)
            return false
        }
    }
    // CloseClipboard appears to change the sequence number...
    CloseClipboard()
    return true
}

fn (cb mut Clipboard) get_text() string {
	cb.foo = 0
    if !cb.get_clipboard_lock() {
        return ""
    }
    h_data := GetClipboardData(C.CF_UNICODETEXT)
    if h_data == HANDLE(C.NULL) {
        CloseClipboard()
        return ""
    }
    str := string_from_wide(&u16(GlobalLock(h_data)))
    GlobalUnlock(h_data)
    return str
}

pub fn new_primary() &Clipboard {
	panic('Primary clipboard is not supported on non-Linux systems.')
}
