module main

#include <windows.h>

fn C.GetErrorMode() u32
fn C.SetErrorMode(mode u32) u32

fn disable_windows_loader_dialogs() {
	// Child processes inherit this mode, including tools probed by `v doctor`.
	// A missing compiler DLL must return an error instead of blocking on a dialog.
	C.SetErrorMode(C.GetErrorMode() | u32(C.SEM_FAILCRITICALERRORS))
}
