module driver

#include <windows.h>

fn C.MoveFileExW(existing_file_name &u16, new_file_name &u16, flags u32) int

// MOVEFILE_REPLACE_EXISTING makes `MoveFileExW` replace a file that already has the new name.
const windows_movefile_replace_existing = u32(1)

// windows_replace_file renames `source` to `destination`, replacing a file that is already
// there. `os.rename` is `_wrename` on Windows, which fails when `destination` exists.
// MOVEFILE_COPY_ALLOWED is not passed, so the move fails instead of turning into a copy
// that could leave a partially written `destination`.
fn windows_replace_file(source string, destination string) ! {
	windows_source := source.replace('/', '\\')
	windows_destination := destination.replace('/', '\\')
	if C.MoveFileExW(windows_source.to_wide(), windows_destination.to_wide(), windows_movefile_replace_existing) == 0 {
		// Read the code before another API call overwrites it.
		code := C.GetLastError()
		return error('failed to replace `${windows_destination}` with `${windows_source}` (Windows error ${code})')
	}
}
