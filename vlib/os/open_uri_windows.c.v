module os

import dl

type ShellExecuteWin = fn (voidptr, &u16, &u16, &u16, &u16, int)

pub fn open_uri(uri string) ! {
	mut vopen_uri_cmd := getenv('VOPEN_URI_CMD')
	if vopen_uri_cmd != '' {
		result := execute('$vopen_uri_cmd "$uri"')
		if result.exit_code != 0 {
			return error('unable to open url: $result.output')
		}
		return
	}
	handle := dl.open_opt('shell32', dl.rtld_now)!
	// https://docs.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-shellexecutew
	func := ShellExecuteWin(dl.sym_opt(handle, 'ShellExecuteW')!)
	func(C.NULL, 'open'.to_wide(), uri.to_wide(), C.NULL, C.NULL, C.SW_SHOWNORMAL)
	dl.close(handle)
}
