module os

// same_file_windows compares file identity through Unicode Win32 handles,
// supporting directories and following symbolic links.
fn same_file_windows(a string, b string) !bool {
	ia := windows_stat_file_information(a) or { return error('os.same_file: cannot query `${a}`') }
	ib := windows_stat_file_information(b) or { return error('os.same_file: cannot query `${b}`') }
	return ia.volume_serial == ib.volume_serial && ia.file_index_high == ib.file_index_high
		&& ia.file_index_low == ib.file_index_low
}
