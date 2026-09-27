import os

fn mktemp(value string) string { return value + '-temporary' }

fn truncate(value string) string { return value[..3] }

fn test_v_file_function_names_can_coexist_with_system_headers() {
	assert os.getpid() > 0
	assert mktemp('file') == 'file-temporary'
	assert truncate('longer') == 'lon'
}
