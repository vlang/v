import os

fn mktemp(value string) string { return value + '-temporary' }

fn truncate(value string) string { return value[..3] }

fn v_mktemp(value string) string { return value + '-v' }

fn v_truncate(value string) string { return value[..2] }

struct CollisionNames {
	mktemp     int
	v_mktemp   int
	truncate   int
	v_truncate int
}

fn test_v_file_function_names_can_coexist_with_system_headers() {
	assert os.getpid() > 0
	assert mktemp('file') == 'file-temporary'
	assert truncate('longer') == 'lon'
	assert v_mktemp('file') == 'file-v'
	assert v_truncate('longer') == 'lo'
	mktemp := 1
	v_mktemp := 2
	truncate := 3
	v_truncate := 4
	assert mktemp + v_mktemp + truncate + v_truncate == 10
	names := CollisionNames{
		mktemp:     mktemp
		v_mktemp:   v_mktemp
		truncate:   truncate
		v_truncate: v_truncate
	}
	assert names.mktemp + names.v_mktemp + names.truncate + names.v_truncate == 10
}
