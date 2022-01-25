import os

fn test_comptime_include() ? {
	assert os.read_file('./v.mod') ? == $include_str('./v.mod')
}
