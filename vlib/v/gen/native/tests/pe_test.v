module tests

import os

fn test_prevent_could_not_find_symbols_regression() {
	res := os.execute('${os.quoted_path(@VEXE)} -b native examples/hello_world.v')
	assert !res.output.contains('CaptureStackBackTrace'), 'Test failed system unable to find symbol: CaptureStackBackTrace'
	assert !res.output.contains('__debugbreak'), 'Test failed system unable to find symbol: __debugbreak'
}
