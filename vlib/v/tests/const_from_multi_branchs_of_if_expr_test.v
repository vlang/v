import os

const bin = $if linux {
	os.join_path(@VMODROOT, 'bin_linux_64')
} $else $if macos {
	os.join_path(@VMODROOT, 'bin_macos')
} $else {
	''
}

fn test_const_from_multi_branches_of_if_expr() {
	println('Hello')
	assert true
}
