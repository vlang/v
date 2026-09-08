const const_file = $embed_file('v.png')
// vtest build: !self_werror?

const src = $embed_file('embed_file_test.v').to_string()

fn test_const_embed_file_to_string() {
	assert src.len > 0
	assert src.split_into_lines()[0].starts_with('const const_file')
	assert src.split_into_lines().last() == '}'
}

fn test_const_embed_file() {
	mut file := const_file
	eprintln('file: ${file}')
	assert file.len == 603
	fdata := file.data()
	eprintln('file after .data() call: ${file}')
	assert file.path == 'v.png'
	assert file.len == 603
	unsafe {
		assert fdata.vbytes(4) == [u8(0x89), `P`, `N`, `G`]
	}
}

fn test_embed_file() {
	mut file := $embed_file('v.png')
	eprintln('file: ${file}')
	assert file.len == 603
	fdata := file.data()
	eprintln('file after .data() call: ${file}')
	assert file.len == 603
	unsafe {
		assert fdata.vbytes(4) == [u8(0x89), `P`, `N`, `G`]
	}
}

fn test_embed_file_as_if_expr_branch_result() {
	embedded := if const_file.len == 603 {
		$embed_file('v.png') // selected branch value
	} else {
		$embed_file('embed_file_test.v')
	}
	assert embedded.path == 'v.png'
	assert embedded.len == 603
}

fn test_parenthesized_embed_file_as_if_expr_branch_result() {
	embedded := if const_file.len == 603 {
		($embed_file('v.png'))
	} else {
		$embed_file('embed_file_test.v')
	}
	assert embedded.path == 'v.png'
	assert embedded.len == 603
}

fn test_embed_file_as_nested_if_expr_branch_result() {
	ok := const_file.len == 603
	other := true
	embedded := if ok {
		if other {
			$embed_file('v.png')
		} else {
			$embed_file('embed_file_test.v')
		}
	} else {
		$embed_file('embed_file_test.v')
	}
	assert embedded.path == 'v.png'
	assert embedded.len == 603
}

fn test_embed_file_as_nested_if_expr_branch_result_with_or_block_condition() {
	embedded := if const_file.len == 603 {
		if optional_bool() or { true } {
			$embed_file('v.png')
		} else {
			$embed_file('embed_file_test.v')
		}
	} else {
		$embed_file('embed_file_test.v')
	}
	assert embedded.path == 'v.png'
	assert embedded.len == 603
}

fn test_embed_file_nested_if_result_used_by_infix_in_if_branch() {
	fallback := $embed_file('v.png')
	ok := fallback.len == 603
	other := true
	same := if ok {
		if other {
			$embed_file('v.png')
		} else {
			fallback
		} == fallback
	} else {
		false
	}
	assert same
}

fn test_embed_file_as_match_expr_branch_result() {
	embedded := match const_file.len {
		603 {
			$embed_file('v.png') // selected match branch value
		}
		else {
			$embed_file('embed_file_test.v')
		}
	}

	assert embedded.path == 'v.png'
	assert embedded.len == 603
}

fn test_embed_file_as_comptime_if_expr_in_if_expr_branch_result() {
	embedded := if const_file.len == 603 {
		$if windows {
			$embed_file('v.png')
		} $else {
			$embed_file('v.png')
		}
	} else {
		$embed_file('embed_file_test.v')
	}

	assert embedded.path == 'v.png'
	assert embedded.len == 603
}

fn optional_bool() ?bool {
	return none
}

fn test_embed_file_as_comptime_match_expr_in_if_expr_branch_result() {
	selector := 'png'
	embedded := if const_file.len == 603 {
		$match selector {
			'png' { $embed_file('v.png') }
		}
	} else {
		$embed_file('embed_file_test.v')
	}

	assert embedded.path == 'v.png'
	assert embedded.len == 603
}
