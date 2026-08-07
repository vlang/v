import v.embed_file

fn test_embed_file_with_import() {
	mut file := $embed_file('v.png')
	eprintln('file: ${file}')
	assert file.len == 603
	fdata := file.data()
	eprintln('file after .data() call: ${file}')
	assert file.len == 603
	unsafe {
		assert fdata.vbytes(4) == [u8(0x89), `P`, `N`, `G`]
	}
	assert check_file(file)
}

fn test_embed_file_as_or_block_result_with_comment() {
	file := maybe_file() or {
		$embed_file('v.png') // fallback value
	}
	assert check_file(file)
}

fn test_parenthesized_embed_file_as_or_block_result() {
	file := (maybe_file() or { $embed_file('v.png') })
	assert check_file(file)
}

fn test_embed_file_as_comptime_if_or_block_result() {
	file := maybe_file() or {
		$if windows {
			$embed_file('v.png')
		} $else {
			$embed_file('v.png')
		}
	}
	assert check_file(file)
}

fn test_embed_file_as_comptime_match_or_block_result() {
	selector := 'png'
	file := maybe_file() or {
		$match selector {
			'png' { $embed_file('v.png') }
		}
	}
	assert check_file(file)
}

fn test_embed_file_or_block_result_used_by_infix_in_if_branch() {
	fallback := $embed_file('v.png')
	ok := fallback.len == 603
	same := if ok {
		maybe_file() or { $embed_file('v.png') } == fallback
	} else {
		false
	}
	assert same
}

fn test_embed_file_or_block_result_before_comma_in_if_branch() {
	ok := true
	fallback := $embed_file('v.png')
	file, n := if ok {
		maybe_file() or { $embed_file('v.png') }, 1
	} else {
		fallback, 0
	}
	assert check_file(file)
	assert n == 1
}

fn check_file(file embed_file.EmbedFileData) bool {
	return file.len == 603
}

fn maybe_file() ?embed_file.EmbedFileData {
	return none
}
