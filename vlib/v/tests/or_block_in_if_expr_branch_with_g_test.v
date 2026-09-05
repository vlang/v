import os

const vexe = @VEXE

// `#line` directives must always be terminated by a newline. When a branch of an
// if/match *expression* is valued by an `or {}` block, cgen cuts the unfinished
// assignment (which can end with a `#line` directive) out of the output and
// writes it back after the `or {}` block. Without a terminating newline the C
// preprocessor swallows the unwrapped value as trailing tokens of the directive.
// See https://github.com/vlang/v/issues/28163 and issue #27495 for the earlier,
// array-literal flavoured instance of the same family.
fn test_or_block_in_if_expr_branch_compiles_with_g() {
	test_dir := os.join_path(os.vtmp_dir(), 'or_block_if_expr_g_test_${os.getpid()}')
	os.mkdir_all(test_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	source := os.join_path(test_dir, 'test.v')
	out_c := os.join_path(test_dir, 'test.c')
	os.write_file(source, "struct Params {
	initial_max_stream_data_bidi_remote ?u64
	initial_max_stream_data_bidi_local  ?u64
}

fn initial_send_limit_for_stream(locally_initiated bool, peer_params Params) u64 {
	return if locally_initiated {
		peer_params.initial_max_stream_data_bidi_remote or { 0 }
	} else {
		peer_params.initial_max_stream_data_bidi_local or { 0 }
	}
}

struct Conn {
	initial_keys_server   int
	handshake_keys_server ?int
}

fn (c &Conn) process(initial bool) ?int {
	keys := if initial {
		c.initial_keys_server
	} else {
		c.handshake_keys_server or { return none }
	}
	return keys
}

fn main() {
	println(initial_send_limit_for_stream(true, Params{ initial_max_stream_data_bidi_remote: u64(7) }))
	println(initial_send_limit_for_stream(false, Params{}))
	c := Conn{ initial_keys_server: 3, handshake_keys_server: 5 }
	println(c.process(true) or { -1 })
	println(c.process(false) or { -1 })
}
")!
	res :=
		os.execute('${os.quoted_path(vexe)} -g -o ${os.quoted_path(out_c)} -b c ${os.quoted_path(source)}')
	assert res.exit_code == 0, res.output
	generated := os.read_file(out_c)!
	for line in generated.split_into_lines() {
		trimmed := line.trim_left(' \t')
		if !trimmed.starts_with('#line ') {
			continue
		}
		// `#line NN "path"` and nothing else may follow on that line
		rest := trimmed.all_after_last('"').trim_space()
		assert rest == '', 'found #line directive glued to an expression: ${line}'
	}
}
