// A Result over a multi-return value must keep its C definition even when a
// generic struct elsewhere has the same short name as one of the returned
// structs. cgen skips Result definitions for unmaterialized generic payloads by
// their C name prefix, and used to read `multi_return_web__Response_bool` as an
// instance of `rpc.Response[T]` (C prefix `Response_`). v-analyzer, which has a
// generic `jsonrpc.Response[T]`, then failed to build `net.http`:
//
//	error: unknown type name '__v_result_multi_return_http__Response_bool'
import os

const vexe = @VEXE

fn test_multi_return_result_is_defined_next_to_a_same_named_generic_struct() {
	dir := os.join_path(os.vtmp_dir(), 'v3_multi_return_generic_name_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'web')) or { panic(err) }
	os.mkdir_all(os.join_path(dir, 'rpc')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'web', 'web.v'), 'module web

pub struct Response {
pub:
	status int
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'rpc', 'rpc.v'), 'module rpc

pub struct Response[T] {
pub:
	value T
}
') or {
		panic(err)
	}
	src := os.join_path(dir, 'main.v')
	os.write_file(src, "module main

import rpc
import web

fn exchange(fail bool) !(web.Response, bool) {
	if fail {
		return error('fail')
	}
	return web.Response{
		status: 200
	}, true
}

fn main() {
	r := rpc.Response[int]{
		value: 3
	}
	resp, ok := exchange(false)!
	println('\${r.value} \${resp.status} \${ok}')
}
") or {
		panic(err)
	}
	out := os.join_path(dir, 'main.exe')
	// `-new-compiler` keeps a C error from being answered by the V 0.5.2 fallback.
	res := os.exec([vexe, '-new-compiler', '-o', out, src])
	assert res.exit_code == 0, res.output
	run := os.exec([out])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '3 200 true'
}
