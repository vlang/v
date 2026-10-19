// Regression tests for https://github.com/vlang/v/issues/30621 : `-autofree` and `-ownership`
// rejected returning module-level `IError` constants, and binding a map slot through a
// borrowed `&map` (`mut t := &table; if val := t[k]`), which `net` and `toml` both do.
import os

fn ownership_ierror_const_source(name string, code string) !string {
	root := os.join_path(os.vtmp_dir(), 'ownership_ierror_const_${name}_${os.getpid()}')
	os.mkdir_all(root)!
	source := os.join_path(root, 'main.v')
	os.write_file(source, code)!
	return source
}

fn ownership_ierror_const_cleanup(source string) {
	os.rmdir_all(os.dir(source)) or {}
}

fn ownership_ierror_const_compile(flags string, source string) os.Result {
	return os.exec([@VEXE, '-new-compiler', '-nocache', ...flags.split(' '), source])
}

fn test_returned_error_constants_give_the_caller_its_own_copy() {
	source := ownership_ierror_const_source('returns', 'import net

const err_busy = error_with_code("busy", 7)
const err_alias = err_busy

struct Holder {
	err IError
}

fn direct() ! {
	return err_busy
}

fn aliased() !int {
	return err_alias
}

fn cross_module() !string {
	return net.err_timed_out
}

fn branch(fail_busy bool) !int {
	return if fail_busy { err_busy } else { error("other") }
}

fn matched(n int) !int {
	return match n {
		0 { err_busy }
		else { net.err_port_out_of_range }
	}
}

fn main() {
	for _ in 0 .. 3 {
		direct() or { println(err.msg()) }
		aliased() or { println(err.code()) }
		cross_module() or { println(err.msg()) }
		branch(true) or { println(err.msg()) }
		branch(false) or { println(err.msg()) }
		matched(0) or { println(err.msg()) }
		matched(1) or { println(err.msg()) }
		_, _ := net.split_address("127.0.0.1:80") or { return }
		local := err_busy
		holder := Holder{
			err: err_busy
		}
		mut errs := []IError{}
		errs << err_busy
		println("\${local.msg()} \${holder.err.msg()} \${errs[0].code()}")
	}
	println("\${err_busy.msg()} \${err_busy.code()} \${net.err_timed_out.msg()}")
}
') or {
		panic(err)
	}
	defer {
		ownership_ierror_const_cleanup(source)
	}
	once := 'busy\n7\nnet: op timed out\nbusy\nother\nbusy\nnet: port out of range\nbusy busy 7'
	expected := [once, once, once, 'busy 7 net: op timed out'].join('\n')
	for flags in ['-autofree -cc clang -no-parallel run', '-autofree -cc clang run',
		'-ownership -d ownership -cc clang run'] {
		out := ownership_ierror_const_compile(flags, source)
		assert out.exit_code == 0, '${flags}: ${out.output}'
		assert out.output.trim_space() == expected, '${flags}: ${out.output}'
	}
}

fn test_issue_programs_pass_autofree_check() {
	programs := {
		'net':     'import net

fn main() {
	mut conn := net.dial_tcp("127.0.0.1:9") or { return }
	mut buf := []u8{len: 16}
	_ := conn.read(mut buf) or { return }
}
'
		'mbedtls': 'import net.mbedtls

fn main() {
	mut listener := mbedtls.new_ssl_listener("127.0.0.1:0", mbedtls.SSLConnectConfig{}, mbedtls.SSLListenerOptions{}) or {
		return
	}
	mut conn := listener.accept() or { return }
	conn.close() or {}
}
'
		'toml':    'import toml

fn main() {
	_ := toml.parse_text("[a.b]\\nc = 1\\n") or { return }
}
'
	}
	for name, code in programs {
		source := ownership_ierror_const_source(name, code) or { panic(err) }
		out := ownership_ierror_const_compile('-autofree -check', source)
		ownership_ierror_const_cleanup(source)
		assert out.exit_code == 0, '${name}: ${out.output}'
	}
}

fn test_map_guard_through_a_mutable_borrowed_alias_is_a_view() {
	source := ownership_ierror_const_source('map_guard', 'type Value = map[string]Value | string | int

fn is_all_tables(table map[string]Value, keys []string) bool {
	unsafe {
		mut t := &table
		for key in keys {
			if val := t[key] {
				if val is map[string]Value {
					t = &val
				} else {
					return false
				}
			} else {
				return false
			}
		}
	}
	return true
}

fn main() {
	mut inner := map[string]Value{}
	inner["c"] = Value("x".repeat(3))
	mut root := map[string]Value{}
	root["a"] = Value(inner)
	for _ in 0 .. 2 {
		println(is_all_tables(root, ["a"]))
		println(is_all_tables(root, ["a", "c"]))
		println(is_all_tables(root, ["b"]))
	}
	println(root)
}
') or {
		panic(err)
	}
	defer {
		ownership_ierror_const_cleanup(source)
	}
	once := 'true\nfalse\nfalse'
	expected := [once, once, "{'a': Value({'c': Value('xxx')})}"].join('\n')
	for flags in ['-autofree -cc clang -no-parallel run', '-autofree -cc clang run'] {
		out := ownership_ierror_const_compile(flags, source)
		assert out.exit_code == 0, '${flags}: ${out.output}'
		assert out.output.trim_space() == expected, '${flags}: ${out.output}'
	}
}

fn test_borrowed_error_without_a_compatible_clone_is_still_rejected() {
	source := ownership_ierror_const_source('no_clone', 'struct DetailError {
	Error
	detail string
}

fn (e DetailError) msg() string {
	return e.detail
}

fn new_detail_error() IError {
	return DetailError{
		detail: "detail".repeat(2)
	}
}

const err_detail = new_detail_error()
const err_busy = error_with_code("busy", 7)

fn detailed() ! {
	return err_detail
}

fn busy() ! {
	return err_busy
}

fn main() {
	detailed() or { println(err.msg()) }
	busy() or { println(err.msg()) }
}
') or {
		panic(err)
	}
	defer {
		ownership_ierror_const_cleanup(source)
	}
	for flags in ['-autofree -check', '-ownership -d ownership -check'] {
		out := ownership_ierror_const_compile(flags, source)
		assert out.exit_code != 0, '${flags}: ${out.output}'
		assert out.output.contains('main.v:20:9: error: cannot copy borrowed `IError` value: `DetailError` requires ownership destruction but has no compatible `clone()` method'), '${flags}: ${out.output}'
		// `err_busy` comes from builtin `error_with_code()` and always holds a MessageError.
		assert !out.output.contains('main.v:24:'), '${flags}: ${out.output}'
	}
}

fn test_owned_value_moved_out_of_a_borrowed_map_is_still_rejected() {
	source := ownership_ierror_const_source('owned_guard', 'struct Entry {
	name string
}

fn main() {
	mut m := map[string]Entry{}
	m["a"] = Entry{"x".repeat(3)}
	mut t := &m
	unsafe {
		if val := t["a"] {
			println(val.name)
		}
	}
	println(m.len)
}
') or {
		panic(err)
	}
	defer {
		ownership_ierror_const_cleanup(source)
	}
	for flags in ['-autofree -check', '-ownership -d ownership -check'] {
		out := ownership_ierror_const_compile(flags, source)
		assert out.exit_code != 0, '${flags}: ${out.output}'
		assert out.output.contains('cannot move `t[a]` because it borrows `m[a]`; attempted move to `val`'), '${flags}: ${out.output}'
	}
}
