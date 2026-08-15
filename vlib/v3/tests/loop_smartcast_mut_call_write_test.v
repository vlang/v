import os

const loop_smartcast_vexe = @VEXE
const loop_smartcast_tests_dir = os.dir(@FILE)
const loop_smartcast_v3_dir = os.dir(loop_smartcast_tests_dir)
const loop_smartcast_vlib_dir = os.dir(loop_smartcast_v3_dir)
const loop_smartcast_v3_src = os.join_path(loop_smartcast_v3_dir, 'v3.v')

fn loop_smartcast_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_loop_smartcast_mut_call_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${loop_smartcast_vexe} -gc none -path "${loop_smartcast_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${loop_smartcast_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn loop_smartcast_run_bad(v3_bin string, name string, source string, expected string) {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains(expected), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn loop_smartcast_run_good(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_mut_sum_call_invalidates_loop_smartcast() {
	v3_bin := loop_smartcast_build_v3()

	// A loop body that passes the loop-narrowed sum value to a `mut` sum parameter lets
	// the callee swap the active variant. The loop-condition narrowing must not be
	// restored for a later variant access, otherwise the checker accepts a field that no
	// longer exists at runtime and emits unsafe C (reading the wrong variant's storage).
	loop_smartcast_run_bad(v3_bin, 'bad_loop_mut_sum_call_stale_smartcast', 'struct Foo {
	foo int
}

struct Bar {}

type Value = Bar | Foo

fn replace(mut v Value) {
	v = Value(Bar{})
}

fn main() {
	mut x := Value(Foo{
		foo: 1
	})
	for x is Foo {
		replace(mut x)
		println(int_str(x.foo))
		break
	}
}
',
		'field `foo` does not exist')

	// Without a body write the loop-condition narrowing stays valid.
	nowrite_out := loop_smartcast_run_good(v3_bin, 'good_loop_smartcast_without_write', 'struct Foo {
	foo int
}

struct Bar {}

type Value = Bar | Foo

fn main() {
	mut x := Value(Foo{
		foo: 7
	})
	for x is Foo {
		println(int_str(x.foo))
		break
	}
}
')
	assert nowrite_out == '7'

	// A `mut Variant` parameter cannot change the runtime tag, so passing the narrowed
	// value to it leaves the narrowing intact and the mutation is observed (no false
	// positive: the smartcast survives the call).
	variant_out := loop_smartcast_run_good(v3_bin, 'good_mut_variant_call_keeps_smartcast', 'struct Foo {
mut:
	foo int
}

struct Bar {}

type Value = Bar | Foo

fn bump(mut f Foo) {
	f.foo = 9
}

fn main() {
	mut x := Value(Foo{
		foo: 1
	})
	for x is Foo {
		bump(mut x)
		println(int_str(x.foo))
		break
	}
}
')
	assert variant_out == '9'
}
