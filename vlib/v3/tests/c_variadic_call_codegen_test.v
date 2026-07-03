import os

const c_variadic_vexe = @VEXE
const c_variadic_tests_dir = os.dir(@FILE)
const c_variadic_v3_dir = os.dir(c_variadic_tests_dir)
const c_variadic_vlib_dir = os.dir(c_variadic_v3_dir)
const c_variadic_v3_src = os.join_path(c_variadic_v3_dir, 'v3.v')

fn c_variadic_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_c_variadic_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${c_variadic_vexe} -gc none -path "${c_variadic_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${c_variadic_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn c_variadic_call_lines(generated string, names []string) []string {
	mut lines := []string{}
	for line in generated.split_into_lines() {
		for name in names {
			if line.contains('${name}(') {
				lines << line
				break
			}
		}
	}
	return lines
}

fn test_c_variadic_calls_emit_real_args_without_v_pack_array() {
	v3_bin := c_variadic_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_c_variadic_${os.getpid()}.v')
	os.write_file(src, "module main

struct C.FILE {}

fn C.printf(fmt &char, args ...voidptr) int
fn C.fprintf(stream &C.FILE, fmt &char, args ...voidptr) int
fn C.sscanf(input &char, fmt &char, args ...voidptr) int
fn C.fflush(stream &C.FILE) int

__global C.stderr &C.FILE

fn sum(xs ...int) int {
	mut total := 0
	for x in xs {
		total += x
	}
	return total
}

fn main() {
	C.printf(&c'plain\\n')
	C.printf(&c'name=%s count=%d\\n', &c'ok', 7)
	C.fprintf(C.stderr, &c'err=%d\\n', 9)
	C.fflush(C.stderr)
	mut a := 0
	mut b := 0
	C.sscanf(&c'12 34', &c'%d %d', &a, &b)
	println(int_str(a + b + sum(1, 2, 3)))
}
") or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_c_variadic_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.contains('plain'), run.output
	assert run.output.contains('name=ok count=7'), run.output
	assert run.output.trim_space().ends_with('52'), run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	c_lines := c_variadic_call_lines(generated, ['printf', 'fprintf', 'sscanf'])
	assert c_lines.len >= 4, generated
	for line in c_lines {
		assert !line.contains('__varargs_'), line
	}
	assert generated.contains('printf("plain\\n");'), generated
	assert generated.contains('printf("name=%s count=%d\\n", "ok", 7);'), generated
	assert generated.contains('fprintf(stderr, "err=%d\\n", 9);'), generated
	assert generated.contains('sscanf("12 34", "%d %d", &a, &b);'), generated
	assert generated.contains('__varargs_'), generated
}

fn test_voidptr_variadic_alias_scalars_promote_before_boxing() {
	v3_bin := c_variadic_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_c_variadic_alias_${os.getpid()}.v')
	os.write_file(src, 'module main

type Small = u8
type Floaty = f32

fn sink(args ...voidptr) int {
	return args.len
}

fn main() {
	println(int_str(sink(Small(7), Floaty(1.5))))
}
') or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_c_variadic_alias_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '2', run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('int __vararg_storage_'), generated
	assert generated.contains('double __vararg_storage_'), generated
	assert !generated.contains('Small __vararg_storage_'), generated
	assert !generated.contains('Floaty __vararg_storage_'), generated
}
