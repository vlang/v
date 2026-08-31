import os

const pr_27962_vexe = @VEXE
const pr_27962_tests_dir = os.dir(@FILE)
const pr_27962_v3_dir = os.dir(pr_27962_tests_dir)
const pr_27962_vlib_dir = os.dir(pr_27962_v3_dir)
const pr_27962_v3_src = os.join_path(pr_27962_v3_dir, 'v3.v')

fn pr_27962_build_v3(root string) string {
	v3_bin := os.join_path(root, 'v3')
	build :=
		os.execute('${os.quoted_path(pr_27962_vexe)} -gc none -prealloc -path "${pr_27962_vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(pr_27962_v3_src)}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn pr_27962_write_source(root string, name string, source string) string {
	path := os.join_path(root, '${name}.v')
	os.write_file(path, source) or { panic(err) }
	return path
}

fn pr_27962_compile_and_run(v3_bin string, root string, name string, source string) string {
	source_path := pr_27962_write_source(root, name, source)
	binary_path := os.join_path(root, name)
	compile :=
		os.execute('${os.quoted_path(v3_bin)} -silent -nocache -o ${os.quoted_path(binary_path)} ${os.quoted_path(source_path)}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(os.quoted_path(binary_path))
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn pr_27962_generate_c(v3_bin string, root string, name string, source string, flags string) string {
	source_path := pr_27962_write_source(root, name, source)
	c_path := os.join_path(root, '${name}.c')
	compile :=
		os.execute('${os.quoted_path(v3_bin)} -silent -nocache ${flags} -o ${os.quoted_path(c_path)} ${os.quoted_path(source_path)}')
	assert compile.exit_code == 0, compile.output
	return os.read_file(c_path) or { panic(err) }
}

fn test_pr_27962_regressions_are_fixed_in_v3() {
	root := os.join_path(os.temp_dir(), 'v3_pr_27962_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := pr_27962_build_v3(root)

	output := pr_27962_compile_and_run(v3_bin, root, 'compiler_regressions', '
fn run_option_callback[T](callback fn () T) ?T {
	mut result := T{}
	result = callback()
	return result
}

fn plain_int_callback() int {
	return 42
}

fn option_int_callback() ?int {
	return 42
}

fn generic_option_default[T]() ?T {
	mut opt := ?T{}
	return opt
}

type GenericKeyMaps = [4]map[T]string
type GenericValueMaps = [4]map[string]T

type Palette = [4]Color

enum Color {
	red
	green
	blue
}

struct Setup {
	palette Palette
}

type UnusedPalette = [3]UnusedColor

enum UnusedColor {
	black
}

@[heap]
struct HeapItem {
mut:
	value int
}

fn (mut item HeapItem) reference() &HeapItem {
	return item
}

fn main() {
	assert (run_option_callback(plain_int_callback) or { -1 }) == 42
	assert (run_option_callback(option_int_callback) or { -1 }) == 42
	assert (generic_option_default[int]() or { -1 }) == 0
	key_maps := GenericKeyMaps{}
	value_maps := GenericValueMaps{}
	assert key_maps.len == 4
	assert value_maps.len == 4
	setup := Setup{}
	assert setup.palette[0] == .red
	mut item := HeapItem{
		value: 1
	}
	mut reference := item.reference()
	reference.value = 2
	assert item.value == 2
	assert reference.value == 2
	println("ok")
}
')
	assert output == 'ok'

	runtime_source := '
import sync
import time

fn work() {}

fn main() {
	mut wg := sync.new_waitgroup()
	wg.go(work)
	wg.wait()
	timer := time.new_timer(time.nanosecond)
	_ = <-timer.c
}
'
	custom_stack_c := pr_27962_generate_c(v3_bin, root, 'runtime_custom_stack', runtime_source,
		'-thread-stack-size 4194304')
	stack_define := '#define V_THREAD_STACK_SIZE 4194304'
	define_index := custom_stack_c.index(stack_define) or {
		assert false, custom_stack_c
		return
	}
	thread_helper_index := custom_stack_c.index('#ifndef V_SYNC_THREAD_HELPER_H') or {
		assert false, custom_stack_c
		return
	}
	assert define_index < thread_helper_index
	assert custom_stack_c.contains('static const size_t __v_thread_stack_size = V_THREAD_STACK_SIZE;')

	x86_c := pr_27962_generate_c(v3_bin, root, 'runtime_x86_stack', 'fn main() {}\n', '-arch i386')
	assert x86_c.contains('#define V_THREAD_STACK_SIZE 2097152'), x86_c

	windows_c := pr_27962_generate_c(v3_bin, root, 'runtime_windows', runtime_source,
		'-os windows -arch amd64')
	assert windows_c.contains('#define V_THREAD_STACK_SIZE 8388608'), windows_c
	assert windows_c.contains('SleepConditionVariableSRW'), windows_c
}
