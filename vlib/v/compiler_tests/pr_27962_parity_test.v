import os
import v.tempname

fn pr_27962_write_source(root string, name string, source string) string {
	path := os.join_path(root, '${name}.v')
	os.write_file(path, source) or { panic(err) }
	return path
}

fn pr_27962_compile_and_run(root string, name string, source string) string {
	source_path := pr_27962_write_source(root, name, source)
	binary_path := os.join_path(root, name)
	compile :=
		os.exec([@VEXE, '-silent', '-nocache', '-o', binary_path, source_path])
	assert compile.exit_code == 0, compile.output
	run := os.exec([binary_path])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn pr_27962_generate_c(root string, name string, source string, flags string) string {
	source_path := pr_27962_write_source(root, name, source)
	c_path := os.join_path(root, '${name}.c')
	compile :=
		os.exec([@VEXE, '-silent', '-nocache', ...(os.split_args(flags) or { panic(err) }), '-o',
			c_path, source_path])
	assert compile.exit_code == 0, compile.output
	return os.read_file(c_path) or { panic(err) }
}

fn test_pr_27962_regressions_are_fixed_in_v3() {
	root := os.join_path(os.vtmp_dir(), 'v3_pr_27962_${tempname.unique_token()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}

	output := pr_27962_compile_and_run(root, 'compiler_regressions', '
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
	timer := sync.new_timer(time.nanosecond)
	_ = <-timer.c
}
'
	custom_stack_c := pr_27962_generate_c(root, 'runtime_custom_stack', runtime_source,
		'-thread-stack-size 4194304')
	stack_define := '#define V_THREAD_STACK_SIZE 4194304'
	define_index := custom_stack_c.index(stack_define) or {
		assert false, custom_stack_c
		return
	}
	// The thread helpers that read the size follow its definition.
	stack_size_index :=
		custom_stack_c.index('static const size_t __v_thread_stack_size = V_THREAD_STACK_SIZE;') or {
			assert false, custom_stack_c
			return
		}
	assert define_index < stack_size_index

	x86_c := pr_27962_generate_c(root, 'runtime_x86_stack', 'fn main() {}\n', '-arch i386')
	assert x86_c.contains('#define V_THREAD_STACK_SIZE 2097152'), x86_c

	windows_c := pr_27962_generate_c(root, 'runtime_windows', runtime_source,
		'-os windows -arch amd64')
	assert windows_c.contains('#define V_THREAD_STACK_SIZE 8388608'), windows_c
	assert windows_c.contains('SleepConditionVariableSRW'), windows_c
}
