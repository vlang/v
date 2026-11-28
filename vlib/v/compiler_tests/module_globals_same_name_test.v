import os

// Several modules, and `main`, can each declare a global of the same name. Inside a
// module, the bare name is that module's own global, with its own type.
fn test_modules_and_main_keep_their_own_globals_of_one_name() {
	root := os.join_path(os.vtmp_dir(), 'module_globals_same_name_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'aa'))!
	os.mkdir_all(os.join_path(root, 'bb'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'aa', 'aa.v'), "module aa

struct Holder {
mut:
	value string
}

__global h = Holder{
	value: 'a'
}
__global counter = 10
__global callback = fn () string {
	return 'aa callback'
}

pub fn get() string {
	h.value += '!'
	counter += 1
	return h.value + ' ' + counter.str() + ' ' + callback()
}
")!
	os.write_file(os.join_path(root, 'aa', 'second.v'), 'module aa

pub fn second() string {
	return h.value + counter.str()
}
')!
	os.write_file(os.join_path(root, 'bb', 'bb.v'), 'module bb

struct Holder {
mut:
	value int
}

__global h = Holder{
	value: 7
}
__global counter = u8(3)
__global callback = fn () int {
	return 42
}

pub fn get() int {
	h.value *= 2
	counter++
	return h.value + int(counter) + callback()
}
')!
	os.write_file(os.join_path(root, 'main.v'), "import aa
import bb

__global h = 100
__global counter = 'main'

fn main() {
	h += 1
	println(h)
	println(counter)
	println(aa.get())
	println(aa.second())
	println(bb.get())
}
")!
	result := os.exec([@VEXE, '-enable-globals', 'run', root])
	assert result.exit_code == 0, result.output
	assert result.output.split_into_lines() == ['101', 'main', 'a! 11 aa callback', 'a!11', '60'], result.output
}
