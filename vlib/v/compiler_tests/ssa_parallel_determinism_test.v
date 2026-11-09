// Building function bodies in parallel must not change the program: the arm64
// output has to be the same for any thread count and any function order.
import os
import v.cmdexec

const program = "
interface Speaker {
	speak() int
}

struct Dog {
	age int
}

fn (d Dog) speak() int {
	return d.age + 1
}

struct Cat {
	lives int
}

fn (c Cat) speak() int {
	return c.lives * 2
}

struct Empty {}

const empty_value = Empty{}

struct Box[T] {
	item T
}

fn (b Box[T]) get() T {
	return b.item
}

fn maybe(n int) ?int {
	if n > 0 {
		return n
	}
	return none
}

fn pair(n int) (int, string) {
	return n * 2, n.str()
}

fn wide(a u64) u64 {
	x := u128(a) * u128(a)
	return u64(x >> 32)
}

fn sum(speakers []Speaker) int {
	mut total := 0
	for s in speakers {
		total += s.speak()
	}
	return total
}

fn main() {
	println(sum([Speaker(Dog{age: 2}), Cat{lives: 3}]))
	worker := spawn fn () int {
		return 5
	}()
	println(worker.wait())
	println(maybe(3) or { 0 })
	a, b := pair(4)
	println('\${a} \${b}')
	println(Box[string]{item: 'boxed'}.get())
	println(Box[int]{item: 9}.get())
	println(wide(0x1_0000_0001))
	_ := empty_value
	mut m := map[string]int{}
	m['k'] = 1
	println(m)
}
"

fn test_arm64_output_does_not_depend_on_parallel_schedule() {
	$if !macos || !arm64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'ssa_parallel_determinism_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'prog.v')
	os.write_file(source, program)!
	mut outputs := [][]u8{}
	for i, setting in ['V3_SSA_JOBS=1', 'V3_SSA_JOBS=2', 'V3_SSA_JOBS=64', 'V3_SSA_SHUFFLE=7',
		'V3_SSA_SHUFFLE=9001'] {
		dir := os.join_path(root, 'r${i}')
		os.mkdir_all(dir)!
		exe := os.join_path(dir, 'prog')
		name, value := setting.split_once('=') or { panic(setting) }
		os.setenv(name, value, true)
		compile := cmdexec.run(@VEXE, ['-b', 'arm64', '-o', exe, source])
		os.unsetenv(name)
		assert compile.exit_code == 0, compile.output
		run := cmdexec.run(exe, []string{})
		assert run.exit_code == 0, run.output
		assert run.output.trim_space().split_into_lines() == ['9', '5', '3', '8 4', 'boxed', '9',
			'4294967298', "{'k': 1}"]
		outputs << os.read_bytes(exe)!
	}
	for i in 1 .. outputs.len {
		assert outputs[i] == outputs[0], 'schedule ${i} changed the binary'
	}
}
