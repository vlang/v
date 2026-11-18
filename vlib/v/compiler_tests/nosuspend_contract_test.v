import os

struct NosuspendCase {
	name     string
	accepted bool
	source   string
}

fn test_nosuspend_contracts_in_build_and_check() {
	root := os.join_path(os.vtmp_dir(), 'nosuspend_contracts_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	cases := [
		NosuspendCase{
			name:     'pure_allocating_body'
			accepted: true
			source:   '@[nosuspend]\nfn handler(n int) int { return [n, 1].len }\nfn main() { _ = handler(3) }\n'
		},
		NosuspendCase{
			name:     'empty_function'
			accepted: true
			source:   '@[nosuspend]\nfn handler() {}\nfn main() { handler() }\n'
		},
		NosuspendCase{
			name:     'transitive_pure'
			accepted: true
			source:   'fn double(n int) int { return n * 2 }\n@[nosuspend]\nfn handler(n int) int { return double(n) + 1 }\nfn main() { _ = handler(2) }\n'
		},
		NosuspendCase{
			name:     'direct_spawn'
			accepted: false
			source:   'fn worker() {}\n@[nosuspend]\nfn handler() { spawn worker() }\nfn main() { handler() }\n'
		},
		NosuspendCase{
			name:     'transitive_spawn'
			accepted: false
			source:   'fn worker() {}\nfn start() { spawn worker() }\n@[nosuspend]\nfn handler() { start() }\nfn main() { handler() }\n'
		},
		NosuspendCase{
			name:     'channel_send'
			accepted: false
			source:   '@[nosuspend]\nfn handler(ch chan int) { ch <- 1 }\nfn main() { ch := chan int{cap: 1}; handler(ch) }\n'
		},
		NosuspendCase{
			name:     'channel_receive'
			accepted: false
			source:   '@[nosuspend]\nfn handler(ch chan int) int { return <-ch }\nfn main() { ch := chan int{cap: 1}; _ = handler(ch) }\n'
		},
		NosuspendCase{
			name:     'transitive_channel'
			accepted: false
			source:   'fn push(ch chan int) { ch <- 1 }\n@[nosuspend]\nfn handler(ch chan int) { push(ch) }\nfn main() { ch := chan int{cap: 1}; handler(ch) }\n'
		},
		NosuspendCase{
			name:     'select_stmt'
			accepted: false
			source:   '@[nosuspend]\nfn handler(a chan int, b chan int) int { select { x := <-a { return x } y := <-b { return y } }; return 0 }\nfn main() {}\n'
		},
		NosuspendCase{
			name:     'lock_stmt'
			accepted: false
			source:   '@[nosuspend]\nfn handler(shared arr []int) { lock arr { unsafe { arr[0] = 1 } } }\nfn main() {}\n'
		},
		NosuspendCase{
			name:     'time_sleep'
			accepted: false
			source:   'import time\n@[nosuspend]\nfn handler() { time.sleep(1) }\nfn main() { handler() }\n'
		},
		NosuspendCase{
			name:     'transitive_sleep'
			accepted: false
			source:   'import time\nfn nap() { time.sleep(1) }\n@[nosuspend]\nfn handler() { nap() }\nfn main() { handler() }\n'
		},
		NosuspendCase{
			name:     'mutex_lock'
			accepted: false
			source:   'import sync\n@[nosuspend]\nfn handler(mut m sync.Mutex) { m.lock(); m.unlock() }\nfn main() { mut m := sync.Mutex{}; handler(mut m) }\n'
		},
		NosuspendCase{
			name:     'waitgroup_wait'
			accepted: false
			source:   'import sync\n@[nosuspend]\nfn handler(mut wg sync.WaitGroup) { wg.wait() }\nfn main() { mut wg := sync.WaitGroup{}; handler(mut wg) }\n'
		},
		NosuspendCase{
			name:     'semaphore_wait'
			accepted: false
			source:   'import sync\n@[nosuspend]\nfn handler(mut s sync.Semaphore) { s.wait() }\nfn main() { mut s := sync.Semaphore{}; handler(mut s) }\n'
		},
		NosuspendCase{
			name:     'foreign_contract'
			accepted: true
			source:   '@[nosuspend]\nfn C.pure_external(n int) int\n@[nosuspend]\nfn handler() int { return C.pure_external(1) }\nfn main() { _ = handler() }\n'
		},
		NosuspendCase{
			name:     'foreign_unknown'
			accepted: false
			source:   'fn C.external(n int) int\n@[nosuspend]\nfn handler() int { return C.external(1) }\nfn main() { _ = handler() }\n'
		},
		NosuspendCase{
			name:     'callback_alias'
			accepted: false
			source:   'fn invoke(f fn () int) int { return f() }\n@[nosuspend]\nfn handler(f fn () int) int { return invoke(f) }\nfn main() { _ = handler(fn () int { return 1 }) }\n'
		},
		NosuspendCase{
			name:     'interface_dispatch'
			accepted: false
			source:   "interface Speaker { speak() string }\nstruct Cat {}\nfn (c Cat) speak() string { return 'meow' }\n@[nosuspend]\nfn handler(s Speaker) string { return s.speak() }\nfn main() { _ = handler(Cat{}) }\n"
		},
		NosuspendCase{
			name:     'bound_method'
			accepted: false
			source:   'struct App { base int }\nfn (app &App) one(x int) int { return app.base + x }\n@[nosuspend]\nfn handler(app &App) int { method := unsafe { app.one }; return method(1) }\nfn main() { _ = handler(&App{base: 1}) }\n'
		},
		NosuspendCase{
			name:     'generic_suspend'
			accepted: false
			source:   'import time\nfn nap[T](v T) { time.sleep(1) }\n@[nosuspend]\nfn handler() { nap[int](1) }\nfn main() { handler() }\n'
		},
		NosuspendCase{
			name:     'generic_pure'
			accepted: true
			source:   '@[nosuspend]\nfn identity[T](v T) T { return v }\nfn main() { _ = identity[int](2) }\n'
		},
		NosuspendCase{
			name:     'invalid_argument'
			accepted: false
			source:   '@[nosuspend: strict]\nfn handler() int { return 1 }\nfn main() { _ = handler() }\n'
		},
		NosuspendCase{
			name:     'unsupported_alias'
			accepted: false
			source:   '@[nosuspend]\ntype Callback = fn () int\nfn main() { _ = Callback(fn () int { return 1 }) }\n'
		},
		NosuspendCase{
			name:     'terminal'
			accepted: true
			source:   "@[nosuspend]\nfn handler() { panic('stopped') }\nfn main() { if false { handler() } }\n"
		},
		NosuspendCase{
			name:     'chain_report'
			accepted: false
			source:   'import time\nfn leaf() { time.sleep(1) }\nfn middle() { leaf() }\n@[nosuspend]\nfn handler() { middle() }\nfn main() { handler() }\n'
		},
		NosuspendCase{
			name:     'local_shadow_direct'
			accepted: false
			source:   'import time\nfn callback() int { return 1 }\nfn blocking() { time.sleep(1) }\n@[nosuspend]\nfn handler() { callback := blocking; callback() }\nfn main() { handler() }\n'
		},
		NosuspendCase{
			name:     'local_shadow_transitive'
			accepted: false
			source:   'import time\nfn callback() int { return 1 }\nfn blocking() { time.sleep(1) }\nfn invoke(f fn ()) { callback := f; callback() }\n@[nosuspend]\nfn handler() { invoke(blocking) }\nfn main() { handler() }\n'
		},
		NosuspendCase{
			name:     'shadowed_terminal_module'
			accepted: false
			source:   "import time\ntype Callback = fn ()\nstruct Actions { exit Callback = unsafe { nil } }\nfn blocking() { println('entered blocking callback')\ntime.sleep(1) }\n@[nosuspend]\nfn handler() { os := Actions{exit: blocking}\nos.exit() }\nfn main() { handler() }\n"
		},
		NosuspendCase{
			name:     'shadowed_terminal_transitive'
			accepted: false
			source:   'import time\ntype Callback = fn ()\nstruct Actions { exit Callback = unsafe { nil } }\nfn blocking() { time.sleep(1) }\nfn middle() { os := Actions{exit: blocking}\nos.exit() }\n@[nosuspend]\nfn handler() { middle() }\nfn main() { handler() }\n'
		},
		NosuspendCase{
			name:     'struct_fn_field'
			accepted: false
			source:   'import time\ntype Callback = fn ()\nstruct Actions { exit Callback = unsafe { nil } }\nfn blocking() { time.sleep(1) }\n@[nosuspend]\nfn handler() { receiver := Actions{exit: blocking}\nreceiver.exit() }\nfn main() { handler() }\n'
		},
		NosuspendCase{
			name:     'terminal_builtin_exit'
			accepted: true
			source:   '@[nosuspend]\nfn handler() { exit(0) }\nfn main() { if false { handler() } }\n'
		},
		NosuspendCase{
			name:     'terminal_c_exit'
			accepted: true
			source:   '@[nosuspend]\nfn handler() { C.exit(0) }\nfn main() { if false { handler() } }\n'
		},
	]
	// The standalone FastC parser must reject contracts it cannot check.
	fastc_source := os.join_path(root, 'fastc.v')
	os.write_file(fastc_source, '@[nosuspend]\nfn handler() int { return 1 }\nfn main() { _ = handler() }\n') or {
		panic(err)
	}
	fastc_result := os.exec([@VEXE, '-b', 'fastc', '-o', os.join_path(root, 'fastc.c'), fastc_source])
	if !fastc_result.output.contains('fastc support is not compiled') {
		assert fastc_result.exit_code != 0 && fastc_result.output.contains('nosuspend'), fastc_result.output
	}

	for case in cases {
		source := os.join_path(root, '${case.name}.c.v')
		os.write_file(source, case.source) or { panic(err) }
		for mode in ['build', 'check'] {
			mut command := [@VEXE, '-gc', 'none']
			if mode == 'check' {
				command << '-check'
			} else {
				command << ['-o', os.join_path(root, '${case.name}.c')]
			}
			command << source
			result := os.exec(command)
			assert (result.exit_code == 0) == case.accepted, '${case.name} ${mode}: ${result.output}'
			if !case.accepted {
				assert result.output.contains('nosuspend'), '${case.name} ${mode}: ${result.output}'
			}
			if case.name == 'chain_report' {
				assert result.output.contains('handler -> middle'), result.output
			}
		}
	}
}
