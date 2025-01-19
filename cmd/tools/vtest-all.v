module main

import os
import term
import time

const vexe_path = os.getenv('VEXE')

const vroot = os.dir(vexe_path)

const vexe = os.quoted_path(vexe_path)

const args_string = os.args[1..].join(' ')

const vargs = args_string.all_before('test-all')

const vtest_nocleanup = os.getenv('VTEST_NOCLEANUP').bool()

const hw_native_no_builtin_size_limit = 300

const l2w_crosscc = os.find_abs_path_of_executable('x86_64-w64-mingw32-gcc-win32') or { '' }

const clang_path = os.find_abs_path_of_executable('clang') or { '' }

fn main() {
	mut commands := get_all_commands()
	// summary
	sw := time.new_stopwatch()
	for mut cmd in commands {
		cmd.run()
	}
	spent := sw.elapsed().milliseconds()
	oks := commands.filter(it.ecode == 0)
	fails := commands.filter(it.ecode != 0)
	flush_stdout()
	println('')
	println(term.header_left(term_highlight('Summary of `v test-all`:'), '-'))
	println(term_highlight('Total runtime: ${spent} ms'))
	for ocmd in oks {
		msg := if ocmd.okmsg != '' { ocmd.okmsg } else { ocmd.line }
		println(term.colorize(term.green, '>          OK: ${msg} '))
	}
	for fcmd in fails {
		msg := if fcmd.errmsg != '' { fcmd.errmsg } else { fcmd.line }
		println(term.failed('>      Failed:') + ' ${msg}')
	}
	flush_stdout()
	if fails.len > 0 {
		exit(1)
	}
}

enum RunCommandKind {
	system
	execute
}

const expect_nothing = '<nothing>'

const starts_with_nothing = '<nothing>'

const ends_with_nothing = '<nothing>'

const contains_nothing = '<nothing>'

type FnCheck = fn () !

struct Command {
mut:
	line        string
	label       string // when set, the label will be printed *before* cmd.line is executed
	ecode       int
	okmsg       string
	errmsg      string
	rmfile      string
	runcmd      RunCommandKind = .system
	expect      string         = expect_nothing
	starts_with string         = starts_with_nothing
	ends_with   string         = ends_with_nothing
	contains    string         = contains_nothing
	output      string
	before_cb   FnCheck = unsafe { nil }
	after_cb    FnCheck = unsafe { nil }
}

fn get_all_commands() []Command {
	mut res := []Command{}
	res << Command{
		line:   '${vexe} examples/hello_world.v'
		okmsg:  'V can compile hello world.'
		rmfile: 'examples/hello_world'
	}
	$if linux {
		if l2w_crosscc != '' {
			res << Command{
				line:   '${vexe} -os windows examples/hello_world.v'
				okmsg:  'V cross compiles hello_world.v on linux, to a windows .exe file'
				rmfile: 'examples/hello_world.exe'
			}
		} else {
			eprintln('Testing cross compilation from linux to windows needs x86_64-w64-mingw32-gcc-win32. Skipping hello_world.exe test.')
		}
	}
	res << Command{
		line:   '${vexe} -o hhww.c examples/hello_world.v'
		okmsg:  'V can output a .c file, without compiling further.'
		rmfile: 'hhww.c'
	}
	res << Command{
		line:   '${vexe} -skip-unused examples/hello_world.v'
		okmsg:  'V can compile hello world with -skip-unused.'
		rmfile: 'examples/hello_world'
	}
	res << Command{
		line:  '${vexe} -skip-unused test vlib/builtin'
		okmsg: 'V can test vlib/builtin with -skip-unused'
	}
	res << Command{
		line:   '${vexe} -skip-unused -profile - examples/hello_world.v'
		okmsg:  'V can compile hello world with both -skip-unused and -profile .'
		rmfile: 'examples/hello_world'
	}
	res << Command{
		line:   '${vexe} -e "print(84/2)"'
		okmsg:  'V can run code given after `-e`'
		runcmd: .execute
		expect: '42'
	}
	res << Command{
		line:   '${vexe} -e "import os; import math; print(os.args#[1..]) print(math.sin(math.pi/2).str())" arg1 arg2'
		okmsg:  'V can run code with `-e`, that use semicolons and several imports, and that accepts CLI parameters.'
		runcmd: .execute
		expect: "['arg1', 'arg2']1.0"
	}
	res << Command{
		line:     '${vexe} -o calling_c.exe run examples/call_c_from_v/main.c.v'
		okmsg:    'V can run main.c.v files'
		runcmd:   .execute
		contains: 'V can call C functions like `puts` too.'
	}
	$if linux || macos {
		res << Command{
			line:   '${vexe} run examples/hello_world.v'
			okmsg:  'V can run hello world.'
			runcmd: .execute
			expect: 'Hello, World!\n'
		}
		if clang_path != '' {
			res << Command{
				line:     '${vexe} -os freebsd -gc none examples/hello_world.v'
				okmsg:    'V cross compiles hello_world.v, to a FreeBSD executable'
				rmfile:   'examples/hello_world'
				after_cb: fn () ! {
					for file in ['examples/hello_world',
						os.join_path(os.vmodules_dir(), 'freebsdroot/usr/include/stdio.h')] {
						if !os.exists(file) {
							return error('>> file ${file} does not exist')
						}
					}
				}
			}
		} else {
			eprintln('Testing cross compilation to FreeBSD, needs clang.')
		}
		if os.getenv('V_CI_MUSL').len == 0 {
			for compiler_name in ['clang', 'gcc'] {
				if _ := os.find_abs_path_of_executable(compiler_name) {
					res << Command{
						line:   '${vexe} -cc ${compiler_name} -gc boehm run examples/hello_world.v'
						okmsg:  '`v -cc ${compiler_name} -gc boehm run examples/hello_world.v` works'
						runcmd: .execute
						expect: 'Hello, World!\n'
					}
				}
			}
		}
		res << Command{
			line:   '${vexe} interpret examples/hello_world.v'
			okmsg:  'V can interpret hello world.'
			runcmd: .execute
			expect: 'Hello, World!\n'
		}
		res << Command{
			line:        '${vexe} interpret examples/hanoi.v'
			okmsg:       'V can interpret hanoi.v'
			runcmd:      .execute
			starts_with: 'Disc 1 from A to C...\n'
			ends_with:   'Disc 1 from A to C...\n'
			contains:    'Disc 7 from A to C...\n'
		}
		res << Command{
			line:  '${vexe} -o - examples/hello_world.v | grep "#define V_COMMIT_HASH" > /dev/null'
			okmsg: 'V prints the generated source code to stdout with `-o -` .'
		}
		res << Command{
			line:  '${vexe} run examples/v_script.vsh > /dev/null'
			okmsg: 'V can run the .VSH script file examples/v_script.vsh'
		}
		// Note: -experimental is used here, just to suppress the warnings,
		// that are otherwise printed by the native backend,
		// until globals and hash statements *are implemented*:
		$if linux {
			res << Command{
				line:  '${vexe} -experimental -b native run examples/native/hello_world.v > /dev/null'
				okmsg: 'V compiles and runs examples/native/hello_world.v on the native backend for linux'
			}
			res << Command{
				line:     '${vexe} -no-builtin -experimental -b native examples/hello_world.v > /dev/null'
				okmsg:    'V compiles examples/hello_world.v on the native backend for linux with `-no-builtin` & the executable is <= ${hw_native_no_builtin_size_limit} bytes'
				rmfile:   'examples/hello_world'
				after_cb: fn () ! {
					file := 'examples/hello_world'
					if !os.exists(file) {
						return error('>> file ${file} does not exist')
					}
					if os.file_size(file) > hw_native_no_builtin_size_limit {
						return error('>> file ${file} bigger than ${hw_native_no_builtin_size_limit} bytes')
					}
				}
			}
		}
		// only compilation:
		res << Command{
			line:   '${vexe} -os linux -experimental -b native -o hw.linux examples/hello_world.v'
			okmsg:  'V compiles hello_world.v on the native backend for linux'
			rmfile: 'hw.linux'
		}
		res << Command{
			line:   '${vexe} -os macos -experimental -b native -o hw.macos examples/hello_world.v'
			okmsg:  'V compiles hello_world.v on the native backend for macos'
			rmfile: 'hw.macos'
		}
		$if windows {
			res << Command{
				line:   '${vexe} -os windows -experimental -b native -o hw.exe examples/hello_world.v'
				okmsg:  'V compiles hello_world.v on the native backend for windows'
				rmfile: 'hw.exe'
			}
		}
		//
		res << Command{
			line:   '${vexe} -b js -o hw.js examples/hello_world.v'
			okmsg:  'V compiles hello_world.v on the JS backend'
			rmfile: 'hw.js'
		}
		res << Command{
			line:   '${vexe} -skip-unused -b js -o hw_skip_unused.js examples/hello_world.v'
			okmsg:  'V compiles hello_world.v on the JS backend, with -skip-unused'
			rmfile: 'hw_skip_unused.js'
		}
		res << Command{
			line:   '${vexe} -skip-unused examples/2048'
			okmsg:  'V can compile 2048 with -skip-unused.'
			rmfile: 'examples/2048/2048'
		}
		if _ := os.find_abs_path_of_executable('emcc') {
			res << Command{
				line:   '${vexe} -os wasm32_emscripten examples/2048'
				okmsg:  'V can compile 2048 with -os wasm32_emscripten, using emcc.'
				rmfile: 'examples/2048/2048'
			}
		} else {
			println('> emcc not found, skipping `v -os wasm32_emscripten examples/2048`.')
		}
		res << Command{
			line:   '${vexe} -skip-unused  -live examples/hot_reload/bounce.v'
			okmsg:  'V can compile the hot code reloading bounce.v example with both: -skip-unused -live'
			rmfile: 'examples/hot_reload/bounce'
		}
	}
	res << Command{
		line:   '${vexe} -o vtmp cmd/v'
		okmsg:  'V can compile itself.'
		rmfile: 'vtmp'
	}
	res << Command{
		line:   '${vexe} -o vtmp_werror -cstrict cmd/v'
		okmsg:  'V can compile itself with -cstrict.'
		rmfile: 'vtmp_werror'
	}
	res << Command{
		line:   '${vexe} -o vtmp_autofree -autofree cmd/v'
		okmsg:  'V can compile itself with -autofree.'
		rmfile: 'vtmp_autofree'
	}
	res << Command{
		line:   '${vexe} -o vtmp_prealloc -prealloc cmd/v'
		okmsg:  'V can compile itself with -prealloc.'
		rmfile: 'vtmp_prealloc'
	}
	res << Command{
		line:   '${vexe} -o vtmp_unused -skip-unused cmd/v'
		okmsg:  'V can compile itself with -skip-unused.'
		rmfile: 'vtmp_unused'
	}
	$if linux {
		res << Command{
			line:   '${vexe} -o swait vlib/v/tests/reliability/semaphore_wait.v'
			okmsg:  'V can compile semaphore_wait.v on Linux with GC on.'
			rmfile: 'swait'
		}
		res << Command{
			line:   '${vexe} -cc gcc -keepc -freestanding -o bel vlib/os/bare/bare_example_linux.v'
			okmsg:  'V can compile with -freestanding on Linux with GCC.'
			rmfile: 'bel'
		}

		res << Command{
			line:   '${vexe} -cc gcc -keepc -freestanding -o str_array vlib/strconv/bare/str_array_example.v'
			okmsg:  'V can compile & allocate memory with -freestanding on Linux with GCC.'
			rmfile: 'str_array'
		}
	}
	////////////////////////////////////////////////////////////////////////
	// Test compilation of a shared library (.so, .dll. .dylib) with -shared:
	common_shared_flags := '-shared -skip-unused -d no_backtrace -o library examples/dynamic_library_loader/modules/library/library.v'
	$if macos {
		res << Command{
			line:   '${vexe} ${common_shared_flags}'
			okmsg:  'V compiles library.v with -shared on macos'
			rmfile: 'library.dylib'
		}
	}
	$if linux {
		res << Command{
			line:   '${vexe} ${common_shared_flags}'
			okmsg:  'V compiles library.v with -shared on linux'
			rmfile: 'library.so'
		}
	}
	// Test cross compilation to windows with -shared:
	$if linux {
		if l2w_crosscc != '' {
			res << Command{
				line:   '${vexe} -os windows ${common_shared_flags}'
				okmsg:  'V cross compiles library.v with -shared on linux, to a windows library.dll file'
				rmfile: 'library.dll'
			}
		} else {
			eprintln('Testing cross compilation from linux to windows needs x86_64-w64-mingw32-gcc-win32. Skipping library.dll test.')
		}
	}
	////////////////////////////////////////////////////////////////////////
	res << Command{
		line:  '${vexe} ${vargs} -progress test-cleancode'
		okmsg: 'All .v files are invariant when processed with `v fmt`'
	}
	res << Command{
		line:  '${vexe} ${vargs} -progress test-fmt'
		okmsg: 'All .v files can be processed with `v fmt`. Note: the result may not always be compilable, but `v fmt` should not crash.'
	}
	res << Command{
		line:  '${vexe} ${vargs} -progress test-self'
		okmsg: 'There are no _test.v file regressions.'
	}
	res << Command{
		line:  '${vexe} ${vargs} -progress -N -W build-tools'
		okmsg: 'All tools can be compiled.'
	}
	res << Command{
		line:  '${vexe} ${vargs} -progress -N -W build-examples'
		okmsg: 'All examples can be compiled.'
	}
	res << Command{
		line:  '${vexe} check-md -hide-warnings .'
		label: 'Check ```v ``` code examples and formatting of .MD files...'
		okmsg: 'All .md files look good.'
	}
	res << Command{
		line:  '${vexe} install nedpals.args'
		okmsg: '`v install` works.'
	}
	res << Command{
		okmsg:       'Running net.http with -d trace_http_request works.'
		line:        '${vexe} -d trace_http_request -e \'import net.http; x := http.fetch(url: "https://vpm.url4e.com/some/unknown/url")!; println(x.status_code)\''
		runcmd:      .execute
		starts_with: '> GET /some/unknown/url HTTP/1.1'
		contains:    'User-Agent: v.http'
		ends_with:   '404\n'
	}
	res << Command{
		okmsg:       'Running net.http with -d trace_http_response works.'
		line:        '${vexe} -d trace_http_response -e \'import net.http; x := http.fetch(url: "https://vpm.url4e.com/some/unknown/url")!; println(x.status_code)\''
		runcmd:      .execute
		starts_with: '< HTTP/1.1 404 Not Found'
		contains:    'Server: nginx'
		ends_with:   '404\n'
	}
	res << Command{
		line:   '${vexe} -usecache -cg examples/hello_world.v'
		okmsg:  '`v -usecache -cg` works.'
		rmfile: 'examples/hello_world'
	}
	// Note: test that a program that depends on thirdparty libraries with its
	// own #flags (tetris depends on gg, which uses sokol) can be compiled
	// with -usecache:
	res << Command{
		line:   '${vexe} -usecache examples/tetris/tetris.v'
		okmsg:  '`v -usecache` works.'
		rmfile: 'examples/tetris/tetris'
	}
	$if macos || linux {
		res << Command{
			line:   '${vexe} -o v.c cmd/v && cc -Werror v.c -lpthread -lm && rm -rf a.out'
			label:  'v.c should be buildable with no warnings...'
			okmsg:  'v.c can be compiled without warnings. This is good :)'
			rmfile: 'v.c'
		}
	}
	$if linux || macos {
		res << Command{
			line:   '${vexe} -gc none -no-retry-compilation -cc tcc -d use_openssl examples/veb/todo/main.v'
			okmsg:  'A simple veb app, compiles with `-gc none -no-retry-compilation -cc tcc -d use_openssl` on macos and linux'
			rmfile: 'examples/veb/todo/main'
		}
	}
	$if linux {
		res << Command{
			line:     '${vexe} vlib/v/tests/bench/bench_stbi_load.v && prlimit -v10485760 vlib/v/tests/bench/bench_stbi_load'
			okmsg:    'STBI load does not leak with GC on, when loading images multiple times (use < 10MB)'
			runcmd:   .execute
			contains: 'logo.png 1000 times.'
			rmfile:   'vlib/v/tests/bench/bench_stbi_load'
		}
	}
	$if !windows {
		res << Command{
			line:   '${vexe} -raw-vsh-tmp-prefix tmp vlib/v/tests/script_with_no_extension'
			okmsg:  'V can crun a script, that lacks a .vsh extension'
			runcmd: .execute
			expect: 'Test\n'
			rmfile: 'vlib/v/tests/tmp.script_with_no_extension'
		}

		res << Command{
			line:   '${vexe} -raw-vsh-tmp-prefix tmp run vlib/v/tests/script_with_no_extension'
			okmsg:  'V can run a script, that lacks a .vsh extension'
			runcmd: .execute
			expect: 'Test\n'
		}
	}
	return res
}

fn (mut cmd Command) run() {
	// Changing the current directory is needed for some of the compiler tests,
	// vlib/v/tests/local_test.v and vlib/v/tests/repl/repl_test.v
	os.chdir(vroot) or {}
	if cmd.label != '' {
		println(term.header_left(cmd.label, '*'))
	}
	if cmd.before_cb != unsafe { nil } {
		cmd.before_cb() or {
			cmd.ecode = -1
			cmd.errmsg = '> before_cb callback for "${cmd.line}" ${term.failed('FAILED')}\n${err}'
			println(cmd.errmsg)
			return
		}
	}
	sw := time.new_stopwatch()
	if cmd.runcmd == .system {
		cmd.ecode = os.system(cmd.line)
		cmd.output = ''
	}
	if cmd.runcmd == .execute {
		res := os.execute(cmd.line)
		cmd.ecode = res.exit_code
		cmd.output = res.output
	}
	spent := sw.elapsed().milliseconds()
	if cmd.after_cb != unsafe { nil } {
		cmd.after_cb() or {
			cmd.ecode = -1
			cmd.errmsg = '> after_cb callback for "${cmd.line}" ${term.failed('FAILED')}\n${err}'
			println(cmd.errmsg)
			return
		}
	}

	mut is_failed := false
	mut is_failed_expected := false
	mut is_failed_starts_with := false
	mut is_failed_ends_with := false
	mut is_failed_contains := false
	if cmd.ecode != 0 {
		is_failed = true
	}
	if cmd.expect != expect_nothing {
		if cmd.output != cmd.expect {
			is_failed = true
			is_failed_expected = true
		}
	}
	if cmd.starts_with != starts_with_nothing {
		if !cmd.output.starts_with(cmd.starts_with) {
			is_failed = true
			is_failed_starts_with = true
		}
	}
	if cmd.ends_with != ends_with_nothing {
		if !cmd.output.ends_with(cmd.ends_with) {
			is_failed = true
			is_failed_ends_with = true
		}
	}
	if cmd.contains != contains_nothing {
		if !cmd.output.contains(cmd.contains) {
			is_failed = true
			is_failed_contains = true
		}
	}

	run_label := if is_failed { term.failed('FAILED') } else { term_highlight('OK') }
	println('> Running: "${cmd.line}" took: ${spent} ms ... ${run_label}')

	if is_failed && is_failed_expected {
		eprintln('> expected:\n${cmd.expect}')
		eprintln('>   output:\n${cmd.output}')
	}
	if is_failed && is_failed_starts_with {
		eprintln('> expected to start with:\n${cmd.starts_with}')
		eprintln('>                 output:\n${cmd.output#[..cmd.starts_with.len]}')
	}
	if is_failed && is_failed_ends_with {
		eprintln('> expected to end with:\n${cmd.ends_with}')
		eprintln('>               output:\n${cmd.output#[-cmd.starts_with.len..]}')
	}
	if is_failed && is_failed_contains {
		eprintln('> expected to contain:\n${cmd.contains}')
		eprintln('>              output:\n${cmd.output}')
	}
	if vtest_nocleanup {
		return
	}
	if cmd.rmfile != '' {
		mut file_existed := rm_existing(cmd.rmfile)
		if os.user_os() == 'windows' {
			file_existed = file_existed || rm_existing(cmd.rmfile + '.exe')
		}
		if !file_existed {
			eprintln('Expected file did not exist: ${cmd.rmfile}')
			cmd.ecode = 999
		}
	}
}

// try to remove a file, return if it existed before the removal attempt
fn rm_existing(path string) bool {
	existed := os.exists(path)
	os.rm(path) or {}
	return existed
}

fn term_highlight(s string) string {
	return term.colorize(term.yellow, term.colorize(term.bold, s))
}
