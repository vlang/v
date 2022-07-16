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
	println(term_highlight('Total runtime: $spent ms'))
	for ocmd in oks {
		msg := if ocmd.okmsg != '' { ocmd.okmsg } else { ocmd.line }
		println(term.colorize(term.green, '>          OK: $msg '))
	}
	for fcmd in fails {
		msg := if fcmd.errmsg != '' { fcmd.errmsg } else { fcmd.line }
		println(term.failed('>      Failed:') + ' $msg')
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

struct Command {
mut:
	line        string
	label       string // when set, the label will be printed *before* cmd.line is executed
	ecode       int
	okmsg       string
	errmsg      string
	rmfile      string
	runcmd      RunCommandKind = .system
	expect      string = expect_nothing
	starts_with string = starts_with_nothing
	ends_with   string = ends_with_nothing
	contains    string = contains_nothing
	output      string
}

fn get_all_commands() []Command {
	mut res := []Command{}
	res << Command{
		line: '$vexe examples/hello_world.v'
		okmsg: 'V can compile hello world.'
		rmfile: 'examples/hello_world'
	}
	res << Command{
		line: '$vexe -o hhww.c examples/hello_world.v'
		okmsg: 'V can output a .c file, without compiling further.'
		rmfile: 'hhww.c'
	}
	res << Command{
		line: '$vexe -skip-unused examples/hello_world.v'
		okmsg: 'V can compile hello world with -skip-unused.'
		rmfile: 'examples/hello_world'
	}
	res << Command{
		line: '$vexe -skip-unused -profile - examples/hello_world.v'
		okmsg: 'V can compile hello world with both -skip-unused and -profile .'
		rmfile: 'examples/hello_world'
	}
	$if linux || macos {
		res << Command{
			line: '$vexe run examples/hello_world.v'
			okmsg: 'V can run hello world.'
			runcmd: .execute
			expect: 'Hello, World!\n'
		}
		if os.getenv('V_CI_MUSL').len == 0 {
			for compiler_name in ['clang', 'gcc'] {
				if _ := os.find_abs_path_of_executable(compiler_name) {
					res << Command{
						line: '$vexe -cc $compiler_name -gc boehm run examples/hello_world.v'
						okmsg: '`v -cc $compiler_name -gc boehm run examples/hello_world.v` works'
						runcmd: .execute
						expect: 'Hello, World!\n'
					}
				}
			}
		}
		res << Command{
			line: '$vexe interpret examples/hello_world.v'
			okmsg: 'V can interpret hello world.'
			runcmd: .execute
			expect: 'Hello, World!\n'
		}
		res << Command{
			line: '$vexe interpret examples/hanoi.v'
			okmsg: 'V can interpret hanoi.v'
			runcmd: .execute
			starts_with: 'Disc 1 from A to C...\n'
			ends_with: 'Disc 1 from A to C...\n'
			contains: 'Disc 7 from A to C...\n'
		}
		res << Command{
			line: '$vexe -o - examples/hello_world.v | grep "#define V_COMMIT_HASH" > /dev/null'
			okmsg: 'V prints the generated source code to stdout with `-o -` .'
		}
		res << Command{
			line: '$vexe run examples/v_script.vsh > /dev/null'
			okmsg: 'V can run the .VSH script file examples/v_script.vsh'
		}
		$if linux {
			res << Command{
				line: '$vexe -b native run examples/native/hello_world.v > /dev/null'
				okmsg: 'V compiles and runs examples/native/hello_world.v on the native backend for linux'
			}
		}
		// only compilation:
		res << Command{
			line: '$vexe -os linux -b native -o hw.linux examples/hello_world.v'
			okmsg: 'V compiles hello_world.v on the native backend for linux'
			rmfile: 'hw.linux'
		}
		res << Command{
			line: '$vexe -os macos -b native -o hw.macos examples/hello_world.v'
			okmsg: 'V compiles hello_world.v on the native backend for macos'
			rmfile: 'hw.macos'
		}
		res << Command{
			line: '$vexe -os windows -b native -o hw.exe examples/hello_world.v'
			okmsg: 'V compiles hello_world.v on the native backend for windows'
			rmfile: 'hw.exe'
		}
		//
		res << Command{
			line: '$vexe -b js -o hw.js examples/hello_world.v'
			okmsg: 'V compiles hello_world.v on the JS backend'
			rmfile: 'hw.js'
		}
		res << Command{
			line: '$vexe -skip-unused -b js -o hw_skip_unused.js examples/hello_world.v'
			okmsg: 'V compiles hello_world.v on the JS backend, with -skip-unused'
			rmfile: 'hw_skip_unused.js'
		}
	}
	res << Command{
		line: '$vexe -o vtmp cmd/v'
		okmsg: 'V can compile itself.'
		rmfile: 'vtmp'
	}
	res << Command{
		line: '$vexe -o vtmp_werror -cstrict cmd/v'
		okmsg: 'V can compile itself with -cstrict.'
		rmfile: 'vtmp_werror'
	}
	res << Command{
		line: '$vexe -o vtmp_autofree -autofree cmd/v'
		okmsg: 'V can compile itself with -autofree.'
		rmfile: 'vtmp_autofree'
	}
	res << Command{
		line: '$vexe -o vtmp_prealloc -prealloc cmd/v'
		okmsg: 'V can compile itself with -prealloc.'
		rmfile: 'vtmp_prealloc'
	}
	res << Command{
		line: '$vexe -o vtmp_unused -skip-unused cmd/v'
		okmsg: 'V can compile itself with -skip-unused.'
		rmfile: 'vtmp_unused'
	}
	$if linux {
		res << Command{
			line: '$vexe -cc gcc -keepc -freestanding -o bel vlib/os/bare/bare_example_linux.v'
			okmsg: 'V can compile with -freestanding on Linux with GCC.'
			rmfile: 'bel'
		}

		res << Command{
			line: '$vexe -cc gcc -keepc -freestanding -o str_array vlib/strconv/bare/str_array_example.v'
			okmsg: 'V can compile & allocate memory with -freestanding on Linux with GCC.'
			rmfile: 'str_array'
		}
	}
	res << Command{
		line: '$vexe $vargs -progress test-cleancode'
		okmsg: 'All .v files are invariant when processed with `v fmt`'
	}
	res << Command{
		line: '$vexe $vargs -progress test-fmt'
		okmsg: 'All .v files can be processed with `v fmt`. Note: the result may not always be compilable, but `v fmt` should not crash.'
	}
	res << Command{
		line: '$vexe $vargs -progress test-self'
		okmsg: 'There are no _test.v file regressions.'
	}
	res << Command{
		line: '$vexe $vargs -progress -W build-tools'
		okmsg: 'All tools can be compiled.'
	}
	res << Command{
		line: '$vexe $vargs -progress -W build-examples'
		okmsg: 'All examples can be compiled.'
	}
	res << Command{
		line: '$vexe check-md -hide-warnings .'
		label: 'Check ```v ``` code examples and formatting of .MD files...'
		okmsg: 'All .md files look good.'
	}
	res << Command{
		line: '$vexe install nedpals.args'
		okmsg: '`v install` works.'
	}
	res << Command{
		line: '$vexe -usecache -cg examples/hello_world.v'
		okmsg: '`v -usecache -cg` works.'
		rmfile: 'examples/hello_world'
	}
	// Note: test that a program that depends on thirdparty libraries with its
	// own #flags (tetris depends on gg, which uses sokol) can be compiled
	// with -usecache:
	res << Command{
		line: '$vexe -usecache examples/tetris/tetris.v'
		okmsg: '`v -usecache` works.'
		rmfile: 'examples/tetris/tetris'
	}
	$if macos || linux {
		res << Command{
			line: '$vexe -o v.c cmd/v && cc -Werror v.c -lpthread -lm && rm -rf a.out'
			label: 'v.c should be buildable with no warnings...'
			okmsg: 'v.c can be compiled without warnings. This is good :)'
			rmfile: 'v.c'
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
	//
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
	//
	run_label := if is_failed { term.failed('FAILED') } else { term_highlight('OK') }
	println('> Running: "$cmd.line" took: $spent ms ... $run_label')
	//
	if is_failed && is_failed_expected {
		eprintln('> expected:\n$cmd.expect')
		eprintln('>   output:\n$cmd.output')
	}
	if is_failed && is_failed_starts_with {
		eprintln('> expected to start with:\n$cmd.starts_with')
		eprintln('>                 output:\n${cmd.output#[..cmd.starts_with.len]}')
	}
	if is_failed && is_failed_ends_with {
		eprintln('> expected to end with:\n$cmd.ends_with')
		eprintln('>               output:\n${cmd.output#[-cmd.starts_with.len..]}')
	}
	if is_failed && is_failed_contains {
		eprintln('> expected to contain:\n$cmd.contains')
		eprintln('>              output:\n$cmd.output')
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
			eprintln('Expected file did not exist: $cmd.rmfile')
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
