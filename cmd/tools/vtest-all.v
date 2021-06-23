module main

import os
import term
import time

const vexe = os.getenv('VEXE')

const vroot = os.dir(vexe)

const args_string = os.args[1..].join(' ')

const vargs = args_string.all_before('test-all')

const vtest_nocleanup = os.getenv('VTEST_NOCLEANUP').bool()

fn main() {
	mut commands := get_all_commands()
	// summary
	sw := time.new_stopwatch({})
	for mut cmd in commands {
		cmd.run()
	}
	spent := sw.elapsed().milliseconds()
	oks := commands.filter(it.ecode == 0)
	fails := commands.filter(it.ecode != 0)
	println('')
	println(term.header_left(term_highlight('Summary of `v test-all`:'), '-'))
	println(term_highlight('Total runtime: $spent ms'))
	for ocmd in oks {
		msg := if ocmd.okmsg != '' { ocmd.okmsg } else { ocmd.line }
		println(term.colorize(term.green, '>          OK: $msg '))
	}
	for fcmd in fails {
		msg := if fcmd.errmsg != '' { fcmd.errmsg } else { fcmd.line }
		println(term.colorize(term.red, '>      Failed: $msg '))
	}
	if fails.len > 0 {
		exit(1)
	}
}

struct Command {
mut:
	line   string
	label  string // when set, the label will be printed *before* cmd.line is executed
	ecode  int
	okmsg  string
	errmsg string
	rmfile string
}

fn get_all_commands() []Command {
	mut res := []Command{}
	res << Command{
		line: '$vexe examples/hello_world.v'
		okmsg: 'V can compile hello world.'
		rmfile: 'examples/hello_world'
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
	}
	res << Command{
		line: '$vexe $vargs -progress test-cleancode'
		okmsg: 'All .v files are invariant when processed with `v fmt`'
	}
	res << Command{
		line: '$vexe $vargs -progress test-fmt'
		okmsg: 'All .v files can be processed with `v fmt`. NB: the result may not always be compilable, but `v fmt` should not crash.'
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
	// NB: test that a program that depends on thirdparty libraries with its
	// own #flags (tetris depends on gg, which uses sokol) can be compiled
	// with -usecache:
	res << Command{
		line: '$vexe -usecache examples/tetris/tetris.v'
		okmsg: '`v -usecache` works.'
		rmfile: 'examples/tetris/tetris'
	}
	$if macos {
		res << Command{
			line: '$vexe -o v.c cmd/v && cc -Werror v.c && rm -rf a.out'
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
	os.chdir(vroot)
	if cmd.label != '' {
		println(term.header_left(cmd.label, '*'))
	}
	sw := time.new_stopwatch({})
	cmd.ecode = os.system(cmd.line)
	spent := sw.elapsed().milliseconds()
	println(term_highlight('> Running: "$cmd.line" took: $spent ms.'))
	if vtest_nocleanup {
		return
	}
	if cmd.rmfile != '' {
		os.rm(cmd.rmfile) or {}
		if os.user_os() == 'windows' {
			os.rm(cmd.rmfile + '.exe') or {}
		}
	}
}

fn term_highlight(s string) string {
	return term.colorize(term.yellow, term.colorize(term.bold, s))
}
